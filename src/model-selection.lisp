(in-package :cl-tensor-decomposition)

;;;; model-selection.lisp - Rank selection by Poisson count thinning.
;;;;
;;;; A coordinate that is absent from a sparse tensor is an observed zero, not a
;;;; missing value, so holding out whole coordinates is not a valid split: the
;;;; training tensor would present each held-out cell as a zero, and the model
;;;; would then be scored on recovering a positive count it was fitted to
;;;; suppress. Splitting the *counts* instead keeps every coordinate present in
;;;; both halves and matches the Poisson model the decomposition assumes.

;;; ============================================================
;;; Count thinning
;;; ============================================================

(defstruct (poisson-folds (:constructor %make-poisson-folds))
  "A set of K cross-validation folds produced by Poisson count thinning.

TENSOR            - The original sparse-tensor, never modified
K                 - Number of folds
VALIDATION-COUNTS - (nnz x K) fixnum array; entry (i, f) is the number of
                    events at stored coordinate i assigned to fold f

The fold tensors themselves are materialized on demand by POISSON-FOLD-TENSORS,
so the retained memory is O(nnz * K) fixnums rather than K full copies."
  (tensor nil :read-only t)
  (k 0 :type fixnum :read-only t)
  (validation-counts nil :type (simple-array fixnum (* *)) :read-only t))

(defun %count-value-as-integer (value position)
  "Return VALUE as a non-negative integer count, or signal INVALID-INPUT-ERROR.

Counts are stored as double-floats, so this rejects fractional, negative and
non-finite values rather than silently truncating them."
  (when (%float-nan-p value)
    (error 'invalid-input-error
           :reason :nan-value
           :details (format nil "count[~D] is NaN" position)))
  (when (%float-infinity-p value)
    (error 'invalid-input-error
           :reason :infinite-value
           :details (format nil "count[~D] is infinite" position)))
  (when (< value 0.0d0)
    (error 'invalid-input-error
           :reason :negative-value
           :details (format nil "count[~D]=~F is negative" position value)))
  (let ((rounded (fround value)))
    (unless (= rounded value)
      (error 'invalid-input-error
             :reason :non-integer-count
             :details (format nil "count[~D]=~F is not an integer; Poisson thinning splits whole events"
                              position value)))
    (values (truncate rounded))))

(defun %tensor-integer-counts (tensor)
  "Return the counts of TENSOR as a fixnum vector, validating each value.

Also signals INVALID-INPUT-ERROR when the tensor carries no events at all."
  (let* ((values (sparse-tensor-values tensor))
         (nnz (length values))
         (counts (make-array nnz :element-type 'fixnum :initial-element 0))
         (total 0))
    (loop for i from 0 below nnz
          do (let ((count (%count-value-as-integer (aref values i) i)))
               (setf (aref counts i) count)
               (incf total count)))
    (when (<= total 0)
      (error 'invalid-input-error
             :reason :empty-tensor
             :details "tensor has a total count of 0; nothing to cross-validate"))
    (values counts total)))

(defun %thin-counts (counts k random-state)
  "Assign every event to one of K folds uniformly at random.

Returns an (nnz x K) fixnum array of validation counts. Row i is a Multinomial
draw with parameters (COUNTS[i]; 1/K, ..., 1/K), obtained by walking the events
of the cell one at a time - no event objects and no per-event storage.

Runs in O(total-count) time and O(nnz * K) space."
  (declare (type (simple-array fixnum (*)) counts)
           (type fixnum k))
  (let* ((nnz (length counts))
         (validation (make-array (list nnz k) :element-type 'fixnum
                                              :initial-element 0)))
    (loop for i from 0 below nnz
          do (loop repeat (aref counts i)
                   do (incf (aref validation i (random k random-state)))))
    validation))

(defun %fold-totals (validation-counts counts fold-index)
  "Return the total training and validation counts of FOLD-INDEX."
  (declare (type (simple-array fixnum (* *)) validation-counts)
           (type (simple-array fixnum (*)) counts))
  (let ((train 0)
        (valid 0))
    (loop for i from 0 below (length counts)
          do (let ((v (aref validation-counts i fold-index)))
               (incf valid v)
               (incf train (- (aref counts i) v))))
    (values train valid)))

(defun %minimum-total-count (k)
  "Smallest event count for which a uniform assignment reliably fills every fold.

A fold is left empty with probability (1 - 1/K)^N, so by the union bound

  P(some fold empty) <= K * (1 - 1/K)^N

Requiring that to be at most 1/2 gives N >= ln(2K) / ln(K / (K-1)). Below this
threshold a draw would usually leave a fold empty: K=10 with exactly 10 events
fills every fold only about 0.036% of the time. Accepting such input and then
failing to sample it would be misleading, so it is rejected up front instead.

The result is always at least K, so it subsumes the hard feasibility bound."
  (max k
       (ceiling (/ (log (* 2.0d0 k))
                   (log (/ (coerce k 'double-float) (1- k)))))))

(defun make-poisson-folds (tensor k &key random-state)
  "Split the counts of TENSOR into K cross-validation folds by Poisson thinning.

Each event at each coordinate is assigned to one fold uniformly at random:

  (V_i^1, ..., V_i^K) | X_i ~ Multinomial(X_i; 1/K, ..., 1/K)

Fold f then uses T_i^f = X_i - V_i^f for training and V_i^f for validation, so
the training exposure is (K-1)/K and the validation exposure is 1/K. Under
Poisson thinning the two halves are independent Poisson samples, which is what
makes this a valid split for a count model whose unstored coordinates are
observed zeros.

TENSOR       - sparse-tensor with non-negative integer counts
K            - number of folds; an integer >= 2. The tensor must also hold at
               least (%MINIMUM-TOTAL-COUNT K) events, the point below which a
               uniform assignment would usually leave some fold empty.
RANDOM-STATE - state used for the assignment; a copy is taken, so the caller's
               state is never advanced. Defaults to *RANDOM-STATE*.

Returns a POISSON-FOLDS structure. Signals INVALID-INPUT-ERROR for a tensor
that is not a valid count tensor, for K below 2, or for a tensor holding too
few events to fill K folds reliably."
  (unless (sparse-tensor-p tensor)
    (error 'invalid-input-error
           :reason :invalid-tensor
           :details (format nil "expected a sparse-tensor, got ~S" (type-of tensor))))
  (unless (and (integerp k) (>= k 2))
    (error 'invalid-input-error
           :reason :invalid-fold-count
           :details (format nil "k must be an integer >= 2, got ~S; a single fold leaves nothing to validate against"
                            k)))
  (multiple-value-bind (counts total) (%tensor-integer-counts tensor)
    (let ((minimum (%minimum-total-count k)))
      (when (< total minimum)
        (error 'invalid-input-error
               :reason :insufficient-counts
               :details (format nil "k=~D needs at least ~D events to fill every fold reliably, but the tensor holds ~D; use a smaller k"
                                k minimum total))))
    (let ((state (make-random-state (or random-state *random-state*))))
      ;; Past the threshold above, a single draw fills every fold at least half
      ;; the time, so 32 consecutive failures has probability below 2^-32.
      (loop for attempt from 1 to 32
            for validation = (%thin-counts counts k state)
            when (loop for fold-index from 0 below k
                       always (multiple-value-bind (train valid)
                                  (%fold-totals validation counts fold-index)
                                (and (plusp train) (plusp valid))))
              do (return (%make-poisson-folds :tensor tensor :k k
                                              :validation-counts validation))
            finally
               (error 'invalid-input-error
                      :reason :degenerate-fold
                      :details
                      (format nil "after ~D attempts some fold still had no training or no validation events (total count ~D, k=~D); use a smaller k"
                              attempt total k))))))

(defun poisson-folds-prediction-scale (folds)
  "Return the exposure ratio p_valid / p_train = 1 / (K - 1) for FOLDS.

A model fitted on a fold's training tensor predicts at the training exposure
(K-1)/K, while the validation counts were drawn at exposure 1/K, so validation
predictions must be multiplied by this ratio."
  (/ 1.0d0 (coerce (1- (poisson-folds-k folds)) 'double-float)))

(defun %fold-sub-tensor (tensor counts n-modes nnz)
  "Build a sparse-tensor from COUNTS, dropping cells whose count is zero.

The shape and domains of TENSOR are reused verbatim, so a mode value that
happens not to appear in this fold does not shrink the tensor."
  (declare (type (simple-array fixnum (*)) counts))
  (let ((kept (loop for i from 0 below nnz
                    count (plusp (aref counts i))))
        (source (sparse-tensor-indices tensor)))
    (let ((indices (make-array (list kept n-modes) :element-type 'fixnum))
          (values (make-array kept :element-type 'double-float))
          (row 0))
      (loop for i from 0 below nnz
            do (when (plusp (aref counts i))
                 (loop for mode from 0 below n-modes
                       do (setf (aref indices row mode) (aref source i mode)))
                 (setf (aref values row) (coerce (aref counts i) 'double-float))
                 (incf row)))
      (make-sparse-tensor (sparse-tensor-shape tensor) indices values
                          :domains (sparse-tensor-domains tensor)))))

(defun poisson-fold-tensors (folds fold-index)
  "Materialize the training and validation tensors of FOLD-INDEX.

Returns four values: the training sparse-tensor, the validation sparse-tensor,
the total training count and the total validation count. Cells with a count of
zero are omitted from the sparse representation; no dense array is built."
  (let* ((tensor (poisson-folds-tensor folds))
         (validation-counts (poisson-folds-validation-counts folds))
         (source-values (sparse-tensor-values tensor))
         (nnz (length source-values))
         (n-modes (array-dimension (sparse-tensor-indices tensor) 1))
         (train-counts (make-array nnz :element-type 'fixnum :initial-element 0))
         (valid-counts (make-array nnz :element-type 'fixnum :initial-element 0))
         (train-total 0)
         (valid-total 0))
    (loop for i from 0 below nnz
          do (let* ((original (truncate (fround (aref source-values i))))
                    (v (aref validation-counts i fold-index)))
               (setf (aref valid-counts i) v)
               (setf (aref train-counts i) (- original v))
               (incf valid-total v)
               (incf train-total (- original v))))
    (values (%fold-sub-tensor tensor train-counts n-modes nnz)
            (%fold-sub-tensor tensor valid-counts n-modes nnz)
            train-total
            valid-total)))

;;; ============================================================
;;; Fold scoring
;;; ============================================================

(defun normalized-generalized-kl (validation-tensor approximation
                                  factor-matrix-vector prediction-scale
                                  validation-count)
  "Default cross-validation fold score: generalized KL per validation event.

  score = D(V || s * T^) / sum_i V_i

D is the generalized KL of SPARSE-KL-DIVERGENCE, so it includes the model's
predicted mass on the implicit-zero coordinates, and s is PREDICTION-SCALE.
Dividing by the fold's total validation count makes folds of different sizes
comparable; lower is better.

This is *not* the Poisson deviance: the deviance is twice this quantity.

VALIDATION-TENSOR    - the fold's validation sparse-tensor
APPROXIMATION        - reconstruction at the validation coordinates, from SDOT
FACTOR-MATRIX-VECTOR - factors fitted on the fold's training tensor
PREDICTION-SCALE     - exposure ratio applied to every prediction
VALIDATION-COUNT     - total validation count of the fold"
  (/ (sparse-kl-divergence (sparse-tensor-indices validation-tensor)
                           (sparse-tensor-values validation-tensor)
                           approximation
                           factor-matrix-vector
                           :prediction-scale prediction-scale)
     (coerce validation-count 'double-float)))

(defun %evaluate-fold (train-tensor validation-tensor rank prediction-scale
                       validation-count init-random-state
                       &key (n-cycle 100) convergence-threshold convergence-window
                            (evaluation-function (function normalized-generalized-kl))
                            verbose)
  "Fit RANK factors on TRAIN-TENSOR and score them on VALIDATION-TENSOR.

INIT-RANDOM-STATE seeds the factor initialization; it is copied, so the same
fold produces the same starting point for every candidate rank."
  (let ((factor-matrix-vector
          (let ((*random-state* (make-random-state init-random-state)))
            (decomposition train-tensor :r rank :n-cycle n-cycle
                                        :convergence-threshold convergence-threshold
                                        :convergence-window convergence-window
                                        :verbose verbose))))
    (let ((approximation
            (make-array (length (sparse-tensor-values validation-tensor))
                        :element-type 'double-float :initial-element 0.0d0)))
      (sdot factor-matrix-vector (sparse-tensor-indices validation-tensor)
            approximation)
      (funcall evaluation-function validation-tensor approximation
               factor-matrix-vector prediction-scale validation-count))))

;;; ============================================================
;;; Cross-validation
;;; ============================================================

(defun %validate-ranks (ranks)
  "Signal INVALID-INPUT-ERROR unless RANKS is a non-empty list of positive integers."
  (unless (and (listp ranks) ranks)
    (error 'invalid-input-error
           :reason :empty-ranks
           :details "ranks must be a non-empty list of candidate ranks"))
  (dolist (rank ranks)
    (unless (and (integerp rank) (plusp rank))
      (error 'invalid-input-error
             :reason :invalid-rank
             :details (format nil "rank ~S is not a positive integer" rank)))))

(defun %spawn-init-states (k random-state)
  "Draw K independent random states from RANDOM-STATE, one per fold.

Generating them up front keeps a fold's initialization identical across
candidate ranks, so reordering RANKS cannot change any rank's scores."
  (loop repeat k
        collect (%seed-random-state (random (expt 2 31) random-state))))

(defun %summarize-scores (rank scores validation-counts k)
  "Build the result alist for RANK from its per-fold SCORES."
  (let* ((n (length scores))
         (mean (/ (reduce (function +) scores) (coerce n 'double-float)))
         (variance (if (> n 1)
                       (/ (reduce (function +) scores
                                  :key (lambda (score) (expt (- score mean) 2)))
                          (coerce (1- n) 'double-float))
                       0d0))
         (std (sqrt variance)))
    (list (cons :rank rank)
          (cons :mean mean)
          (cons :std std)
          (cons :standard-error (/ std (sqrt (coerce k 'double-float))))
          (cons :scores scores)
          (cons :validation-counts validation-counts))))

(defun cross-validate-rank (tensor ranks
                            &key (k 5) (n-cycle 100)
                                 convergence-threshold convergence-window
                                 (evaluation-function
                                  (function normalized-generalized-kl))
                                 random-state verbose)
  "Score each candidate rank by K-fold cross-validation over Poisson-thinned counts.

TENSOR       - sparse-tensor with non-negative integer counts
RANKS        - non-empty list of positive candidate ranks
K            - number of folds (default 5); integer >= 2, at most the total count
N-CYCLE, CONVERGENCE-THRESHOLD, CONVERGENCE-WINDOW control each fit
EVALUATION-FUNCTION - fold score; defaults to NORMALIZED-GENERALIZED-KL and is
               called as

                 (funcall fn validation-tensor approximation
                             factor-matrix-vector prediction-scale
                             validation-count)

               where APPROXIMATION holds the training-exposure reconstruction at
               the validation coordinates and PREDICTION-SCALE is the exposure
               ratio the metric must apply to it.
RANDOM-STATE - controls both the thinning and the per-fold factor
               initialization; it is copied, so the caller's state is never
               advanced. Defaults to *RANDOM-STATE*.
VERBOSE      - when true, report progress on *STANDARD-OUTPUT*; nothing is
               printed otherwise.

The folds are drawn once and shared by every candidate rank, and each fold's
initialization state is fixed in advance, so a rank's scores do not depend on
the order of RANKS.

Returns a list of result alists in the order of RANKS, each containing:
  :rank              - the candidate rank
  :mean              - mean fold score (lower is better)
  :std               - sample standard deviation of the fold scores
  :standard-error    - :std divided by sqrt(K)
  :scores            - the per-fold scores
  :validation-counts - the per-fold total validation counts

Signals INVALID-INPUT-ERROR for invalid ranks, an invalid K, or a tensor whose
counts are not usable non-negative integers."
  (%validate-ranks ranks)
  (let* ((state (make-random-state (or random-state *random-state*)))
         (folds (make-poisson-folds tensor k :random-state state))
         (prediction-scale (poisson-folds-prediction-scale folds))
         (init-states (%spawn-init-states k state))
         (fold-data (loop for fold-index from 0 below k
                          collect (multiple-value-list
                                   (poisson-fold-tensors folds fold-index))))
         (total (* (length ranks) k))
         (completed 0))
    (loop for rank in ranks
          collect (let ((scores '())
                        (counts '()))
                    (loop for datum in fold-data
                          for init-state in init-states
                          for fold-index from 0
                          do (destructuring-bind (train valid train-total valid-total)
                                 datum
                               (declare (ignore train-total))
                               (let ((score (%evaluate-fold
                                             train valid rank prediction-scale
                                             valid-total init-state
                                             :n-cycle n-cycle
                                             :convergence-threshold convergence-threshold
                                             :convergence-window convergence-window
                                             :evaluation-function evaluation-function
                                             :verbose verbose)))
                                 (push score scores)
                                 (push valid-total counts)
                                 (incf completed)
                                 (when verbose
                                   (format t "rank ~D fold ~D/~D: score ~,6F (~D/~D done)~%"
                                           rank (1+ fold-index) k score completed total)
                                   (finish-output)))))
                    (%summarize-scores rank (nreverse scores) (nreverse counts) k)))))

;;; ============================================================
;;; Rank selection
;;; ============================================================

(defun %best-result (cv-results)
  "Return the result with the lowest :MEAN, breaking ties toward the smaller rank.

Does not modify CV-RESULTS."
  (reduce (lambda (best candidate)
            (let ((best-mean (cdr (assoc :mean best)))
                  (candidate-mean (cdr (assoc :mean candidate))))
              (cond ((< candidate-mean best-mean) candidate)
                    ((> candidate-mean best-mean) best)
                    ((< (cdr (assoc :rank candidate)) (cdr (assoc :rank best)))
                     candidate)
                    (t best))))
          cv-results))

(defun select-rank (tensor ranks
                    &key (k 5) (n-cycle 100)
                         convergence-threshold convergence-window
                         (evaluation-function (function normalized-generalized-kl))
                         random-state verbose)
  "Select the rank with the lowest mean cross-validation score.

Arguments are passed through to CROSS-VALIDATE-RANK. Ties are broken toward the
smaller rank, and the returned result list is the one CROSS-VALIDATE-RANK
produced: same length, same order as RANKS, never sorted in place.

Returns two values:
  1. The result alist for the selected rank
  2. The complete list of cross-validation results

Example:
  (select-rank tensor '(2 4 8) :k 5 :n-cycle 50)"
  (let ((cv-results (cross-validate-rank tensor ranks
                                         :k k
                                         :n-cycle n-cycle
                                         :convergence-threshold convergence-threshold
                                         :convergence-window convergence-window
                                         :evaluation-function evaluation-function
                                         :random-state random-state
                                         :verbose verbose)))
    (values (%best-result cv-results) cv-results)))

(defun select-rank-1se (tensor ranks
                        &key (k 5) (n-cycle 100)
                             convergence-threshold convergence-window
                             (evaluation-function
                              (function normalized-generalized-kl))
                             random-state verbose)
  "Select the smallest rank within one standard error of the best mean score.

The threshold uses the standard error of the mean, std / sqrt(K), as is standard
in the cross-validation literature - not the raw standard deviation. Favouring
the simplest model inside that band guards against reading noise as signal.

Arguments are passed through to CROSS-VALIDATE-RANK. The returned result list is
never sorted in place.

Returns two values:
  1. The result alist for the selected rank
  2. The complete list of cross-validation results"
  (let* ((cv-results (cross-validate-rank tensor ranks
                                          :k k
                                          :n-cycle n-cycle
                                          :convergence-threshold convergence-threshold
                                          :convergence-window convergence-window
                                          :evaluation-function evaluation-function
                                          :random-state random-state
                                          :verbose verbose))
         (best (%best-result cv-results))
         (threshold (+ (cdr (assoc :mean best))
                       (cdr (assoc :standard-error best))))
         (selected (reduce (lambda (chosen candidate)
                             (if (and (<= (cdr (assoc :mean candidate)) threshold)
                                      (or (null chosen)
                                          (< (cdr (assoc :rank candidate))
                                             (cdr (assoc :rank chosen)))))
                                 candidate
                                 chosen))
                           cv-results
                           :initial-value nil)))
    (values (or selected best) cv-results)))

(defun poisson-folds-count (folds)
  "Return the number of folds in FOLDS. Alias for POISSON-FOLDS-K."
  (poisson-folds-k folds))
