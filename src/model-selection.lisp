(in-package :cl-tensor-decomposition)

(defun make-fold-splits (indices counts k &key (random-state *random-state*))
  "Create K randomized fold splits for cross-validation.

INDICES is the sparse tensor index matrix (NNZ x N-MODES).
COUNTS is the observation count vector (currently unused but kept for API consistency).
K is the number of folds to create.
RANDOM-STATE controls the random shuffling for reproducibility.

Returns a list of K sublists, each containing observation indices for that fold.
The observations are randomly shuffled before splitting to ensure unbiased folds."
  (declare (ignore counts))
  (let* ((nnz (array-dimension indices 0))
         (order (make-array nnz :element-type 'fixnum)))
    (loop for i from 0 below nnz do
      (setf (aref order i) i))
    (let ((*random-state* random-state))
      (loop for i from 0 below nnz do
        (let* ((j (+ i (random (- nnz i))))
               (tmp (aref order i)))
          (setf (aref order i) (aref order j))
          (setf (aref order j) tmp))))
    (loop with fold-size = (ceiling nnz k)
          for fold-index from 0 below k collect
            (loop for offset from (* fold-index fold-size)
                  below (min nnz (* (1+ fold-index) fold-size))
                  collect (aref order offset)))))

(defun %subset-tensor (indices counts subset)
  "Extract a subset of sparse tensor data by observation indices.

INDICES is the full sparse tensor index matrix.
COUNTS is the full observation count vector.
SUBSET is a list of observation indices to extract.

Returns two values:
  1. New index matrix containing only the subset rows
  2. New count vector containing only the subset values"
  (let* ((subset-size (length subset))
         (dimension (array-dimension indices 1))
         (sub-indices (make-array (list subset-size dimension)
                                  :element-type 'fixnum))
         (sub-counts (make-array subset-size :element-type 'double-float)))
    (loop for pos from 0 below subset-size
          for original-index in subset do
            (loop for dim from 0 below dimension do
              (setf (aref sub-indices pos dim)
                    (aref indices original-index dim)))
            (setf (aref sub-counts pos)
                  (coerce (aref counts original-index) 'double-float)))
    (values sub-indices sub-counts)))

(defun %tensor-dimensions (indices)
  "Compute the maximum index value for each tensor mode.

Returns a list of maximum indices (0-based) observed in each dimension.
Used internally by %BUILD-SHAPE to infer tensor shape from sparse data."
  (loop for dim from 0 below (array-dimension indices 1)
        collect (loop for row from 0 below (array-dimension indices 0)
                      maximize (aref indices row dim))))

(defun %build-shape (indices)
  "Infer tensor shape from sparse index matrix.

Returns a list of dimension sizes (1-based) by adding 1 to the maximum
index observed in each dimension. Note: This may underestimate dimensions
if some categories are unobserved in the data."
  (mapcar #'1+ (%tensor-dimensions indices)))

(defun %complement-subset (nnz subset)
  "Compute the complement of a subset of observation indices.

NNZ is the total number of observations.
SUBSET is a list of indices to exclude.

Returns a list of all indices from 0 to NNZ-1 that are NOT in SUBSET.
Used to create training sets from validation fold indices."
  (let ((mask (make-array nnz :element-type 'bit :initial-element 0)))
    (dolist (idx subset)
      (setf (aref mask idx) 1))
    (loop for i from 0 below nnz
          unless (= (aref mask i) 1)
            collect i)))

(defun %evaluate-fold
       (shape train-indices train-counts valid-indices valid-counts rank
        &key (n-cycle 100) convergence-threshold convergence-window
        (evaluation-function #'sparse-kl-divergence) verbose)
  "Evaluate a single fold of cross-validation.

Fits a decomposition model on training data and evaluates it on validation data.

SHAPE is the tensor dimensions.
TRAIN-INDICES, TRAIN-COUNTS are the training set sparse tensor.
VALID-INDICES, VALID-COUNTS are the validation set sparse tensor.
RANK is the number of latent factors to use.
N-CYCLE, CONVERGENCE-THRESHOLD, CONVERGENCE-WINDOW control decomposition.
EVALUATION-FUNCTION computes the validation score (default: sparse-kl-divergence).
VERBOSE controls output during decomposition.

Returns the evaluation score on the validation set."
  (let ((train-tensor (make-sparse-tensor shape train-indices train-counts)))
    (multiple-value-bind (factor-matrix-vector iterations)
        (decomposition train-tensor :r rank :n-cycle n-cycle
         :convergence-threshold convergence-threshold :convergence-window
         convergence-window :verbose verbose)
      (declare (ignore iterations))
      (let ((approx
             (make-array (length valid-counts) :element-type 'double-float
                         :initial-element 0.0d0)))
        (sdot factor-matrix-vector valid-indices approx)
        (funcall evaluation-function valid-indices valid-counts approx)))))

(defun cross-validate-rank (indices counts ranks &key (k 5)
                                    (n-cycle 100)
                                    convergence-threshold
                                    convergence-window
                                    (evaluation-function #'sparse-kl-divergence)
                                    random-state
                                    verbose)
  "Perform K-fold cross-validation to evaluate multiple rank values.

INDICES is the sparse tensor index matrix (NNZ x N-MODES).
COUNTS is the observation count vector.
RANKS is a list of rank values to evaluate.
K is the number of cross-validation folds (default 5).
N-CYCLE, CONVERGENCE-THRESHOLD, CONVERGENCE-WINDOW control decomposition.
EVALUATION-FUNCTION computes validation score (default: sparse-kl-divergence).
RANDOM-STATE controls fold randomization for reproducibility.
VERBOSE controls output during decomposition.

Returns a list of result alists, one per rank, each containing:
  :rank - the rank value tested
  :mean - mean validation score across folds
  :std  - standard deviation of validation scores
  :scores - list of individual fold scores

Lower mean scores indicate better fit (for KL divergence).
Progress is printed to *STANDARD-OUTPUT* during execution."
  (let* ((nnz (array-dimension indices 0))
         (shape (%build-shape indices))
         (folds (if random-state
                    (make-fold-splits indices counts k :random-state random-state)
                    (make-fold-splits indices counts k)))
         (total (* (length ranks) (length folds)))
         (completed 0)
         (results '()))
    (dolist (rank ranks)
      (let ((fold-scores '()))
        (dolist (subset folds)
          (multiple-value-bind (valid-indices valid-counts)
              (%subset-tensor indices counts subset)
            (multiple-value-bind (train-indices train-counts)
                (%subset-tensor indices counts (%complement-subset nnz subset))
              (push (%evaluate-fold shape
                                     train-indices train-counts
                                     valid-indices valid-counts rank
                                     :n-cycle n-cycle
                                     :convergence-threshold convergence-threshold
                                     :convergence-window convergence-window
                                     :evaluation-function evaluation-function
                                     :verbose verbose)
                    fold-scores)
               (incf completed)
               (let* ((ratio (/ completed (max 1 total)))
                      (percent (* 100d0 (coerce ratio 'double-float))))
                 (format t "Cross-validation progress ~D/~D (~,2F%%)~%"
                         completed total percent)
                 (finish-output)))))
        (let* ((mean (/ (reduce #'+ fold-scores) (length fold-scores)))
               (variance (if (> (length fold-scores) 1)
                             (/ (reduce #'+ fold-scores
                                        :key (lambda (score)
                                               (expt (- score mean) 2)))
                                (1- (length fold-scores)))
                             0d0))
               (std (sqrt variance)))
          (push (list (cons :rank rank)
                      (cons :mean mean)
                      (cons :std std)
                      (cons :scores (nreverse fold-scores)))
                results))))
    (nreverse results)))

(defun select-rank (indices counts ranks &key (k 5)
                             (n-cycle 100)
                             convergence-threshold
                             convergence-window
                             (evaluation-function #'sparse-kl-divergence)
                             random-state
                             verbose)
  "Select the best rank for tensor decomposition via cross-validation.

This is a convenience wrapper around CROSS-VALIDATE-RANK that returns
the rank with the lowest mean validation score.

INDICES is the sparse tensor index matrix (NNZ x N-MODES).
COUNTS is the observation count vector.
RANKS is a list of candidate rank values to evaluate.
Other parameters are passed through to CROSS-VALIDATE-RANK.

Returns two values:
  1. The result alist for the best rank (lowest mean validation score)
  2. The complete list of all cross-validation results

Example:
  (select-rank indices counts '(2 3 4 5) :k 5 :n-cycle 50)
  => ((:rank . 3) (:mean . 0.123) (:std . 0.015) (:scores . (...)))
     (((:rank . 2) ...) ((:rank . 3) ...) ...)"
  (let* ((cv-key-args (list :k k
                            :n-cycle n-cycle
                            :convergence-threshold convergence-threshold
                            :convergence-window convergence-window
                            :evaluation-function evaluation-function
                            :verbose verbose))
         (cv-key-args (if random-state
                          (append cv-key-args (list :random-state random-state))
                          cv-key-args))
         (cv-results (apply #'cross-validate-rank
                            indices counts ranks cv-key-args)))
    (values (car (sort cv-results #'<
                       :key (lambda (result)
                              (cdr (assoc :mean result)))))
            cv-results)))

(defun select-rank-1se
       (indices counts ranks
        &key (k 5) (n-cycle 100) convergence-threshold convergence-window
        (evaluation-function #'sparse-kl-divergence) random-state verbose)
  "Select rank using the 1-SE rule for tensor decomposition.

The 1-SE (one standard error) rule selects the simplest model (smallest rank)
whose validation score is within one standard error of the best model's mean.
This approach favors parsimony and helps prevent overfitting.

Note: The threshold uses the standard error of the mean (std / sqrt(k)),
not the standard deviation, as is standard in cross-validation literature.

INDICES is the sparse tensor index matrix (NNZ x N-MODES).
COUNTS is the observation count vector.
RANKS is a list of candidate rank values to evaluate.
K is the number of cross-validation folds (default 5).
Other parameters are passed through to CROSS-VALIDATE-RANK.

Returns two values:
  1. The result alist for the selected rank (simplest within 1-SE of best)
  2. The complete list of all cross-validation results

Example:
  (select-rank-1se indices counts '(2 3 4 5 6 7 8) :k 5 :n-cycle 50)
  ;; If rank=5 has best mean=0.10 with std=0.02, SE=0.02/sqrt(5)=0.009
  ;; threshold = 0.10 + 0.009 = 0.109
  ;; Selects smallest rank with mean <= 0.109
  => ((:rank . 4) (:mean . 0.108) (:std . 0.015) (:scores . (...)))
     (((:rank . 2) ...) ((:rank . 3) ...) ...)"
  (let* ((cv-key-args
          (list :k k :n-cycle n-cycle :convergence-threshold
                convergence-threshold :convergence-window convergence-window
                :evaluation-function evaluation-function :verbose verbose))
         (cv-key-args
          (if random-state
              (append cv-key-args (list :random-state random-state))
              cv-key-args))
         (cv-results
          (apply #'cross-validate-rank indices counts ranks cv-key-args))
         (sqrt-k (sqrt (coerce k 'double-float))))
    ;; Find the best result (lowest mean)
    (let* ((best (car (sort (copy-list cv-results) #'<
                            :key (lambda (r) (cdr (assoc :mean r))))))
           (best-mean (cdr (assoc :mean best)))
           (best-std (cdr (assoc :std best)))
           ;; Use standard error of the mean: std / sqrt(k)
           (best-se (/ best-std sqrt-k))
           (threshold (+ best-mean best-se))
           ;; Filter results within 1-SE threshold
           (within-threshold
            (remove-if (lambda (r) (> (cdr (assoc :mean r)) threshold))
                       cv-results))
           ;; Select smallest rank among those within threshold
           (selected (car (sort within-threshold #'<
                                :key (lambda (r) (cdr (assoc :rank r)))))))
      (values selected cv-results))))
