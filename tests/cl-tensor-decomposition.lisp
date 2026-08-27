(defpackage cltd-test
  (:use :cl
        :cltd
        :rove))
(in-package :cltd-test)

(defparameter X-shape '(2 3 4))
(defparameter X-indices-matrix
  (make-array '(3 3) :element-type 'fixnum
              :initial-contents '((0 1 0)            ; The row corresponds one datum
                                  (1 2 3)
                                  (0 0 1))))
(defparameter X-value-vector
  (make-array 3 :element-type 'double-float :initial-contents '(1.0d0 2.0d0 3.0d0)))

;; Pre-built sparse tensor for tests using the new API
(defparameter X-tensor
  (cltd:make-sparse-tensor X-shape X-indices-matrix X-value-vector))

(defparameter +test-epsilon+ 1d-6)

;; Cross-validation splits counts, not coordinates, so its fixture needs enough
;; events per cell to fill several folds. Shape is deliberately wider than the
;; observed maxima so fold tensors can be checked for shape preservation.
(defparameter CV-tensor
  (cltd:make-sparse-tensor
   '(3 4 2)
   (make-array '(6 3) :element-type 'fixnum
               :initial-contents '((0 0 0) (1 1 1) (2 2 0)
                                   (0 3 1) (1 0 1) (2 1 0)))
   (make-array 6 :element-type 'double-float
               :initial-contents '(12d0 8d0 15d0 6d0 20d0 9d0))))

;;; ---------------------------------------------------------------------------
;;; Generalized KL divergence over implicit zeros
;;;
;;; Unregistered coordinates are observed zeros, not missing values, so the
;;; loss must include their reconstruction mass.  The helpers below enumerate
;;; the dense tensor as a reference implementation; they exist for tests only
;;; and must never be used by library code.
;;; ---------------------------------------------------------------------------

(defun %all-coordinates (shape)
  "Return every coordinate of SHAPE as a list of index lists (test-only)."
  (if (null shape)
      (list '())
      (loop for i from 0 below (car shape)
            append (loop for rest in (%all-coordinates (cdr shape))
                         collect (cons i rest)))))

(defun %sparse-lookup (indices values coord)
  "Return the observed value stored at COORD, or 0 when absent (test-only)."
  (loop for row from 0 below (array-dimension indices 0)
        when (loop for mode from 0 below (array-dimension indices 1)
                   always (= (aref indices row mode) (nth mode coord)))
          do (return (aref values row))
        finally (return 0d0)))

(defun %cp-predict (factor-matrix-vector coord)
  "Evaluate the CP model at COORD by summing over rank-one components (test-only)."
  (loop for ri from 0 below (array-dimension (svref factor-matrix-vector 0) 1)
        sum (let ((prod 1d0))
              (loop for mode from 0 below (length factor-matrix-vector)
                    do (setf prod
                             (* prod (aref (svref factor-matrix-vector mode)
                                           (nth mode coord)
                                           ri))))
              prod)
        double-float))

(defun %dense-generalized-kl (shape factor-matrix-vector indices values
                              &optional (epsilon cltd::*epsilon*) (scale 1d0))
  "Reference generalized KL computed by enumerating the whole dense tensor (test-only).
SCALE multiplies every prediction, mirroring the exposure correction applied
during cross-validation; EPSILON is added after scaling, never scaled itself."
  (loop for coord in (%all-coordinates shape)
        sum (let ((x (%sparse-lookup indices values coord))
                  (x-hat (* scale (%cp-predict factor-matrix-vector coord))))
              (+ (if (> x 0d0)
                     (* x (log (/ x (+ x-hat epsilon))))
                     0d0)
                 (- x)
                 x-hat))
        double-float))

(defun %dense-total-mass (shape factor-matrix-vector)
  "Sum the CP reconstruction over every coordinate of SHAPE (test-only)."
  (loop for coord in (%all-coordinates shape)
        sum (%cp-predict factor-matrix-vector coord)
        double-float))

(deftest initialize-matrix-fills-matrix
  (let ((matrix (make-array '(2 2) :element-type 'double-float :initial-element 0d0)))
    (cltd:initialize-matrix matrix 2d0)
    (ok (loop for i from 0 below 2 always
              (loop for j from 0 below 2 always (= (aref matrix i j) 2d0)))
        "initialize-matrix fills matrix with default value")))

(deftest initialize-random-matrix-deterministic-with-seed
  (let* ((matrix (make-array '(2 3) :element-type 'double-float :initial-element 0d0))
         (state (make-random-state t))
         (expected-state (make-random-state state))
         (expected (make-array '(2 3) :element-type 'double-float)))
    (loop for i from 0 below 2 do
      (loop for j from 0 below 3 do
        (setf (aref expected i j) (random 1.0d0 expected-state))))
    (let ((*random-state* state))
      (cltd:initialize-random-matrix matrix))
    (let ((max-diff 0d0))
      (loop for i from 0 below 2 do
        (loop for j from 0 below 3 do
          (setf max-diff (max max-diff
                               (abs (- (aref matrix i j)
                                       (aref expected i j)))))))
      (ok (< max-diff +test-epsilon+)
          "initialize-random-matrix draws reproducible values"))))

(deftest sparse-kl-divergence-matches-manual
  (let* ((mode0 (make-array '(2 1) :element-type 'double-float
                            :initial-contents '((0.5d0) (1.5d0))))
         (mode1 (make-array '(3 1) :element-type 'double-float
                            :initial-contents '((0.4d0) (1.2d0) (0.9d0))))
         (mode2 (make-array '(4 1) :element-type 'double-float
                            :initial-contents '((1.1d0) (0.7d0) (0.3d0) (1.4d0))))
         (factors (make-array 3 :initial-contents (list mode0 mode1 mode2)))
         (values (make-array 3 :element-type 'double-float
                             :initial-contents '(1d0 2d0 3d0)))
         (approx (make-array 3 :element-type 'double-float :initial-element 0d0))
         (epsilon cltd::*epsilon*)
         (expected 0d0))
    (cltd:sdot factors X-indices-matrix approx)
    ;; Local term over the stored non-zeros ...
    (loop for idx from 0 below (length values) do
      (let* ((x (aref values idx))
             (xhat (aref approx idx)))
        (incf expected (- (* x (log (/ x (+ xhat epsilon))))
                          x))))
    ;; ... plus the predicted mass of every coordinate, implicit zeros included.
    (incf expected (%dense-total-mass X-shape factors))
    (ok (< (abs (- (cltd:sparse-kl-divergence X-indices-matrix values approx factors)
                   expected))
           +test-epsilon+)
        "sparse-kl-divergence matches manual computation")))

(deftest sdot-multiplies-factors-into-reconstruction
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.6d0 0.4d0)
                                                (0.2d0 0.8d0))))
         (mode1 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.5d0 0.5d0)
                                                (0.7d0 0.3d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(2 2) :element-type 'fixnum
                              :initial-contents '((0 1)
                                                  (1 0))))
         (approx (make-array 2 :element-type 'double-float :initial-element 0d0)))
    (cltd:sdot factor-matrices indices approx)
    (let ((expected '(0.54d0 0.5d0)))
      (ok (loop for idx from 0 below (length expected) always
                (< (abs (- (aref approx idx) (nth idx expected))) +test-epsilon+))
          "sdot multiplies factors into reconstruction"))))

(deftest ranking-sorts-labels-by-score
  (let* ((labels '("alpha" "beta" "gamma"))
         (matrix (make-array '(3 2) :element-type 'double-float
                             :initial-contents '((0.15d0 0.60d0)
                                                 (0.45d0 0.25d0)
                                                 (0.40d0 0.15d0))))
         (ranking (cltd:ranking labels matrix 0)))
    (ok (equal ranking '(("beta" . 0.45d0)
                         ("gamma" . 0.40d0)
                         ("alpha" . 0.15d0)))
        "ranking sorts labels by score")))

(deftest decomposition-produces-factorization
  (ok (decomposition X-tensor :n-cycle 100 :R 2 :verbose t)
      "decomposition returns non-nil result"))

(deftest factor-card-generation-produces-artifacts
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.6d0 0.2d0)
                                                (0.4d0 0.8d0))))
         (mode1 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.7d0 0.2d0)
                                                (0.2d0 0.3d0)
                                                (0.1d0 0.5d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(4 2) :element-type 'fixnum
                              :initial-contents '((0 0)
                                                  (0 1)
                                                  (1 0)
                                                  (1 2))))
         (counts (make-array 4 :element-type 'double-float
                             :initial-contents '(30d0 10d0 5d0 15d0)))
         (metadata (list
                    (cltd:make-mode-metadata :purchase '("purchase" "not_purchase")
                                             :role :purchase
                                             :positive-label "purchase"
                                             :negative-label "not_purchase"
                                             :discretization "binary")
                    (cltd:make-mode-metadata "genre" '("gourmet" "beauty" "travel")
                                             :discretization "manual top3"))))
    (let ((cards (cltd:generate-factor-cards factor-matrices indices counts metadata)))
      (ok (= (length cards) 2) "Generated cards for each factor")
      (let ((share-sum (loop for card in cards
                             sum (cdr (assoc :share (cdr (assoc :coverage card)))))))
        (ok (< (abs (- share-sum 1d0)) 0.02d0) "Coverage shares sum to ~1"))
      (let ((markdown (cltd::factor-report-markdown-string cards)))
        (ok (search "## Factor 1" markdown)
            "Markdown report includes headings"))
      (uiop:with-temporary-file (:pathname json-path :suffix "json")
        (cltd:write-factor-cards-json cards json-path
                                      :serializer (lambda (data stream)
                                                    (write data :stream stream :readably t)))
        (ok (probe-file json-path) "factor_cards.json written"))
      (uiop:with-temporary-file (:pathname report-path :suffix "md")
        (cltd:write-scenario-report cards report-path)
        (ok (and (probe-file report-path)
                 (search "Scenario Cards" (uiop:read-file-string report-path)))
            "report.md written with content")))))

(deftest decomposition-converges-before-max-iterations
  (multiple-value-bind (result-vec iterations)
      (cltd:decomposition X-tensor
                          :n-cycle 100
                          :R 2
                          :convergence-threshold 1d6
                          :convergence-window 3)
    (declare (ignore result-vec))
    (ok (< iterations 100) "Convergence threshold stops early")
    (ok (>= iterations 3) "At least window iterations executed")))

(deftest select-rank-matches-manual-search
  "SELECT-RANK returns the lowest-mean entry of the results it also returns."
  (let* ((ranks '(1 2))
         (cv-results (cltd:cross-validate-rank CV-tensor ranks
                                               :k 3
                                               :n-cycle 10
                                               :random-state (cltd:%seed-random-state 31)))
         (best-rank nil)
         (best-mean most-positive-double-float))
    (ok (= (length cv-results) (length ranks))
        "Cross-validation returns one entry per rank")
    (dolist (result cv-results)
      (let ((rank (cdr (assoc :rank result)))
            (mean (cdr (assoc :mean result))))
        (when (< mean best-mean)
          (setf best-mean mean
                best-rank rank))))
    (multiple-value-bind (best all-results)
        (cltd:select-rank CV-tensor ranks
                          :k 3
                          :n-cycle 10
                          :random-state (cltd:%seed-random-state 31))
      (ok (= (length all-results) (length ranks))
          "select-rank echoes full results")
      (ok (member (cdr (assoc :rank best)) ranks)
          "Best rank within candidates")
      (ok (= (cdr (assoc :rank best)) best-rank)
          "select-rank matches manual search"))))

(deftest cross-validate-rank-rejects-degenerate-fold-counts
  "k must leave something to validate against, and cannot exceed the event count."
  (ok (handler-case (progn (cltd:cross-validate-rank CV-tensor '(1) :k 1 :n-cycle 5) nil)
        (cltd:invalid-input-error () t))
      "k = 1 is rejected rather than degenerating to a single fold")
  (let ((tiny (cltd:make-sparse-tensor
               '(2 2)
               (make-array '(1 2) :element-type 'fixnum :initial-contents '((0 0)))
               (make-array 1 :element-type 'double-float :initial-contents '(3d0)))))
    (ok (handler-case (progn (cltd:cross-validate-rank tiny '(1) :k 5 :n-cycle 5) nil)
          (cltd:invalid-input-error () t))
        "Too few events for the requested k is rejected")))

(deftest cross-validate-rank-scores-are-finite
  "Every fold score is a finite double-float."
  (let ((results (cltd:cross-validate-rank CV-tensor '(1 2)
                                           :k 3
                                           :n-cycle 10
                                           :random-state (cltd:%seed-random-state 5))))
    (dolist (result results)
      (dolist (score (cdr (assoc :scores result)))
        (ok (typep score 'double-float)
            (format nil "rank ~D fold score is a double-float"
                    (cdr (assoc :rank result))))
        (ok (and (not (cltd:%float-nan-p score))
                 (not (cltd:%float-infinity-p score)))
            (format nil "rank ~D fold score ~,6F is finite"
                    (cdr (assoc :rank result)) score))))))

(deftest cross-validate-rank-is-reproducible-with-same-random-state
  "One random state drives both the thinning and the factor initialization."
  (let ((first-run (cltd:cross-validate-rank CV-tensor '(1 2)
                                             :k 3
                                             :n-cycle 10
                                             :random-state (cltd:%seed-random-state 77)))
        (second-run (cltd:cross-validate-rank CV-tensor '(1 2)
                                              :k 3
                                              :n-cycle 10
                                              :random-state (cltd:%seed-random-state 77))))
    (ok (equalp first-run second-run)
        "The same seed reproduces folds and scores exactly")))

(deftest cross-validate-rank-does-not-advance-caller-random-state
  "The supplied random state is copied, never advanced as a side effect."
  (let* ((state (cltd:%seed-random-state 4321))
         (first-run (cltd:cross-validate-rank CV-tensor '(1 2)
                                              :k 3 :n-cycle 10 :random-state state))
         (second-run (cltd:cross-validate-rank CV-tensor '(1 2)
                                               :k 3 :n-cycle 10 :random-state state)))
    (ok (equalp first-run second-run)
        "Reusing the same state object yields identical results")))

(deftest cross-validate-rank-is-independent-of-rank-order
  "A rank's scores do not depend on where it appears in RANKS."
  (let ((forward (cltd:cross-validate-rank CV-tensor '(1 2 3)
                                           :k 3 :n-cycle 10
                                           :random-state (cltd:%seed-random-state 909)))
        (reversed (cltd:cross-validate-rank CV-tensor '(3 2 1)
                                            :k 3 :n-cycle 10
                                            :random-state (cltd:%seed-random-state 909))))
    (ok (equal (mapcar (lambda (r) (cdr (assoc :rank r))) forward) '(1 2 3))
        "Results keep the order of the input ranks")
    (ok (equal (mapcar (lambda (r) (cdr (assoc :rank r))) reversed) '(3 2 1))
        "Reversed input keeps its own order")
    (dolist (rank '(1 2 3))
      (let ((a (find rank forward :key (lambda (r) (cdr (assoc :rank r)))))
            (b (find rank reversed :key (lambda (r) (cdr (assoc :rank r))))))
        (ok (equalp (cdr (assoc :scores a)) (cdr (assoc :scores b)))
            (format nil "rank ~D scores unchanged by rank ordering" rank))))))

(deftest cross-validate-rank-is-quiet-unless-verbose
  "Nothing reaches *STANDARD-OUTPUT* when VERBOSE is NIL."
  (let ((output (with-output-to-string (*standard-output*)
                  (cltd:cross-validate-rank CV-tensor '(1)
                                            :k 2 :n-cycle 5
                                            :random-state (cltd:%seed-random-state 3)))))
    (ok (zerop (length output))
        (format nil "verbose=nil produces no output (got ~D characters)"
                (length output)))))

(deftest cross-validate-rank-reports-validation-counts
  "Each result carries the per-fold validation totals and a standard error."
  (let* ((k 3)
         (result (first (cltd:cross-validate-rank CV-tensor '(2)
                                                  :k k :n-cycle 10
                                                  :random-state (cltd:%seed-random-state 8))))
         (counts (cdr (assoc :validation-counts result))))
    (ok (assoc :standard-error result) "Result carries :standard-error")
    (ok (< (abs (- (cdr (assoc :standard-error result))
                   (/ (cdr (assoc :std result)) (sqrt (coerce k 'double-float)))))
           1d-12)
        "standard-error is std / sqrt(k)")
    (ok (= (length counts) k) "One validation count per fold")
    (ok (every #'plusp counts) "Every fold has a positive validation count")
    (ok (= (reduce #'+ counts) 70)
        "Validation counts across folds sum to the tensor's total count")))

(deftest cross-validate-rank-respects-custom-evaluation-function
  "Custom metrics receive the validation tensor, reconstruction, factors, scale and count."
  (let ((seen '()))
    (labels ((probe (valid-tensor approximation factors scale valid-count)
               (push (list (cltd:sparse-tensor-shape valid-tensor)
                           (length approximation)
                           (length factors)
                           scale
                           valid-count)
                     seen)
               42d0))
      (let ((results (cltd:cross-validate-rank CV-tensor '(1 2)
                                               :k 4
                                               :n-cycle 5
                                               :random-state (cltd:%seed-random-state 6)
                                               :evaluation-function #'probe)))
        (dolist (result results)
          (ok (every (lambda (score) (= score 42d0)) (cdr (assoc :scores result)))
              "Custom evaluation function overrides the fold score")
          (ok (= (cdr (assoc :mean result)) 42d0)
              "Mean reflects the custom metric"))
        (ok (= (length seen) 8) "Called once per rank per fold")
        (dolist (call seen)
          (destructuring-bind (shape approx-length n-modes scale valid-count) call
            (ok (equal shape '(3 4 2)) "Validation tensor keeps the original shape")
            (ok (= n-modes 3) "Factor matrix vector has one matrix per mode")
            (ok (< (abs (- scale (/ 1d0 3d0))) 1d-12)
                "Prediction scale is 1/(k-1)")
            (ok (and (plusp valid-count) (plusp approx-length))
                "Validation count and reconstruction are non-empty")))))))

(deftest select-rank-returns-expected-defaults
  (let ((default-ranks '(1 2)))
    (multiple-value-bind (default-best default-results)
        (cltd:select-rank CV-tensor default-ranks :k 3 :n-cycle 10)
      (ok (= (length default-results) (length default-ranks))
          "Default select-rank returns per-rank results")
      (ok (member (cdr (assoc :rank default-best)) default-ranks)
          "Default select-rank chooses rank from candidates"))))

(deftest select-rank-does-not-mutate-results
  "SELECT-RANK must not sort CV-RESULTS in place."
  (let ((ranks '(3 1 2)))
    (multiple-value-bind (best all-results)
        (cltd:select-rank CV-tensor ranks
                          :k 3 :n-cycle 10
                          :random-state (cltd:%seed-random-state 17))
      (ok (= (length all-results) (length ranks))
          "All candidate ranks survive selection")
      (ok (equal (mapcar (lambda (r) (cdr (assoc :rank r))) all-results) ranks)
          "Results keep the input rank order")
      (let ((minimum (reduce #'min all-results
                             :key (lambda (r) (cdr (assoc :mean r))))))
        (ok (< (abs (- (cdr (assoc :mean best)) minimum)) 1d-12)
            "Selected result has the lowest mean")))))

(deftest select-rank-breaks-ties-toward-the-smaller-rank
  "With equal means the smaller rank wins, deterministically."
  (labels ((constant-score (valid-tensor approximation factors scale valid-count)
             (declare (ignore valid-tensor approximation factors scale valid-count))
             1d0))
    (multiple-value-bind (best all-results)
        (cltd:select-rank CV-tensor '(3 1 2)
                          :k 3 :n-cycle 5
                          :random-state (cltd:%seed-random-state 23)
                          :evaluation-function #'constant-score)
      (ok (= (length all-results) 3) "All ranks reported")
      (ok (= (cdr (assoc :rank best)) 1)
          (format nil "Tie broken toward the smaller rank (got ~D)"
                  (cdr (assoc :rank best)))))))

(deftest cross-validate-rank-rejects-invalid-ranks
  "RANKS must be a non-empty list of positive integers."
  (ok (handler-case (progn (cltd:cross-validate-rank CV-tensor '() :k 3) nil)
        (cltd:invalid-input-error () t))
      "Empty rank list is rejected")
  (ok (handler-case (progn (cltd:cross-validate-rank CV-tensor '(0) :k 3) nil)
        (cltd:invalid-input-error () t))
      "Rank 0 is rejected")
  (ok (handler-case (progn (cltd:cross-validate-rank CV-tensor '(2 -1) :k 3) nil)
        (cltd:invalid-input-error () t))
      "Negative rank is rejected")
  (ok (handler-case (progn (cltd:cross-validate-rank "not-a-tensor" '(1) :k 3) nil)
        (cltd:invalid-input-error () t))
      "A non-tensor first argument is rejected"))

(deftest ensure-mode-specs-validates-metadata
  (let* ((mode0 (make-array '(2 1) :element-type 'double-float :initial-element 0.5d0))
         (mode1 (make-array '(3 1) :element-type 'double-float :initial-element 0.3d0))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (metadata (list (cltd:make-mode-metadata "only" '("a" "b")))))
    (ok (handler-case
            (progn
              (cltd::ensure-mode-specs metadata factor-matrices)
              nil)
          (error () t))
        "ensure-mode-specs rejects metadata count mismatch")
    (ok (handler-case
            (progn
              (cltd::ensure-mode-specs (list (cltd:make-mode-metadata "mode" '("a")))
                                       (make-array 1 :initial-contents (list mode0)))
              nil)
          (error () t))
        "ensure-mode-specs rejects label count mismatch")
    (ok (handler-case
            (progn
              (cltd::ensure-mode-specs (list (list :name "mode"
                                                   :labels '("yes" "no")
                                                   :positive-label "missing"))
                                       (make-array 1 :initial-contents (list mode0)))
              nil)
          (error () t))
        "ensure-mode-specs rejects unknown positive label")))

(deftest write-factor-cards-json-requires-serializer
  (uiop:with-temporary-file (:pathname json-path :suffix "json")
    (ok (handler-case
            (progn
              (cltd:write-factor-cards-json '() json-path)
              nil)
          (error () t))
        "write-factor-cards-json requires serializer")))

(deftest generate-report-artifacts-produces-files
  (uiop:with-temporary-file (:pathname json-path :suffix "json" :keep t)
    (uiop:with-temporary-file (:pathname report-path :suffix "md" :keep t)
      (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                                :initial-contents '((0.6d0 0.2d0)
                                                    (0.4d0 0.8d0))))
             (mode1 (make-array '(3 2) :element-type 'double-float
                                :initial-contents '((0.7d0 0.2d0)
                                                    (0.2d0 0.3d0)
                                                    (0.1d0 0.5d0))))
             (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
             (indices (make-array '(4 2) :element-type 'fixnum
                                  :initial-contents '((0 0)
                                                      (0 1)
                                                      (1 0)
                                                      (1 2))))
             (counts (make-array 4 :element-type 'double-float
                                 :initial-contents '(30d0 10d0 5d0 15d0)))
             (metadata (list (cltd:make-mode-metadata :purchase '("purchase" "not_purchase")
                                                      :role :purchase
                                                      :positive-label "purchase"
                                                      :negative-label "not_purchase"
                                                      :discretization "binary")
                             (cltd:make-mode-metadata "genre" '("gourmet" "beauty" "travel")
                                                      :discretization "manual top3"))))
        (cltd:generate-report-artifacts factor-matrices indices counts metadata
                                        :factor-json-path json-path
                                        :report-path report-path
                                        :json-serializer (lambda (cards stream)
                                                           (declare (ignore cards))
                                                           (write-string "[]" stream)))
        (ok (probe-file json-path)
            "generate-report-artifacts writes JSON output")
        (ok (search "Scenario Cards" (uiop:read-file-string report-path))
            "generate-report-artifacts writes markdown report")))))

(deftest generate-factor-cards-with-diagnostics
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.6d0 0.2d0)
                                                (0.4d0 0.8d0))))
         (mode1 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.7d0 0.2d0)
                                                (0.2d0 0.3d0)
                                                (0.1d0 0.5d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(4 2) :element-type 'fixnum
                              :initial-contents '((0 0)
                                                  (0 1)
                                                  (1 0)
                                                  (1 2))))
         (counts (make-array 4 :element-type 'double-float
                             :initial-contents '(30d0 10d0 5d0 15d0)))
         (metadata (list
                    (cltd:make-mode-metadata :purchase '("purchase" "not_purchase")
                                             :role :purchase
                                             :positive-label "purchase"
                                             :negative-label "not_purchase"
                                             :discretization "binary")
                    (cltd:make-mode-metadata "genre" '("gourmet" "beauty" "travel")
                                             :discretization "manual top3"))))
    ;; Test without diagnostics (backward compatibility)
    (let ((cards-only (cltd:generate-factor-cards factor-matrices indices counts metadata)))
      (ok (listp cards-only)
          "Without diagnostics returns a list")
      (ok (= (length cards-only) 2)
          "Without diagnostics returns correct number of cards")
      (ok (assoc :factor_id (first cards-only))
          "Without diagnostics cards have factor_id"))

    ;; Test with diagnostics
    (let ((result (cltd:generate-factor-cards factor-matrices indices counts metadata
                                              :include-diagnostics t)))
      (ok (assoc :model_diagnostics result)
          "With diagnostics result has :model_diagnostics")
      (ok (assoc :factors result)
          "With diagnostics result has :factors")

      ;; Check model-level diagnostics
      (let ((diag (cdr (assoc :model_diagnostics result))))
        (ok (assoc :kl_divergence diag)
            "Model diagnostics has :kl_divergence")
        (ok (assoc :factor_similarity diag)
            "Model diagnostics has :factor_similarity")
        (ok (assoc :exclusivity diag)
            "Model diagnostics has :exclusivity")
        (ok (assoc :overlap diag)
            "Model diagnostics has :overlap")
        (ok (assoc :responsibility_stats diag)
            "Model diagnostics has :responsibility_stats")
        (ok (assoc :residual_stats diag)
            "Model diagnostics has :residual_stats")
        (ok (assoc :kl_contributions diag)
            "Model diagnostics has :kl_contributions"))

      ;; Check factor-level diagnostics
      (let ((factors (cdr (assoc :factors result))))
        (ok (= (length factors) 2)
            "With diagnostics returns correct number of factors")
        (let ((first-factor (first factors)))
          (ok (assoc :kl_contribution first-factor)
              "Factor card has :kl_contribution")
          (ok (assoc :contribution_rank first-factor)
              "Factor card has :contribution_rank"))))))

;;; ============================================================================
;;; Diagnostics Tests
;;; ============================================================================

(deftest factor-similarity-self-similarity-is-one
  (let* ((mode0 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.6d0 0.2d0)
                                                (0.3d0 0.5d0)
                                                (0.1d0 0.3d0))))
         (mode1 (make-array '(4 2) :element-type 'double-float
                            :initial-contents '((0.4d0 0.1d0)
                                                (0.3d0 0.4d0)
                                                (0.2d0 0.3d0)
                                                (0.1d0 0.2d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (sim-matrix (cltd:compute-factor-similarity-matrix factor-matrices)))
    (ok (< (abs (- (aref sim-matrix 0 0) 1.0d0)) +test-epsilon+)
        "Self-similarity of factor 0 is 1.0")
    (ok (< (abs (- (aref sim-matrix 1 1) 1.0d0)) +test-epsilon+)
        "Self-similarity of factor 1 is 1.0")))

(deftest factor-similarity-is-symmetric
  (let* ((mode0 (make-array '(3 3) :element-type 'double-float
                            :initial-contents '((0.6d0 0.2d0 0.1d0)
                                                (0.3d0 0.5d0 0.4d0)
                                                (0.1d0 0.3d0 0.5d0))))
         (factor-matrices (make-array 1 :initial-contents (list mode0)))
         (sim-matrix (cltd:compute-factor-similarity-matrix factor-matrices)))
    (ok (< (abs (- (aref sim-matrix 0 1) (aref sim-matrix 1 0))) +test-epsilon+)
        "Similarity(0,1) = Similarity(1,0)")
    (ok (< (abs (- (aref sim-matrix 0 2) (aref sim-matrix 2 0))) +test-epsilon+)
        "Similarity(0,2) = Similarity(2,0)")
    (ok (< (abs (- (aref sim-matrix 1 2) (aref sim-matrix 2 1))) +test-epsilon+)
        "Similarity(1,2) = Similarity(2,1)")))

(deftest factor-similarity-identical-factors
  (let* ((mode0 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.5d0 0.5d0)
                                                (0.3d0 0.3d0)
                                                (0.2d0 0.2d0))))
         (factor-matrices (make-array 1 :initial-contents (list mode0)))
         (sim-matrix (cltd:compute-factor-similarity-matrix factor-matrices)))
    (ok (< (abs (- (aref sim-matrix 0 1) 1.0d0)) +test-epsilon+)
        "Identical factors have similarity 1.0")))

(deftest factor-similarity-orthogonal-factors
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((1.0d0 0.0d0)
                                                (0.0d0 1.0d0))))
         (factor-matrices (make-array 1 :initial-contents (list mode0)))
         (sim-matrix (cltd:compute-factor-similarity-matrix factor-matrices)))
    (ok (< (abs (aref sim-matrix 0 1)) +test-epsilon+)
        "Orthogonal factors have similarity 0.0")))

(deftest extract-similar-factor-pairs-filters-by-threshold
  (let* ((mode0 (make-array '(3 3) :element-type 'double-float
                            :initial-contents '((0.9d0 0.9d0 0.1d0)
                                                (0.1d0 0.1d0 0.9d0)
                                                (0.0d0 0.0d0 0.0d0))))
         (factor-matrices (make-array 1 :initial-contents (list mode0)))
         (sim-matrix (cltd:compute-factor-similarity-matrix factor-matrices))
         (high-pairs (cltd:extract-similar-factor-pairs sim-matrix :threshold 0.9d0))
         (low-pairs (cltd:extract-similar-factor-pairs sim-matrix :threshold 0.1d0)))
    (ok (= (length high-pairs) 1)
        "Only one pair above 0.9 threshold")
    (ok (and (= (first (first high-pairs)) 0)
             (= (second (first high-pairs)) 1))
        "Factors 0 and 1 are the similar pair")
    (ok (>= (length low-pairs) 1)
        "At least one pair above 0.1 threshold")))

(deftest similarity-matrix-alist-format
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.8d0 0.6d0)
                                                (0.2d0 0.4d0))))
         (factor-matrices (make-array 1 :initial-contents (list mode0)))
         (sim-matrix (cltd:compute-factor-similarity-matrix factor-matrices))
         (alist (cltd:similarity-matrix->alist sim-matrix :threshold 0.5d0)))
    (ok (assoc :matrix alist)
        "Alist contains :matrix key")
    (ok (assoc :similar_pairs alist)
        "Alist contains :similar_pairs key")
    (ok (assoc :threshold alist)
        "Alist contains :threshold key")
    (let ((matrix-list (cdr (assoc :matrix alist))))
      (ok (= (length matrix-list) 2)
          "Matrix has correct dimensions")
      (ok (= (length (first matrix-list)) 2)
          "Matrix rows have correct dimensions"))))

(deftest factor-redundancy-score-range
  (let* ((mode0 (make-array '(3 3) :element-type 'double-float
                            :initial-contents '((0.9d0 0.9d0 0.1d0)
                                                (0.1d0 0.1d0 0.9d0)
                                                (0.0d0 0.0d0 0.0d0))))
         (factor-matrices (make-array 1 :initial-contents (list mode0)))
         (sim-matrix (cltd:compute-factor-similarity-matrix factor-matrices))
         (redundancy (cltd:compute-factor-redundancy-score sim-matrix :threshold 0.8d0)))
    (ok (>= redundancy 0.0d0)
        "Redundancy score >= 0")
    (ok (<= redundancy 1.0d0)
        "Redundancy score <= 1")
    (ok (> redundancy 0.0d0)
        "Non-zero redundancy for similar factors")))

;;; ============================================================================
;;; Factor KL Contribution Tests
;;; ============================================================================

(deftest kl-contributions-are-non-negative
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.6d0 0.2d0)
                                                (0.4d0 0.8d0))))
         (mode1 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.5d0 0.3d0)
                                                (0.3d0 0.4d0)
                                                (0.2d0 0.3d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(4 2) :element-type 'fixnum
                              :initial-contents '((0 0)
                                                  (0 1)
                                                  (1 0)
                                                  (1 2))))
         (counts (make-array 4 :element-type 'double-float
                             :initial-contents '(30d0 10d0 5d0 15d0)))
         (contributions (cltd:compute-factor-kl-contributions
                         factor-matrices indices counts)))
    (ok (= (length contributions) 2)
        "One contribution per factor")
    (ok (every (lambda (i) (>= (aref contributions i) 0.0d0))
               '(0 1))
        "All contributions are non-negative")))

(deftest normalize-contributions-sums-to-one
  (let* ((contributions (make-array 3 :element-type 'double-float
                                    :initial-contents '(0.5d0 0.3d0 0.2d0)))
         (normalized (cltd:normalize-contributions contributions))
         (sum (loop for i from 0 below 3 sum (aref normalized i))))
    (ok (< (abs (- sum 1.0d0)) +test-epsilon+)
        "Normalized contributions sum to 1.0")))

(deftest normalize-contributions-handles-zero-total
  (let* ((contributions (make-array 3 :element-type 'double-float
                                    :initial-contents '(0.0d0 0.0d0 0.0d0)))
         (normalized (cltd:normalize-contributions contributions)))
    (ok (< (abs (- (aref normalized 0) (/ 1.0d0 3))) +test-epsilon+)
        "Zero contributions result in uniform distribution")))

(deftest kl-contributions-alist-format
  (let* ((contributions (make-array 2 :element-type 'double-float
                                    :initial-contents '(0.4d0 0.6d0)))
         (alist (cltd:kl-contributions->alist contributions :normalize t)))
    (ok (assoc :contributions alist)
        "Alist contains :contributions key")
    (ok (assoc :total alist)
        "Alist contains :total key")
    (ok (assoc :normalized alist)
        "Alist contains :normalized key when normalize is true")
    (let ((contrib-list (cdr (assoc :contributions alist))))
      (ok (= (length contrib-list) 2)
          "Contributions list has correct length"))))

(deftest rank-factors-by-contribution-sorts-descending
  (let* ((contributions (make-array 4 :element-type 'double-float
                                    :initial-contents '(0.1d0 0.4d0 0.2d0 0.3d0)))
         (ranked (cltd:rank-factors-by-contribution contributions)))
    (ok (= (length ranked) 4)
        "All factors ranked")
    (ok (= (car (first ranked)) 1)
        "Highest contributing factor (index 1) is first")
    (ok (= (car (second ranked)) 3)
        "Second highest (index 3) is second")
    (ok (= (car (third ranked)) 2)
        "Third highest (index 2) is third")
    (ok (= (car (fourth ranked)) 0)
        "Lowest (index 0) is last")))

;;; ============================================================================
;;; Observation Responsibilities Tests
;;; ============================================================================

(deftest responsibilities-rows-sum-to-one
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.6d0 0.2d0)
                                                (0.4d0 0.8d0))))
         (mode1 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.5d0 0.3d0)
                                                (0.3d0 0.4d0)
                                                (0.2d0 0.3d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(4 2) :element-type 'fixnum
                              :initial-contents '((0 0)
                                                  (0 1)
                                                  (1 0)
                                                  (1 2))))
         (responsibilities (cltd:compute-observation-responsibilities
                            factor-matrices indices)))
    (ok (= (array-dimension responsibilities 0) 4)
        "Responsibilities has correct number of rows")
    (ok (= (array-dimension responsibilities 1) 2)
        "Responsibilities has correct number of columns")
    (ok (every (lambda (obs)
                 (let ((row-sum (+ (aref responsibilities obs 0)
                                   (aref responsibilities obs 1))))
                   (< (abs (- row-sum 1.0d0)) +test-epsilon+)))
               '(0 1 2 3))
        "Each row sums to 1.0")))

(deftest responsibilities-are-non-negative
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.6d0 0.2d0)
                                                (0.4d0 0.8d0))))
         (mode1 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.5d0 0.3d0)
                                                (0.3d0 0.4d0)
                                                (0.2d0 0.3d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(4 2) :element-type 'fixnum
                              :initial-contents '((0 0)
                                                  (0 1)
                                                  (1 0)
                                                  (1 2))))
         (responsibilities (cltd:compute-observation-responsibilities
                            factor-matrices indices)))
    (ok (loop for obs from 0 below 4
              always (loop for r from 0 below 2
                           always (>= (aref responsibilities obs r) 0.0d0)))
        "All responsibilities are non-negative")))

(deftest responsibility-stats-valid-ranges
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.6d0 0.2d0)
                                                (0.4d0 0.8d0))))
         (mode1 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.5d0 0.3d0)
                                                (0.3d0 0.4d0)
                                                (0.2d0 0.3d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(4 2) :element-type 'fixnum
                              :initial-contents '((0 0)
                                                  (0 1)
                                                  (1 0)
                                                  (1 2))))
         (counts (make-array 4 :element-type 'double-float
                             :initial-contents '(30d0 10d0 5d0 15d0)))
         (responsibilities (cltd:compute-observation-responsibilities
                            factor-matrices indices)))
    (multiple-value-bind (mean-max ambiguous-rate mean-entropy dominant-counts)
        (cltd:compute-responsibility-stats responsibilities counts)
      (ok (and (>= mean-max 0.0d0) (<= mean-max 1.0d0))
          "mean-max-responsibility in [0, 1]")
      (ok (and (>= ambiguous-rate 0.0d0) (<= ambiguous-rate 1.0d0))
          "ambiguous-rate in [0, 1]")
      (ok (>= mean-entropy 0.0d0)
          "mean-entropy is non-negative")
      (ok (= (length dominant-counts) 2)
          "dominant-counts has correct length"))))

(deftest responsibility-stats-alist-format
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.6d0 0.2d0)
                                                (0.4d0 0.8d0))))
         (mode1 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.5d0 0.3d0)
                                                (0.3d0 0.4d0)
                                                (0.2d0 0.3d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(4 2) :element-type 'fixnum
                              :initial-contents '((0 0)
                                                  (0 1)
                                                  (1 0)
                                                  (1 2))))
         (counts (make-array 4 :element-type 'double-float
                             :initial-contents '(30d0 10d0 5d0 15d0)))
         (responsibilities (cltd:compute-observation-responsibilities
                            factor-matrices indices))
         (alist (cltd:responsibility-stats->alist responsibilities counts)))
    (ok (assoc :mean_max_responsibility alist)
        "Alist contains :mean_max_responsibility key")
    (ok (assoc :ambiguous_rate alist)
        "Alist contains :ambiguous_rate key")
    (ok (assoc :mean_entropy alist)
        "Alist contains :mean_entropy key")
    (ok (assoc :dominant_factor_counts alist)
        "Alist contains :dominant_factor_counts key")))

(deftest find-ambiguous-observations-filters-correctly
  (let* ((responsibilities (make-array '(4 2) :element-type 'double-float
                                       :initial-contents '((0.9d0 0.1d0)
                                                           (0.4d0 0.6d0)
                                                           (0.3d0 0.7d0)
                                                           (0.45d0 0.55d0))))
         (indices (make-array '(4 2) :element-type 'fixnum
                              :initial-contents '((0 0) (0 1) (1 0) (1 1))))
         (ambiguous (cltd:find-ambiguous-observations responsibilities indices
                                                       :threshold 0.6d0)))
    (ok (= (length ambiguous) 1)
        "Only one observation below 0.6 threshold")
    (ok (= (cdr (assoc :observation (first ambiguous))) 3)
        "Observation 3 (0.55 max) is the ambiguous one")))

;;; ============================================================================
;;; Factor Exclusivity Tests
;;; ============================================================================

(deftest exclusivity-range-valid
  (let* ((responsibilities (make-array '(4 2) :element-type 'double-float
                                       :initial-contents '((0.9d0 0.1d0)
                                                           (0.8d0 0.2d0)
                                                           (0.7d0 0.3d0)
                                                           (0.6d0 0.4d0))))
         (counts (make-array 4 :element-type 'double-float
                             :initial-contents '(10d0 20d0 30d0 40d0))))
    (multiple-value-bind (exclusivity overlap bins)
        (cltd:compute-factor-exclusivity responsibilities counts)
      (ok (and (>= exclusivity 0.0d0) (<= exclusivity 1.0d0))
          "Exclusivity in [0, 1]")
      (ok (and (>= overlap 0.0d0) (<= overlap 1.0d0))
          "Overlap in [0, 1]")
      (ok (< (abs (- (+ exclusivity overlap) 1.0d0)) +test-epsilon+)
          "Exclusivity + overlap = 1")
      (ok (= (length bins) 5)
          "Bins array has 5 elements"))))

(deftest exclusivity-alist-format
  (let* ((responsibilities (make-array '(4 2) :element-type 'double-float
                                       :initial-contents '((0.9d0 0.1d0)
                                                           (0.8d0 0.2d0)
                                                           (0.7d0 0.3d0)
                                                           (0.6d0 0.4d0))))
         (counts (make-array 4 :element-type 'double-float
                             :initial-contents '(10d0 20d0 30d0 40d0)))
         (alist (cltd:factor-exclusivity->alist responsibilities counts)))
    (ok (assoc :exclusivity alist)
        "Alist contains :exclusivity key")
    (ok (assoc :overlap alist)
        "Alist contains :overlap key")
    (ok (assoc :interpretation alist)
        "Alist contains :interpretation key")
    (ok (assoc :distribution alist)
        "Alist contains :distribution key")))

(deftest exclusivity-high-for-clear-segmentation
  (let* ((responsibilities (make-array '(3 2) :element-type 'double-float
                                       :initial-contents '((0.99d0 0.01d0)
                                                           (0.01d0 0.99d0)
                                                           (0.98d0 0.02d0))))
         (counts (make-array 3 :element-type 'double-float
                             :initial-contents '(1d0 1d0 1d0))))
    (multiple-value-bind (exclusivity overlap bins)
        (cltd:compute-factor-exclusivity responsibilities counts)
      (declare (ignore bins))
      (ok (> exclusivity 0.9d0)
          "Exclusivity high for clear segmentation")
      (ok (< overlap 0.1d0)
          "Overlap low for clear segmentation"))))

;;; ============================================================================
;;; Per-Observation Residuals Tests
;;; ============================================================================

(deftest residuals-are-computed
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.6d0 0.2d0)
                                                (0.4d0 0.8d0))))
         (mode1 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.5d0 0.3d0)
                                                (0.3d0 0.4d0)
                                                (0.2d0 0.3d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(4 2) :element-type 'fixnum
                              :initial-contents '((0 0)
                                                  (0 1)
                                                  (1 0)
                                                  (1 2))))
         (counts (make-array 4 :element-type 'double-float
                             :initial-contents '(30d0 10d0 5d0 15d0)))
         (residuals (cltd:compute-observation-residuals
                     factor-matrices indices counts)))
    (ok (= (length residuals) 4)
        "One residual per observation")
    (ok (every (lambda (i) (numberp (aref residuals i)))
               '(0 1 2 3))
        "All residuals are numbers")))

(deftest residual-stats-valid
  (let* ((residuals (make-array 5 :element-type 'double-float
                                :initial-contents '(0.1d0 0.2d0 0.15d0 0.3d0 0.05d0)))
         (counts (make-array 5 :element-type 'double-float
                             :initial-contents '(10d0 20d0 15d0 5d0 50d0))))
    (multiple-value-bind (mean std p95 outlier-count outlier-weight)
        (cltd:compute-residual-stats residuals counts)
      (ok (>= mean 0.0d0)
          "Mean is non-negative")
      (ok (>= std 0.0d0)
          "Std is non-negative")
      (ok (>= p95 mean)
          "P95 >= mean")
      (ok (>= outlier-count 0)
          "Outlier count is non-negative")
      (ok (>= outlier-weight 0.0d0)
          "Outlier weight is non-negative"))))

(deftest residual-stats-alist-format
  (let* ((residuals (make-array 4 :element-type 'double-float
                                :initial-contents '(0.1d0 0.2d0 0.15d0 0.3d0)))
         (counts (make-array 4 :element-type 'double-float
                             :initial-contents '(10d0 20d0 15d0 5d0)))
         (alist (cltd:residual-stats->alist residuals counts)))
    (ok (assoc :mean alist)
        "Alist contains :mean key")
    (ok (assoc :std alist)
        "Alist contains :std key")
    (ok (assoc :p95 alist)
        "Alist contains :p95 key")
    (ok (assoc :outlier_count alist)
        "Alist contains :outlier_count key")
    (ok (assoc :outlier_rate alist)
        "Alist contains :outlier_rate key")))

(deftest find-high-residual-observations-works
  (let* ((residuals (make-array 5 :element-type 'double-float
                                :initial-contents '(0.1d0 0.5d0 0.2d0 0.8d0 0.05d0)))
         (indices (make-array '(5 2) :element-type 'fixnum
                              :initial-contents '((0 0) (0 1) (1 0) (1 1) (0 2))))
         (top2 (cltd:find-high-residual-observations residuals indices :top-n 2)))
    (ok (= (length top2) 2)
        "Returns top 2 observations")
    (ok (= (cdr (assoc :observation (first top2))) 3)
        "Highest residual observation (0.8) is first")
    (ok (= (cdr (assoc :observation (second top2))) 1)
        "Second highest (0.5) is second")))

;;; ============================================================================
;;; Regression Tests for Bug Fixes
;;; ============================================================================

(deftest residuals-handle-zero-counts-without-nan
  "Regression test: x=0 should not produce NaN in residuals.
When x=0, the KL contribution simplifies to x-hat (the reconstruction value)."
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.5d0 0.3d0)
                                                (0.5d0 0.7d0))))
         (mode1 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.6d0 0.4d0)
                                                (0.4d0 0.6d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(3 2) :element-type 'fixnum
                              :initial-contents '((0 0)
                                                  (0 1)
                                                  (1 0))))
         ;; Include a zero count to test the edge case
         (counts (make-array 3 :element-type 'double-float
                             :initial-contents '(10.0d0 0.0d0 5.0d0)))
         (residuals (cltd:compute-observation-residuals
                     factor-matrices indices counts)))
    ;; Check no NaN values
    (ok (every (lambda (i)
                 (let ((r (aref residuals i)))
                   (and (numberp r) (not (cltd:%float-nan-p r)))))
               '(0 1 2))
        "No NaN values in residuals when x=0 is present")
    ;; The residual for x=0 should be x-hat (non-negative)
    (ok (>= (aref residuals 1) 0.0d0)
        "Residual for x=0 observation is non-negative (equals x-hat)")))

(deftest normalize-contributions-clamps-negative-values
  "Regression test: negative contributions should be clamped to 0 before normalization."
  (let* ((contributions (make-array 4 :element-type 'double-float
                                    :initial-contents '(0.5d0 -0.1d0 0.3d0 -0.05d0)))
         (normalized (cltd:normalize-contributions contributions)))
    ;; All normalized values should be non-negative
    (ok (every (lambda (i) (>= (aref normalized i) 0.0d0))
               '(0 1 2 3))
        "All normalized values are non-negative after clamping")
    ;; Negative contributions should become 0
    (ok (< (aref normalized 1) +test-epsilon+)
        "Originally negative contribution (index 1) is now 0")
    (ok (< (aref normalized 3) +test-epsilon+)
        "Originally negative contribution (index 3) is now 0")
    ;; Sum should still be 1.0
    (let ((sum (loop for i from 0 below 4 sum (aref normalized i))))
      (ok (< (abs (- sum 1.0d0)) +test-epsilon+)
          "Normalized contributions still sum to 1.0"))
    ;; Positive contributions should maintain relative proportions
    ;; Original: 0.5, 0.3 -> clamped: 0.5, 0.3 -> normalized: 0.625, 0.375
    (ok (< (abs (- (aref normalized 0) (/ 0.5d0 0.8d0))) +test-epsilon+)
        "First positive contribution normalized correctly")
    (ok (< (abs (- (aref normalized 2) (/ 0.3d0 0.8d0))) +test-epsilon+)
        "Second positive contribution normalized correctly")))

;;; ============================================================================
;;; Additional Coverage Tests
;;; ============================================================================

(deftest sparse-kl-divergence-handles-zero-x
  "sparse-kl-divergence should not produce NaN when x contains zeros."
  (let* ((indices (make-array '(3 2) :element-type 'fixnum
                              :initial-contents '((0 0) (0 1) (1 0))))
         (x-values (make-array 3 :element-type 'double-float
                               :initial-contents '(10.0d0 0.0d0 5.0d0)))
         (x-hat (make-array 3 :element-type 'double-float
                            :initial-contents '(9.0d0 1.0d0 6.0d0)))
         (factors (make-array 2 :initial-contents
                              (list (make-array '(2 1) :element-type 'double-float
                                                :initial-contents '((3.0d0) (2.0d0)))
                                    (make-array '(2 1) :element-type 'double-float
                                                :initial-contents '((3.0d0) (0.5d0))))))
         (kl (cltd:sparse-kl-divergence indices x-values x-hat factors)))
    (ok (numberp kl)
        "KL divergence is a number")
    (ok (not (cltd:%float-nan-p kl))
        "KL divergence is not NaN when x contains zero")))

(deftest compute-observation-residuals-zero-x-equals-x-hat
  "When x=0, residual should equal x-hat (reconstruction value)."
  (let* ((mode0 (make-array '(2 1) :element-type 'double-float
                            :initial-contents '((0.5d0) (0.5d0))))
         (mode1 (make-array '(2 1) :element-type 'double-float
                            :initial-contents '((0.6d0) (0.4d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(2 2) :element-type 'fixnum
                              :initial-contents '((0 0) (1 1))))
         ;; x=0 for observation 0
         (counts (make-array 2 :element-type 'double-float
                             :initial-contents '(0.0d0 5.0d0)))
         (residuals (cltd:compute-observation-residuals
                     factor-matrices indices counts)))
    ;; x-hat for observation 0: 0.5 * 0.6 = 0.3
    (let ((expected-x-hat (* 0.5d0 0.6d0)))
      (ok (< (abs (- (aref residuals 0) expected-x-hat)) +test-epsilon+)
          "Residual for x=0 equals x-hat"))))

(deftest compute-observation-responsibilities-zero-denominator
  "When all factor scores are zero, should return uniform distribution."
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.0d0 0.0d0)
                                                (0.5d0 0.5d0))))
         (mode1 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.0d0 0.0d0)
                                                (0.5d0 0.5d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         ;; Observation 0 maps to all-zero entries
         (indices (make-array '(2 2) :element-type 'fixnum
                              :initial-contents '((0 0) (1 1))))
         (responsibilities (cltd:compute-observation-responsibilities
                            factor-matrices indices)))
    ;; Observation 0 should have uniform distribution (0.5, 0.5)
    (ok (< (abs (- (aref responsibilities 0 0) 0.5d0)) +test-epsilon+)
        "Zero denominator gives uniform distribution (factor 0)")
    (ok (< (abs (- (aref responsibilities 0 1) 0.5d0)) +test-epsilon+)
        "Zero denominator gives uniform distribution (factor 1)")))

(deftest factor-similarity-aggregation-modes
  "compute-factor-similarity-matrix should support different aggregation modes."
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.9d0 0.1d0)
                                                (0.1d0 0.9d0))))
         (mode1 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.5d0 0.5d0)
                                                (0.5d0 0.5d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (sim-mean (cltd:compute-factor-similarity-matrix factor-matrices :aggregation :mean))
         (sim-min (cltd:compute-factor-similarity-matrix factor-matrices :aggregation :min))
         (sim-max (cltd:compute-factor-similarity-matrix factor-matrices :aggregation :max))
         (sim-geo (cltd:compute-factor-similarity-matrix factor-matrices :aggregation :geometric)))
    ;; Mode0 has low similarity (orthogonal-ish), mode1 has high similarity (identical)
    ;; Min should give lowest value, max should give highest
    (ok (<= (aref sim-min 0 1) (aref sim-mean 0 1))
        ":min aggregation <= :mean")
    (ok (>= (aref sim-max 0 1) (aref sim-mean 0 1))
        ":max aggregation >= :mean")
    ;; Geometric mean should be between min and max
    (ok (and (>= (aref sim-geo 0 1) (aref sim-min 0 1))
             (<= (aref sim-geo 0 1) (aref sim-max 0 1)))
        ":geometric between :min and :max")))

(deftest factor-redundancy-score-single-factor
  "compute-factor-redundancy-score should return 0 for R<2 (no pairs)."
  (let* ((mode0 (make-array '(3 1) :element-type 'double-float
                            :initial-contents '((0.5d0) (0.3d0) (0.2d0))))
         (factor-matrices (make-array 1 :initial-contents (list mode0)))
         (sim-matrix (cltd:compute-factor-similarity-matrix factor-matrices))
         (score (cltd:compute-factor-redundancy-score sim-matrix)))
    (ok (< (abs score) +test-epsilon+)
        "Redundancy score is 0 when R=1 (no pairs to compare)")))

(deftest find-high-residual-observations-with-threshold
  "find-high-residual-observations should filter by threshold when specified."
  (let* ((residuals (make-array 5 :element-type 'double-float
                                :initial-contents '(0.1d0 0.5d0 0.2d0 0.8d0 0.05d0)))
         (indices (make-array '(5 2) :element-type 'fixnum
                              :initial-contents '((0 0) (0 1) (1 0) (1 1) (0 2))))
         (filtered (cltd:find-high-residual-observations residuals indices
                                                          :threshold 0.4d0)))
    ;; Only observations with residual >= 0.4 (indices 1 and 3)
    (ok (= (length filtered) 2)
        "Only observations above threshold returned")
    ;; Highest first (0.8), then 0.5
    (ok (= (cdr (assoc :observation (first filtered))) 3)
        "Observation 3 (0.8) is first")
    (ok (= (cdr (assoc :observation (second filtered))) 1)
        "Observation 1 (0.5) is second")))

(deftest responsibility-stats-alist-omits-dominant-counts
  "responsibility-stats->alist with :include-dominant-counts nil should omit key."
  (let* ((responsibilities (make-array '(3 2) :element-type 'double-float
                                       :initial-contents '((0.8d0 0.2d0)
                                                           (0.3d0 0.7d0)
                                                           (0.5d0 0.5d0))))
         (counts (make-array 3 :element-type 'double-float
                             :initial-contents '(10d0 20d0 30d0)))
         (alist (cltd:responsibility-stats->alist responsibilities counts
                                                   :include-dominant-counts nil)))
    (ok (not (assoc :dominant_factor_counts alist))
        ":dominant_factor_counts key is absent when include-dominant-counts is nil")
    (ok (assoc :mean_max_responsibility alist)
        "Other keys are still present")))

(deftest generate-factor-cards-backward-compatibility-strict
  "generate-factor-cards without :include-diagnostics returns plain list of cards."
  (let* ((random-state (cltd:%seed-random-state 42))
         (*random-state* random-state))
    (multiple-value-bind (factor-matrices)
        (cltd:decomposition X-tensor :n-cycle 20 :r 2)
      (let* ((metadata (list
                        (cltd:make-mode-metadata "mode0" '("a" "b"))
                        (cltd:make-mode-metadata "mode1" '("x" "y" "z"))
                        (cltd:make-mode-metadata "mode2" '("p" "q" "r" "s"))))
             (cards (cltd:generate-factor-cards factor-matrices
                                                 X-indices-matrix X-value-vector
                                                 metadata)))
        ;; Should be a plain list, not an alist with :factors
        (ok (listp cards)
            "Result is a list")
        (ok (not (and (listp (first cards))
                      (eq (car (first cards)) :model_diagnostics)))
            "Result is NOT an alist starting with :model_diagnostics")
        ;; Each element should be a card (alist with :factor_id)
        (ok (every (lambda (card) (assoc :factor_id card)) cards)
            "Each element is a card with :factor_id")
        (ok (= (length cards) 2)
            "Returns one card per factor")
        ;; Cards should NOT have diagnostics keys
        (ok (not (assoc :kl_contribution (first cards)))
            "Cards do not have :kl_contribution without diagnostics")
        (ok (not (assoc :contribution_rank (first cards)))
            "Cards do not have :contribution_rank without diagnostics")))))

(deftest generate-factor-cards-kl-contributions-has-normalized
  ":kl_contributions in model_diagnostics should contain normalized shares."
  (let* ((random-state (cltd:%seed-random-state 42))
         (*random-state* random-state))
    (multiple-value-bind (factor-matrices)
        (cltd:decomposition X-tensor :n-cycle 20 :r 2)
      (let* ((metadata (list
                        (cltd:make-mode-metadata "mode0" '("a" "b"))
                        (cltd:make-mode-metadata "mode1" '("x" "y" "z"))
                        (cltd:make-mode-metadata "mode2" '("p" "q" "r" "s"))))
             (result (cltd:generate-factor-cards factor-matrices
                                                  X-indices-matrix X-value-vector
                                                  metadata
                                                  :include-diagnostics t))
             (model-diag (cdr (assoc :model_diagnostics result)))
             (kl-contrib (cdr (assoc :kl_contributions model-diag))))
        (ok (assoc :contributions kl-contrib)
            ":kl_contributions has :contributions key")
        (ok (assoc :total kl-contrib)
            ":kl_contributions has :total key")
        (ok (assoc :normalized kl-contrib)
            ":kl_contributions has :normalized key")
        ;; Verify normalized shares sum to ~1.0
        (let* ((normalized-list (cdr (assoc :normalized kl-contrib)))
               (share-sum (loop for item in normalized-list
                                sum (cdr (assoc :share item)))))
          (ok (< (abs (- share-sum 1.0d0)) 0.01d0)
              "Normalized shares sum to approximately 1.0"))))))

(deftest generate-report-artifacts-diagnostics-json-structure
  "generate-report-artifacts with :include-diagnostics t should write proper JSON structure."
  (let* ((random-state (cltd:%seed-random-state 42))
         (*random-state* random-state))
    (declare (ignorable random-state))
    (multiple-value-bind (factor-matrices)
        (cltd:decomposition X-tensor :n-cycle 20 :r 2)
      (let ((metadata (list
                       (cltd:make-mode-metadata "mode0" '("a" "b"))
                       (cltd:make-mode-metadata "mode1" '("x" "y" "z"))
                       (cltd:make-mode-metadata "mode2" '("p" "q" "r" "s")))))
        ;; Test that we get the right structure when include-diagnostics is t
        (let ((result (cltd:generate-factor-cards factor-matrices
                                                   X-indices-matrix X-value-vector
                                                   metadata
                                                   :include-diagnostics t)))
          ;; Verify structure for JSON serialization
          (ok (listp result)
              "Result with diagnostics is a list (alist)")
          (ok (assoc :model_diagnostics result)
              "Result has :model_diagnostics key")
          (ok (assoc :factors result)
              "Result has :factors key")
          ;; Verify model_diagnostics has expected keys
          (let ((diag (cdr (assoc :model_diagnostics result))))
            (ok (assoc :kl_divergence diag)
                "model_diagnostics has :kl_divergence")
            (ok (assoc :factor_similarity diag)
                "model_diagnostics has :factor_similarity")
            (ok (assoc :exclusivity diag)
                "model_diagnostics has :exclusivity")
            (ok (assoc :kl_contributions diag)
                "model_diagnostics has :kl_contributions"))
          ;; Verify factors have diagnostics keys
          (let ((factors (cdr (assoc :factors result))))
            (ok (listp factors)
                ":factors is a list")
            (ok (> (length factors) 0)
                ":factors is not empty")
            (ok (assoc :kl_contribution (first factors))
                "Factor cards have :kl_contribution")
            (ok (assoc :contribution_rank (first factors))
                "Factor cards have :contribution_rank")))))))

(deftest build-card-alist-optional-keys-absent
  "build-card-alist should not include :kl_contribution/:contribution_rank when not provided."
  ;; Create minimal inputs for build-card-alist
  (let* ((lambda-vec (make-array 2 :element-type 'double-float
                                 :initial-contents '(0.6d0 0.4d0)))
         (cov-counts (make-array 2 :element-type 'double-float
                                 :initial-contents '(100d0 50d0)))
         (cov-shares (make-array 2 :element-type 'double-float
                                 :initial-contents '(0.67d0 0.33d0)))
         ;; Empty summaries for simplicity
         (summaries nil)
         (card (cltd::build-card-alist 0 lambda-vec cov-counts cov-shares
                                        summaries nil nil)))
    (ok (assoc :factor_id card)
        "Card has :factor_id")
    (ok (not (assoc :kl_contribution card))
        "Card does NOT have :kl_contribution when not provided")
    (ok (not (assoc :contribution_rank card))
        "Card does NOT have :contribution_rank when not provided")))

;;; ---------------------------------------------------------------------------
;;; Mode-Spec Reflection in Report Tests
;;; ---------------------------------------------------------------------------

(deftest mode-spec-discretization-reflected-in-card
  "mode-spec discretization should be reflected in factor cards."
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.7d0 0.3d0) (0.2d0 0.8d0))))
         (mode1 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.5d0 0.5d0) (0.3d0 0.7d0) (0.4d0 0.6d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(3 2) :element-type 'fixnum
                              :initial-contents '((0 0) (0 1) (1 2))))
         (counts (make-array 3 :element-type 'double-float
                             :initial-contents '(10d0 20d0 15d0)))
         (metadata (list
                    (cltd:make-mode-metadata "user_type" '("premium" "free")
                                             :discretization "membership-tier")
                    (cltd:make-mode-metadata "category" '("food" "electronics" "clothing")
                                             :discretization "top-3-categories")))
         (cards (cltd:generate-factor-cards factor-matrices indices counts metadata)))
    ;; Check :notes -> :discretization contains mode discretizations
    (let* ((card (first cards))
           (notes (cdr (assoc :notes card)))
           (disc-info (cdr (assoc :discretization notes))))
      (ok disc-info
          "Card :notes has :discretization key")
      (ok (find "membership-tier" disc-info :key #'cdr :test #'string=)
          "discretization includes 'membership-tier' for user_type mode")
      (ok (find "top-3-categories" disc-info :key #'cdr :test #'string=)
          "discretization includes 'top-3-categories' for category mode"))))

(deftest mode-spec-role-reflected-in-card
  "mode-spec role should be reflected in factor cards."
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.8d0 0.2d0) (0.3d0 0.7d0))))
         (mode1 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.6d0 0.4d0) (0.2d0 0.8d0) (0.5d0 0.5d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(3 2) :element-type 'fixnum
                              :initial-contents '((0 0) (0 1) (1 2))))
         (counts (make-array 3 :element-type 'double-float
                             :initial-contents '(10d0 20d0 15d0)))
         (metadata (list
                    (cltd:make-mode-metadata "conversion" '("converted" "not_converted")
                                             :role :purchase
                                             :positive-label "converted"
                                             :negative-label "not_converted")
                    (cltd:make-mode-metadata "segment" '("young" "middle" "senior")
                                             :role :demographic)))
         (cards (cltd:generate-factor-cards factor-matrices indices counts metadata)))
    ;; Check :mode_roles contains the role information
    (let* ((card (first cards))
           (mode-roles (cdr (assoc :mode_roles card))))
      (ok mode-roles
          "Card has :mode_roles key")
      (ok (find :purchase mode-roles :key #'cdr)
          ":mode_roles includes :purchase role")
      (ok (find :demographic mode-roles :key #'cdr)
          ":mode_roles includes :demographic role"))))

(deftest mode-spec-purchase-bias-computed
  "purchase_bias should be computed when :role :purchase is specified."
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.9d0 0.1d0) (0.2d0 0.8d0))))
         (mode1 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.5d0 0.5d0) (0.3d0 0.7d0) (0.4d0 0.6d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(3 2) :element-type 'fixnum
                              :initial-contents '((0 0) (0 1) (1 2))))
         (counts (make-array 3 :element-type 'double-float
                             :initial-contents '(10d0 20d0 15d0)))
         (metadata (list
                    (cltd:make-mode-metadata "purchase" '("yes" "no")
                                             :role :purchase
                                             :positive-label "yes"
                                             :negative-label "no")
                    (cltd:make-mode-metadata "category" '("A" "B" "C"))))
         (cards (cltd:generate-factor-cards factor-matrices indices counts metadata)))
    ;; Check :purchase_bias is present and is an alist with :purchase key
    (let* ((card0 (first cards))
           (card1 (second cards))
           (bias0 (cdr (assoc :purchase_bias card0)))
           (bias1 (cdr (assoc :purchase_bias card1)))
           (purchase0 (cdr (assoc :purchase bias0)))
           (purchase1 (cdr (assoc :purchase bias1))))
      (ok (listp bias0)
          "Factor 0 has :purchase_bias alist")
      (ok (listp bias1)
          "Factor 1 has :purchase_bias alist")
      ;; Factor 0 has high weight on "yes" (0.9)
      (ok (and (numberp purchase0) (> purchase0 0.8))
          "Factor 0 with high 'yes' weight has high purchase probability")
      ;; Factor 1 has low weight on "yes" (0.1)
      (ok (and (numberp purchase1) (< purchase1 0.2))
          "Factor 1 with low 'yes' weight has low purchase probability"))))

(deftest mode-spec-without-purchase-role-has-zero-bias
  "purchase_bias should have zero values when no mode has :role :purchase."
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.7d0 0.3d0) (0.2d0 0.8d0))))
         (mode1 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.5d0 0.5d0) (0.3d0 0.7d0) (0.4d0 0.6d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(3 2) :element-type 'fixnum
                              :initial-contents '((0 0) (0 1) (1 2))))
         (counts (make-array 3 :element-type 'double-float
                             :initial-contents '(10d0 20d0 15d0)))
         ;; No :role :purchase in any metadata
         (metadata (list
                    (cltd:make-mode-metadata "type" '("A" "B")
                                             :role :category)
                    (cltd:make-mode-metadata "segment" '("X" "Y" "Z")
                                             :role :demographic)))
         (cards (cltd:generate-factor-cards factor-matrices indices counts metadata)))
    (let* ((card (first cards))
           (bias (cdr (assoc :purchase_bias card)))
           (purchase-val (cdr (assoc :purchase bias)))
           (not-purchase-val (cdr (assoc :not_purchase bias))))
      (ok (and (numberp purchase-val) (= purchase-val 0.0d0))
          ":purchase value is 0.0 when no mode has :role :purchase")
      (ok (and (numberp not-purchase-val) (= not-purchase-val 0.0d0))
          ":not_purchase value is 0.0 when no mode has :role :purchase"))))

(deftest mode-summaries-contain-full-mode-spec-info
  "mode_summaries should contain discretization and role for each mode."
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.7d0 0.3d0) (0.3d0 0.7d0))))
         (mode1 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.4d0 0.6d0) (0.5d0 0.5d0) (0.6d0 0.4d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(3 2) :element-type 'fixnum
                              :initial-contents '((0 0) (0 1) (1 2))))
         (counts (make-array 3 :element-type 'double-float
                             :initial-contents '(10d0 20d0 15d0)))
         (metadata (list
                    (cltd:make-mode-metadata "action" '("click" "skip")
                                             :role :engagement
                                             :discretization "binary-action")
                    (cltd:make-mode-metadata "time" '("morning" "afternoon" "evening")
                                             :role :temporal
                                             :discretization "time-of-day")))
         (cards (cltd:generate-factor-cards factor-matrices indices counts metadata)))
    (let* ((card (first cards))
           (summaries (cdr (assoc :mode_summaries card))))
      (ok (= (length summaries) 2)
          "mode_summaries has entry for each mode")
      ;; Check first mode summary
      (let ((summary0 (first summaries)))
        (ok (string= (cdr (assoc :name summary0)) "action")
            "First mode summary has correct name")
        (ok (eq (cdr (assoc :role summary0)) :engagement)
            "First mode summary has :role :engagement")
        (ok (string= (cdr (assoc :discretization summary0)) "binary-action")
            "First mode summary has discretization 'binary-action'"))
      ;; Check second mode summary
      (let ((summary1 (second summaries)))
        (ok (string= (cdr (assoc :name summary1)) "time")
            "Second mode summary has correct name")
        (ok (eq (cdr (assoc :role summary1)) :temporal)
            "Second mode summary has :role :temporal")
        (ok (string= (cdr (assoc :discretization summary1)) "time-of-day")
            "Second mode summary has discretization 'time-of-day'")))))

;;; ============================================================================
;;; Boundary Case Tests
;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; R=1 (Single Factor) Tests
;;; ---------------------------------------------------------------------------

(deftest single-factor-decomposition
  "Decomposition with R=1 should produce valid single-factor matrices."
  (let* ((random-state (cltd:%seed-random-state 42))
         (*random-state* random-state))
    (multiple-value-bind (factor-matrices iterations)
        (cltd:decomposition X-tensor :n-cycle 50 :r 1)
      (ok (= (length factor-matrices) 3)
          "Returns 3 factor matrices (one per mode)")
      (ok (= (array-dimension (svref factor-matrices 0) 1) 1)
          "Each factor matrix has R=1 column")
      (ok (numberp iterations)
          "Returns iteration count"))))

(deftest single-factor-similarity-matrix
  "Factor similarity with R=1 returns 1x1 matrix with self-similarity 1.0."
  (let* ((mode0 (make-array '(3 1) :element-type 'double-float
                            :initial-contents '((0.5d0) (0.3d0) (0.2d0))))
         (mode1 (make-array '(4 1) :element-type 'double-float
                            :initial-contents '((0.4d0) (0.3d0) (0.2d0) (0.1d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (sim-matrix (cltd:compute-factor-similarity-matrix factor-matrices)))
    (ok (= (array-dimension sim-matrix 0) 1)
        "Similarity matrix is 1x1")
    (ok (= (array-dimension sim-matrix 1) 1)
        "Similarity matrix is 1x1")
    (ok (< (abs (- (aref sim-matrix 0 0) 1.0d0)) 1.0d-6)
        "Self-similarity is 1.0")))

(deftest single-factor-kl-contributions
  "KL contributions with R=1 should return single contribution."
  (let* ((mode0 (make-array '(2 1) :element-type 'double-float
                            :initial-contents '((0.6d0) (0.4d0))))
         (mode1 (make-array '(3 1) :element-type 'double-float
                            :initial-contents '((0.5d0) (0.3d0) (0.2d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(3 2) :element-type 'fixnum
                              :initial-contents '((0 0) (0 1) (1 2))))
         (counts (make-array 3 :element-type 'double-float
                             :initial-contents '(10.0d0 5.0d0 3.0d0)))
         (contributions (cltd:compute-factor-kl-contributions
                         factor-matrices indices counts)))
    (ok (= (length contributions) 1)
        "Returns single contribution for R=1")
    (ok (numberp (aref contributions 0))
        "Contribution is a number")
    ;; With only one factor, removing it should increase KL significantly
    (ok (>= (aref contributions 0) 0.0d0)
        "Single factor contribution is non-negative")))

(deftest single-factor-responsibilities
  "Responsibilities with R=1 should be all 1.0 (100% to the single factor)."
  (let* ((mode0 (make-array '(2 1) :element-type 'double-float
                            :initial-contents '((0.6d0) (0.4d0))))
         (mode1 (make-array '(3 1) :element-type 'double-float
                            :initial-contents '((0.5d0) (0.3d0) (0.2d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(4 2) :element-type 'fixnum
                              :initial-contents '((0 0) (0 1) (1 0) (1 2))))
         (responsibilities (cltd:compute-observation-responsibilities
                            factor-matrices indices)))
    (ok (= (array-dimension responsibilities 1) 1)
        "Responsibilities has 1 column for R=1")
    ;; All responsibilities should be 1.0
    (loop for obs from 0 below (array-dimension responsibilities 0) do
      (ok (< (abs (- (aref responsibilities obs 0) 1.0d0)) 1.0d-6)
          (format nil "Observation ~D has 100% responsibility to single factor" obs)))))

(deftest single-factor-exclusivity
  "Exclusivity with R=1 should be 1.0 (perfect hard segmentation)."
  (let* ((mode0 (make-array '(2 1) :element-type 'double-float
                            :initial-contents '((0.6d0) (0.4d0))))
         (mode1 (make-array '(3 1) :element-type 'double-float
                            :initial-contents '((0.5d0) (0.3d0) (0.2d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(3 2) :element-type 'fixnum
                              :initial-contents '((0 0) (0 1) (1 2))))
         (counts (make-array 3 :element-type 'double-float
                             :initial-contents '(10.0d0 5.0d0 3.0d0)))
         (responsibilities (cltd:compute-observation-responsibilities
                            factor-matrices indices)))
    (multiple-value-bind (exclusivity overlap)
        (cltd:compute-factor-exclusivity responsibilities counts)
      (ok (< (abs (- exclusivity 1.0d0)) 1.0d-6)
          "Exclusivity is 1.0 for R=1")
      (ok (< overlap 1.0d-6)
          "Overlap is 0.0 for R=1"))))

;;; ---------------------------------------------------------------------------
;;; Identical Factors Tests
;;; ---------------------------------------------------------------------------

(deftest identical-factors-similarity
  "When all factors are identical, similarity should be 1.0 for all pairs."
  (let* ((mode0 (make-array '(3 3) :element-type 'double-float
                            :initial-contents '((0.5d0 0.5d0 0.5d0)
                                                (0.3d0 0.3d0 0.3d0)
                                                (0.2d0 0.2d0 0.2d0))))
         (mode1 (make-array '(4 3) :element-type 'double-float
                            :initial-contents '((0.4d0 0.4d0 0.4d0)
                                                (0.3d0 0.3d0 0.3d0)
                                                (0.2d0 0.2d0 0.2d0)
                                                (0.1d0 0.1d0 0.1d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (sim-matrix (cltd:compute-factor-similarity-matrix factor-matrices)))
    ;; All pairs should have similarity 1.0
    (loop for r1 from 0 below 3 do
      (loop for r2 from 0 below 3 do
        (ok (< (abs (- (aref sim-matrix r1 r2) 1.0d0)) 1.0d-6)
            (format nil "Similarity(~D,~D) = 1.0 for identical factors" r1 r2))))))

(deftest identical-factors-redundancy
  "Redundancy score should be 1.0 when all factors are identical."
  (let* ((mode0 (make-array '(3 3) :element-type 'double-float
                            :initial-contents '((0.5d0 0.5d0 0.5d0)
                                                (0.3d0 0.3d0 0.3d0)
                                                (0.2d0 0.2d0 0.2d0))))
         (mode1 (make-array '(4 3) :element-type 'double-float
                            :initial-contents '((0.4d0 0.4d0 0.4d0)
                                                (0.3d0 0.3d0 0.3d0)
                                                (0.2d0 0.2d0 0.2d0)
                                                (0.1d0 0.1d0 0.1d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (sim-matrix (cltd:compute-factor-similarity-matrix factor-matrices))
         (redundancy (cltd:compute-factor-redundancy-score sim-matrix :threshold 0.9d0)))
    (ok (< (abs (- redundancy 1.0d0)) 1.0d-6)
        "Redundancy is 1.0 for identical factors")))

(deftest identical-factors-responsibilities-uniform
  "With identical factors, responsibilities should be uniform across factors."
  (let* ((mode0 (make-array '(2 3) :element-type 'double-float
                            :initial-contents '((0.5d0 0.5d0 0.5d0)
                                                (0.5d0 0.5d0 0.5d0))))
         (mode1 (make-array '(2 3) :element-type 'double-float
                            :initial-contents '((0.5d0 0.5d0 0.5d0)
                                                (0.5d0 0.5d0 0.5d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(2 2) :element-type 'fixnum
                              :initial-contents '((0 0) (1 1))))
         (responsibilities (cltd:compute-observation-responsibilities
                            factor-matrices indices)))
    ;; Each factor should get 1/3 responsibility
    (let ((expected (/ 1.0d0 3.0d0)))
      (loop for obs from 0 below 2 do
        (loop for r from 0 below 3 do
          (ok (< (abs (- (aref responsibilities obs r) expected)) 1.0d-6)
              (format nil "Obs ~D factor ~D has uniform 1/3 responsibility" obs r)))))))

;;; ---------------------------------------------------------------------------
;;; Extremely Sparse Data (nnz=1) Tests
;;; ---------------------------------------------------------------------------

(deftest single-observation-decomposition
  "Decomposition with nnz=1 should complete without error."
  (let* ((x-shape '(3 4))
         (indices (make-array '(1 2) :element-type 'fixnum
                              :initial-contents '((1 2))))
         (counts (make-array 1 :element-type 'double-float
                             :initial-contents '(5.0d0)))
         (tensor (cltd:make-sparse-tensor x-shape indices counts))
         (random-state (cltd:%seed-random-state 42))
         (*random-state* random-state))
    (multiple-value-bind (factor-matrices iterations)
        (cltd:decomposition tensor :n-cycle 10 :r 2)
      (ok (= (length factor-matrices) 2)
          "Returns factor matrices for 2 modes")
      (ok (numberp iterations)
          "Returns iteration count"))))

(deftest single-observation-kl-divergence
  "KL divergence with nnz=1 should be computable."
  (let* ((indices (make-array '(1 2) :element-type 'fixnum
                              :initial-contents '((0 0))))
         (x-values (make-array 1 :element-type 'double-float
                               :initial-contents '(10.0d0)))
         (x-hat (make-array 1 :element-type 'double-float
                            :initial-contents '(8.0d0)))
         (factors (make-array 2 :initial-contents
                              (list (make-array '(1 1) :element-type 'double-float
                                                :initial-contents '((4.0d0)))
                                    (make-array '(1 1) :element-type 'double-float
                                                :initial-contents '((2.0d0))))))
         (kl (cltd:sparse-kl-divergence indices x-values x-hat factors)))
    (ok (numberp kl)
        "KL divergence is a number")
    (ok (not (cltd:%float-nan-p kl))
        "KL divergence is not NaN")
    (ok (>= kl 0.0d0)
        "KL divergence is non-negative")))

(deftest single-observation-responsibilities
  "Responsibilities with nnz=1 should work correctly."
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.6d0 0.4d0) (0.3d0 0.7d0))))
         (mode1 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.5d0 0.5d0) (0.4d0 0.6d0) (0.3d0 0.7d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(1 2) :element-type 'fixnum
                              :initial-contents '((0 1))))
         (responsibilities (cltd:compute-observation-responsibilities
                            factor-matrices indices)))
    (ok (= (array-dimension responsibilities 0) 1)
        "Has one row for single observation")
    (let ((row-sum (+ (aref responsibilities 0 0) (aref responsibilities 0 1))))
      (ok (< (abs (- row-sum 1.0d0)) 1.0d-6)
          "Single observation responsibilities sum to 1.0"))))

(deftest single-observation-residuals
  "Residuals with nnz=1 should be computable."
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.6d0 0.4d0) (0.3d0 0.7d0))))
         (mode1 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.5d0 0.5d0) (0.4d0 0.6d0) (0.3d0 0.7d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(1 2) :element-type 'fixnum
                              :initial-contents '((0 1))))
         (counts (make-array 1 :element-type 'double-float
                             :initial-contents '(10.0d0)))
         (residuals (cltd:compute-observation-residuals
                     factor-matrices indices counts)))
    (ok (= (length residuals) 1)
        "Returns one residual")
    (ok (numberp (aref residuals 0))
        "Residual is a number")
    (ok (not (cltd:%float-nan-p (aref residuals 0)))
        "Residual is not NaN")))

;;; ---------------------------------------------------------------------------
;;; Large Values (Overflow Resistance) Tests
;;; ---------------------------------------------------------------------------

(deftest large-values-kl-divergence
  "KL divergence with large values (1e10) should not overflow."
  (let* ((indices (make-array '(3 2) :element-type 'fixnum
                              :initial-contents '((0 0) (0 1) (1 0))))
         (x-values (make-array 3 :element-type 'double-float
                               :initial-contents '(1.0d10 5.0d9 2.0d10)))
         (x-hat (make-array 3 :element-type 'double-float
                            :initial-contents '(9.0d9 6.0d9 1.8d10)))
         (factors (make-array 2 :initial-contents
                              (list (make-array '(2 1) :element-type 'double-float
                                                :initial-contents '((1.0d5) (1.0d5)))
                                    (make-array '(2 1) :element-type 'double-float
                                                :initial-contents '((1.0d5) (1.0d5))))))
         (kl (cltd:sparse-kl-divergence indices x-values x-hat factors)))
    (ok (numberp kl)
        "KL divergence is a number with large values")
    (ok (not (cltd:%float-nan-p kl))
        "KL divergence is not NaN with large values")
    (ok (not (cltd:%float-infinity-p kl))
        "KL divergence is not Inf with large values")))

(deftest large-values-responsibilities
  "Responsibilities with large factor values should not overflow."
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((1.0d5 1.0d5) (1.0d5 1.0d5))))
         (mode1 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((1.0d5 1.0d5) (1.0d5 1.0d5))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(2 2) :element-type 'fixnum
                              :initial-contents '((0 0) (1 1))))
         (responsibilities (cltd:compute-observation-responsibilities
                            factor-matrices indices)))
    ;; Check no NaN or Inf
    (loop for obs from 0 below 2 do
      (loop for r from 0 below 2 do
        (let ((val (aref responsibilities obs r)))
          (ok (not (cltd:%float-nan-p val))
              (format nil "Responsibility(~D,~D) is not NaN" obs r))
          (ok (not (cltd:%float-infinity-p val))
              (format nil "Responsibility(~D,~D) is not Inf" obs r)))))
    ;; Rows should still sum to 1.0
    (loop for obs from 0 below 2 do
      (let ((row-sum (+ (aref responsibilities obs 0) (aref responsibilities obs 1))))
        (ok (< (abs (- row-sum 1.0d0)) 1.0d-6)
            (format nil "Row ~D sums to 1.0" obs))))))

(deftest large-values-residuals
  "Residuals with large count values should not overflow."
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((1.0d3 1.0d3) (1.0d3 1.0d3))))
         (mode1 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((1.0d3 1.0d3) (1.0d3 1.0d3))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(2 2) :element-type 'fixnum
                              :initial-contents '((0 0) (1 1))))
         (counts (make-array 2 :element-type 'double-float
                             :initial-contents '(1.0d10 5.0d9)))
         (residuals (cltd:compute-observation-residuals
                     factor-matrices indices counts)))
    (loop for i from 0 below 2 do
      (ok (not (cltd:%float-nan-p (aref residuals i)))
          (format nil "Residual ~D is not NaN" i))
      (ok (not (cltd:%float-infinity-p (aref residuals i)))
          (format nil "Residual ~D is not Inf" i)))))

;;; ---------------------------------------------------------------------------
;;; Small Values (Underflow Resistance) Tests
;;; ---------------------------------------------------------------------------

(deftest small-values-kl-divergence
  "KL divergence with very small positive values should not produce NaN."
  (let* ((indices (make-array '(3 2) :element-type 'fixnum
                              :initial-contents '((0 0) (0 1) (1 0))))
         (x-values (make-array 3 :element-type 'double-float
                               :initial-contents '(1.0d-100 1.0d-100 1.0d-100)))
         (x-hat (make-array 3 :element-type 'double-float
                            :initial-contents '(1.0d-100 1.0d-100 1.0d-100)))
         (factors (make-array 2 :initial-contents
                              (list (make-array '(2 1) :element-type 'double-float
                                                :initial-contents '((1.0d-50) (1.0d-50)))
                                    (make-array '(2 1) :element-type 'double-float
                                                :initial-contents '((1.0d-50) (1.0d-50))))))
         (kl (cltd:sparse-kl-divergence indices x-values x-hat factors)))
    (ok (numberp kl)
        "KL divergence is a number with tiny values")
    (ok (not (cltd:%float-nan-p kl))
        "KL divergence is not NaN with tiny values")))

(deftest small-factor-values-similarity
  "Similarity with very small factor values should not produce NaN."
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((1.0d-150 1.0d-150)
                                                (1.0d-150 1.0d-150))))
         (mode1 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((1.0d-150 1.0d-150)
                                                (1.0d-150 1.0d-150))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (sim-matrix (cltd:compute-factor-similarity-matrix factor-matrices)))
    ;; With epsilon protection, should return 0.0 rather than NaN
    (loop for r1 from 0 below 2 do
      (loop for r2 from 0 below 2 do
        (ok (not (cltd:%float-nan-p (aref sim-matrix r1 r2)))
            (format nil "Similarity(~D,~D) is not NaN with tiny values" r1 r2))))))

(deftest small-factor-values-responsibilities
  "Responsibilities with very small factor values should use uniform fallback."
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((1.0d-200 1.0d-200)
                                                (1.0d-200 1.0d-200))))
         (mode1 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((1.0d-200 1.0d-200)
                                                (1.0d-200 1.0d-200))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(2 2) :element-type 'fixnum
                              :initial-contents '((0 0) (1 1))))
         (responsibilities (cltd:compute-observation-responsibilities
                            factor-matrices indices)))
    ;; Should fall back to uniform distribution (1/R each)
    (loop for obs from 0 below 2 do
      (loop for r from 0 below 2 do
        (ok (not (cltd:%float-nan-p (aref responsibilities obs r)))
            (format nil "Responsibility(~D,~D) is not NaN" obs r)))
      ;; Row should sum to 1.0
      (let ((row-sum (+ (aref responsibilities obs 0) (aref responsibilities obs 1))))
        (ok (< (abs (- row-sum 1.0d0)) 1.0d-6)
            (format nil "Row ~D sums to 1.0 with tiny factor values" obs))))))

(deftest zero-factor-column-similarity
  "Similarity when a factor column is all zeros should be 0 (not NaN)."
  (let* ((mode0 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.5d0 0.0d0)
                                                (0.3d0 0.0d0)
                                                (0.2d0 0.0d0))))
         (mode1 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.6d0 0.0d0)
                                                (0.4d0 0.0d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (sim-matrix (cltd:compute-factor-similarity-matrix factor-matrices)))
    ;; Factor 1 has zero norm, similarity with factor 0 should be 0
    (ok (not (cltd:%float-nan-p (aref sim-matrix 0 1)))
        "Similarity with zero-column factor is not NaN")
    (ok (< (abs (aref sim-matrix 0 1)) 1.0d-6)
        "Similarity with zero-column factor is 0")))

;;; ---------------------------------------------------------------------------
;;; Edge Case: All Zeros
;;; ---------------------------------------------------------------------------

(deftest all-zero-counts-residuals
  "Residuals with all zero counts should equal reconstruction values."
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.3d0 0.7d0) (0.5d0 0.5d0))))
         (mode1 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.4d0 0.6d0) (0.6d0 0.4d0))))
         (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(2 2) :element-type 'fixnum
                              :initial-contents '((0 0) (1 1))))
         (counts (make-array 2 :element-type 'double-float
                             :initial-contents '(0.0d0 0.0d0)))
         (residuals (cltd:compute-observation-residuals
                     factor-matrices indices counts)))
    ;; When x=0, residual = x-hat (the reconstruction)
    (loop for i from 0 below 2 do
      (ok (not (cltd:%float-nan-p (aref residuals i)))
          (format nil "Residual ~D is not NaN when count is 0" i))
      (ok (>= (aref residuals i) 0.0d0)
          (format nil "Residual ~D is non-negative when count is 0" i)))))

(deftest normalize-contributions-all-zero
  "normalize-contributions with all zeros should return uniform distribution."
  (let* ((contributions (make-array 4 :element-type 'double-float
                                    :initial-contents '(0.0d0 0.0d0 0.0d0 0.0d0)))
         (normalized (cltd:normalize-contributions contributions)))
    (ok (= (length normalized) 4)
        "Returns array of same length")
    (loop for i from 0 below 4 do
      (ok (< (abs (- (aref normalized i) 0.25d0)) 1.0d-6)
          (format nil "Element ~D is 0.25 (uniform)" i)))
    (let ((sum (loop for i from 0 below 4 sum (aref normalized i))))
      (ok (< (abs (- sum 1.0d0)) 1.0d-6)
          "Sum is 1.0"))))

;;; ===========================================================================
;;; Input Validation Tests
;;; ===========================================================================

(deftest validate-input-data-accepts-valid-data
  "validate-input-data returns T for valid input."
  (let ((x-shape '(2 3 4))
        (indices (make-array '(3 3) :element-type 'fixnum
                             :initial-contents '((0 1 0) (1 2 3) (0 0 1))))
        (values (make-array 3 :element-type 'double-float
                            :initial-contents '(1.0d0 2.0d0 3.0d0))))
    (ok (eq t (cltd:validate-input-data x-shape indices values))
        "Valid data returns T")))

(defun %test-signals-invalid-input-error (thunk message)
  "Helper to test that THUNK signals invalid-input-error."
  (let ((caught nil))
    (handler-case
        (funcall thunk)
      (cltd:invalid-input-error (c)
        (declare (ignore c))
        (setf caught t)))
    (ok caught message)))

(deftest validate-input-data-rejects-empty-shape
  "validate-input-data signals error for empty x-shape."
  (let ((x-shape '())
        (indices (make-array '(1 0) :element-type 'fixnum))
        (values (make-array 1 :element-type 'double-float
                            :initial-contents '(1.0d0))))
    (%test-signals-invalid-input-error
     (lambda () (cltd:validate-input-data x-shape indices values))
     "Empty x-shape signals invalid-input-error")))

(deftest validate-input-data-rejects-non-positive-dimension
  "validate-input-data signals error for zero or negative dimension."
  (let ((x-shape '(2 0 4))
        (indices (make-array '(1 3) :element-type 'fixnum
                             :initial-contents '((0 0 0))))
        (values (make-array 1 :element-type 'double-float
                            :initial-contents '(1.0d0))))
    (%test-signals-invalid-input-error
     (lambda () (cltd:validate-input-data x-shape indices values))
     "Zero dimension signals invalid-input-error")))

(deftest validate-input-data-rejects-mode-count-mismatch
  "validate-input-data signals error when mode counts don't match."
  (let ((x-shape '(2 3))  ; 2 modes
        (indices (make-array '(1 3) :element-type 'fixnum  ; 3 modes
                             :initial-contents '((0 0 0))))
        (values (make-array 1 :element-type 'double-float
                            :initial-contents '(1.0d0))))
    (%test-signals-invalid-input-error
     (lambda () (cltd:validate-input-data x-shape indices values))
     "Mode count mismatch signals invalid-input-error")))

(deftest validate-input-data-rejects-length-mismatch
  "validate-input-data signals error when value vector length doesn't match."
  (let ((x-shape '(2 3))
        (indices (make-array '(2 2) :element-type 'fixnum
                             :initial-contents '((0 0) (1 1))))
        (values (make-array 3 :element-type 'double-float  ; 3 values but 2 observations
                            :initial-contents '(1.0d0 2.0d0 3.0d0))))
    (%test-signals-invalid-input-error
     (lambda () (cltd:validate-input-data x-shape indices values))
     "Value vector length mismatch signals invalid-input-error")))

(deftest validate-input-data-rejects-out-of-bounds-index
  "validate-input-data signals error for out-of-bounds indices."
  (let ((x-shape '(2 3))
        (indices (make-array '(1 2) :element-type 'fixnum
                             :initial-contents '((0 5))))  ; 5 >= 3
        (values (make-array 1 :element-type 'double-float
                            :initial-contents '(1.0d0))))
    (%test-signals-invalid-input-error
     (lambda () (cltd:validate-input-data x-shape indices values))
     "Out-of-bounds index signals invalid-input-error")))

(deftest validate-input-data-rejects-negative-value
  "validate-input-data signals error for negative counts."
  (let ((x-shape '(2 3))
        (indices (make-array '(1 2) :element-type 'fixnum
                             :initial-contents '((0 0))))
        (values (make-array 1 :element-type 'double-float
                            :initial-contents '(-1.0d0))))
    (%test-signals-invalid-input-error
     (lambda () (cltd:validate-input-data x-shape indices values))
     "Negative value signals invalid-input-error")))

(deftest validate-input-data-rejects-nan-value
  "validate-input-data signals error for NaN in values."
  (let ((x-shape '(2 3))
        (indices (make-array '(2 2) :element-type 'fixnum
                             :initial-contents '((0 0) (1 1))))
        (values (make-array 2 :element-type 'double-float
                            :initial-contents '(1.0d0 1.0d0))))
    ;; Create NaN portably
    (setf (aref values 1)
          #+sbcl (sb-kernel:make-double-float -524288 0)
          #-sbcl (- cltd:+double-float-positive-infinity+
                    cltd:+double-float-positive-infinity+))
    ;; Ensure we have a NaN
    (when (cltd:%float-nan-p (aref values 1))
      (let ((caught nil))
        (handler-case
            (cltd:validate-input-data x-shape indices values)
          (cltd:invalid-input-error (c)
            (declare (ignore c))
            (setf caught t)))
        (ok caught
            "NaN value signals invalid-input-error")))))

(deftest validate-input-data-rejects-infinite-value
  "validate-input-data signals error for infinite values."
  (let ((x-shape '(2 3))
        (indices (make-array '(1 2) :element-type 'fixnum
                             :initial-contents '((0 0))))
        (values (make-array 1 :element-type 'double-float
                            :initial-contents '(1.0d0))))
    ;; Use SBCL's infinity constant
    (setf (aref values 0) cltd:+double-float-positive-infinity+)
    (let ((caught nil))
      (handler-case
          (cltd:validate-input-data x-shape indices values)
        (cltd:invalid-input-error (c)
          (declare (ignore c))
          (setf caught t)))
      (ok caught
          "Infinite value signals invalid-input-error"))))

(deftest validate-input-data-non-error-mode
  "validate-input-data returns multiple values instead of error when error-on-invalid=nil."
  (let ((x-shape '(2 -1))  ; Invalid: negative dimension
        (indices (make-array '(1 2) :element-type 'fixnum
                             :initial-contents '((0 0))))
        (values (make-array 1 :element-type 'double-float
                            :initial-contents '(1.0d0))))
    (multiple-value-bind (valid reason details)
        (cltd:validate-input-data x-shape indices values :error-on-invalid nil)
      (ok (null valid)
          "Returns NIL for invalid data")
      (ok (eq reason :invalid-shape)
          "Returns :invalid-shape as reason")
      (ok (stringp details)
          "Returns string details"))))

;;; ===========================================================================
;;; Condition Type Tests
;;; ===========================================================================

(deftest condition-invalid-input-error-hierarchy
  "invalid-input-error is a subtype of tensor-decomposition-error."
  (let ((condition (make-condition 'cltd:invalid-input-error
                                   :reason :test-reason
                                   :details "test details")))
    (ok (typep condition 'cltd:tensor-decomposition-error)
        "invalid-input-error is a tensor-decomposition-error")
    (ok (typep condition 'error)
        "invalid-input-error is an error")
    (ok (eq (cltd:invalid-input-reason condition) :test-reason)
        "Reason accessor works")
    (ok (equal (cltd:invalid-input-details condition) "test details")
        "Details accessor works")))

(deftest condition-numerical-instability-error-hierarchy
  "numerical-instability-error is a subtype of tensor-decomposition-error."
  (let ((condition (make-condition 'cltd:numerical-instability-error
                                   :location "test-location"
                                   :value cltd:+double-float-positive-infinity+
                                   :operation "test-op")))
    (ok (typep condition 'cltd:tensor-decomposition-error)
        "numerical-instability-error is a tensor-decomposition-error")
    (ok (typep condition 'error)
        "numerical-instability-error is an error")
    (ok (equal (cltd:instability-location condition) "test-location")
        "Location accessor works")
    (ok (equal (cltd:instability-operation condition) "test-op")
        "Operation accessor works")))

(deftest make-sparse-tensor-validates-input
  "make-sparse-tensor validates input and signals invalid-input-error for bad data."
  (let ((x-shape '(2 3))
        (indices (make-array '(1 2) :element-type 'fixnum
                             :initial-contents '((0 5))))  ; Out of bounds
        (values (make-array 1 :element-type 'double-float
                            :initial-contents '(1.0d0))))
    ;; Use handler-case to verify the right condition is raised
    (let ((caught nil))
      (handler-case
          (cltd:make-sparse-tensor x-shape indices values)
        (cltd:invalid-input-error (c)
          (declare (ignore c))
          (setf caught t)))
      (ok caught
          "make-sparse-tensor signals invalid-input-error for out-of-bounds indices"))))

(deftest make-sparse-tensor-validates-domains
  "make-sparse-tensor validates domains and rejects invalid domain specs."
  (let ((x-shape '(2 3))
        (indices (make-array '(1 2) :element-type 'fixnum
                             :initial-contents '((0 1))))
        (values (make-array 1 :element-type 'double-float
                            :initial-contents '(1.0d0)))
        ;; Invalid domain: list without :name key
        (bad-domains (list '(:labels ("a" "b"))  ; missing :name
                           nil)))
    (let ((caught nil))
      (handler-case
          (cltd:make-sparse-tensor x-shape indices values :domains bad-domains)
        (cltd:invalid-input-error (c)
          (declare (ignore c))
          (setf caught t)))
      (ok caught
          "make-sparse-tensor signals invalid-input-error for domains without :name"))))

(deftest make-sparse-tensor-validates-domains-length
  "make-sparse-tensor validates that domains length matches mode count."
  (let ((x-shape '(2 3))
        (indices (make-array '(1 2) :element-type 'fixnum
                             :initial-contents '((0 1))))
        (values (make-array 1 :element-type 'double-float
                            :initial-contents '(1.0d0))))
    ;; Test: too few domains
    (let ((short-domains (list (cltd:make-mode-metadata "mode0" '("a" "b")))))
      (let ((caught nil))
        (handler-case
            (cltd:make-sparse-tensor x-shape indices values :domains short-domains)
          (cltd:invalid-input-error (c)
            (declare (ignore c))
            (setf caught t)))
        (ok caught
            "make-sparse-tensor signals invalid-input-error for too few domains")))
    ;; Test: too many domains
    (let ((long-domains (list (cltd:make-mode-metadata "mode0" '("a" "b"))
                              (cltd:make-mode-metadata "mode1" '("x" "y" "z"))
                              (cltd:make-mode-metadata "mode2" '("extra")))))
      (let ((caught nil))
        (handler-case
            (cltd:make-sparse-tensor x-shape indices values :domains long-domains)
          (cltd:invalid-input-error (c)
            (declare (ignore c))
            (setf caught t)))
        (ok caught
            "make-sparse-tensor signals invalid-input-error for too many domains")))))

(deftest sparse-tensor-nnz-returns-correct-count
  "sparse-tensor-nnz returns the number of non-zero entries."
  ;; x-tensor has 3 non-zero entries (defined at top of file)
  (ok (= 3 (cltd:sparse-tensor-nnz x-tensor))
      "sparse-tensor-nnz returns 3 for x-tensor"))

(deftest sparse-tensor-nnz-empty-tensor
  "sparse-tensor-nnz returns 0 for an empty tensor."
  (let* ((shape '(2 3))
         (indices (make-array '(0 2) :element-type 'fixnum))
         (values (make-array 0 :element-type 'double-float))
         (tensor (cltd:make-sparse-tensor shape indices values)))
    (ok (= 0 (cltd:sparse-tensor-nnz tensor))
        "sparse-tensor-nnz returns 0 for empty tensor")))

(deftest sparse-tensor-n-modes-2-mode
  "sparse-tensor-n-modes returns 2 for a 2-mode tensor."
  (let* ((shape '(2 3))
         (indices (make-array '(1 2) :element-type 'fixnum
                              :initial-contents '((0 1))))
         (values (make-array 1 :element-type 'double-float
                             :initial-contents '(1.0d0)))
         (tensor (cltd:make-sparse-tensor shape indices values)))
    (ok (= 2 (cltd:sparse-tensor-n-modes tensor))
        "sparse-tensor-n-modes returns 2 for 2-mode tensor")))

(deftest sparse-tensor-n-modes-3-mode
  "sparse-tensor-n-modes returns 3 for x-tensor (3-mode)."
  ;; x-tensor has shape (2 3 4)
  (ok (= 3 (cltd:sparse-tensor-n-modes x-tensor))
      "sparse-tensor-n-modes returns 3 for x-tensor"))

(deftest sparse-tensor-n-modes-4-mode
  "sparse-tensor-n-modes returns 4 for a 4-mode tensor."
  (let* ((shape '(2 3 4 5))
         (indices (make-array '(1 4) :element-type 'fixnum
                              :initial-contents '((0 1 2 3))))
         (values (make-array 1 :element-type 'double-float
                             :initial-contents '(1.0d0)))
         (tensor (cltd:make-sparse-tensor shape indices values)))
    (ok (= 4 (cltd:sparse-tensor-n-modes tensor))
        "sparse-tensor-n-modes returns 4 for 4-mode tensor")))

(deftest sparse-tensor-mode-labels-with-domains
  "sparse-tensor-mode-labels returns labels when domains are set."
  (let* ((shape '(2 3))
         (indices (make-array '(1 2) :element-type 'fixnum
                              :initial-contents '((0 1))))
         (values (make-array 1 :element-type 'double-float
                             :initial-contents '(1.0d0)))
         (domains (list (cltd:make-mode-metadata "mode0" '("a" "b"))
                        (cltd:make-mode-metadata "mode1" '("x" "y" "z"))))
         (tensor (cltd:make-sparse-tensor shape indices values :domains domains)))
    (ok (equalp #("a" "b") (cltd:sparse-tensor-mode-labels tensor 0))
        "sparse-tensor-mode-labels returns correct labels for mode 0")
    (ok (equalp #("x" "y" "z") (cltd:sparse-tensor-mode-labels tensor 1))
        "sparse-tensor-mode-labels returns correct labels for mode 1")))

(deftest sparse-tensor-mode-labels-nil-domains
  "sparse-tensor-mode-labels returns NIL when domains is NIL."
  ;; x-tensor has no domains set
  (ok (null (cltd:sparse-tensor-mode-labels x-tensor 0))
      "sparse-tensor-mode-labels returns NIL for tensor without domains"))

(deftest sparse-tensor-mode-labels-nil-mode-spec
  "sparse-tensor-mode-labels returns NIL when mode-spec is NIL."
  (let* ((shape '(2 3))
         (indices (make-array '(1 2) :element-type 'fixnum
                              :initial-contents '((0 1))))
         (values (make-array 1 :element-type 'double-float
                             :initial-contents '(1.0d0)))
         ;; First mode has NIL, second mode has a mode-spec
         (domains (vector nil (cltd:make-mode-metadata "mode1" '("x" "y" "z"))))
         (tensor (cltd:make-sparse-tensor shape indices values :domains domains)))
    (ok (null (cltd:sparse-tensor-mode-labels tensor 0))
        "sparse-tensor-mode-labels returns NIL for NIL mode-spec")
    (ok (equalp #("x" "y" "z") (cltd:sparse-tensor-mode-labels tensor 1))
        "sparse-tensor-mode-labels returns labels for non-NIL mode-spec")))

(deftest sparse-tensor-mode-name-with-domains
  "sparse-tensor-mode-name returns mode name when domains are set."
  (let* ((shape '(2 3))
         (indices (make-array '(1 2) :element-type 'fixnum
                              :initial-contents '((0 1))))
         (values (make-array 1 :element-type 'double-float
                             :initial-contents '(1.0d0)))
         (domains (list (cltd:make-mode-metadata "user" '("a" "b"))
                        (cltd:make-mode-metadata "product" '("x" "y" "z"))))
         (tensor (cltd:make-sparse-tensor shape indices values :domains domains)))
    (ok (string= "user" (cltd:sparse-tensor-mode-name tensor 0))
        "sparse-tensor-mode-name returns correct name for mode 0")
    (ok (string= "product" (cltd:sparse-tensor-mode-name tensor 1))
        "sparse-tensor-mode-name returns correct name for mode 1")))

(deftest sparse-tensor-mode-name-nil-domains
  "sparse-tensor-mode-name returns NIL when domains is NIL."
  ;; x-tensor has no domains set
  (ok (null (cltd:sparse-tensor-mode-name x-tensor 0))
      "sparse-tensor-mode-name returns NIL for tensor without domains"))

(deftest sparse-tensor-total-count-sums-values
  "sparse-tensor-total-count returns the sum of all values."
  ;; x-tensor has values (1.0d0 2.0d0 3.0d0), sum = 6.0d0
  (ok (< (abs (- 6.0d0 (cltd:sparse-tensor-total-count x-tensor))) +test-epsilon+)
      "sparse-tensor-total-count returns 6.0d0 for x-tensor"))

(deftest sparse-tensor-total-count-returns-double-float
  "sparse-tensor-total-count returns a double-float."
  (ok (typep (cltd:sparse-tensor-total-count x-tensor) 'double-float)
      "sparse-tensor-total-count returns double-float type"))

(deftest sparse-tensor-with-aux-data
  "make-sparse-tensor accepts :aux argument and sparse-tensor-aux retrieves it."
  (let* ((shape '(2 3))
         (indices (make-array '(1 2) :element-type 'fixnum
                              :initial-contents '((0 1))))
         (values (make-array 1 :element-type 'double-float
                             :initial-contents '(1.0d0)))
         (aux-data '(:metadata "test" :version 1))
         (tensor (cltd:make-sparse-tensor shape indices values :aux aux-data)))
    (ok (equal aux-data (cltd:sparse-tensor-aux tensor))
        "sparse-tensor-aux returns the auxiliary data")))

(deftest sparse-tensor-aux-default-nil
  "sparse-tensor-aux returns NIL when :aux is not specified."
  ;; x-tensor has no aux data set
  (ok (null (cltd:sparse-tensor-aux x-tensor))
      "sparse-tensor-aux returns NIL for tensor without aux data"))


;;; ============================================================
;;; select-rank-1se tests
;;; ============================================================

(deftest select-rank-1se-returns-expected-structure
  "select-rank-1se returns two values: selected result alist and full cv-results."
  (let ((ranks '(1 2)))
    (multiple-value-bind (selected all-results)
        (cltd:select-rank-1se CV-tensor ranks
                              :k 3 :n-cycle 10
                              :random-state (cltd:%seed-random-state 51))
      (dolist (key '(:rank :mean :std :standard-error :scores :validation-counts))
        (ok (assoc key selected)
            (format nil "Selected result has ~S key" key)))
      (ok (= (length all-results) (length ranks))
          "All-results has one entry per rank")
      (ok (equal (mapcar (lambda (r) (cdr (assoc :rank r))) all-results) ranks)
          "All-results keeps the input rank order")
      (ok (member (cdr (assoc :rank selected)) ranks)
          "Selected rank is from candidates"))))

(deftest select-rank-1se-selects-smaller-or-equal-rank
  "select-rank-1se should select a rank <= the rank selected by select-rank.

The 1-SE rule favors simpler models, so when the best rank has high variance,
1-SE may select a smaller rank that is within one standard error of the best."
  (let ((ranks '(1 2))
        (seed 88))
    (multiple-value-bind (best-1se results-1se)
        (cltd:select-rank-1se CV-tensor ranks
                              :k 3 :n-cycle 10
                              :random-state (cltd:%seed-random-state seed))
      (declare (ignore results-1se))
      (multiple-value-bind (best-min results-min)
          (cltd:select-rank CV-tensor ranks
                            :k 3 :n-cycle 10
                            :random-state (cltd:%seed-random-state seed))
        (declare (ignore results-min))
        (let ((rank-1se (cdr (assoc :rank best-1se)))
              (rank-min (cdr (assoc :rank best-min))))
          (ok (<= rank-1se rank-min)
              (format nil "1-SE rank (~D) <= min rank (~D)" rank-1se rank-min)))))))

(deftest select-rank-1se-threshold-uses-standard-error
  "The 1-SE threshold uses std/sqrt(k), not the raw standard deviation."
  (let ((ranks '(1 2)))
    (multiple-value-bind (selected cv-results)
        (cltd:select-rank-1se CV-tensor ranks
                              :k 3 :n-cycle 10
                              :random-state (cltd:%seed-random-state 64))
      (let* ((best (reduce (lambda (a b)
                             (if (<= (cdr (assoc :mean a)) (cdr (assoc :mean b))) a b))
                           cv-results))
             (best-mean (cdr (assoc :mean best)))
             (best-se (cdr (assoc :standard-error best)))
             (threshold (+ best-mean best-se))
             (selected-mean (cdr (assoc :mean selected))))
        (ok (< (abs (- best-se (/ (cdr (assoc :std best)) (sqrt 3.0d0)))) 1d-12)
            "The reported standard error is std / sqrt(k)")
        (ok (<= selected-mean (+ threshold +test-epsilon+))
            (format nil "Selected mean (~,6F) <= threshold (~,6F)"
                    selected-mean threshold))))))

(deftest select-rank-1se-selects-smallest-within-threshold
  "When multiple ranks are within the 1-SE threshold, the smallest is chosen."
  (let ((ranks '(1 2 3)))
    (multiple-value-bind (selected cv-results)
        (cltd:select-rank-1se CV-tensor ranks
                              :k 3 :n-cycle 10
                              :random-state (cltd:%seed-random-state 72))
      (let* ((best (reduce (lambda (a b)
                             (if (<= (cdr (assoc :mean a)) (cdr (assoc :mean b))) a b))
                           cv-results))
             (threshold (+ (cdr (assoc :mean best))
                           (cdr (assoc :standard-error best))))
             (within (remove-if (lambda (r) (> (cdr (assoc :mean r)) threshold))
                                cv-results))
             (smallest-rank (reduce #'min within
                                    :key (lambda (r) (cdr (assoc :rank r))))))
        (ok (= (cdr (assoc :rank selected)) smallest-rank)
            (format nil "Selected smallest rank (~D) within threshold"
                    (cdr (assoc :rank selected))))
        (ok (= (length cv-results) (length ranks))
            "1-SE selection leaves the result list intact")))))
(deftest sparse-kl-divergence-includes-implicit-zero-mass
  "Every coordinate of a 2x2 rank-1 model predicts 2, so the total mass is 8."
  (let* ((mode0 (make-array '(2 1) :element-type 'double-float
                            :initial-contents '((2d0) (2d0))))
         (mode1 (make-array '(2 1) :element-type 'double-float
                            :initial-contents '((1d0) (1d0))))
         (factors (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(1 2) :element-type 'fixnum
                              :initial-contents '((0 0))))
         (values (make-array 1 :element-type 'double-float
                             :initial-contents '(1d0)))
         (x-hat (make-array 1 :element-type 'double-float :initial-element 0d0)))
    (cltd:sdot factors indices x-hat)
    (ok (< (abs (- (aref x-hat 0) 2d0)) +test-epsilon+)
        "Reconstruction at the observed coordinate is 2")
    (let ((kl (cltd:sparse-kl-divergence indices values x-hat factors))
          (expected (+ 8d0 (log (/ 1d0 2d0)) -1d0)))
      (ok (< (abs (- kl expected)) 1d-5)
          (format nil "KL ~,9F matches ~,9F including implicit-zero mass"
                  kl expected))
      (ok (> kl 6d0)
          "KL is not the nnz-only value (~0.30685) of the old implementation"))))

(deftest sparse-kl-divergence-matches-dense-enumeration
  "Sparse KL agrees with a dense enumeration of the generalized KL objective."
  (let* ((shape '(3 2 4))
         (mode0 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.7d0 0.2d0)
                                                (0.1d0 0.9d0)
                                                (0.4d0 0.5d0))))
         (mode1 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((1.3d0 0.6d0)
                                                (0.8d0 1.1d0))))
         (mode2 (make-array '(4 2) :element-type 'double-float
                            :initial-contents '((0.3d0 1.2d0)
                                                (0.9d0 0.4d0)
                                                (1.5d0 0.7d0)
                                                (0.2d0 0.8d0))))
         (factors (make-array 3 :initial-contents (list mode0 mode1 mode2)))
         (indices (make-array '(5 3) :element-type 'fixnum
                              :initial-contents '((0 0 0)
                                                  (1 1 2)
                                                  (2 0 3)
                                                  (0 1 1)
                                                  (2 1 0))))
         (values (make-array 5 :element-type 'double-float
                             :initial-contents '(2d0 5d0 1d0 3d0 4d0)))
         (x-hat (make-array 5 :element-type 'double-float :initial-element 0d0)))
    (cltd:sdot factors indices x-hat)
    (let ((sparse-kl (cltd:sparse-kl-divergence indices values x-hat factors))
          (dense-kl (%dense-generalized-kl shape factors indices values)))
      (ok (< (abs (- sparse-kl dense-kl)) 1d-9)
          (format nil "Sparse KL ~,9F matches dense reference ~,9F"
                  sparse-kl dense-kl)))))

(deftest cp-total-mass-matches-dense-sum
  "Total predicted mass aggregated from CP structure equals the dense sum."
  (let* ((shape '(3 2 4))
         (mode0 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.7d0 0.2d0)
                                                (0.1d0 0.9d0)
                                                (0.4d0 0.5d0))))
         (mode1 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((1.3d0 0.6d0)
                                                (0.8d0 1.1d0))))
         (mode2 (make-array '(4 2) :element-type 'double-float
                            :initial-contents '((0.3d0 1.2d0)
                                                (0.9d0 0.4d0)
                                                (1.5d0 0.7d0)
                                                (0.2d0 0.8d0))))
         (factors (make-array 3 :initial-contents (list mode0 mode1 mode2))))
    (ok (< (abs (- (cltd::%cp-total-mass factors)
                   (%dense-total-mass shape factors)))
           1d-9)
        "%cp-total-mass equals the sum of the dense reconstruction")))

(deftest sparse-kl-divergence-ignores-explicit-zeros
  "An explicitly stored zero must not add its reconstruction mass twice."
  (let* ((mode0 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((0.6d0 0.4d0)
                                                (0.3d0 0.7d0))))
         (mode1 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.5d0 0.5d0)
                                                (0.4d0 0.6d0)
                                                (0.9d0 0.1d0))))
         (factors (make-array 2 :initial-contents (list mode0 mode1)))
         (dense-indices (make-array '(3 2) :element-type 'fixnum
                                    :initial-contents '((0 0) (0 1) (1 2))))
         (dense-values (make-array 3 :element-type 'double-float
                                   :initial-contents '(2d0 0d0 5d0)))
         (dense-hat (make-array 3 :element-type 'double-float :initial-element 0d0))
         (lean-indices (make-array '(2 2) :element-type 'fixnum
                                   :initial-contents '((0 0) (1 2))))
         (lean-values (make-array 2 :element-type 'double-float
                                  :initial-contents '(2d0 5d0)))
         (lean-hat (make-array 2 :element-type 'double-float :initial-element 0d0)))
    (cltd:sdot factors dense-indices dense-hat)
    (cltd:sdot factors lean-indices lean-hat)
    (let ((with-zero (cltd:sparse-kl-divergence dense-indices dense-values
                                                dense-hat factors))
          (without-zero (cltd:sparse-kl-divergence lean-indices lean-values
                                                   lean-hat factors)))
      (ok (< (abs (- with-zero without-zero)) 1d-12)
          (format nil "Explicit zero leaves KL unchanged (~,9F vs ~,9F)"
                  with-zero without-zero)))))

(deftest decomposition-final-kl-matches-final-factors
  "FINAL-KL and the last KL-HISTORY entry describe the returned factor matrices."
  (let* ((shape '(3 4))
         (indices (make-array '(4 2) :element-type 'fixnum
                              :initial-contents '((0 0) (1 1) (2 2) (0 3))))
         (values (make-array 4 :element-type 'double-float
                             :initial-contents '(3d0 1d0 4d0 2d0)))
         (tensor (cltd:make-sparse-tensor shape indices values))
         (*random-state* (cltd:%seed-random-state 7)))
    (multiple-value-bind (factors iterations final-kl kl-history)
        (cltd:decomposition tensor :r 2 :n-cycle 20)
      (declare (ignore iterations))
      (let ((x-hat (make-array 4 :element-type 'double-float :initial-element 0d0)))
        (cltd:sdot factors indices x-hat)
        (let ((recomputed (cltd:sparse-kl-divergence indices values x-hat factors)))
          (ok (< (abs (- final-kl recomputed)) 1d-9)
              (format nil "final-kl ~,9F equals KL recomputed from final factors ~,9F"
                      final-kl recomputed))
          (ok (< (abs (- (aref kl-history (1- (length kl-history))) recomputed))
                 1d-9)
              "Last kl-history entry equals KL recomputed from final factors"))))))


;;; ---------------------------------------------------------------------------
;;; Exposure correction: PREDICTION-SCALE
;;; ---------------------------------------------------------------------------

(deftest sparse-kl-divergence-scales-entries-and-total-mass
  "PREDICTION-SCALE must scale the stored-entry predictions and the total mass."
  (let* ((shape '(3 2 4))
         (mode0 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.7d0 0.2d0)
                                                (0.1d0 0.9d0)
                                                (0.4d0 0.5d0))))
         (mode1 (make-array '(2 2) :element-type 'double-float
                            :initial-contents '((1.3d0 0.6d0)
                                                (0.8d0 1.1d0))))
         (mode2 (make-array '(4 2) :element-type 'double-float
                            :initial-contents '((0.3d0 1.2d0)
                                                (0.9d0 0.4d0)
                                                (1.5d0 0.7d0)
                                                (0.2d0 0.8d0))))
         (factors (make-array 3 :initial-contents (list mode0 mode1 mode2)))
         (indices (make-array '(4 3) :element-type 'fixnum
                              :initial-contents '((0 0 0) (1 1 2) (2 0 3) (0 1 1))))
         (values (make-array 4 :element-type 'double-float
                             :initial-contents '(2d0 5d0 1d0 3d0)))
         (x-hat (make-array 4 :element-type 'double-float :initial-element 0d0))
         (scale 0.25d0)
         (mass-before (cltd::%cp-total-mass factors)))
    (cltd:sdot factors indices x-hat)
    (let ((scaled (cltd:sparse-kl-divergence indices values x-hat factors
                                             :prediction-scale scale))
          (dense (%dense-generalized-kl shape factors indices values
                                        cltd::*epsilon* scale)))
      (ok (< (abs (- scaled dense)) 1d-9)
          (format nil "Scaled sparse KL ~,9F matches dense reference ~,9F"
                  scaled dense)))
    (ok (< (abs (- (cltd::%cp-total-mass factors) mass-before)) 1d-12)
        "Factor matrices are not mutated by PREDICTION-SCALE")
    (ok (< (abs (- (cltd:sparse-kl-divergence indices values x-hat factors
                                              :prediction-scale 1d0)
                   (cltd:sparse-kl-divergence indices values x-hat factors)))
           1d-12)
        "PREDICTION-SCALE defaults to 1 and leaves the unscaled result unchanged")))

(deftest sparse-kl-divergence-does-not-scale-epsilon
  "The stabilizing epsilon is added after scaling, so it is never scaled itself."
  (let* ((factors (make-array 2 :initial-contents
                              (list (make-array '(1 1) :element-type 'double-float
                                                :initial-contents '((2d0)))
                                    (make-array '(1 1) :element-type 'double-float
                                                :initial-contents '((3d0))))))
         (indices (make-array '(1 2) :element-type 'fixnum
                              :initial-contents '((0 0))))
         (values (make-array 1 :element-type 'double-float
                             :initial-contents '(4d0)))
         (x-hat (make-array 1 :element-type 'double-float :initial-element 0d0))
         (scale 0.5d0))
    (cltd:sdot factors indices x-hat)
    (let ((expected (+ (- (* 4d0 (log (/ 4d0 (+ (* scale 6d0) cltd::*epsilon*))))
                          4d0)
                       (* scale 6d0))))
      (ok (< (abs (- (cltd:sparse-kl-divergence indices values x-hat factors
                                                :prediction-scale scale)
                     expected))
             1d-12)
          (format nil "Epsilon applied after scaling (expected ~,12F)" expected)))))


;;; ---------------------------------------------------------------------------
;;; Poisson count thinning for cross-validation
;;;
;;; Folds split the *counts* at each coordinate, not the coordinates themselves,
;;; so the same cell can appear in both the training and the validation tensor.
;;; ---------------------------------------------------------------------------

(defun %tensor-cell-table (tensor)
  "Map each stored coordinate of TENSOR to its count (test-only)."
  (let ((table (make-hash-table :test #'equal))
        (idx (cltd:sparse-tensor-indices tensor))
        (val (cltd:sparse-tensor-values tensor)))
    (loop for row from 0 below (array-dimension idx 0)
          do (setf (gethash (loop for m from 0 below (array-dimension idx 1)
                                  collect (aref idx row m))
                            table)
                   (aref val row)))
    table))

(defun %cell-count (table coord)
  (or (gethash coord table) 0d0))

(defun %make-count-tensor ()
  "A 4x3x2 tensor whose shape is deliberately larger than the observed maxima."
  (cltd:make-sparse-tensor
   '(4 3 2)
   (make-array '(5 3) :element-type 'fixnum
               :initial-contents '((0 0 0) (1 1 1) (2 0 1) (0 2 0) (1 0 0)))
   (make-array 5 :element-type 'double-float
               :initial-contents '(17d0 4d0 23d0 9d0 31d0))))

(defun %single-cell-tensor (count)
  "A 2x2 tensor whose only stored cell carries COUNT events (test-only)."
  (cltd:make-sparse-tensor
   '(2 2)
   (make-array '(1 2) :element-type 'fixnum :initial-contents '((0 0)))
   (make-array 1 :element-type 'double-float
               :initial-contents (list (coerce count 'double-float)))))

(deftest poisson-folds-conserve-counts
  "For every fold: train + validation = original, cell by cell."
  (let* ((tensor (%make-count-tensor))
         (original (%tensor-cell-table tensor))
         (k 3)
         (folds (cltd:make-poisson-folds tensor k
                                         :random-state (cltd:%seed-random-state 4242)))
         (validation-total (make-hash-table :test #'equal)))
    (ok (= (cltd:poisson-folds-count folds) k)
        "Requested number of folds is produced")
    (dotimes (f k)
      (multiple-value-bind (train valid) (cltd:poisson-fold-tensors folds f)
        (let ((train-table (%tensor-cell-table train))
              (valid-table (%tensor-cell-table valid)))
          (maphash (lambda (coord count)
                     (incf (gethash coord validation-total 0d0)
                           (%cell-count valid-table coord))
                     (ok (< (abs (- (+ (%cell-count train-table coord)
                                       (%cell-count valid-table coord))
                                    count))
                            1d-12)
                         (format nil "fold ~D cell ~A: train + valid = ~,1F" f coord count)))
                   original)
          (ok (every (lambda (v) (> v 0d0)) (cltd:sparse-tensor-values train))
              (format nil "fold ~D training tensor stores no zero rows" f))
          (ok (every (lambda (v) (> v 0d0)) (cltd:sparse-tensor-values valid))
              (format nil "fold ~D validation tensor stores no zero rows" f)))))
    (maphash (lambda (coord count)
               (ok (< (abs (- (%cell-count validation-total coord) count)) 1d-12)
                   (format nil "validation counts over all folds sum to ~,1F at ~A"
                           count coord)))
             original)
    (let ((after (%tensor-cell-table tensor)))
      (ok (block same
            (maphash (lambda (coord count)
                       (unless (= (%cell-count after coord) count)
                         (return-from same nil)))
                     original)
            t)
          "Input tensor is not modified by thinning"))))

(deftest poisson-folds-preserve-shape
  "Folds keep the original shape even when a mode value never appears in them."
  (let* ((tensor (%make-count-tensor))
         (folds (cltd:make-poisson-folds tensor 4
                                         :random-state (cltd:%seed-random-state 7))))
    (dotimes (f 4)
      (multiple-value-bind (train valid) (cltd:poisson-fold-tensors folds f)
        (ok (equal (cltd:sparse-tensor-shape train) '(4 3 2))
            (format nil "fold ~D training shape preserved" f))
        (ok (equal (cltd:sparse-tensor-shape valid) '(4 3 2))
            (format nil "fold ~D validation shape preserved" f))))))

(deftest poisson-folds-split-counts-not-coordinates
  "A single coordinate with a large count can still be split into k folds."
  (let* ((tensor (cltd:make-sparse-tensor
                  '(2 2)
                  (make-array '(1 2) :element-type 'fixnum :initial-contents '((0 0)))
                  (make-array 1 :element-type 'double-float :initial-contents '(200d0))))
         (k 5)
         (folds (cltd:make-poisson-folds tensor k
                                         :random-state (cltd:%seed-random-state 11)))
         (total 0d0))
    (ok (= (cltd:poisson-folds-count folds) k)
        "k > nnz is accepted when k <= total count")
    (dotimes (f k)
      (multiple-value-bind (train valid) (cltd:poisson-fold-tensors folds f)
        (let ((tv (reduce #'+ (cltd:sparse-tensor-values train) :initial-value 0d0))
              (vv (reduce #'+ (cltd:sparse-tensor-values valid) :initial-value 0d0)))
          (incf total vv)
          (ok (> tv 0d0) (format nil "fold ~D has a positive training count" f))
          (ok (> vv 0d0) (format nil "fold ~D has a positive validation count" f)))))
    (ok (< (abs (- total 200d0)) 1d-12)
        "Validation counts across folds recover the original count")))

(deftest poisson-folds-are-reproducible-and-non-destructive
  "The same seed reproduces the folds, and the caller's random state is untouched."
  (let* ((tensor (%make-count-tensor))
         (state (cltd:%seed-random-state 999))
         (first-run (cltd:make-poisson-folds tensor 3 :random-state state))
         (second-run (cltd:make-poisson-folds tensor 3 :random-state state)))
    (ok (equalp (cltd::poisson-folds-validation-counts first-run)
                (cltd::poisson-folds-validation-counts second-run))
        "Repeated calls with the same state object produce identical folds")
    (let ((fresh (cltd:make-poisson-folds tensor 3
                                          :random-state (cltd:%seed-random-state 999))))
      (ok (equalp (cltd::poisson-folds-validation-counts first-run)
                  (cltd::poisson-folds-validation-counts fresh))
          "A fresh state from the same seed reproduces the folds"))))

(deftest poisson-folds-reject-invalid-input
  "Thinning validates k and the count values."
  (let ((tensor (%make-count-tensor)))
    (ok (handler-case (progn (cltd:make-poisson-folds tensor 1) nil)
          (cltd:invalid-input-error () t))
        "k = 1 is rejected")
    (ok (handler-case (progn (cltd:make-poisson-folds tensor 0) nil)
          (cltd:invalid-input-error () t))
        "k = 0 is rejected"))
  (let ((fractional (cltd:make-sparse-tensor
                     '(2 2)
                     (make-array '(1 2) :element-type 'fixnum :initial-contents '((0 0)))
                     (make-array 1 :element-type 'double-float :initial-contents '(2.5d0)))))
    (ok (handler-case (progn (cltd:make-poisson-folds fractional 2) nil)
          (cltd:invalid-input-error () t))
        "Fractional counts are rejected"))
  (let ((zero (cltd:make-sparse-tensor
               '(2 2)
               (make-array '(1 2) :element-type 'fixnum :initial-contents '((0 0)))
               (make-array 1 :element-type 'double-float :initial-contents '(0d0)))))
    (ok (handler-case (progn (cltd:make-poisson-folds zero 2) nil)
          (cltd:invalid-input-error () t))
        "A tensor with zero total count is rejected"))
  (let ((tiny (cltd:make-sparse-tensor
               '(2 2)
               (make-array '(1 2) :element-type 'fixnum :initial-contents '((0 0)))
               (make-array 1 :element-type 'double-float :initial-contents '(3d0)))))
    (ok (handler-case (progn (cltd:make-poisson-folds tiny 5) nil)
          (cltd:invalid-input-error () t))
        "Too few events for the requested k is rejected")))

;;; ---------------------------------------------------------------------------
;;; Exposure correction and fold scoring
;;; ---------------------------------------------------------------------------

(deftest poisson-folds-prediction-scale-is-exposure-ratio
  "Training exposure is (k-1)/k and validation exposure 1/k, so the ratio is 1/(k-1)."
  (dolist (k '(2 3 5 10))
    (let ((folds (cltd:make-poisson-folds
                  (%single-cell-tensor (cltd::%minimum-total-count k)) k
                  :random-state (cltd:%seed-random-state 1))))
      (ok (< (abs (- (cltd:poisson-folds-prediction-scale folds)
                     (/ 1d0 (coerce (1- k) 'double-float))))
             1d-12)
          (format nil "k=~D gives prediction scale 1/~D" k (1- k))))))

(deftest normalized-generalized-kl-matches-scaled-dense-reference
  "The fold score is the exposure-scaled generalized KL per validation event.

Comparing against a dense enumeration checks in one shot that the scale reaches
both the stored-entry predictions and the predicted mass on the implicit zeros."
  (let* ((shape '(3 4))
         (mode0 (make-array '(3 2) :element-type 'double-float
                            :initial-contents '((0.8d0 0.3d0)
                                                (0.2d0 1.1d0)
                                                (0.5d0 0.6d0))))
         (mode1 (make-array '(4 2) :element-type 'double-float
                            :initial-contents '((1.2d0 0.4d0)
                                                (0.7d0 0.9d0)
                                                (0.3d0 1.4d0)
                                                (1.0d0 0.2d0))))
         (factors (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(3 2) :element-type 'fixnum
                              :initial-contents '((0 0) (1 2) (2 1))))
         (values (make-array 3 :element-type 'double-float
                             :initial-contents '(4d0 7d0 2d0)))
         (valid-tensor (cltd:make-sparse-tensor shape indices values))
         (approximation (make-array 3 :element-type 'double-float :initial-element 0d0))
         (scale (/ 1d0 4d0))                ; k = 5
         (valid-count 13d0))
    (cltd:sdot factors indices approximation)
    (let ((score (cltd:normalized-generalized-kl valid-tensor approximation factors
                                                 scale valid-count))
          (reference (/ (%dense-generalized-kl shape factors indices values
                                               cltd::*epsilon* scale)
                        valid-count)))
      (ok (< (abs (- score reference)) 1d-9)
          (format nil "Fold score ~,9F matches scaled dense reference ~,9F"
                  score reference)))
    (ok (< (abs (- (cltd:normalized-generalized-kl valid-tensor approximation factors
                                                   scale valid-count)
                   (/ (cltd:sparse-kl-divergence indices values approximation factors
                                                 :prediction-scale scale)
                      valid-count)))
           1d-12)
        "Fold score is the raw generalized KL divided by the validation count")))

(deftest fold-score-includes-implicit-zero-mass
  "The predicted mass on unstored coordinates is part of the fold score."
  (let* ((shape '(2 2))
         (mode0 (make-array '(2 1) :element-type 'double-float
                            :initial-contents '((2d0) (2d0))))
         (mode1 (make-array '(2 1) :element-type 'double-float
                            :initial-contents '((1d0) (1d0))))
         (factors (make-array 2 :initial-contents (list mode0 mode1)))
         (indices (make-array '(1 2) :element-type 'fixnum :initial-contents '((0 0))))
         (values (make-array 1 :element-type 'double-float :initial-contents '(1d0)))
         (valid-tensor (cltd:make-sparse-tensor shape indices values))
         (approximation (make-array 1 :element-type 'double-float :initial-element 0d0))
         (scale 0.5d0))
    (cltd:sdot factors indices approximation)
    ;; Every coordinate predicts 2, so the total mass is 8 and the scaled mass 4.
    ;; Only one coordinate is stored: an nnz-only score would see 1 of that mass.
    (let* ((score (cltd:normalized-generalized-kl valid-tensor approximation factors
                                                  scale 1d0))
           (expected (+ 4d0 (log (/ 1d0 1d0)) -1d0))
           ;; What a stored-coordinates-only score would report: the local term
           ;; with its own +s*x^ back, and no mass from the three implicit zeros.
           (stored-only (+ (* 1d0 (log (/ 1d0 (+ (* scale 2d0) cltd::*epsilon*))))
                           -1d0
                           (* scale 2d0))))
      (ok (< (abs (- score expected)) 1d-5)
          (format nil "Fold score ~,6F carries the mass of all four cells (~,6F)"
                  score expected))
      (ok (< (abs (- (- score stored-only) 3d0)) 1d-5)
          (format nil "Fold score exceeds the stored-only value ~,6F by the ~
                       scaled mass of the three implicit zeros (3.0)"
                  stored-only)))))

(deftest poisson-folds-accept-only-reliably-fillable-k
  "The accepted range matches what can actually be sampled.

A uniform assignment leaves a fold empty with probability at most k*(1-1/k)^N,
so counts near k almost never fill every fold. Such input is rejected up front
rather than accepted and then failed on most seeds."
  (let ((k 10))
    (ok (> (cltd::%minimum-total-count k) k)
        (format nil "k=~D needs more than ~D events (~D)" k k
                (cltd::%minimum-total-count k)))
    (ok (handler-case
            (progn (cltd:make-poisson-folds (%single-cell-tensor k) k
                                            :random-state (cltd:%seed-random-state 1))
                   nil)
          (cltd:invalid-input-error () t))
        "total = k is rejected up front, not by an unlucky draw")
    (let ((tensor (%single-cell-tensor (cltd::%minimum-total-count k))))
      (ok (loop for seed from 1 to 200
                always (handler-case
                           (progn (cltd:make-poisson-folds
                                   tensor k :random-state (cltd:%seed-random-state seed))
                                  t)
                         (cltd:invalid-input-error () nil)))
          (format nil "k=~D at the accepted minimum (~D events) succeeds for 200 seeds"
                  k (cltd::%minimum-total-count k)))))
  (dolist (k '(2 3 5 20))
    (let ((tensor (%single-cell-tensor (cltd::%minimum-total-count k))))
      (ok (loop for seed from 1 to 100
                always (handler-case
                           (progn (cltd:make-poisson-folds
                                   tensor k :random-state (cltd:%seed-random-state seed))
                                  t)
                         (cltd:invalid-input-error () nil)))
          (format nil "k=~D at its minimum (~D events) succeeds for 100 seeds"
                  k (cltd::%minimum-total-count k))))))


(deftest poisson-folds-do-not-condition-away-count-variability
  "The retry loop must not reshape the advertised multinomial draw.

Retrying until every fold is nonempty conditions the distribution. The accepted
range is therefore set where an empty fold has probability at most 1e-6, so the
retry is a safety net rather than a sampler: a first draw is accepted essentially
always, and fold sizes keep the spread a multinomial draw has."
  (dolist (k '(2 3 5 10))
    (let* ((minimum (cltd::%minimum-total-count k))
           (counts (make-array 1 :element-type 'fixnum :initial-contents (list minimum)))
           (rejected 0))
      (loop for seed from 1 to 500
            do (let ((validation (cltd::%thin-counts counts k (cltd:%seed-random-state seed))))
                 (unless (loop for fold-index from 0 below k
                               always (multiple-value-bind (train valid)
                                          (cltd::%fold-totals validation counts fold-index)
                                        (and (plusp train) (plusp valid))))
                   (incf rejected))))
      (ok (zerop rejected)
          (format nil "k=~D at its minimum (~D events): ~D/500 first draws rejected"
                  k minimum rejected))))
  (let* ((k 2)
         (minimum (cltd::%minimum-total-count k))
         (tensor (%single-cell-tensor minimum))
         (sizes (make-hash-table)))
    (loop for seed from 1 to 100
          do (let ((folds (cltd:make-poisson-folds
                           tensor k :random-state (cltd:%seed-random-state seed))))
               (incf (gethash (aref (cltd::poisson-folds-validation-counts folds) 0 0)
                              sizes 0))))
    (ok (> (hash-table-count sizes) 4)
        (format nil "k=2 with ~D events yields ~D distinct fold sizes over 100 seeds"
                minimum (hash-table-count sizes)))
    (let ((low most-positive-fixnum)
          (high 0))
      (maphash (lambda (size count)
                 (declare (ignore count))
                 (setf low (min low size))
                 (setf high (max high size)))
               sizes)
      (ok (>= (- high low) 3)
          (format nil "Fold sizes span ~D..~D rather than collapsing onto a balanced split"
                  low high))))
  (ok (handler-case (progn (cltd:make-poisson-folds (%single-cell-tensor 2) 2) nil)
        (cltd:invalid-input-error () t))
      "k=2 with only 2 events is rejected: conditioning could return only the 1/1 split"))

;;; ---------------------------------------------------------------------------
;;; Elbow-based early stopping of the rank sweep
;;; ---------------------------------------------------------------------------

(deftest select-rank-elbow-stops-before-exhausting-the-grid
  "The sweep walks ranks upward and stops once a rank stops paying for itself."
  (let ((ranks '(1 2 3 4 5)))
    (multiple-value-bind (best evaluated)
        (cltd:select-rank-elbow CV-tensor ranks
                                :k 3 :n-cycle 20
                                :random-state (cltd:%seed-random-state 41))
      (ok (< (length evaluated) (length ranks))
          (format nil "Stopped after ~D of ~D ranks" (length evaluated) (length ranks)))
      (ok (equal (mapcar (lambda (r) (cdr (assoc :rank r))) evaluated)
                 (subseq ranks 0 (length evaluated)))
          "Evaluated ranks are the ascending prefix of the grid")
      (ok (member (cdr (assoc :rank best))
                  (mapcar (lambda (r) (cdr (assoc :rank r))) evaluated))
          "Selected rank is one of the ranks actually evaluated")
      (dolist (key '(:rank :mean :std :standard-error :scores :validation-counts))
        (ok (assoc key best) (format nil "Selected result has ~S key" key))))))

(deftest select-rank-elbow-shares-folds-with-cross-validate-rank
  "Per-rank scores match the whole-grid call, so the early stop loses nothing."
  (let* ((ranks '(1 2 3))
         (seed 41)
         (whole (cltd:cross-validate-rank CV-tensor ranks
                                          :k 3 :n-cycle 20
                                          :random-state (cltd:%seed-random-state seed))))
    (multiple-value-bind (best evaluated)
        (cltd:select-rank-elbow CV-tensor ranks
                                :k 3 :n-cycle 20 :patience 99
                                :random-state (cltd:%seed-random-state seed))
      (declare (ignore best))
      (ok (equalp whole evaluated)
          "With patience high enough to reach the end, results are identical"))))

(deftest select-rank-elbow-patience-widens-the-sweep
  "A larger PATIENCE never evaluates fewer ranks."
  (let ((ranks '(1 2 3 4 5))
        (previous 0))
    (dolist (patience '(1 2 3))
      (multiple-value-bind (best evaluated)
          (cltd:select-rank-elbow CV-tensor ranks
                                  :k 3 :n-cycle 20 :patience patience
                                  :random-state (cltd:%seed-random-state 41))
        (declare (ignore best))
        (ok (>= (length evaluated) previous)
            (format nil "patience ~D evaluated ~D ranks (>= ~D)"
                    patience (length evaluated) previous))
        (setf previous (length evaluated))))))

(deftest select-rank-elbow-ignores-duplicate-ranks
  "A repeated candidate must not read as a rank that failed to pay.

Scoring the same rank twice against the same folds yields the same mean, so the
gain is zero; with the default :patience 1 the sweep would otherwise stop on the
duplicate without ever reaching the larger candidates."
  (let ((seed 41))
    (flet ((sweep (ranks)
             (multiple-value-bind (best evaluated)
                 (cltd:select-rank-elbow CV-tensor ranks
                                         :k 3 :n-cycle 20
                                         :random-state (cltd:%seed-random-state seed))
               (list (cdr (assoc :rank best))
                     (mapcar (lambda (r) (cdr (assoc :rank r))) evaluated)))))
      (ok (equal (sweep '(1 1 2)) (sweep '(1 2)))
          (format nil "(1 1 2) behaves as (1 2): ~A" (sweep '(1 1 2))))
      (ok (equal (second (sweep '(1 1 2))) '(1 2))
          "The duplicate does not cut the sweep short")
      (ok (equal (sweep '(3 1 2 1 3)) (sweep '(1 2 3)))
          (format nil "(3 1 2 1 3) behaves as (1 2 3): ~A" (sweep '(3 1 2 1 3)))))))

(deftest select-rank-elbow-does-not-mutate-the-rank-list
  "Sorting the candidates must not touch the caller's list."
  (let* ((ranks (list 3 1 2))
         (copy (copy-list ranks)))
    (cltd:select-rank-elbow CV-tensor ranks
                            :k 3 :n-cycle 20
                            :random-state (cltd:%seed-random-state 41))
    (ok (equal ranks copy)
        (format nil "Caller's rank list is unchanged (~A)" ranks))))

(deftest select-rank-elbow-is-quiet-unless-verbose
  (let ((output (with-output-to-string (*standard-output*)
                  (cltd:select-rank-elbow CV-tensor '(1 2 3)
                                          :k 3 :n-cycle 10
                                          :random-state (cltd:%seed-random-state 3)))))
    (ok (zerop (length output))
        (format nil "verbose=nil produces no output (got ~D characters)"
                (length output)))))

(deftest select-rank-elbow-rejects-invalid-input
  (ok (handler-case (progn (cltd:select-rank-elbow CV-tensor '() :k 3) nil)
        (cltd:invalid-input-error () t))
      "Empty rank list is rejected")
  (ok (handler-case (progn (cltd:select-rank-elbow CV-tensor '(1 2) :k 3 :patience 0) nil)
        (cltd:invalid-input-error () t))
      "patience 0 is rejected")
  (ok (handler-case (progn (cltd:select-rank-elbow CV-tensor '(1 2) :k 3 :tolerance -1d0) nil)
        (cltd:invalid-input-error () t))
      "Negative tolerance is rejected"))

;;; ---------------------------------------------------------------------------
;;; Optimizer robustness: outer iterations, KKT, inadmissible zeros, lambda
;;; ---------------------------------------------------------------------------

(defun %make-workspace (shape r)
  "Allocate the scratch arrays DECOMPOSITION-INNER expects (test-only)."
  (let ((n-modes (length shape)))
    (values (make-array n-modes :initial-contents
                        (loop for dim in shape
                              collect (make-array (list dim r) :element-type 'double-float
                                                              :initial-element 0d0)))
            (make-array (list n-modes r) :element-type 'double-float
                                         :initial-element 1d0))))

(defun %copy-factors (factors)
  (make-array (length factors) :initial-contents
              (loop for m across factors
                    collect (let ((c (make-array (array-dimensions m)
                                                 :element-type 'double-float)))
                              (loop for i from 0 below (array-dimension m 0)
                                    do (loop for r from 0 below (array-dimension m 1)
                                             do (setf (aref c i r) (aref m i r))))
                              c))))

(defun %factors-differ-p (a b)
  (loop for i from 0 below (array-dimension a 0)
        thereis (loop for r from 0 below (array-dimension a 1)
                      thereis (> (abs (- (aref a i r) (aref b i r))) 0d0))))

(deftest one-outer-iteration-updates-every-mode
  "A cycle is a full sweep: one iteration must touch all three factor matrices."
  (let* ((shape '(2 3 4))
         (r 2)
         (values X-value-vector)
         (x-hat (make-array (length values) :element-type 'double-float
                                            :initial-element 1d0))
         (*random-state* (cltd:%seed-random-state 5)))
    (multiple-value-bind (numerator denominator) (%make-workspace shape r)
      (let ((factors (make-array 3 :initial-contents
                                 (loop for dim in shape
                                       collect (cltd:initialize-random-matrix
                                                (make-array (list dim r)
                                                            :element-type 'double-float))))))
        (let ((before (%copy-factors factors)))
          (multiple-value-bind (iterations final-kl kl-history)
              (cltd:decomposition-inner 1 X-indices-matrix values x-hat
                                        factors numerator denominator)
            (declare (ignore final-kl))
            (ok (= iterations 1) "One outer iteration was executed")
            (ok (= (length kl-history) 1) "One KL history entry per outer iteration")
            (dotimes (mode 3)
              (ok (%factors-differ-p (svref factors mode) (svref before mode))
                  (format nil "Mode ~D was updated within the single outer iteration"
                          mode)))))))))

(deftest decomposition-reports-lambda-and-normalized-modes
  "Columns are normalized and the component weights are returned explicitly."
  (let* ((*random-state* (cltd:%seed-random-state 13))
         (r 3))
    (multiple-value-bind (factors iterations final-kl kl-history converged-p lambda)
        (cltd:decomposition X-tensor :r r :n-cycle 20)
      (declare (ignore iterations kl-history converged-p))
      (ok (= (length lambda) r) "One lambda per component")
      (ok (every (lambda (v) (>= v 0d0)) lambda) "Component weights are non-negative")
      ;; Total predicted mass is exactly the sum of the component weights.
      (ok (< (abs (- (reduce #'+ lambda) (cltd::%cp-total-mass factors))) 1d-8)
          (format nil "sum(lambda) ~,8F equals the total predicted mass ~,8F"
                  (reduce #'+ lambda) (cltd::%cp-total-mass factors)))
      ;; Every mode but the first carries unit-sum columns; mode 0 carries lambda.
      (loop for mode from 1 below (length factors)
            do (let ((m (svref factors mode)))
                 (dotimes (ri r)
                   (let ((column-sum (loop for i from 0 below (array-dimension m 0)
                                           sum (aref m i ri))))
                     (ok (< (abs (- column-sum 1d0)) 1d-8)
                         (format nil "mode ~D column ~D sums to 1 (~,8F)"
                                 mode ri column-sum))))))
      (dotimes (ri r)
        (let* ((m (svref factors 0))
               (column-sum (loop for i from 0 below (array-dimension m 0)
                                 sum (aref m i ri))))
          (ok (< (abs (- column-sum (aref lambda ri))) 1d-8)
              (format nil "mode 0 column ~D sums to lambda[~D]" ri ri))))
      ;; Normalization is a change of representation only: the loss is unchanged.
      (let ((x-hat (make-array (length X-value-vector) :element-type 'double-float
                                                       :initial-element 0d0)))
        (cltd:sdot factors X-indices-matrix x-hat)
        (ok (< (abs (- final-kl (cltd:sparse-kl-divergence X-indices-matrix X-value-vector
                                                           x-hat factors)))
               1d-9)
            "final-kl still matches a KL recomputed from the returned factors")))))

(deftest decomposition-reports-kkt-residual
  "The KKT residual is returned and drives convergence."
  (let ((*random-state* (cltd:%seed-random-state 21)))
    (multiple-value-bind (factors iterations final-kl kl-history converged-p lambda residual)
        (cltd:decomposition X-tensor :r 2 :n-cycle 400 :kkt-tolerance 1d-4)
      (declare (ignore factors final-kl kl-history lambda))
      (ok (typep residual 'double-float) "KKT residual is a double-float")
      (ok (>= residual 0d0) "KKT residual is non-negative")
      (when converged-p
        (ok (< residual 1d-4)
            (format nil "Converged with residual ~,3E below the tolerance" residual))
        (ok (< iterations 400) "Stopped before the iteration cap"))))
  (let ((*random-state* (cltd:%seed-random-state 21)))
    (multiple-value-bind (factors iterations final-kl kl-history converged-p)
        (cltd:decomposition X-tensor :r 2 :n-cycle 15 :kkt-tolerance 0d0)
      (declare (ignore factors final-kl kl-history))
      (ok (= iterations 15) "A zero tolerance runs the full iteration budget")
      (ok (not converged-p) "A zero tolerance never reports convergence"))))

(deftest inadmissible-zero-is-lifted-off-the-boundary
  "An entry pinned at zero whose gradient wants it to grow must be able to.

A plain multiplicative step keeps a zero at zero forever, because the step is a
product, so the fit can stall at a point that is not KKT. Chi and Kolda nudge
such an entry to KAPPA first. This exercises UPDATE directly: the condition only
holds while the gradient is still negative, and within a full run the surrounding
modes may collapse alongside it into a genuine boundary stationary point, which
is a local optimum rather than a missed fix and is what :N-STARTS addresses."
  (labels ((run (kappa)
             (let* ((rank 1)
                    (x-hat (make-array (length X-value-vector)
                                       :element-type 'double-float :initial-element 1d0))
                    (*random-state* (cltd:%seed-random-state 77)))
               (multiple-value-bind (numerator denominator) (%make-workspace '(2 3 4) rank)
                 (let ((factors (make-array 3 :initial-contents
                                            (loop for dim in '(2 3 4)
                                                  collect (cltd:initialize-random-matrix
                                                           (make-array (list dim rank)
                                                                       :element-type 'double-float))))))
                   (setf (aref (svref factors 0) 0 0) 0d0)
                   (cltd:sdot factors X-indices-matrix x-hat)
                   (let ((residual (cltd::update X-indices-matrix X-value-vector x-hat
                                                 factors 0 numerator denominator
                                                 :kappa kappa :allow-scooch t)))
                     (values (aref (svref factors 0) 0 0)
                             residual
                             (- (aref denominator 0 0)
                                (aref (svref numerator 0) 0 0)))))))))
    (multiple-value-bind (value residual gradient) (run 1d-2)
      (ok (minusp gradient)
          (format nil "The pinned entry has a negative gradient (~,3E), so it wants to grow"
                  gradient))
      (ok (> residual 1d0)
          (format nil "The KKT residual reports the violation (~,3E)" residual))
      (ok (plusp value)
          (format nil "With kappa 1e-2 the entry escapes zero (~,4F)" value)))
    (ok (zerop (run 0d0))
        "With kappa 0 the entry stays at zero, as a plain multiplicative step does")))

(deftest decomposition-n-starts-keeps-the-best-fit
  "With several starts the reported fit is no worse than the first one alone."
  (let ((single (let ((*random-state* (cltd:%seed-random-state 31)))
                  (nth-value 2 (cltd:decomposition X-tensor :r 3 :n-cycle 30
                                                            :n-starts 1))))
        (multi (let ((*random-state* (cltd:%seed-random-state 31)))
                 (nth-value 2 (cltd:decomposition X-tensor :r 3 :n-cycle 30
                                                           :n-starts 4)))))
    (ok (<= multi (+ single 1d-9))
        (format nil "4 starts reach ~,6F, no worse than 1 start at ~,6F" multi single)))
  (ok (handler-case (progn (cltd:decomposition X-tensor :r 2 :n-cycle 5 :n-starts 0) nil)
        (cltd:invalid-input-error () t))
      ":n-starts 0 is rejected"))

(deftest decomposition-signals-numerical-instability
  "NaN and infinite factor entries are reported rather than returned.

Infinity is used for the literal because it has a portable one; the scan treats
NaN identically."
  (let ((lambda-vector (make-array 2 :element-type 'double-float :initial-element 1d0))
        (factors (make-array 2 :initial-contents
                             (list (make-array '(2 2) :element-type 'double-float
                                                      :initial-element 0.5d0)
                                   (make-array '(3 2) :element-type 'double-float
                                                      :initial-element 0.5d0)))))
    (ok (null (cltd::%check-factor-health factors lambda-vector))
        "Healthy factors report nothing")
    (setf (aref (svref factors 1) 1 0) cltd:+double-float-positive-infinity+)
    (ok (handler-case (progn (cltd::%check-factor-health factors lambda-vector) nil)
          (cltd:numerical-instability-error (condition)
            (and (equal (cltd:instability-location condition) '(:mode 1 :row 1 :column 0))
                 (cltd:%float-infinity-p (cltd:instability-value condition)))))
        "An infinite entry signals numerical-instability-error naming its position")))

(deftest decomposition-reports-dead-components
  "A component whose weight collapses is reported, but does not abort the fit.

Erroring by default would break rank sweeps, where a rank past what the data
supports is exactly the answer being looked for."
  (let ((factors (make-array 2 :initial-contents
                             (list (make-array '(2 3) :element-type 'double-float
                                                      :initial-element 0.5d0)
                                   (make-array '(3 3) :element-type 'double-float
                                                      :initial-element 0.5d0))))
        (lambda-vector (make-array 3 :element-type 'double-float
                                     :initial-contents '(1d0 0d0 2d0))))
    (ok (equal (handler-bind ((warning #'muffle-warning))
                 (cltd::%check-factor-health factors lambda-vector))
               '(1))
        "The dead component is identified by index")
    (ok (handler-case (progn (cltd::%check-factor-health factors lambda-vector
                                                         :on-dead-component :warn)
                             nil)
          (warning () t))
        "The default warns")
    (ok (handler-case (progn (cltd::%check-factor-health factors lambda-vector
                                                         :on-dead-component :error)
                             nil)
          (cltd:numerical-instability-error () t))
        ":error signals numerical-instability-error")
    (ok (handler-case (progn (cltd::%check-factor-health factors lambda-vector
                                                         :on-dead-component :ignore)
                             t)
          (warning () nil))
        ":ignore is silent")
    (ok (null (handler-bind ((warning #'muffle-warning))
                (cltd::%check-factor-health
                 factors (make-array 3 :element-type 'double-float :initial-element 1d0))))
        "Live components report nothing")))

(deftest decomposition-kkt-residual-describes-the-returned-model
  "The reported residual must belong to the factors that come back.

Each mode's residual is measured before that mode is updated, so the value
observed during a sweep mixes staggered pre-update states and can be orders of
magnitude away from the residual of the completed, normalized model."
  (let ((*random-state* (cltd:%seed-random-state 9)))
    (multiple-value-bind (factors iterations final-kl kl-history converged-p lambda residual)
        ;; A zero tolerance exhausts the budget, the case where a stale in-sweep
        ;; value drifts furthest from the returned model.
        (cltd:decomposition X-tensor :r 2 :n-cycle 3 :kkt-tolerance 0d0
                                     :on-dead-component :ignore)
      (declare (ignore iterations final-kl kl-history converged-p lambda))
      (multiple-value-bind (numerator denominator) (%make-workspace '(2 3 4) 2)
        (let ((x-hat (make-array (length X-value-vector) :element-type 'double-float
                                                         :initial-element 0d0)))
          (cltd:sdot factors X-indices-matrix x-hat)
          (let ((recomputed (cltd::%kkt-residual X-indices-matrix X-value-vector x-hat
                                                 factors numerator denominator)))
            (ok (< (abs (- residual recomputed)) 1d-9)
                (format nil "Reported residual ~,6E matches the returned model's ~,6E"
                        residual recomputed))))))))

(deftest decomposition-converges-on-the-completed-model
  "Convergence is confirmed against the finished sweep, not the in-sweep screen."
  (let ((*random-state* (cltd:%seed-random-state 9)))
    (multiple-value-bind (factors iterations final-kl kl-history converged-p lambda residual)
        (cltd:decomposition X-tensor :r 2 :n-cycle 200 :kkt-tolerance 1d-4
                                     :on-dead-component :ignore)
      (declare (ignore iterations final-kl kl-history lambda))
      (when converged-p
        (ok (< residual 1d-4)
            (format nil "Reported residual ~,6E is below the tolerance it stopped on"
                    residual))
        (multiple-value-bind (numerator denominator) (%make-workspace '(2 3 4) 2)
          (let ((x-hat (make-array (length X-value-vector) :element-type 'double-float
                                                           :initial-element 0d0)))
            (cltd:sdot factors X-indices-matrix x-hat)
            (ok (< (cltd::%kkt-residual X-indices-matrix X-value-vector x-hat
                                        factors numerator denominator)
                   1d-4)
                "The returned model really satisfies the tolerance")))))))

(deftest decomposition-accepts-ordinary-numbers-for-kappa
  "KAPPA and KAPPA-TOLERANCE are coerced at the public boundary.

UPDATE declares them double-float and compiles with (safety 0), so a plain 0 or
a single-float from a caller must not reach it undeclared."
  (dolist (kappa (list 0 1/100 0.01 0.01d0))
    (ok (handler-case
            (let ((*random-state* (cltd:%seed-random-state 9)))
              (nth-value 2 (cltd:decomposition X-tensor :r 2 :n-cycle 5
                                                        :kappa kappa
                                                        :on-dead-component :ignore))
              t)
          (error () nil))
        (format nil "kappa ~S (~A) is accepted" kappa (type-of kappa))))
  (dolist (tolerance (list 0 1/1000000 1e-10 1d-10))
    (ok (handler-case
            (let ((*random-state* (cltd:%seed-random-state 9)))
              (nth-value 2 (cltd:decomposition X-tensor :r 2 :n-cycle 5
                                                        :kappa-tolerance tolerance
                                                        :on-dead-component :ignore))
              t)
          (error () nil))
        (format nil "kappa-tolerance ~S (~A) is accepted" tolerance (type-of tolerance)))))

(deftest decomposition-convergence-agrees-with-the-reported-residual
  "A run must not report a residual under the tolerance while denying convergence.

The in-sweep screen is measured before each mode's own update, so a sweep that
lands on a solution shows a large screen and a small settled residual. Gating the
exact check on the screen alone let a budget-exhausting run return a residual of
1.0e-6 against a 1d-4 tolerance with CONVERGED-P nil."
  (dolist (n-cycle '(1 2 3 4 5 6 8))
    (let ((*random-state* (cltd:%seed-random-state 20260827)))
      (multiple-value-bind (factors iterations final-kl kl-history converged-p lambda residual)
          (cltd:decomposition X-tensor :r 2 :n-cycle n-cycle :kkt-tolerance 1d-4
                                       :on-dead-component :ignore)
        (declare (ignore factors iterations final-kl kl-history lambda))
        (ok (or converged-p (>= residual 1d-4))
            (format nil "n-cycle ~D: residual ~,4E and converged-p ~A agree"
                    n-cycle residual converged-p))))))

(deftest decomposition-inner-reports-bad-factors-end-to-end
  "A NaN or infinite factor reaches the caller as NUMERICAL-INSTABILITY-ERROR.

The checker is unit tested separately; this drives the whole optimizer, which is
where an implementation's own floating-point condition could otherwise surface
first."
  (let* ((x-hat (make-array (length X-value-vector) :element-type 'double-float
                                                    :initial-element 1d0)))
    (multiple-value-bind (numerator denominator) (%make-workspace '(2 3 4) 2)
      (let ((factors (make-array 3 :initial-contents
                                 (loop for dim in '(2 3 4)
                                       collect (make-array (list dim 2)
                                                           :element-type 'double-float
                                                           :initial-element 0.5d0)))))
        (setf (aref (svref factors 1) 1 0) cltd:+double-float-positive-infinity+)
        (ok (handler-case
                (progn (cltd:decomposition-inner 3 X-indices-matrix X-value-vector x-hat
                                                 factors numerator denominator
                                                 :on-dead-component :ignore)
                       nil)
              (cltd:numerical-instability-error () t)
              (error () nil))
            "An infinite factor supplied by the caller signals the promised condition")))
    (multiple-value-bind (numerator denominator) (%make-workspace '(2 3 4) 2)
      (let ((factors (make-array 3 :initial-contents
                                 (loop for dim in '(2 3 4)
                                       collect (make-array (list dim 2)
                                                           :element-type 'double-float
                                                           :initial-element 1d0)))))
        ;; Large enough that the multiplicative step overflows during the sweep.
        (setf (aref (svref factors 0) 0 0) 1d308)
        (setf (aref (svref factors 1) 0 0) 1d308)
        (ok (handler-case
                (progn (cltd:decomposition-inner 3 X-indices-matrix X-value-vector x-hat
                                                 factors numerator denominator
                                                 :on-dead-component :ignore)
                       :no-error)
              (cltd:numerical-instability-error () t)
              (error () nil))
            "An overflow produced during the sweep does not escape as a float condition")))))

(deftest decomposition-honours-its-contract-with-a-zero-budget
  "A zero iteration budget still returns the documented representation.

The loop body is what normalizes the columns, fills in the weights and computes
the loss, so a budget of zero used to hand back raw random factors with a weight
vector of all ones and a final KL of 0."
  (let ((*random-state* (cltd:%seed-random-state 61))
        (rank 3))
    (multiple-value-bind (factors iterations final-kl kl-history converged-p lambda residual)
        (cltd:decomposition X-tensor :r rank :n-cycle 0 :on-dead-component :ignore)
      (ok (zerop iterations) "No iterations were executed")
      (ok (zerop (length kl-history)) "History has one entry per iteration, so none")
      (ok (not converged-p) "A zero budget does not report convergence")
      (ok (>= residual 0d0) "The residual is still measured")
      (ok (< (abs (- (reduce #'+ lambda) (cltd::%cp-total-mass factors))) 1d-8)
          (format nil "sum(lambda) ~,8F equals the total predicted mass ~,8F"
                  (reduce #'+ lambda) (cltd::%cp-total-mass factors)))
      (loop for mode from 1 below (length factors)
            do (let ((m (svref factors mode)))
                 (dotimes (ri rank)
                   (let ((column-sum (loop for i from 0 below (array-dimension m 0)
                                           sum (aref m i ri))))
                     (ok (< (abs (- column-sum 1d0)) 1d-8)
                         (format nil "mode ~D column ~D sums to 1 (~,8F)"
                                 mode ri column-sum))))))
      (let ((x-hat (make-array (length X-value-vector) :element-type 'double-float
                                                       :initial-element 0d0)))
        (cltd:sdot factors X-indices-matrix x-hat)
        (ok (< (abs (- final-kl (cltd:sparse-kl-divergence X-indices-matrix X-value-vector
                                                           x-hat factors)))
               1d-9)
            (format nil "final-kl ~,6F is the loss of the returned factors" final-kl))))))

(deftest decomposition-rejects-a-nonsensical-iteration-budget
  (dolist (n-cycle '(-1 2.5 :many))
    (ok (handler-case (progn (cltd:decomposition X-tensor :r 2 :n-cycle n-cycle) nil)
          (cltd:invalid-input-error () t))
        (format nil ":n-cycle ~S is rejected" n-cycle))))

(deftest normalize-factors-reports-an-overflowing-column-sum
  "Finite entries can still sum past the double range.

Inside the optimizer the IEEE traps are masked so the library can report where a
bad value came from, which means an overflowing column sum yields infinity
rather than trapping. Every entry is finite on its own, so the entry scan passes
and the weight went infinite, then travelled through the reconstruction and the
loss before anything looked again."
  (cltd::%with-float-traps-masked
   (let ((factors (make-array 2 :initial-contents
                              (list (make-array '(2 1) :element-type 'double-float
                                                       :initial-contents '((1d0) (1d0)))
                                    (make-array '(2 1) :element-type 'double-float
                                                       :initial-contents '((1d308) (1d308))))))
         (lambda-vector (make-array 1 :element-type 'double-float :initial-element 1d0)))
     (ok (null (cltd::%check-factor-values factors))
         "Each entry on its own is finite, so the entry scan passes")
     (ok (handler-case (progn (cltd::%normalize-factors factors lambda-vector) nil)
           (cltd:numerical-instability-error (condition)
             (and (equal (cltd:instability-location condition) '(:mode 1 :column 0))
                  (cltd:%float-infinity-p (cltd:instability-value condition)))))
         "The overflowing column sum is reported, naming the mode and column"))))

(deftest normalize-factors-reports-an-overflowing-weight
  "A component weight can overflow across modes even when no column sum does."
  (cltd::%with-float-traps-masked
   (let ((factors (make-array 2 :initial-contents
                              (list (make-array '(1 1) :element-type 'double-float
                                                       :initial-contents '((1d200)))
                                    (make-array '(1 1) :element-type 'double-float
                                                       :initial-contents '((1d200))))))
         (lambda-vector (make-array 1 :element-type 'double-float :initial-element 1d0)))
     (ok (handler-case (progn (cltd::%normalize-factors factors lambda-vector) nil)
           (cltd:numerical-instability-error (condition)
             (equal (cltd:instability-location condition) '(:component 0))))
         "The accumulated weight overflow is reported, naming the component")))
  (cltd::%with-float-traps-masked
   (let ((factors (make-array 2 :initial-contents
                              (list (make-array '(2 1) :element-type 'double-float
                                                       :initial-contents '((2d0) (3d0)))
                                    (make-array '(2 1) :element-type 'double-float
                                                       :initial-contents '((1d0) (4d0))))))
         (lambda-vector (make-array 1 :element-type 'double-float :initial-element 1d0)))
     (cltd::%normalize-factors factors lambda-vector)
     (ok (< (abs (- (aref lambda-vector 0) 25d0)) 1d-9)
         (format nil "Ordinary factors still normalize (weight ~,4F = 5 * 5)"
                 (aref lambda-vector 0))))))

(deftest decomposition-inner-keeps-its-tail-under-the-trap-mask
  "The settled residual is computed after the loop, and must be guarded too.

%KKT-RESIDUAL runs CALC-NUMERATOR, which divides an observed count by the
reconstruction. A large count over a small reconstruction overflows, so leaving
that call outside the mask let an implementation floating-point condition escape
instead of the library's own."
  (let ((indices (make-array '(1 1) :element-type 'fixnum :initial-contents '((0))))
        (values (make-array 1 :element-type 'double-float :initial-contents '(1d308)))
        (x-hat (make-array 1 :element-type 'double-float :initial-element 1d0))
        (numerator (make-array 1 :initial-contents
                               (list (make-array '(1 1) :element-type 'double-float
                                                        :initial-element 0d0))))
        (denominator (make-array '(1 1) :element-type 'double-float
                                        :initial-element 1d0))
        (factors (make-array 1 :initial-contents
                             (list (make-array '(1 1) :element-type 'double-float
                                                      :initial-contents '((0.1d0)))))))
    (ok (handler-case
            (progn (cltd:decomposition-inner 0 indices values x-hat
                                             factors numerator denominator
                                             :on-dead-component :ignore)
                   :returned-normally)
          (cltd:numerical-instability-error () t)
          (error () nil))
        "A residual that leaves the double range is reported by the library")))

(deftest decomposition-inner-reports-a-non-finite-residual
  "A residual that is not finite describes no model and must not be returned."
  (cltd::%with-float-traps-masked
   (let ((indices (make-array '(1 1) :element-type 'fixnum :initial-contents '((0))))
         (values (make-array 1 :element-type 'double-float :initial-contents '(1d308)))
         (x-hat (make-array 1 :element-type 'double-float :initial-element 1d0))
         (numerator (make-array 1 :initial-contents
                                (list (make-array '(1 1) :element-type 'double-float
                                                         :initial-element 0d0))))
         (denominator (make-array '(1 1) :element-type 'double-float
                                         :initial-element 1d0))
         (factors (make-array 1 :initial-contents
                              (list (make-array '(1 1) :element-type 'double-float
                                                       :initial-contents '((0.1d0)))))))
     (cltd:sdot factors indices x-hat)
     (ok (cltd:%float-infinity-p
          (cltd::%kkt-residual indices values x-hat factors numerator denominator))
         "The residual really does overflow for this input, so the guard has work to do"))))

(defparameter overflow-tensor
  (cltd:make-sparse-tensor
   '(2)
   (make-array '(2 1) :element-type 'fixnum :initial-contents '((0) (1)))
   (make-array 2 :element-type 'double-float :initial-contents '(1d308 1d308)))
  "Counts that are individually finite but whose model mass cannot be represented.")

(deftest sparse-kl-divergence-can-go-non-finite-with-finite-parts
  "The loss needs its own check: nothing else on the path shows the failure.

Each factor entry is finite, each reconstruction is finite, and the KKT residual
is small. Only the two aggregates inside the loss overflow -- the local term to
negative infinity, the total predicted mass to positive infinity -- and their sum
is a NaN."
  (cltd::%with-float-traps-masked
   (let ((factors (make-array 1 :initial-contents
                              (list (make-array '(2 4) :element-type 'double-float
                                                       :initial-element 4.4d307))))
         (indices (cltd:sparse-tensor-indices overflow-tensor))
         (values (cltd:sparse-tensor-values overflow-tensor))
         (x-hat (make-array 2 :element-type 'double-float :initial-element 0d0))
         (numerator (make-array 1 :initial-contents
                                (list (make-array '(2 4) :element-type 'double-float
                                                         :initial-element 0d0))))
         (denominator (make-array '(1 4) :element-type 'double-float
                                         :initial-element 1d0)))
     (cltd:sdot factors indices x-hat)
     (ok (null (cltd::%check-factor-values factors))
         "Every factor entry is finite, so the entry scan passes")
     (ok (every (lambda (v) (not (cltd:%float-infinity-p v))) x-hat)
         "Every reconstruction is finite, so nothing shows up there either")
     (let ((residual (cltd::%kkt-residual indices values x-hat factors
                                          numerator denominator)))
       (ok (and (not (cltd:%float-nan-p residual))
                (not (cltd:%float-infinity-p residual)))
           (format nil "The KKT residual is finite (~,4F), so it cannot catch this"
                   residual)))
     (ok (cltd:%float-nan-p (cltd:sparse-kl-divergence indices values x-hat factors))
         "Yet the loss itself is a NaN"))))

(deftest decomposition-rejects-a-non-finite-loss
  "A loss that is not finite must be reported, not returned or compared against."
  (dolist (n-starts '(1 4))
    (ok (handler-case
            (let ((*random-state* (cltd:%seed-random-state 3)))
              (nth-value 2 (cltd:decomposition overflow-tensor
                                               :r 4 :n-cycle 1 :n-starts n-starts
                                               :on-dead-component :ignore))
              ;; Returning at all is the failure this guards against.
              nil)
          (cltd:numerical-instability-error () t)
          (error () nil))
        (format nil ":n-starts ~D signals rather than returning a NaN loss" n-starts))))
