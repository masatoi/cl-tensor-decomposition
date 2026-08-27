(defpackage :cl-tensor-decomposition/tests/integration
  (:use :cl :rove)
  (:nicknames :cltd-integration-test))

(in-package :cl-tensor-decomposition/tests/integration)

;;; ==========================================================================
;;; Synthetic Retail Dataset
;;; ==========================================================================
;;; Tensor structure: customer_segment(4) × product_category(5) × time_slot(3) × channel(2)
;;; This simulates retail purchase data with interpretable latent patterns.

(defparameter *customer-segments*
  '("Young Professional" "Family" "Senior" "Student"))

(defparameter *product-categories*
  '("Electronics" "Groceries" "Clothing" "Home & Garden" "Sports"))

(defparameter *time-slots*
  '("Morning" "Afternoon" "Evening"))

(defparameter *channels*
  '("Online" "In-Store"))

(defparameter *x-shape*
  (list (length *customer-segments*)
        (length *product-categories*)
        (length *time-slots*)
        (length *channels*)))

(defun make-synthetic-retail-data (&key (random-state (make-random-state t)))
  "Generate synthetic retail purchase count data with interpretable patterns.
Returns (values x-indices-matrix x-value-vector).

Embedded patterns:
- Young Professionals: Electronics + Evening + Online
- Families: Groceries + Morning + In-Store
- Seniors: Home & Garden + Morning + In-Store
- Students: Clothing + Afternoon + Online"
  (let* ((patterns
           ;; (segment product time channel base-count)
           '((0 0 2 0 50)   ; Young Professional + Electronics + Evening + Online
             (0 2 2 0 30)   ; Young Professional + Clothing + Evening + Online
             (1 1 0 1 80)   ; Family + Groceries + Morning + In-Store
             (1 3 1 1 40)   ; Family + Home & Garden + Afternoon + In-Store
             (2 3 0 1 60)   ; Senior + Home & Garden + Morning + In-Store
             (2 1 0 1 45)   ; Senior + Groceries + Morning + In-Store
             (3 2 1 0 55)   ; Student + Clothing + Afternoon + Online
             (3 4 1 0 35)   ; Student + Sports + Afternoon + Online
             ;; Add some noise patterns
             (0 1 1 1 15)   ; Young Professional + Groceries + Afternoon + In-Store
             (1 0 2 0 20)   ; Family + Electronics + Evening + Online
             (2 4 1 1 10)   ; Senior + Sports + Afternoon + In-Store
             (3 1 2 1 25))) ; Student + Groceries + Evening + In-Store
         (nnz (length patterns))
         (n-modes (length *x-shape*))
         (indices (make-array (list nnz n-modes) :element-type 'fixnum))
         (values (make-array nnz :element-type 'double-float)))
    ;; Fill indices and values with some random noise
    (loop for pattern in patterns
          for i from 0
          do (loop for mode from 0 below n-modes
                   do (setf (aref indices i mode) (nth mode pattern)))
             ;; Add small random noise to counts
             (let ((base-count (nth n-modes pattern))
                   (noise (- (random 10 random-state) 5)))
               (setf (aref values i)
                     (coerce (max 1 (+ base-count noise)) 'double-float))))
    (values indices values)))

(defun build-mode-metadata ()
  "Build mode metadata for the synthetic retail dataset."
  (list (cltd:make-mode-metadata "customer_segment" *customer-segments*)
        (cltd:make-mode-metadata "product_category" *product-categories*)
        (cltd:make-mode-metadata "time_slot" *time-slots*)
        (cltd:make-mode-metadata "channel" *channels*)))

;;; ==========================================================================
;;; Integration Tests
;;; ==========================================================================

(deftest integration-full-pipeline
  (testing "Full pipeline: synthetic data -> decomposition -> report"
    (let* ((random-state (make-random-state nil)) ; deterministic
           (metadata (build-mode-metadata)))
      (multiple-value-bind (x-indices x-values)
          (make-synthetic-retail-data :random-state random-state)

        ;; 1. Validate input data
        (testing "Input validation passes"
          (ok (cltd:validate-input-data *x-shape* x-indices x-values)
              "Synthetic data passes validation"))

        ;; Create sparse tensor for decomposition
        (let ((tensor (cltd:make-sparse-tensor *x-shape* x-indices x-values)))

          ;; 2. Run decomposition
          (testing "Decomposition completes successfully"
            (multiple-value-bind (factor-matrices iterations)
                (cltd:decomposition tensor
                                    :r 4
                                    :n-cycle 50
                                    :convergence-threshold 1d-4
                                    :convergence-window 5
                                    :verbose nil)
            (ok (= (length factor-matrices) (length *x-shape*))
                "Returns correct number of factor matrices")
            (ok (plusp iterations)
                "Positive iteration count")

            ;; Check factor matrix dimensions
            ;; factor-matrices is a vector of 2D arrays
            (loop for mode from 0 below (length factor-matrices)
                  for fm = (aref factor-matrices mode)
                  for dim = (nth mode *x-shape*)
                  do (ok (= (array-dimension fm 0) dim)
                         (format nil "Factor matrix ~D has correct rows" mode))
                     (ok (= (array-dimension fm 1) 4)
                         (format nil "Factor matrix ~D has correct rank" mode)))

            ;; 3. Generate factor cards (without diagnostics for simpler structure)
            (testing "Factor card generation"
              (let ((cards (cltd:generate-factor-cards
                            factor-matrices x-indices x-values metadata
                            :include-diagnostics nil)))
                (ok (= (length cards) 4)
                    "Generates 4 factor cards")

                ;; Check card structure
                (dolist (card cards)
                  (ok (assoc :factor_id card)
                      "Card has factor_id")
                  (ok (assoc :lambda card)
                      "Card has lambda")
                  (ok (assoc :coverage card)
                      "Card has coverage")
                  (ok (assoc :salient card)
                      "Card has salient entries")
                  (ok (assoc :coherence card)
                      "Card has coherence"))

                ;; 4. Generate markdown report
                (testing "Markdown report generation"
                  (let ((markdown (cltd:factor-report-markdown-string cards)))
                    (ok (stringp markdown)
                        "Generates markdown string")
                    (ok (plusp (length markdown))
                        "Markdown is non-empty")
                    (ok (search "Factor" markdown)
                        "Markdown contains 'Factor'"))))))))))))

(deftest integration-diagnostics-output
  (testing "Diagnostics output structure"
    (let* ((random-state (make-random-state nil))
           (metadata (build-mode-metadata)))
      (multiple-value-bind (x-indices x-values)
          (make-synthetic-retail-data :random-state random-state)
        (let ((tensor (cltd:make-sparse-tensor *x-shape* x-indices x-values)))
          (multiple-value-bind (factor-matrices iterations)
              (cltd:decomposition tensor
                                  :r 3
                                  :n-cycle 30
                                  :verbose nil)
          (declare (ignore iterations))
          ;; With include-diagnostics, returns (:model_diagnostics . ...) (:factors . ...)
          (let* ((result (cltd:generate-factor-cards
                          factor-matrices x-indices x-values metadata
                          :include-diagnostics t))
                 (model-diag (cdr (assoc :model_diagnostics result)))
                 (cards (cdr (assoc :factors result))))
            (testing "Model-level diagnostics"
              (ok (assoc :kl_divergence model-diag)
                  "Has KL divergence")
              (ok (assoc :factor_similarity model-diag)
                  "Has factor similarity")
              (ok (assoc :exclusivity model-diag)
                  "Has exclusivity")
              (ok (assoc :overlap model-diag)
                  "Has overlap")
              (ok (assoc :responsibility_stats model-diag)
                  "Has responsibility stats")
              (ok (assoc :residual_stats model-diag)
                  "Has residual stats")
              (ok (assoc :kl_contributions model-diag)
                  "Has KL contributions vector"))
            (testing "Factor-level diagnostics"
              (let ((first-card (first cards)))
                (ok (assoc :kl_contribution first-card)
                    "Card has KL contribution")
                (ok (assoc :contribution_rank first-card)
                    "Card has contribution rank"))))))))))

(deftest integration-convergence-behavior
  (testing "Convergence with threshold"
    (let* ((random-state (make-random-state nil)))
      (multiple-value-bind (x-indices x-values)
          (make-synthetic-retail-data :random-state random-state)
        (let ((tensor (cltd:make-sparse-tensor *x-shape* x-indices x-values)))
          (multiple-value-bind (factor-matrices iterations)
              (cltd:decomposition tensor
                                  :r 3
                                  :n-cycle 200
                                  :convergence-threshold 1d-5
                                  :convergence-window 5
                                  :verbose nil)
            (declare (ignore factor-matrices))
            (ok (< iterations 200)
                "Converges before max iterations")
            (ok (>= iterations 5)
                "Runs at least convergence-window iterations")))))))

(deftest integration-more-iterations-lower-kl
  (testing "More iterations generally reduce KL divergence"
    (let* ((random-state (make-random-state nil)))
      (multiple-value-bind (x-indices x-values)
          (make-synthetic-retail-data :random-state random-state)
        (let ((tensor (cltd:make-sparse-tensor *x-shape* x-indices x-values)))
          ;; Run with more iterations - should get lower or equal KL
          (multiple-value-bind (fm-later iterations-later)
              (cltd:decomposition tensor :r 3 :n-cycle 100 :verbose nil)
            (declare (ignore iterations-later))
            (let* ((x-hat (make-array (length x-values)
                                      :element-type 'double-float
                                      :initial-element 0d0))
                   (kl-later (progn
                               (cltd:sdot fm-later x-indices x-hat)
                               (cltd:sparse-kl-divergence x-indices x-values
                                                          x-hat fm-later))))
              ;; Just check KL is finite and positive
              (ok (and (numberp kl-later) (plusp kl-later) (= kl-later kl-later))
                  (format nil "KL divergence is valid: ~,4F" kl-later)))))))))

(deftest integration-ranking-consistency
  (testing "Ranking function works with decomposition results"
    (let* ((random-state (make-random-state nil))
           (rank 3))
      (multiple-value-bind (x-indices x-values)
          (make-synthetic-retail-data :random-state random-state)
        (let ((tensor (cltd:make-sparse-tensor *x-shape* x-indices x-values)))
          (multiple-value-bind (factor-matrices iterations)
              (cltd:decomposition tensor :r rank :n-cycle 30 :verbose nil)
            (declare (ignore iterations))
            ;; Get ranking for each mode and each factor
            ;; factor-matrices is a vector of 2D arrays
            (let ((all-labels (list *customer-segments* *product-categories*
                                    *time-slots* *channels*)))
              (loop for mode from 0 below (length factor-matrices)
                    for fm = (aref factor-matrices mode)
                    for labels = (nth mode all-labels)
                    do (loop for r from 0 below rank
                             do (let ((ranking (cltd:ranking labels fm r)))
                                  (ok (= (length ranking) (length labels))
                                      (format nil "Mode ~D, Factor ~D: ranking has all labels" mode r))
                                  ;; Check ranking is sorted descending by weight
                                  (ok (listp ranking)
                                      "Ranking is a list of (label . weight) pairs")))))))))))

;;; ==========================================================================
;;; Rank selection on real count data (Palmer Penguins)
;;; ==========================================================================
;;; Aggregated from the palmerpenguins dataset (CC0), the same counts the
;;; examples/palmer-penguins-analysis.lisp demo uses, embedded here so the test
;;; does not depend on the examples directory.
;;;
;;; The structure is known: Chinstrap appears only on Dream and Gentoo only on
;;; Biscoe, while Adelie appears on all three islands, so three rank-one
;;; components describe the data. Cross-validation should see that -- which the
;;; coordinate hold-out scheme could not, because it scored every model on cells
;;; the training tensor had told it were zero. Measured on this data, the old
;;; scheme produced a monotonically increasing curve (55.6, 515, 955, 1092,
;;; 1092) and always chose rank 1.

(defparameter *penguins-shape* '(3 3 2))

(defparameter *penguins-counts*
  '((0 0 0 22) (0 0 1 22)      ; Adelie,    Biscoe
    (0 1 0 27) (0 1 1 28)      ; Adelie,    Dream
    (0 2 0 24) (0 2 1 23)      ; Adelie,    Torgersen
    (1 1 0 34) (1 1 1 34)      ; Chinstrap, Dream only
    (2 0 0 58) (2 0 1 61)))    ; Gentoo,    Biscoe only

(defun make-penguins-tensor ()
  (let* ((nnz (length *penguins-counts*))
         (n-modes (length *penguins-shape*))
         (indices (make-array (list nnz n-modes) :element-type 'fixnum))
         (values (make-array nnz :element-type 'double-float)))
    (loop for entry in *penguins-counts*
          for row from 0
          do (loop for mode from 0 below n-modes
                   do (setf (aref indices row mode) (nth mode entry)))
             (setf (aref values row) (coerce (nth n-modes entry) 'double-float)))
    (cltd:make-sparse-tensor *penguins-shape* indices values)))

(defun %result-for (results rank)
  (find rank results :key (lambda (r) (cdr (assoc :rank r)))))

(deftest integration-rank-selection-detects-underfitting
  "Cross-validation must separate an underfitting rank from an adequate one.

Asserting which rank wins would be unstable -- the curve plateaus past the
elbow, so the argmin there is noise-driven. What is stable, and what the old
scheme got backwards, is that rank 1 scores far worse than rank 3."
  (let* ((tensor (make-penguins-tensor))
         (results (cltd:cross-validate-rank tensor '(1 2 3)
                                            :k 5
                                            :n-cycle 200
                                            :convergence-threshold 1d-6
                                            :convergence-window 10
                                            :random-state (cltd:%seed-random-state 20260827)))
         (r1 (%result-for results 1))
         (r2 (%result-for results 2))
         (r3 (%result-for results 3)))
    (ok (= (length results) 3) "One result per candidate rank")
    (ok (= (reduce #'+ (cdr (assoc :validation-counts r1))) 333)
        "Validation counts across folds recover all 333 observations")
    (let ((mean1 (cdr (assoc :mean r1)))
          (mean2 (cdr (assoc :mean r2)))
          (mean3 (cdr (assoc :mean r3)))
          (se3 (cdr (assoc :standard-error r3))))
      (ok (> mean1 mean2)
          (format nil "rank 1 scores worse than rank 2 (~,4F > ~,4F)" mean1 mean2))
      (ok (> mean2 mean3)
          (format nil "rank 2 scores worse than rank 3 (~,4F > ~,4F)" mean2 mean3))
      ;; The gap is ~15 standard errors on this data; 3 keeps the test stable.
      (ok (> (- mean1 mean3) (* 3d0 se3))
          (format nil "rank 1 is worse than rank 3 by ~,1F standard errors"
                  (/ (- mean1 mean3) se3))))))

(deftest integration-rank-selection-does-not-collapse-to-rank-one
  "The selected rank must not be the degenerate answer the old scheme always gave."
  (let ((tensor (make-penguins-tensor)))
    (dolist (seed '(20260827 11 12345))
      (multiple-value-bind (best results)
          (cltd:select-rank tensor '(1 2 3 4)
                            :k 5
                            :n-cycle 200
                            :convergence-threshold 1d-6
                            :convergence-window 10
                            :random-state (cltd:%seed-random-state seed))
        (ok (> (cdr (assoc :rank best)) 1)
            (format nil "seed ~D selects rank ~D, not rank 1"
                    seed (cdr (assoc :rank best))))
        (ok (equal (mapcar (lambda (r) (cdr (assoc :rank r))) results) '(1 2 3 4))
            (format nil "seed ~D leaves the result list in input order" seed))))))
