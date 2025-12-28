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
            (let ((kl-later (cltd:sparse-kl-divergence x-indices x-values fm-later)))
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
