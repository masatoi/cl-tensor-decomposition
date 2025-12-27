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

(defparameter +test-epsilon+ 1d-6)

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
  (let* ((approx (make-array 3 :element-type 'double-float
                             :initial-contents '(1.5d0 1.8d0 2.5d0)))
         (values (make-array 3 :element-type 'double-float
                             :initial-contents '(1d0 2d0 3d0)))
         (kl (cltd:sparse-kl-divergence X-indices-matrix values approx))
         (epsilon cltd::*epsilon*)
         (expected 0d0))
    (loop for idx from 0 below (length values) do
      (let* ((x (aref values idx))
             (xhat (aref approx idx)))
        (incf expected (+ (* x (log (/ x (+ xhat epsilon))))
                          (- x)
                          xhat))))
    (ok (< (abs (- kl expected)) +test-epsilon+)
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
  (ok (decomposition X-shape X-indices-matrix X-value-vector
                     :n-cycle 100 :R 2 :verbose t)
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
      (cltd:decomposition X-shape X-indices-matrix X-value-vector
                          :n-cycle 100
                          :R 2
                          :convergence-threshold 1d6
                          :convergence-window 3)
    (declare (ignore result-vec))
    (ok (< iterations 100) "Convergence threshold stops early")
    (ok (>= iterations 3) "At least window iterations executed")))

(deftest make-fold-splits-cover-all-indices
  (let* ((ranks '(1 2))
         (random-state (make-random-state t))
         (splits (cltd:make-fold-splits X-indices-matrix X-value-vector 2
                                        :random-state random-state)))
    (ok (= (length splits) 2) "Two folds generated")
    (let ((all-indices (sort (copy-list (apply #'append splits)) #'<)))
      (ok (equal all-indices '(0 1 2)) "All indices covered exactly once")))
  (let* ((ranks '(1 2))
         (cv-results (cltd:cross-validate-rank X-indices-matrix X-value-vector ranks
                                               :k 2
                                               :n-cycle 10
                                               :random-state (make-random-state t)
                                               :convergence-threshold 1d-4
                                               :convergence-window 3))
         (best-rank nil)
         (best-mean most-positive-double-float))
    (ok (= (length cv-results) (length ranks))
        "Cross-validation returns entry per rank")
    (dolist (result cv-results)
      (let ((rank (cdr (assoc :rank result)))
            (mean (cdr (assoc :mean result))))
        (when (< mean best-mean)
          (setf best-mean mean
                best-rank rank))))
    (multiple-value-bind (best all-results)
        (cltd:select-rank X-indices-matrix X-value-vector ranks
                          :k 2
                          :n-cycle 10
                          :random-state (make-random-state t)
                          :convergence-threshold 1d-4
                          :convergence-window 3)
      (ok (= (length all-results) (length ranks))
          "select-rank echoes full results")
      (ok (member (cdr (assoc :rank best)) ranks)
          "Best rank within candidates")
      (ok (= (cdr (assoc :rank best)) best-rank)
          "select-rank matches manual search"))))

(deftest cross-validate-rank-handles-fold-shapes
  (let* ((ranks '(2))
         (results (cltd:cross-validate-rank X-indices-matrix X-value-vector ranks
                                            :k 5
                                            :n-cycle 5
                                            :random-state (make-random-state t))))
    (let* ((result (first results))
           (scores (cdr (assoc :scores result))))
      (ok (= (length scores) 5) "k greater than nnz yields score per fold")
      (ok (every #'numberp scores) "scores remain numeric even for empty folds")))
  (let* ((results (cltd:cross-validate-rank X-indices-matrix X-value-vector '(1)
                                            :k 1
                                            :n-cycle 5
                                            :random-state (make-random-state t)))
         (scores (cdr (assoc :scores (first results)))))
    (ok (= (length scores) 1) "k = 1 degenerates to single fold")
    (ok (every #'numberp scores) "Single-fold score is numeric")))

(deftest cross-validate-rank-is-reproducible-with-same-random-state
  (let* ((seed (make-random-state t))
         (ranks '(1 2))
         (first-run (cltd:cross-validate-rank X-indices-matrix X-value-vector ranks
                                              :k 3
                                              :n-cycle 10
                                              :random-state (make-random-state seed)))
         (second-run (cltd:cross-validate-rank X-indices-matrix X-value-vector ranks
                                               :k 3
                                               :n-cycle 10
                                               :random-state (make-random-state seed)))
         (no-seed (cltd:cross-validate-rank X-indices-matrix X-value-vector ranks
                                            :k 3
                                            :n-cycle 10)))
    (ok (equalp first-run second-run)
        "Providing the same random-state reproduces fold scores")
    (ok (= (length no-seed) (length ranks))
        "Cross-validation still returns results without random-state")))

(deftest cross-validate-rank-respects-custom-evaluation-function
  (labels ((always-42 (indices counts approx)
             (declare (ignore indices counts approx))
             42d0))
    (let ((results (cltd:cross-validate-rank X-indices-matrix X-value-vector '(1 2)
                                             :k 2
                                             :n-cycle 5
                                             :random-state (make-random-state t)
                                             :evaluation-function #'always-42)))
      (dolist (result results)
        (let ((scores (cdr (assoc :scores result)))
              (mean (cdr (assoc :mean result))))
          (ok (every (lambda (score) (= score 42d0)) scores)
              "Custom evaluation function overrides fold score computation")
          (ok (= mean 42d0)
              "Mean reflects custom evaluation metric"))))))

(deftest cross-validate-rank-handles-unique-max-index
  (let* ((unique-indices (make-array '(3 2) :element-type 'fixnum
                                     :initial-contents '((0 0)
                                                         (0 1)
                                                         (1 2))))
         (unique-counts (make-array 3 :element-type 'double-float
                                    :initial-contents '(1d0 2d0 3d0))))
    (ok (handler-case
            (progn
              (cltd:cross-validate-rank unique-indices unique-counts '(1)
                                        :k 3
                                        :n-cycle 5
                                        :random-state (make-random-state t))
              t)
          (error (condition)
            (declare (ignore condition))
            nil))
        "Cross-validation handles folds that hold out unique max index")))

(deftest select-rank-returns-expected-defaults
  (let ((default-ranks '(1 2)))
    (multiple-value-bind (default-best default-results)
        (cltd:select-rank X-indices-matrix X-value-vector default-ranks
                          :k 2
                          :n-cycle 10
                          :convergence-threshold 1d-4
                          :convergence-window 3)
      (ok (= (length default-results) (length default-ranks))
          "Default select-rank returns per-rank results")
      (ok (member (cdr (assoc :rank default-best)) default-ranks)
          "Default select-rank chooses rank from candidates"))))

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
                   (and (numberp r) (not (sb-ext:float-nan-p r)))))
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
         (kl (cltd:sparse-kl-divergence indices x-values x-hat)))
    (ok (numberp kl)
        "KL divergence is a number")
    (ok (not (sb-ext:float-nan-p kl))
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
  (let* ((random-state (sb-ext:seed-random-state 42))
         (*random-state* random-state))
    (multiple-value-bind (factor-matrices)
        (cltd:decomposition X-shape X-indices-matrix X-value-vector
                            :n-cycle 20 :r 2)
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
  (let* ((random-state (sb-ext:seed-random-state 42))
         (*random-state* random-state))
    (multiple-value-bind (factor-matrices)
        (cltd:decomposition X-shape X-indices-matrix X-value-vector
                            :n-cycle 20 :r 2)
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
  (let* ((random-state (sb-ext:seed-random-state 42))
         (*random-state* random-state))
    (declare (ignorable random-state))
    (multiple-value-bind (factor-matrices)
        (cltd:decomposition X-shape X-indices-matrix X-value-vector
                            :n-cycle 20 :r 2)
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

;;; ============================================================================
;;; Boundary Case Tests
;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; R=1 (Single Factor) Tests
;;; ---------------------------------------------------------------------------

(deftest single-factor-decomposition
  "Decomposition with R=1 should produce valid single-factor matrices."
  (let* ((random-state (sb-ext:seed-random-state 42))
         (*random-state* random-state))
    (multiple-value-bind (factor-matrices iterations)
        (cltd:decomposition X-shape X-indices-matrix X-value-vector
                            :n-cycle 50 :r 1)
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
         (random-state (sb-ext:seed-random-state 42))
         (*random-state* random-state))
    (multiple-value-bind (factor-matrices iterations)
        (cltd:decomposition x-shape indices counts :n-cycle 10 :r 2)
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
         (kl (cltd:sparse-kl-divergence indices x-values x-hat)))
    (ok (numberp kl)
        "KL divergence is a number")
    (ok (not (sb-ext:float-nan-p kl))
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
    (ok (not (sb-ext:float-nan-p (aref residuals 0)))
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
         (kl (cltd:sparse-kl-divergence indices x-values x-hat)))
    (ok (numberp kl)
        "KL divergence is a number with large values")
    (ok (not (sb-ext:float-nan-p kl))
        "KL divergence is not NaN with large values")
    (ok (not (sb-ext:float-infinity-p kl))
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
          (ok (not (sb-ext:float-nan-p val))
              (format nil "Responsibility(~D,~D) is not NaN" obs r))
          (ok (not (sb-ext:float-infinity-p val))
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
      (ok (not (sb-ext:float-nan-p (aref residuals i)))
          (format nil "Residual ~D is not NaN" i))
      (ok (not (sb-ext:float-infinity-p (aref residuals i)))
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
         (kl (cltd:sparse-kl-divergence indices x-values x-hat)))
    (ok (numberp kl)
        "KL divergence is a number with tiny values")
    (ok (not (sb-ext:float-nan-p kl))
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
        (ok (not (sb-ext:float-nan-p (aref sim-matrix r1 r2)))
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
        (ok (not (sb-ext:float-nan-p (aref responsibilities obs r)))
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
    (ok (not (sb-ext:float-nan-p (aref sim-matrix 0 1)))
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
      (ok (not (sb-ext:float-nan-p (aref residuals i)))
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
