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
