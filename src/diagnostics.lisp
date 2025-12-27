;;; -*- coding:utf-8; mode:lisp -*-

;;;; diagnostics.lisp - Diagnostic metrics for tensor decomposition results
;;;;
;;;; This module provides additional metrics for interpreting and validating
;;;; tensor decomposition results, primarily for LLM-based report generation.

(in-package :cl-tensor-decomposition)

;;; ============================================================================
;;; Factor Similarity Matrix
;;; ============================================================================

(defun compute-factor-column-norm (matrix column-index)
  "Compute the L2 norm of a column in MATRIX."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (* *)) matrix)
           (type fixnum column-index))
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (loop for i fixnum from 0 below (array-dimension matrix 0) do
      (let ((val (aref matrix i column-index)))
        (incf sum (* val val))))
    (sqrt sum)))

(defun compute-factor-column-dot (matrix col1 col2)
  "Compute the dot product of two columns in MATRIX."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (* *)) matrix)
           (type fixnum col1 col2))
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (loop for i fixnum from 0 below (array-dimension matrix 0) do
      (incf sum (* (aref matrix i col1) (aref matrix i col2))))
    sum))

(defun compute-cosine-similarity-for-mode (matrix r1 r2 epsilon)
  "Compute cosine similarity between columns R1 and R2 of MATRIX."
  (declare (type (simple-array double-float (* *)) matrix)
           (type fixnum r1 r2)
           (type double-float epsilon))
  (let ((dot (compute-factor-column-dot matrix r1 r2))
        (norm1 (compute-factor-column-norm matrix r1))
        (norm2 (compute-factor-column-norm matrix r2)))
    (if (or (< norm1 epsilon) (< norm2 epsilon))
        0.0d0
        (/ dot (* norm1 norm2)))))

(defun compute-factor-similarity-matrix (factor-matrix-vector
                                         &key (epsilon 1.0d-10)
                                              (aggregation :mean))
  "Compute pairwise similarity between factors across all modes.

FACTOR-MATRIX-VECTOR is the result of tensor decomposition.
EPSILON is used to avoid division by zero.
AGGREGATION specifies how to combine similarities across modes:
  :mean - arithmetic mean (default)
  :min  - minimum similarity (conservative, all modes must agree)
  :max  - maximum similarity (liberal, any mode similarity counts)
  :geometric - geometric mean

Returns a symmetric R x R matrix of similarity scores in [0, 1]."
  (declare (type simple-vector factor-matrix-vector)
           (type double-float epsilon))
  (let* ((num-modes (length factor-matrix-vector))
         (r (array-dimension (svref factor-matrix-vector 0) 1))
         (similarity-matrix (make-array (list r r)
                                        :element-type 'double-float
                                        :initial-element 0.0d0))
         (mode-similarities (make-array num-modes :element-type 'double-float)))
    (loop for r1 from 0 below r do
      (setf (aref similarity-matrix r1 r1) 1.0d0)
      (loop for r2 from (1+ r1) below r do
        ;; Compute similarity for each mode
        (loop for mode-index from 0 below num-modes do
          (let ((matrix (svref factor-matrix-vector mode-index)))
            (setf (aref mode-similarities mode-index)
                  (compute-cosine-similarity-for-mode matrix r1 r2 epsilon))))
        ;; Aggregate across modes
        (let ((aggregated
                (ecase aggregation
                  (:mean
                   (/ (loop for i from 0 below num-modes
                            sum (aref mode-similarities i))
                      num-modes))
                  (:min
                   (loop for i from 0 below num-modes
                         minimize (aref mode-similarities i)))
                  (:max
                   (loop for i from 0 below num-modes
                         maximize (aref mode-similarities i)))
                  (:geometric
                   (let ((product 1.0d0))
                     (loop for i from 0 below num-modes do
                       (setf product (* product
                                        (max epsilon (aref mode-similarities i)))))
                     (expt product (/ 1.0d0 num-modes)))))))
          (setf (aref similarity-matrix r1 r2) aggregated)
          (setf (aref similarity-matrix r2 r1) aggregated))))
    similarity-matrix))

(defun extract-similar-factor-pairs (similarity-matrix &key (threshold 0.7d0))
  "Extract pairs of factors with similarity above THRESHOLD.

Returns a list of (r1 r2 similarity) tuples, sorted by similarity descending.
Diagonal elements (self-similarity) are excluded."
  (declare (type (simple-array double-float (* *)) similarity-matrix)
           (type double-float threshold))
  (let ((r (array-dimension similarity-matrix 0))
        (pairs nil))
    (loop for r1 from 0 below r do
      (loop for r2 from (1+ r1) below r do
        (let ((sim (aref similarity-matrix r1 r2)))
          (when (>= sim threshold)
            (push (list r1 r2 sim) pairs)))))
    (sort pairs #'> :key #'third)))

(defun similarity-matrix->alist (similarity-matrix &key (threshold 0.5d0))
  "Convert similarity matrix to alist format for JSON export.

Only includes pairs with similarity >= THRESHOLD to reduce output size.
Returns alist with :matrix (full matrix as nested lists) and
:similar_pairs (high-similarity pairs)."
  (declare (type (simple-array double-float (* *)) similarity-matrix))
  (let* ((r (array-dimension similarity-matrix 0))
         (matrix-list
           (loop for i from 0 below r
                 collect (loop for j from 0 below r
                               collect (round-to (aref similarity-matrix i j)))))
         (pairs (extract-similar-factor-pairs similarity-matrix :threshold threshold)))
    (list (cons :matrix matrix-list)
          (cons :similar_pairs
                (mapcar (lambda (p)
                          (list (cons :factor1 (first p))
                                (cons :factor2 (second p))
                                (cons :similarity (round-to (third p)))))
                        pairs))
          (cons :threshold threshold))))

(defun compute-factor-redundancy-score (similarity-matrix &key (threshold 0.8d0))
  "Compute an overall redundancy score for the factor model.

Returns a value in [0, 1] indicating the fraction of factor pairs
that are highly similar (above THRESHOLD).
High redundancy suggests the rank may be too high."
  (declare (type (simple-array double-float (* *)) similarity-matrix)
           (type double-float threshold))
  (let* ((r (array-dimension similarity-matrix 0))
         (num-pairs (/ (* r (1- r)) 2))
         (redundant-count 0))
    (when (zerop num-pairs)
      (return-from compute-factor-redundancy-score 0.0d0))
    (loop for r1 from 0 below r do
      (loop for r2 from (1+ r1) below r do
        (when (>= (aref similarity-matrix r1 r2) threshold)
          (incf redundant-count))))
    (/ (coerce redundant-count 'double-float) num-pairs)))

;;; ============================================================================
;;; Factor KL Contribution
;;; ============================================================================

(defun sdot-excluding-factor (factor-matrix-vector x-indices-matrix x^-value-vector
                              excluded-factor)
  "Reconstruct sparse observations excluding one factor.

Similar to SDOT but skips the contribution of EXCLUDED-FACTOR.
Used to measure how much each factor contributes to the reconstruction."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) x-indices-matrix)
           (type (simple-array double-float) x^-value-vector)
           (type fixnum excluded-factor))
  (let ((r (array-dimension (svref factor-matrix-vector 0) 1))
        (num-modes (length factor-matrix-vector)))
    (declare (type fixnum r num-modes))
    (loop for datum-index fixnum from 0 below (array-dimension x-indices-matrix 0) do
      (setf (aref x^-value-vector datum-index)
            (loop for ri fixnum from 0 below r
                  ;; Skip the excluded factor
                  when (/= ri excluded-factor)
                  sum (let ((prod 1.0d0))
                        (declare (type double-float prod))
                        (loop for mode-index fixnum from 0 below num-modes do
                          (let ((matrix (svref factor-matrix-vector mode-index)))
                            (declare (type (simple-array double-float) matrix))
                            (setf prod
                                  (* prod (aref matrix
                                                (aref x-indices-matrix datum-index mode-index)
                                                ri)))))
                        prod)
                  double-float)))))

(defun compute-factor-kl-contributions (factor-matrix-vector x-indices-matrix x-value-vector
                                        &key (epsilon 1.0d-6))
  "Compute the KL divergence contribution of each factor.

For each factor r, computes:
  contribution(r) = KL(X, X^_{-r}) - KL(X, X^)

where X^_{-r} is the reconstruction excluding factor r.

Returns a vector of contributions, one per factor.
Higher values indicate the factor is more important for reconstruction."
  (declare (type simple-vector factor-matrix-vector)
           (type (simple-array fixnum) x-indices-matrix)
           (type (simple-array double-float) x-value-vector))
  (let* ((r (array-dimension (svref factor-matrix-vector 0) 1))
         (nnz (length x-value-vector))
         (contributions (make-array r :element-type 'double-float :initial-element 0.0d0))
         (x^-full (make-array nnz :element-type 'double-float :initial-element 0.0d0))
         (x^-partial (make-array nnz :element-type 'double-float :initial-element 0.0d0))
         (*epsilon* epsilon))
    ;; Compute full reconstruction KL
    (sdot factor-matrix-vector x-indices-matrix x^-full)
    (let ((kl-full (sparse-kl-divergence x-indices-matrix x-value-vector x^-full)))
      ;; Compute KL without each factor
      (loop for factor-index from 0 below r do
        (sdot-excluding-factor factor-matrix-vector x-indices-matrix x^-partial factor-index)
        (let ((kl-partial (sparse-kl-divergence x-indices-matrix x-value-vector x^-partial)))
          (setf (aref contributions factor-index)
                (- kl-partial kl-full)))))
    contributions))

(defun normalize-contributions (contributions &key (epsilon 1.0d-10))
  "Normalize contribution vector to sum to 1.0.

Returns a new vector where each element represents the fraction of
total contribution for that factor.

Negative contributions (which may occur due to numerical issues) are
clamped to 0 before normalization. If total contribution is below
epsilon, returns a uniform distribution."
  (let* ((r (length contributions))
         (clamped (make-array r :element-type 'double-float))
         (total 0.0d0)
         (normalized (make-array r :element-type 'double-float)))
    ;; Clamp negative values to 0 and compute total
    (loop for i from 0 below r
          do (let ((val (max 0.0d0 (aref contributions i))))
               (setf (aref clamped i) val)
               (incf total val)))
    ;; Normalize or use uniform distribution
    (if (< total epsilon)
        (loop for i from 0 below r
              do (setf (aref normalized i) (/ 1.0d0 r)))
        (loop for i from 0 below r
              do (setf (aref normalized i) (/ (aref clamped i) total))))
    normalized))

(defun kl-contributions->alist (contributions &key normalize)
  "Convert KL contributions to alist format for JSON export.

If NORMALIZE is true, also includes normalized (proportional) contributions.

Note: The :total field represents the sum of clamped contributions (negatives
treated as 0), which is consistent with the normalization basis. The raw
per-factor contributions are preserved in :contributions for diagnostic purposes."
  (let* ((r (length contributions))
         (contribution-list
           (loop for i from 0 below r
                 collect (list (cons :factor i)
                               (cons :contribution
                                     (round-to (aref contributions i))))))
         ;; Compute clamped total for consistency with normalize-contributions
         (clamped-total
           (loop for i from 0 below r
                 sum (max 0.0d0 (aref contributions i))))
         (result
           (list (cons :contributions contribution-list)
                 (cons :total (round-to clamped-total)))))
    (when normalize
      (let ((normalized (normalize-contributions contributions)))
        (push
         (cons :normalized
               (loop for i from 0 below r
                     collect (list (cons :factor i)
                                   (cons :share
                                         (round-to (aref normalized i))))))
         result)))
    result))

(defun rank-factors-by-contribution (contributions)
  "Return factor indices sorted by contribution (descending).

Returns a list of (factor-index contribution) pairs."
  (let* ((r (length contributions))
         (pairs (loop for i from 0 below r
                      collect (cons i (aref contributions i)))))
    (sort pairs #'> :key #'cdr)))

;;; ============================================================================
;;; Observation Responsibilities
;;; ============================================================================

(defun compute-observation-responsibilities
       (factor-matrix-vector x-indices-matrix &key (epsilon 1.0d-10))
  "Compute responsibility of each observation to each factor.

For each observation n and factor r:
  responsibility(n, r) = Π_m a_{x_n^m, r} / Σ_r' Π_m a_{x_n^m, r'}

This measures how much each factor contributes to explaining each observation.
For unnormalized factor matrices, we use raw factor products without lambda
scaling to maintain consistency with sdot reconstruction.

Returns an NNZ × R matrix where NNZ is the number of observations.
Each row sums to 1.0 (or close to it, within epsilon tolerance)."
  (declare (type simple-vector factor-matrix-vector)
           (type (simple-array fixnum) x-indices-matrix))
  (let* ((nnz (array-dimension x-indices-matrix 0))
         (num-modes (length factor-matrix-vector))
         (r (array-dimension (svref factor-matrix-vector 0) 1))
         (responsibilities
          (make-array (list nnz r) :element-type 'double-float :initial-element
                      0.0d0))
         (scores (make-array r :element-type 'double-float)))
    ;; For each observation, compute the factor products and normalize
    (loop for obs from 0 below nnz
          do (let ((denominator 0.0d0))
               ;; Compute score for each factor: Π_m a_{i_m, r}
               (loop for ri from 0 below r
                     do (let ((score 1.0d0))
                          (loop for mode-index from 0 below num-modes
                                do (let* ((idx
                                           (aref x-indices-matrix obs
                                                 mode-index))
                                          (matrix
                                           (svref factor-matrix-vector
                                                  mode-index)))
                                     (setf score
                                           (* score (aref matrix idx ri)))))
                          (setf (aref scores ri) score)
                          (incf denominator score)))
               ;; Normalize to get responsibilities
               (if (< denominator epsilon)
                   ;; If all scores are near zero, use uniform distribution
                   (loop for ri from 0 below r
                         do (setf (aref responsibilities obs ri)
                                  (/ 1.0d0 r)))
                   (loop for ri from 0 below r
                         do (setf (aref responsibilities obs ri)
                                  (/ (aref scores ri) denominator))))))
    responsibilities))

(defun compute-responsibility-stats (responsibilities x-value-vector
                                     &key (ambiguity-threshold 0.5d0))
  "Compute summary statistics for observation responsibilities.

RESPONSIBILITIES is an NNZ × R matrix from compute-observation-responsibilities.
X-VALUE-VECTOR contains the count/weight for each observation.
AMBIGUITY-THRESHOLD defines when an observation is considered ambiguous
  (max responsibility < threshold).

Returns multiple values:
  1. mean-max-responsibility: weighted average of max responsibility per observation
  2. ambiguous-rate: fraction of observations with max responsibility < threshold
  3. entropy-mean: weighted average entropy of responsibility distributions
  4. dominant-factor-counts: vector of how many observations each factor dominates"
  (declare (type (simple-array double-float (* *)) responsibilities)
           (type (simple-array double-float) x-value-vector))
  (let* ((nnz (array-dimension responsibilities 0))
         (r (array-dimension responsibilities 1))
         (total-weight 0.0d0)
         (weighted-max-sum 0.0d0)
         (weighted-entropy-sum 0.0d0)
         (ambiguous-weight 0.0d0)
         (dominant-counts (make-array r :element-type 'double-float :initial-element 0.0d0)))
    (loop for obs from 0 below nnz do
      (let ((weight (aref x-value-vector obs))
            (max-resp 0.0d0)
            (max-factor 0)
            (entropy 0.0d0))
        (incf total-weight weight)
        ;; Find max responsibility and compute entropy
        (loop for ri from 0 below r do
          (let ((resp (aref responsibilities obs ri)))
            (when (> resp max-resp)
              (setf max-resp resp
                    max-factor ri))
            (when (> resp 1.0d-10)
              (decf entropy (* resp (log resp))))))
        ;; Accumulate statistics
        (incf weighted-max-sum (* weight max-resp))
        (incf weighted-entropy-sum (* weight entropy))
        (incf (aref dominant-counts max-factor) weight)
        (when (< max-resp ambiguity-threshold)
          (incf ambiguous-weight weight))))
    (if (< total-weight 1.0d-10)
        (values 0.0d0 0.0d0 0.0d0 dominant-counts)
        (values (/ weighted-max-sum total-weight)
                (/ ambiguous-weight total-weight)
                (/ weighted-entropy-sum total-weight)
                dominant-counts))))

(defun responsibility-stats->alist (responsibilities x-value-vector
                                    &key (ambiguity-threshold 0.5d0)
                                         (include-dominant-counts t))
  "Convert responsibility statistics to alist format for JSON export.

Returns alist with:
  :mean_max_responsibility - average confidence of factor assignment
  :ambiguous_rate - fraction of observations without clear factor assignment
  :mean_entropy - average uncertainty in factor assignment (0 = certain, log(R) = uniform)
  :dominant_factor_counts - (optional) weight assigned to each factor as dominant"
  (multiple-value-bind (mean-max ambiguous-rate mean-entropy dominant-counts)
      (compute-responsibility-stats responsibilities x-value-vector
                                    :ambiguity-threshold ambiguity-threshold)
    (let ((result (list (cons :mean_max_responsibility (round-to mean-max))
                        (cons :ambiguous_rate (round-to ambiguous-rate))
                        (cons :mean_entropy (round-to mean-entropy))
                        (cons :ambiguity_threshold ambiguity-threshold))))
      (when include-dominant-counts
        (let ((r (length dominant-counts)))
          (push (cons :dominant_factor_counts
                      (loop for i from 0 below r
                            collect (list (cons :factor i)
                                          (cons :weight (round-to (aref dominant-counts i))))))
                result)))
      result)))

(defun find-ambiguous-observations (responsibilities x-indices-matrix
                                    &key (threshold 0.5d0) (max-results 100))
  "Find observations with ambiguous factor assignments.

Returns observations where max responsibility < threshold.
Each result contains the observation index and its responsibility distribution.
Limited to MAX-RESULTS to avoid excessive output."
  (declare (type (simple-array double-float (* *)) responsibilities)
           (type (simple-array fixnum) x-indices-matrix))
  (let* ((nnz (array-dimension responsibilities 0))
         (r (array-dimension responsibilities 1))
         (num-modes (array-dimension x-indices-matrix 1))
         (results nil)
         (count 0))
    (loop for obs from 0 below nnz
          while (< count max-results) do
      (let ((max-resp 0.0d0))
        (loop for ri from 0 below r do
          (setf max-resp (max max-resp (aref responsibilities obs ri))))
        (when (< max-resp threshold)
          (incf count)
          (push (list (cons :observation obs)
                      (cons :max_responsibility (round-to max-resp))
                      (cons :indices
                            (loop for m from 0 below num-modes
                                  collect (aref x-indices-matrix obs m)))
                      (cons :responsibilities
                            (loop for ri from 0 below r
                                  collect (round-to (aref responsibilities obs ri)))))
                results))))
    (nreverse results)))

;;; ============================================================================
;;; Factor Exclusivity
;;; ============================================================================

(defun compute-factor-exclusivity (responsibilities x-value-vector)
  "Compute factor exclusivity metrics from responsibility matrix.

Exclusivity measures how clearly observations belong to single factors:
  exclusivity = weighted mean of max responsibility per observation
  overlap = 1 - exclusivity

High exclusivity (close to 1) means clear segmentation.
Low exclusivity means soft clustering with observations spread across factors.

Returns multiple values:
  1. exclusivity: weighted average of max responsibilities [0, 1]
  2. overlap: 1 - exclusivity [0, 1]
  3. exclusivity-distribution: histogram of max responsibilities
     (bins: [0-0.2), [0.2-0.4), [0.4-0.6), [0.6-0.8), [0.8-1.0])"
  (declare (type (simple-array double-float (* *)) responsibilities)
           (type (simple-array double-float) x-value-vector))
  (let* ((nnz (array-dimension responsibilities 0))
         (r (array-dimension responsibilities 1))
         (total-weight 0.0d0)
         (weighted-max-sum 0.0d0)
         (bins (make-array 5 :element-type 'double-float :initial-element 0.0d0)))
    (loop for obs from 0 below nnz do
      (let ((weight (aref x-value-vector obs))
            (max-resp 0.0d0))
        (incf total-weight weight)
        ;; Find max responsibility
        (loop for ri from 0 below r do
          (setf max-resp (max max-resp (aref responsibilities obs ri))))
        ;; Accumulate weighted sum
        (incf weighted-max-sum (* weight max-resp))
        ;; Add to histogram bin
        (let ((bin-index (min 4 (floor (* max-resp 5)))))
          (incf (aref bins bin-index) weight))))
    (let* ((exclusivity (if (< total-weight 1.0d-10)
                            0.0d0
                            (/ weighted-max-sum total-weight)))
           (overlap (- 1.0d0 exclusivity)))
      ;; Normalize bins to proportions
      (when (> total-weight 1.0d-10)
        (loop for i from 0 below 5 do
          (setf (aref bins i) (/ (aref bins i) total-weight))))
      (values exclusivity overlap bins))))

(defun factor-exclusivity->alist (responsibilities x-value-vector)
  "Convert factor exclusivity metrics to alist format for JSON export.

Returns alist with:
  :exclusivity - average max responsibility (clarity of segmentation)
  :overlap - 1 - exclusivity (degree of soft clustering)
  :interpretation - textual interpretation of exclusivity level
  :distribution - histogram of max responsibilities by bin"
  (multiple-value-bind (exclusivity overlap bins)
      (compute-factor-exclusivity responsibilities x-value-vector)
    (let ((interpretation
            (cond
              ((>= exclusivity 0.8d0) "hard_segmentation")
              ((>= exclusivity 0.6d0) "clear_segmentation")
              ((>= exclusivity 0.4d0) "moderate_overlap")
              (t "soft_clustering"))))
      (list (cons :exclusivity (round-to exclusivity))
            (cons :overlap (round-to overlap))
            (cons :interpretation interpretation)
            (cons :distribution
                  (list (cons :bin_0_20 (round-to (aref bins 0)))
                        (cons :bin_20_40 (round-to (aref bins 1)))
                        (cons :bin_40_60 (round-to (aref bins 2)))
                        (cons :bin_60_80 (round-to (aref bins 3)))
                        (cons :bin_80_100 (round-to (aref bins 4)))))))))

;;; ============================================================================
;;; Per-Observation Residuals
;;; ============================================================================

(defun compute-observation-residuals
       (factor-matrix-vector x-indices-matrix x-value-vector
        &key (epsilon 1.0d-10))
  "Compute per-observation reconstruction residuals.

For each observation n:
  residual(n) = x_n * log(x_n / x^_n) - x_n + x^_n

This is the per-observation contribution to KL divergence.
High residuals indicate observations that are poorly explained by the model.

When x_n = 0, the KL contribution simplifies to x^_n (the reconstruction value).
This follows from the limit: lim_{x->0+} x*log(x/y) = 0.

Returns a vector of residuals, one per observation."
  (declare (type simple-vector factor-matrix-vector)
           (type (simple-array fixnum) x-indices-matrix)
           (type (simple-array double-float) x-value-vector))
  (let* ((nnz (array-dimension x-indices-matrix 0))
         (num-modes (length factor-matrix-vector))
         (r (array-dimension (svref factor-matrix-vector 0) 1))
         (residuals
          (make-array nnz :element-type 'double-float :initial-element 0.0d0)))
    (loop for obs from 0 below nnz
          do (let ((x (aref x-value-vector obs)) (x-hat 0.0d0))
               ;; Compute reconstruction x-hat = Σ_r Π_m a_{i_m, r}
               (loop for ri from 0 below r
                     do (let ((prod 1.0d0))
                          (loop for mode-index from 0 below num-modes
                                do (let* ((idx
                                           (aref x-indices-matrix obs
                                                 mode-index))
                                          (matrix
                                           (svref factor-matrix-vector
                                                  mode-index)))
                                     (setf prod
                                           (* prod (aref matrix idx ri)))))
                          (incf x-hat prod)))
               ;; Compute residual with proper handling of x=0 case
               (setf (aref residuals obs)
                     (if (> x 0.0d0)
                         ;; Standard KL contribution: x*log(x/x^) - x + x^
                         (+ (* x (log (/ x (+ x-hat epsilon)))) (- x) x-hat)
                         ;; When x=0: lim_{x->0+} x*log(x/y) = 0, so residual = x^
                         x-hat))))
    residuals))

(defun compute-residual-stats (residuals x-value-vector &key (outlier-sigma 3.0d0))
  "Compute summary statistics for per-observation residuals.

Returns multiple values:
  1. mean: weighted mean of residuals
  2. std: weighted standard deviation
  3. p95: 95th percentile of residuals
  4. outlier-count: number of observations with residual > mean + outlier-sigma * std
  5. outlier-weight: total weight of outlier observations"
  (declare (type (simple-array double-float) residuals x-value-vector))
  (let* ((n (length residuals))
         (total-weight 0.0d0)
         (weighted-sum 0.0d0)
         (weighted-sq-sum 0.0d0)
         (sorted-pairs nil))
    ;; First pass: compute mean
    (loop for i from 0 below n do
      (let ((weight (aref x-value-vector i))
            (residual (aref residuals i)))
        (incf total-weight weight)
        (incf weighted-sum (* weight residual))
        (push (cons residual weight) sorted-pairs)))
    (when (< total-weight 1.0d-10)
      (return-from compute-residual-stats
        (values 0.0d0 0.0d0 0.0d0 0 0.0d0)))
    (let ((mean (/ weighted-sum total-weight)))
      ;; Second pass: compute variance
      (loop for i from 0 below n do
        (let* ((weight (aref x-value-vector i))
               (residual (aref residuals i))
               (diff (- residual mean)))
          (incf weighted-sq-sum (* weight diff diff))))
      (let* ((variance (/ weighted-sq-sum total-weight))
             (std (sqrt variance))
             (outlier-threshold (+ mean (* outlier-sigma std)))
             (outlier-count 0)
             (outlier-weight 0.0d0))
        ;; Count outliers
        (loop for i from 0 below n do
          (when (> (aref residuals i) outlier-threshold)
            (incf outlier-count)
            (incf outlier-weight (aref x-value-vector i))))
        ;; Compute p95
        (setf sorted-pairs (sort sorted-pairs #'< :key #'car))
        (let ((cumulative 0.0d0)
              (p95-threshold (* 0.95d0 total-weight))
              (p95 0.0d0))
          (loop for pair in sorted-pairs do
            (incf cumulative (cdr pair))
            (when (>= cumulative p95-threshold)
              (setf p95 (car pair))
              (return)))
          (values mean std p95 outlier-count outlier-weight))))))

(defun residual-stats->alist (residuals x-value-vector &key (outlier-sigma 3.0d0))
  "Convert residual statistics to alist format for JSON export.

Returns alist with:
  :mean - weighted average residual
  :std - weighted standard deviation
  :p95 - 95th percentile residual
  :outlier_count - number of poorly-fitted observations
  :outlier_rate - fraction of weight in outliers
  :outlier_threshold - residual value defining outliers"
  (multiple-value-bind (mean std p95 outlier-count outlier-weight)
      (compute-residual-stats residuals x-value-vector :outlier-sigma outlier-sigma)
    (let ((total-weight (loop for i from 0 below (length x-value-vector)
                              sum (aref x-value-vector i))))
      (list (cons :mean (round-to mean))
            (cons :std (round-to std))
            (cons :p95 (round-to p95))
            (cons :outlier_count outlier-count)
            (cons :outlier_rate (if (> total-weight 0.0d0)
                                    (round-to (/ outlier-weight total-weight))
                                    0.0d0))
            (cons :outlier_threshold (round-to (+ mean (* outlier-sigma std))))))))

(defun find-high-residual-observations (residuals x-indices-matrix
                                        &key (threshold nil)
                                             (top-n 20))
  "Find observations with highest reconstruction residuals.

If THRESHOLD is provided, returns observations with residual >= threshold.
Otherwise, returns the TOP-N observations with highest residuals."
  (declare (type (simple-array double-float) residuals)
           (type (simple-array fixnum) x-indices-matrix))
  (let* ((nnz (array-dimension x-indices-matrix 0))
         (num-modes (array-dimension x-indices-matrix 1))
         (pairs (loop for obs from 0 below nnz
                      collect (cons obs (aref residuals obs)))))
    ;; Sort by residual descending
    (setf pairs (sort pairs #'> :key #'cdr))
    ;; Filter or limit
    (let ((filtered (if threshold
                        (remove-if-not (lambda (p) (>= (cdr p) threshold)) pairs)
                        (subseq pairs 0 (min top-n (length pairs))))))
      ;; Convert to alist format
      (mapcar (lambda (p)
                (let ((obs (car p)))
                  (list (cons :observation obs)
                        (cons :residual (round-to (cdr p)))
                        (cons :indices
                              (loop for m from 0 below num-modes
                                    collect (aref x-indices-matrix obs m))))))
              filtered))))
