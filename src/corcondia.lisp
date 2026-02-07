(in-package :cl-tensor-decomposition)

;;; ============================================================
;;; CORCONDIA (Core Consistency Diagnostic) Implementation
;;; ============================================================
;;;
;;; Reference:
;;;   Bro, R., & Kiers, H. A. (2003). A new efficient method for
;;;   determining the number of components in PARAFAC models.
;;;   Journal of Chemometrics, 17(5), 274-286.
;;;
;;; CORCONDIA measures how well a CP decomposition fits the ideal
;;; superdiagonal structure. A score close to 100% indicates the
;;; rank is appropriate; lower scores suggest overfitting.
;;;
;;; Algorithm:
;;;   1. Compute pseudo-inverse A⁺ of each factor matrix A
;;;   2. Compute core tensor: G = X ×₁ A₁⁺ ×₂ A₂⁺ ... ×ₙ Aₙ⁺
;;;   3. Compare G with ideal superdiagonal tensor T (T_{rr...r} = 1)
;;;   4. CORCONDIA = 100 × (1 - ||G - T||² / R)

;;; ============================================================
;;; Matrix Operations for Pseudo-Inverse Computation
;;; ============================================================

(defun %matrix-transpose (a)
  "Transpose matrix A (m×n) to produce Aᵀ (n×m).
Returns a new array."
  (declare (type (simple-array double-float (* *)) a))
  (let* ((m (array-dimension a 0))
         (n (array-dimension a 1))
         (at (make-array (list n m) :element-type 'double-float)))
    (declare (type fixnum m n))
    (loop for i fixnum from 0 below m do
      (loop for j fixnum from 0 below n do
        (setf (aref at j i) (aref a i j))))
    at))

(defun %matrix-multiply (a b)
  "Multiply matrices A (m×k) and B (k×n) to produce C (m×n).
Uses naive O(mnk) algorithm suitable for small matrices."
  (declare (type (simple-array double-float (* *)) a b))
  (let* ((m (array-dimension a 0))
         (k (array-dimension a 1))
         (n (array-dimension b 1))
         (c (make-array (list m n) :element-type 'double-float
                                   :initial-element 0.0d0)))
    (declare (type fixnum m k n))
    (assert (= k (array-dimension b 0)) ()
            "Matrix dimension mismatch: A is ~Dx~D, B is ~Dx~D"
            m k (array-dimension b 0) n)
    (loop for i fixnum from 0 below m do
      (loop for j fixnum from 0 below n do
        (let ((sum 0.0d0))
          (declare (type double-float sum))
          (loop for p fixnum from 0 below k do
            (incf sum (* (aref a i p) (aref b p j))))
          (setf (aref c i j) sum))))
    c))

(defun %matrix-inverse (a &key (epsilon 1.0d-12))
  "Compute inverse of square matrix A using Gauss-Jordan elimination.
Returns NIL if matrix is singular (determinant near zero).
EPSILON controls singularity detection threshold."
  (declare (type (simple-array double-float (* *)) a))
  (let* ((n (array-dimension a 0))
         ;; Work on a copy augmented with identity matrix
         (aug (make-array (list n (* 2 n)) :element-type 'double-float
                                           :initial-element 0.0d0)))
    (declare (type fixnum n))
    (assert (= n (array-dimension a 1)) ()
            "Matrix must be square, got ~Dx~D" n (array-dimension a 1))
    ;; Initialize augmented matrix [A | I]
    (loop for i fixnum from 0 below n do
      (loop for j fixnum from 0 below n do
        (setf (aref aug i j) (aref a i j)))
      (setf (aref aug i (+ i n)) 1.0d0))
    ;; Forward elimination with partial pivoting
    (loop for col fixnum from 0 below n do
      ;; Find pivot
      (let ((max-val (abs (aref aug col col)))
            (max-row col))
        (declare (type double-float max-val)
                 (type fixnum max-row))
        (loop for row fixnum from (1+ col) below n do
          (when (> (abs (aref aug row col)) max-val)
            (setf max-val (abs (aref aug row col))
                  max-row row)))
        ;; Check for singularity
        (when (< max-val epsilon)
          (return-from %matrix-inverse nil))
        ;; Swap rows if needed
        (when (/= max-row col)
          (loop for j fixnum from 0 below (* 2 n) do
            (rotatef (aref aug col j) (aref aug max-row j))))
        ;; Scale pivot row
        (let ((pivot (aref aug col col)))
          (loop for j fixnum from 0 below (* 2 n) do
            (setf (aref aug col j) (/ (aref aug col j) pivot))))
        ;; Eliminate column
        (loop for row fixnum from 0 below n do
          (unless (= row col)
            (let ((factor (aref aug row col)))
              (loop for j fixnum from 0 below (* 2 n) do
                (decf (aref aug row j) (* factor (aref aug col j)))))))))
    ;; Extract inverse from right half
    (let ((inv (make-array (list n n) :element-type 'double-float)))
      (loop for i fixnum from 0 below n do
        (loop for j fixnum from 0 below n do
          (setf (aref inv i j) (aref aug i (+ j n)))))
      inv)))

(defun %pseudo-inverse (a &key (epsilon 1.0d-12))
  "Compute Moore-Penrose pseudo-inverse of matrix A (m×n).

For m >= n (tall matrix): A⁺ = (AᵀA)⁻¹Aᵀ
For m < n (wide matrix): A⁺ = Aᵀ(AAᵀ)⁻¹

Returns NIL if the matrix is rank-deficient.
EPSILON controls singularity detection."
  (declare (type (simple-array double-float (* *)) a))
  (let ((m (array-dimension a 0))
        (n (array-dimension a 1)))
    (declare (type fixnum m n))
    (let ((at (%matrix-transpose a)))
      (if (>= m n)
          ;; Tall matrix: A⁺ = (AᵀA)⁻¹Aᵀ
          (let* ((ata (%matrix-multiply at a))
                 (ata-inv (%matrix-inverse ata :epsilon epsilon)))
            (when ata-inv
              (%matrix-multiply ata-inv at)))
          ;; Wide matrix: A⁺ = Aᵀ(AAᵀ)⁻¹
          (let* ((aat (%matrix-multiply a at))
                 (aat-inv (%matrix-inverse aat :epsilon epsilon)))
            (when aat-inv
              (%matrix-multiply at aat-inv)))))))

;;; ============================================================
;;; Core Tensor Computation for Sparse Tensors
;;; ============================================================


(defun %compute-core-tensor-sparse (x-indices-matrix x-value-vector
                                    pseudo-inverses rank)
  "Compute the core tensor G from sparse tensor data and pseudo-inverses.

For each element G[p,q,r,...] of the R×R×...×R core tensor:
  G[p,q,r,...] = Σᵢ x[i] × A₁⁺[p,i₁] × A₂⁺[q,i₂] × A₃⁺[r,i₃] × ...

where:
  - x[i] is the i-th non-zero value
  - (i₁,i₂,i₃,...) are the coordinates of the i-th non-zero entry
  - Aₙ⁺ is the pseudo-inverse of the n-th factor matrix (R × Iₙ)

Returns a flat vector representing the dense core tensor in row-major order."
  (declare (type (simple-array fixnum (* *)) x-indices-matrix)
           (type (simple-array double-float (*)) x-value-vector)
           (type simple-vector pseudo-inverses)
           (type fixnum rank))
  (let* ((n-modes (length pseudo-inverses))
         (nnz (array-dimension x-indices-matrix 0))
         ;; Core tensor size is R^n-modes
         (core-size (expt rank n-modes))
         (core (make-array core-size :element-type 'double-float
                                     :initial-element 0.0d0)))
    (declare (type fixnum n-modes nnz core-size))
    ;; For each non-zero entry in the sparse tensor
    (loop for obs fixnum from 0 below nnz do
      (let ((x-val (aref x-value-vector obs)))
        (declare (type double-float x-val))
        ;; For each element in the core tensor
        (loop for core-idx fixnum from 0 below core-size do
          ;; Decode core-idx to (p, q, r, ...) indices
          (let ((contribution x-val)
                (idx core-idx))
            (declare (type double-float contribution)
                     (type fixnum idx))
            ;; Multiply by pseudo-inverse elements for each mode
            ;; A⁺ is (R × I), so access is A⁺[core-mode-idx, tensor-mode-idx]
            (loop for mode fixnum from (1- n-modes) downto 0 do
              (let* ((pinv (aref pseudo-inverses mode))
                     (core-mode-idx (mod idx rank))
                     (tensor-mode-idx (aref x-indices-matrix obs mode)))
                (declare (type fixnum core-mode-idx tensor-mode-idx))
                (setf contribution (* contribution
                                      (aref pinv core-mode-idx tensor-mode-idx)))
                (setf idx (floor idx rank))))
            (incf (aref core core-idx) contribution)))))
    core))


(defun %compute-corcondia-from-core (core rank n-modes)
  "Compute CORCONDIA score from the core tensor.

The ideal superdiagonal tensor T has T[r,r,r,...] = 1 for r = 0..R-1
and all other elements = 0.

CORCONDIA = 100 × (1 - Σ(G[i] - T[i])² / R)

where the sum is over all core tensor elements."
  (declare (type (simple-array double-float (*)) core)
           (type fixnum rank n-modes))
  (let ((sum-sq-diff 0.0d0)
        (core-size (length core)))
    (declare (type double-float sum-sq-diff)
             (type fixnum core-size))
    (loop for core-idx fixnum from 0 below core-size do
      ;; Check if this is a superdiagonal element (all mode indices equal)
      (let ((is-superdiag t)
            (idx core-idx)
            (first-mode-idx nil))
        (declare (type (or null fixnum) first-mode-idx))
        ;; Decode and check if all indices are equal
        (loop for mode fixnum from (1- n-modes) downto 0 do
          (let ((mode-idx (mod idx rank)))
            (if (null first-mode-idx)
                (setf first-mode-idx mode-idx)
                (unless (= mode-idx first-mode-idx)
                  (setf is-superdiag nil)))
            (setf idx (floor idx rank))))
        ;; Compute squared difference from ideal
        (let* ((ideal (if is-superdiag 1.0d0 0.0d0))
               (diff (- (aref core core-idx) ideal)))
          (incf sum-sq-diff (* diff diff)))))
    ;; CORCONDIA formula: 100 * (1 - sum_sq_diff / R)
    (let ((score (* 100.0d0 (- 1.0d0 (/ sum-sq-diff (coerce rank 'double-float))))))
      ;; Clip to [0, 100] range
      ;; Negative values can occur with very poor fits
      (max 0.0d0 (min 100.0d0 score)))))

;;; ============================================================
;;; Main CORCONDIA Function
;;; ============================================================

(defun corcondia (tensor factor-matrix-vector
                  &key (epsilon 1.0d-12) verbose)
  "Compute CORCONDIA (Core Consistency Diagnostic) for a CP decomposition.

TENSOR is a sparse-tensor structure containing the original data.
FACTOR-MATRIX-VECTOR is a vector of factor matrices from CP decomposition,
  where each matrix has dimensions (mode-size × rank).

Returns a CORCONDIA score from 0 to 100:
  - ~100: Appropriate rank (good superdiagonal structure)
  - 50-90: Possibly overfitting, consider smaller rank
  - <50: Likely overfitting, rank is too high

Optional parameters:
  EPSILON - Threshold for singularity detection in pseudo-inverse (default 1e-12)
  VERBOSE - When T, print diagnostic information including core tensor structure

Reference:
  Bro, R., & Kiers, H. A. (2003). A new efficient method for determining
  the number of components in PARAFAC models. J. Chemometrics, 17, 274-286.

Example:
  (let* ((tensor (make-sparse-tensor shape indices values))
         (factors (decomposition tensor :r 3)))
    (corcondia tensor factors))
  => 85.3  ; Good fit at rank 3"
  (declare (type sparse-tensor tensor)
           (type simple-vector factor-matrix-vector))
  (let* ((n-modes (length factor-matrix-vector))
         (rank (array-dimension (aref factor-matrix-vector 0) 1))
         (x-indices (sparse-tensor-indices tensor))
         (x-values (sparse-tensor-values tensor))
         ;; Compute pseudo-inverse of each factor matrix
         (pseudo-inverses (make-array n-modes)))
    (declare (type fixnum n-modes rank))
    ;; Validate inputs
    (assert (= n-modes (sparse-tensor-n-modes tensor)) ()
            "Number of factor matrices (~D) must match tensor modes (~D)"
            n-modes (sparse-tensor-n-modes tensor))
    ;; Compute pseudo-inverses
    (when verbose
      (format t "~&Computing pseudo-inverses for ~D factor matrices...~%" n-modes))
    (loop for mode from 0 below n-modes do
      (let* ((fm (aref factor-matrix-vector mode))
             (pinv (%pseudo-inverse fm :epsilon epsilon)))
        (unless pinv
          (warn "Factor matrix ~D is rank-deficient; CORCONDIA may be unreliable" mode)
          ;; Use a regularized pseudo-inverse or return early
          (return-from corcondia 0.0d0))
        (setf (aref pseudo-inverses mode) pinv)
        (when verbose
          (format t "  Mode ~D: ~Dx~D -> pseudo-inverse ~Dx~D~%"
                  mode
                  (array-dimension fm 0) (array-dimension fm 1)
                  (array-dimension pinv 0) (array-dimension pinv 1)))))
    ;; Compute core tensor
    (when verbose
      (format t "Computing ~D-way core tensor of size ~D^~D = ~D elements...~%"
              n-modes rank n-modes (expt rank n-modes)))
    (let ((core (%compute-core-tensor-sparse x-indices x-values
                                              pseudo-inverses rank)))
      ;; Optionally print core tensor structure
      (when verbose
        (format t "~&Core tensor structure (superdiagonal elements highlighted):~%")
        (let ((core-size (length core)))
          (loop for core-idx from 0 below (min core-size 50) do
            ;; Decode indices for display
            (let ((indices nil)
                  (idx core-idx))
              (loop for mode from (1- n-modes) downto 0 do
                (push (mod idx rank) indices)
                (setf idx (floor idx rank)))
              (let ((is-superdiag (apply #'= indices)))
                (format t "  G~A = ~,4F~A~%"
                        indices
                        (aref core core-idx)
                        (if is-superdiag " *" "")))))
          (when (> core-size 50)
            (format t "  ... (~D more elements)~%" (- core-size 50)))))
      ;; Compute and return CORCONDIA score
      (let ((score (%compute-corcondia-from-core core rank n-modes)))
        (when verbose
          (format t "~&CORCONDIA = ~,2F%~%" score)
          (cond
            ((>= score 90)
             (format t "Interpretation: Excellent fit, rank ~D is appropriate.~%" rank))
            ((>= score 70)
             (format t "Interpretation: Good fit, rank ~D is reasonable.~%" rank))
            ((>= score 50)
             (format t "Interpretation: Marginal fit, consider smaller rank.~%"))
            (t
             (format t "Interpretation: Poor fit, rank ~D is likely too high.~%" rank))))
        score))))
