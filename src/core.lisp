;;; -*- coding:utf-8; mode:lisp -*-

(defpackage cl-tensor-decomposition
  (:use :cl)
  (:nicknames :cltd)
  (:export :initialize-matrix
           :initialize-random-matrix
           :sparse-kl-divergence
           :sdot
           :decomposition
           :ranking
           :decomposition-inner
           ;; model selection via Poisson count thinning
           :make-poisson-folds
           :poisson-folds
           :poisson-folds-p
           :poisson-folds-k
           :poisson-folds-count
           :poisson-folds-tensor
           :poisson-folds-prediction-scale
           :poisson-fold-tensors
           :normalized-generalized-kl
           :cross-validate-rank
           :select-rank
           :select-rank-1se
           :select-rank-elbow
           ;; sparse-tensor structure and accessors
           :sparse-tensor
           :make-sparse-tensor
           :sparse-tensor-shape
           :sparse-tensor-indices
           :sparse-tensor-values
           :sparse-tensor-domains
           :sparse-tensor-aux
           :sparse-tensor-nnz
           :sparse-tensor-n-modes
           :sparse-tensor-mode-labels
           :sparse-tensor-mode-name
           :sparse-tensor-total-count
           ;; mode-spec structure and constructor
           :mode-spec
           :make-mode-metadata
           :mode-spec-name
           :mode-spec-labels
           :mode-spec-discretization
           :mode-spec-missing-labels
           :mode-spec-role
           :mode-spec-positive-label
           :mode-spec-negative-label
           ;; reporting
           :generate-factor-cards
           :write-factor-cards-json
           :write-scenario-report
           :generate-report-artifacts
           :factor-report-markdown-string
           ;; conditions
           :tensor-decomposition-error
           :invalid-input-error
           :invalid-input-reason
           :invalid-input-details
           :numerical-instability-error
           :instability-location
           :instability-value
           :instability-operation
           ;; validation
           :validate-input-data
           ;; portable helpers for testing
           :%float-nan-p
           :%float-infinity-p
           :%seed-random-state
           :+double-float-positive-infinity+
           ;; diagnostics - factor similarity
           :compute-factor-similarity-matrix
           :extract-similar-factor-pairs
           :similarity-matrix->alist
           :compute-factor-redundancy-score
           ;; diagnostics - factor kl contribution
           :compute-factor-kl-contributions
           :normalize-contributions
           :kl-contributions->alist
           :rank-factors-by-contribution
           ;; diagnostics - observation responsibilities
           :compute-observation-responsibilities
           :compute-responsibility-stats
           :responsibility-stats->alist
           :find-ambiguous-observations
           ;; diagnostics - factor exclusivity
           :compute-factor-exclusivity
           :factor-exclusivity->alist
           ;; diagnostics - per-observation residuals
           :compute-observation-residuals
           :compute-residual-stats
           :residual-stats->alist
           :find-high-residual-observations))

(in-package :cl-tensor-decomposition)

;;; ============================================================
;;; Condition Types
;;; ============================================================

(define-condition tensor-decomposition-error (error)
  ()
  (:documentation "Base condition for all tensor decomposition errors."))

(define-condition invalid-input-error (tensor-decomposition-error)
  ((reason :initarg :reason
           :reader invalid-input-reason
           :type keyword
           :documentation "Category of validation failure (e.g., :shape-mismatch, :nan-value)")
   (details :initarg :details
            :reader invalid-input-details
            :initform nil
            :documentation "Additional context about the validation failure"))
  (:report (lambda (condition stream)
             (format stream "Invalid input data: ~A~@[ — ~A~]"
                     (invalid-input-reason condition)
                     (invalid-input-details condition))))
  (:documentation "Signaled when input data fails validation checks."))

(define-condition numerical-instability-error (tensor-decomposition-error)
  ((location :initarg :location
             :reader instability-location
             :documentation "Where the instability was detected (e.g., matrix index or function name)")
   (value :initarg :value
          :reader instability-value
          :documentation "The problematic value (NaN, Inf, or negative)")
   (operation :initarg :operation
              :reader instability-operation
              :initform nil
              :documentation "The operation that produced the unstable value"))
  (:report (lambda (condition stream)
             (format stream "Numerical instability detected at ~A: value=~A~@[ during ~A~]"
                     (instability-location condition)
                     (instability-value condition)
                     (instability-operation condition))))
  (:documentation "Signaled when NaN, Inf, or unexpected negative values are encountered."))

;;; ============================================================
;;; Input Validation
;;; ============================================================

(defun %float-nan-p (x)
  "Check if X is NaN (Not a Number). Portable across implementations."
  (and (floatp x)
       #+sbcl (sb-ext:float-nan-p x)
       #+ccl (ccl::nan-or-infinity-p x)
       #+ecl (ext:float-nan-p x)
       #+clisp (ext:float-nan-p x)
       #+allegro (excl:nan-p x)
       #+lispworks (sys:nan-p x)
       #-(or sbcl ccl ecl clisp allegro lispworks)
       (/= x x)))

(defun %float-infinity-p (x)
  "Check if X is positive or negative infinity. Portable across implementations."
  (and (floatp x)
       (not (%float-nan-p x))
       #+sbcl (sb-ext:float-infinity-p x)
       #+ccl (and (ccl::nan-or-infinity-p x) (not (ccl::nan-p x)))
       #+ecl (ext:float-infinity-p x)
       #+clisp (ext:float-infinity-p x)
       #+allegro (excl:infinityp x)
       #+lispworks (or (sys:infinity-p x) (sys:minus-infinity-p x))
       #-(or sbcl ccl ecl clisp allegro lispworks)
       (or (> x most-positive-double-float)
           (< x most-negative-double-float))))

(defun %seed-random-state (seed)
  "Create a random state from an integer SEED. Portable across implementations.
This allows reproducible random number generation for testing."
  #+sbcl (sb-ext:seed-random-state seed)
  #+ccl (ccl::initialize-random-state seed (make-random-state))
  #+ecl (make-random-state seed)
  #+clisp (make-random-state seed)
  #+allegro (make-random-state seed)
  #+lispworks (make-random-state seed)
  #-(or sbcl ccl ecl clisp allegro lispworks)
  (let ((state (make-random-state t)))
    ;; Fallback: consume some random values based on seed to get different states
    (dotimes (i (mod seed 1000))
      (random 1.0d0 state))
    state))

(defparameter +double-float-positive-infinity+
  #+sbcl sb-ext:double-float-positive-infinity
  #+ccl 1d308
  #+ecl ext:double-float-positive-infinity
  #+clisp ext:double-float-positive-infinity
  #+allegro excl:*infinity-double*
  #+lispworks 1d308
  #-(or sbcl ccl ecl clisp allegro lispworks) most-positive-double-float
  "Positive infinity as a double-float. Portable across implementations.")

(defun validate-input-data (x-shape x-indices-matrix x-value-vector
                            &key (error-on-invalid t))
  "Validate input data for tensor decomposition.

X-SHAPE          — list of tensor dimensions per mode (must be positive integers).
X-INDICES-MATRIX — fixnum matrix of non-zero coordinates.
X-VALUE-VECTOR   — double-float vector of observed counts.
ERROR-ON-INVALID — if T (default), signal invalid-input-error on failure;
                   if NIL, return (values nil reason details) instead.

Returns T if validation passes. When ERROR-ON-INVALID is NIL and validation fails,
returns (values NIL reason details)."
  (flet ((fail (reason details)
           (if error-on-invalid
               (error 'invalid-input-error :reason reason :details details)
               (return-from validate-input-data (values nil reason details)))))
    ;; 1. Check x-shape is a non-empty list of positive integers
    (unless (and (listp x-shape) (plusp (length x-shape)))
      (fail :invalid-shape "x-shape must be a non-empty list"))
    (loop for dim in x-shape
          for mode from 0
          unless (and (integerp dim) (plusp dim))
            do (fail :invalid-shape
                     (format nil "x-shape[~D]=~S must be a positive integer" mode dim)))
    ;; 2. Check x-indices-matrix dimensions
    (unless (and (arrayp x-indices-matrix)
                 (= 2 (array-rank x-indices-matrix)))
      (fail :invalid-indices-matrix "x-indices-matrix must be a 2D array"))
    (let ((nnz (array-dimension x-indices-matrix 0))
          (n-modes (array-dimension x-indices-matrix 1)))
      (unless (= n-modes (length x-shape))
        (fail :mode-count-mismatch
              (format nil "x-indices-matrix has ~D columns but x-shape has ~D modes"
                      n-modes (length x-shape))))
      ;; 3. Check x-value-vector length matches
      (unless (and (vectorp x-value-vector)
                   (= (length x-value-vector) nnz))
        (fail :value-vector-length-mismatch
              (format nil "x-value-vector length ~D does not match nnz ~D"
                      (length x-value-vector) nnz)))
      ;; 4. Check indices are within bounds and values are valid
      (loop for datum-index from 0 below nnz
            do
               ;; Check each index is in range
               (loop for mode from 0 below n-modes
                     for idx = (aref x-indices-matrix datum-index mode)
                     for dim = (nth mode x-shape)
                     unless (and (integerp idx) (<= 0 idx) (< idx dim))
                       do (fail :index-out-of-bounds
                                (format nil "index[~D,~D]=~S out of bounds [0,~D)"
                                        datum-index mode idx dim)))
               ;; Check value is valid (non-negative, not NaN/Inf)
               (let ((val (aref x-value-vector datum-index)))
                 (cond
                   ((%float-nan-p val)
                    (fail :nan-value
                          (format nil "x-value-vector[~D] is NaN" datum-index)))
                   ((%float-infinity-p val)
                    (fail :infinite-value
                          (format nil "x-value-vector[~D] is infinite" datum-index)))
                   ((< val 0.0d0)
                    (fail :negative-value
                          (format nil "x-value-vector[~D]=~F is negative"
                                  datum-index val)))))))
    t))
(defparameter *epsilon* 0.000001d0)

(defun initialize-matrix (matrix default-value)
  "Fill MATRIX with DEFAULT-VALUE and return the mutated matrix."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float) matrix)
           (type double-float default-value))
  (loop for i fixnum from 0 below (array-dimension matrix 0) do
    (loop for j fixnum from 0 below (array-dimension matrix 1) do
      (setf (aref matrix i j) default-value)))
  matrix)

(defun initialize-random-matrix (matrix)
  "Fill MATRIX with uniform random double-float values in [0, 1)."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float) matrix))
  (loop for i fixnum from 0 below (array-dimension matrix 0) do
    (loop for j fixnum from 0 below (array-dimension matrix 1) do
      (setf (aref matrix i j) (random 1.0d0))))
  matrix)

(defun %cp-component-masses (factor-matrix-vector)
  "Return the predicted mass contributed by each rank-one component of the CP model.

For component r the mass is the product of that column's sum over every mode:

  m_r = prod_m (sum_j A^(m)_{j,r})

Because the CP model factorizes, summing m_r over all components yields the
reconstruction mass of the *entire* tensor, implicit zeros included, without
enumerating any coordinate.

FACTOR-MATRIX-VECTOR - Vector of factor matrices (one per mode)

Returns a double-float vector of length R. Runs in O(R * sum_m I_m) time and
O(R) space; no dense tensor is materialized."
  (declare (optimize (speed 3) (safety 0))
           (type simple-array factor-matrix-vector))
  (let* ((rank (array-dimension (svref factor-matrix-vector 0) 1))
         (masses (make-array rank :element-type 'double-float
                             :initial-element 1.0d0)))
    (declare (type fixnum rank)
             (type (simple-array double-float (*)) masses))
    (loop for mode fixnum from 0 below (length factor-matrix-vector)
          do (let ((factor-matrix (svref factor-matrix-vector mode)))
               (declare (type (simple-array double-float) factor-matrix))
               (loop for ri fixnum from 0 below rank
                     do (setf (aref masses ri)
                              (* (aref masses ri)
                                 (loop for i fixnum
                                         from 0 below (array-dimension factor-matrix 0)
                                       sum (aref factor-matrix i ri)
                                       double-float))))))
    masses))

(defun %cp-total-mass (factor-matrix-vector)
  "Return sum_i x^_i over *every* coordinate of the CP reconstruction.

This is the total predicted mass of the model, aggregated from the factor
matrices via %CP-COMPONENT-MASSES rather than by expanding the tensor, so it
costs O(R * sum_m I_m) and never allocates a dense array."
  (declare (optimize (speed 3) (safety 0))
           (type simple-array factor-matrix-vector))
  (let ((masses (%cp-component-masses factor-matrix-vector)))
    (declare (type (simple-array double-float (*)) masses))
    (loop for ri fixnum from 0 below (length masses)
          sum (aref masses ri)
          double-float)))

(defun %sparse-kl-local-term (X-indices-matrix X-value-vector X^-value-vector
                              prediction-scale)
  "Accumulate the stored-non-zero part of the generalized KL divergence.

Returns sum over the rows of X-INDICES-MATRIX of

  x_i * log(x_i / (s * x^_i + *epsilon*)) - x_i

where s is PREDICTION-SCALE. Entries whose observed value is zero contribute
nothing here: for them the limit x*log(x/y) as x->0+ is 0, and their remaining
-x + x^ part belongs to the total predicted mass, so adding it here would
double-count it.

The +x^ term is likewise omitted for every entry, since the total predicted
mass already covers all coordinates including the stored ones.

*epsilon* is added after scaling, so the stabilizer keeps a fixed magnitude
regardless of s."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) X-indices-matrix)
           (type (simple-array double-float) X-value-vector X^-value-vector)
           (type double-float prediction-scale))
  (loop for datum-index fixnum from 0 below (array-dimension X-indices-matrix 0)
        sum (let ((x (aref X-value-vector datum-index))
                  (x^ (* prediction-scale (aref X^-value-vector datum-index))))
              (declare (type double-float x x^))
              (if (> x 0.0d0)
                  (- (* x (the double-float
                              (log (/ x (+ x^ (the double-float *epsilon*))))))
                     x)
                  0.0d0))
        double-float))

(defun sparse-kl-divergence (X-indices-matrix X-value-vector X^-value-vector
                             factor-matrix-vector &key (prediction-scale 1.0d0))
  "Compute the generalized (Poisson) KL divergence between a sparse tensor and
its CP reconstruction:

  D(X||X^) = sum_i [ x_i * log(x_i / x^_i) - x_i + x^_i ]

Coordinates absent from X-INDICES-MATRIX are *observed zeros*, not missing
values, so their reconstruction still contributes to the loss. The sum
therefore splits into a term over the stored non-zeros and the total predicted
mass of the model:

  D(X||X^) = sum_{i in nnz} [ x_i * log(x_i / x^_i) - x_i ] + sum_i x^_i

The trailing term is aggregated from the CP structure by %CP-TOTAL-MASS in
O(R * sum_m I_m) time, so the whole computation stays sparse.

X-INDICES-MATRIX     - Sparse tensor indices, shape (nnz, n-modes)
X-VALUE-VECTOR       - Observed counts at each stored index
X^-VALUE-VECTOR      - Reconstruction at the stored indices, as produced by SDOT
FACTOR-MATRIX-VECTOR - The factor matrices that produced X^-VALUE-VECTOR
PREDICTION-SCALE     - Multiplier s applied to every prediction; defaults to 1

X^-VALUE-VECTOR and FACTOR-MATRIX-VECTOR must describe the same model state;
callers are responsible for calling SDOT before this function whenever the
factors have changed.

PREDICTION-SCALE exists for exposure correction: a model fitted on a thinned
tensor predicts at the training exposure, and scoring it against a validation
sample taken at a different exposure needs the ratio applied to the *whole*
model. Both the per-entry predictions and the total predicted mass are scaled,
so the identity D(X||s*X^) is preserved; the factor matrices are never mutated.

Numerical stabilization: the logarithm divides by x^ + *epsilon* so that an
underflowed reconstruction cannot produce -infinity or NaN. This biases the
result by O(*epsilon*) per non-zero entry. *epsilon* is deliberately NOT added
to the total predicted mass, which needs no such guard and where the bias would
otherwise scale with the number of coordinates in the tensor."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) X-indices-matrix)
           (type (simple-array double-float) X-value-vector X^-value-vector)
           (type simple-array factor-matrix-vector))
  (let ((scale (coerce prediction-scale 'double-float)))
    (+ (%sparse-kl-local-term X-indices-matrix X-value-vector X^-value-vector scale)
       (* scale (%cp-total-mass factor-matrix-vector)))))

(defun calc-denominator (factor-matrix-vector factor-index denominator-tmp)
  "Compute the normalization denominator for multiplicative update.

For each latent factor r, accumulates the product of column sums from all
factor matrices except the one at FACTOR-INDEX. This is used to normalize
the update step in the MU algorithm.

FACTOR-MATRIX-VECTOR - Vector of factor matrices (one per mode)
FACTOR-INDEX         - Index of the mode being updated (excluded from product)
DENOMINATOR-TMP      - Output array to store denominator values, shape (n-modes, rank)"
  (declare (optimize (speed 3) (safety 0))
           (type simple-array factor-matrix-vector)
           (type (simple-array double-float) denominator-tmp)
           (type fixnum factor-index))
  (loop for other-factor-index fixnum from 0 below (length factor-matrix-vector)
        if (not (= factor-index other-factor-index))
        do (let ((factor-matrix (svref factor-matrix-vector other-factor-index)))
             (declare (type (simple-array double-float) factor-matrix))
             (loop for ri fixnum from 0 below (array-dimension factor-matrix 1)
                   do (setf (aref denominator-tmp factor-index ri)
                            (* (aref denominator-tmp factor-index ri)
                               (loop for i fixnum from 0 below (array-dimension factor-matrix 0)
                                     sum (aref factor-matrix i ri) double-float)))))))

(defun calc-numerator (x-indices-matrix x-value-vector x^-value-vector
                        factor-matrix-vector factor-index numerator-tmp)
  "Compute the numerator for multiplicative update of a factor matrix.

For each observation and latent factor r, computes the weighted contribution
based on the ratio x/x^ and the product of factor values from other modes.
This implements the numerator term of the MU update rule for KL divergence
minimization.

X-INDICES-MATRIX     - Sparse tensor indices, shape (nnz, n-modes)
X-VALUE-VECTOR       - Observed counts at each index
X^-VALUE-VECTOR      - Reconstructed values at each index
FACTOR-MATRIX-VECTOR - Vector of factor matrices
FACTOR-INDEX         - Index of the mode being updated
NUMERATOR-TMP        - Output vector of arrays to accumulate numerator values"
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) x-indices-matrix)
           (type (simple-array double-float) x-value-vector x^-value-vector)
           (type simple-array factor-matrix-vector)
           (type fixnum factor-index))
  (loop for datum-index fixnum from 0 below (array-dimension x-indices-matrix 0)
        do (let ((x/x^ (/ (aref x-value-vector datum-index)
                          (+ (aref x^-value-vector datum-index)
                             (the double-float *epsilon*)))))
             (declare (type double-float x/x^))
             (let ((numerator-tmp-elem (svref numerator-tmp factor-index)))
               (declare (type (simple-array double-float) numerator-tmp-elem))
               (loop for ri fixnum from 0 below (array-dimension numerator-tmp-elem 1)
                     do (let ((factor-prod 1.0d0))
                          (declare (type double-float factor-prod))
                          (loop for other-factor-index fixnum from 0 below (length factor-matrix-vector)
                                if (not (= factor-index other-factor-index))
                                do (let ((factor-matrix (svref factor-matrix-vector other-factor-index)))
                                     (declare (type (simple-array double-float) factor-matrix))
                                     (setf factor-prod
                                           (* factor-prod
                                              (aref factor-matrix
                                                    (aref x-indices-matrix datum-index other-factor-index)
                                                    ri)))))
                          (incf (aref numerator-tmp-elem
                                      (aref x-indices-matrix datum-index factor-index)
                                      ri)
                                (* x/x^ factor-prod))))))))

(defun update (x-indices-matrix x-value-vector x^-value-vector
               factor-matrix-vector factor-index numerator-tmp denominator-tmp
               &key (kappa 0.0d0) (kappa-tolerance 1.0d-10) allow-scooch)
  "Perform one multiplicative update step for a single factor matrix.

Updates the factor matrix at FACTOR-INDEX in place using the multiplicative
update rule: A_new = A_old * (numerator / denominator). This is the core step of
the MU algorithm for non-negative tensor decomposition with KL divergence
objective.

Returns the largest KKT violation seen on this mode, measured at the point the
update starts from. For a non-negativity constrained minimum every entry must
satisfy A >= 0, gradient >= 0 and A * gradient = 0, so |min(A, gradient)| is 0
exactly at a stationary point. The gradient of the generalized KL objective with
respect to A(i,r) is denominator(r) - numerator(i,r): the first term is how much
extra predicted mass the entry buys, the second how much observed count it
explains.

Inadmissible zeros: a plain multiplicative update can never revive an entry that
has reached zero, because the step is a product. When such an entry has a
negative gradient it wants to grow, and the fit converges to a point that is not
KKT. Following Chi and Kolda (arXiv:1112.2414, Algorithm 3) the entry is nudged
to KAPPA before the step. ALLOW-SCOOCH gates this to iterations after the first,
matching their k > 1 condition.

X-INDICES-MATRIX     - Sparse tensor indices
X-VALUE-VECTOR       - Observed counts
X^-VALUE-VECTOR      - Current reconstructed values
FACTOR-MATRIX-VECTOR - Vector of factor matrices (modified in place)
FACTOR-INDEX         - Index of the mode to update
NUMERATOR-TMP        - Temporary storage for numerator computation
DENOMINATOR-TMP      - Temporary storage for denominator computation
KAPPA                - Value a pinned zero is lifted to; 0 disables the fix
KAPPA-TOLERANCE      - Below this an entry counts as pinned at zero
ALLOW-SCOOCH         - When NIL the inadmissible-zero fix is skipped

The step uses the numerator computed before the nudge, so a lifted entry
overshoots slightly on the iteration that revives it and settles on the next."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) x-indices-matrix)
           (type (simple-array double-float) x-value-vector x^-value-vector denominator-tmp)
           (type fixnum factor-index)
           (type double-float kappa kappa-tolerance))
  (initialize-matrix (svref numerator-tmp factor-index) 0.0d0)
  (initialize-matrix denominator-tmp 1.0d0)
  (calc-denominator factor-matrix-vector factor-index denominator-tmp)
  (calc-numerator x-indices-matrix x-value-vector x^-value-vector
                  factor-matrix-vector factor-index numerator-tmp)
  (let ((factor-matrix (svref factor-matrix-vector factor-index))
        (numerator-tmp-elem (svref numerator-tmp factor-index))
        (residual 0.0d0))
    (declare (type (simple-array double-float) factor-matrix numerator-tmp-elem)
             (type double-float residual))
    (loop for i from 0 below (array-dimension factor-matrix 0)
          do (loop for ri from 0 below (array-dimension factor-matrix 1)
                   do (let* ((value (aref factor-matrix i ri))
                             (numerator (aref numerator-tmp-elem i ri))
                             (denominator (aref denominator-tmp factor-index ri))
                             (gradient (- denominator numerator)))
                        (declare (type double-float value numerator denominator gradient))
                        (setf residual (max residual (abs (min value gradient))))
                        (when (and allow-scooch
                                   (> kappa 0.0d0)
                                   (< value kappa-tolerance)
                                   (< gradient 0.0d0))
                          (setf value kappa))
                        (setf (aref factor-matrix i ri)
                              (* value
                                 (/ numerator
                                    (+ denominator (the double-float *epsilon*))))))))
    residual))

(defun sdot (factor-matrix-vector X-indices-matrix X^-value-vector)
  "Reconstruct sparse observations into X^-VALUE-VECTOR using FACTOR-MATRIX-VECTOR."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) X-indices-matrix)
           (type (simple-array double-float) X^-value-vector))
  (let ((R (array-dimension (svref factor-matrix-vector 0) 1)))
    (declare (type fixnum R))
    (loop for datum-index fixnum from 0 below (array-dimension X-indices-matrix 0) do
      (setf (aref X^-value-vector datum-index)
            (loop for ri fixnum from 0 below R
                  sum (let ((prod 1.0d0))
                        (declare (type double-float prod))
                        (loop for factor-index fixnum from 0 below (length factor-matrix-vector) do
                          (let ((factor-matrix (svref factor-matrix-vector factor-index)))
                            (declare (type (simple-array double-float) factor-matrix))
                            (setf prod
                                  (* prod (aref factor-matrix
                                                (aref X-indices-matrix datum-index factor-index)
                                                ri)))))
                        prod)
                  double-float)))))

(defun %check-loss (kl-value)
  "Signal NUMERICAL-INSTABILITY-ERROR unless KL-VALUE is a finite loss.

The loss is a sum of two aggregates -- the local term over the stored non-zeros
and the total predicted mass -- and either can leave the double range while every
factor entry, every reconstruction and the KKT residual stay finite. Their sum is
then a NaN that nothing else on the path can see, so it needs its own check.

A NaN also poisons every comparison it takes part in, which is how an unusable
fit could otherwise be selected by :N-STARTS and returned as the answer."
  (when (or (%float-nan-p kl-value) (%float-infinity-p kl-value))
    (error 'numerical-instability-error
           :location :kl-divergence
           :value kl-value
           :operation "KL divergence"))
  kl-value)

(defun %kkt-residual (x-indices-matrix x-value-vector x^-value-vector
                      factor-matrix-vector numerator-tmp denominator-tmp)
  "Largest KKT violation of the current model, measured without changing it.

UPDATE reports the residual it sees on entry, which is cheap because it already
has the numerator and denominator, but that value belongs to the point the mode
started from. Once a sweep has updated every mode and normalized the columns,
those per-mode values describe states that no longer exist. This recomputes the
residual against one settled model, which is what a reported or asserted
residual has to mean.

X^-VALUE-VECTOR must already match FACTOR-MATRIX-VECTOR; call SDOT first.
Costs one numerator pass per mode, the same as a sweep's dominant term, so it is
used at the end of a run and to confirm a candidate convergence rather than
every iteration."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) x-indices-matrix)
           (type (simple-array double-float) x-value-vector x^-value-vector denominator-tmp))
  (let ((residual 0.0d0))
    (declare (type double-float residual))
    (loop for mode fixnum from 0 below (length factor-matrix-vector)
          do (initialize-matrix (svref numerator-tmp mode) 0.0d0)
             (initialize-matrix denominator-tmp 1.0d0)
             (calc-denominator factor-matrix-vector mode denominator-tmp)
             (calc-numerator x-indices-matrix x-value-vector x^-value-vector
                             factor-matrix-vector mode numerator-tmp)
             (let ((factor-matrix (svref factor-matrix-vector mode))
                   (numerator-tmp-elem (svref numerator-tmp mode)))
               (declare (type (simple-array double-float)
                              factor-matrix numerator-tmp-elem))
               (loop for i from 0 below (array-dimension factor-matrix 0)
                     do (loop for ri from 0 below (array-dimension factor-matrix 1)
                              do (setf residual
                                       (max residual
                                            (abs (min (aref factor-matrix i ri)
                                                      (- (aref denominator-tmp mode ri)
                                                         (aref numerator-tmp-elem i ri))))))))))
    residual))

(defun %copy-factor-matrices (factor-matrix-vector)
  "Return a fresh deep copy of FACTOR-MATRIX-VECTOR."
  (make-array (length factor-matrix-vector) :initial-contents
              (loop for matrix across factor-matrix-vector
                    collect (let ((copy (make-array (array-dimensions matrix)
                                                    :element-type 'double-float)))
                              (loop for i from 0 below (array-dimension matrix 0)
                                    do (loop for ri from 0 below (array-dimension matrix 1)
                                             do (setf (aref copy i ri) (aref matrix i ri))))
                              copy))))

(defun %normalize-factors (factor-matrix-vector lambda-vector)
  "Scale every mode's columns to unit sum, collecting the scale into LAMBDA-VECTOR.

A CP model is invariant to moving scale between modes, so the factors carry an
arbitrary split of it and can drift far apart numerically. Pulling the scale out
into an explicit weight per component fixes that and makes the weight itself
meaningful: LAMBDA-VECTOR[r] becomes the predicted mass of component r.

A column that has collapsed to all zeros gets weight 0 and is left alone rather
than divided by zero.

Signals NUMERICAL-INSTABILITY-ERROR when a column sum or an accumulated weight
leaves the double range. Both are aggregates, so they can overflow while every
entry feeding them is finite, which is precisely what the per-entry scan in
%CHECK-FACTOR-VALUES cannot catch.

Returns LAMBDA-VECTOR."
  (fill lambda-vector 1.0d0)
  (loop for mode from 0 below (length factor-matrix-vector)
        do (let ((matrix (svref factor-matrix-vector mode)))
             (declare (type (simple-array double-float) matrix))
             (loop for ri from 0 below (array-dimension matrix 1)
                   do (let ((column-sum (loop for i from 0 below (array-dimension matrix 0)
                                              sum (aref matrix i ri)
                                              double-float)))
                        ;; Entries that are each finite can still sum past the
                        ;; double range. With the traps masked that yields an
                        ;; infinity the per-entry scan cannot see, and dividing
                        ;; by it turns the column into zeros whose weight is
                        ;; infinite -- which then reaches the reconstruction and
                        ;; the loss as an infinity or, where an entry normalized
                        ;; to zero, a NaN. Stop at the aggregate instead.
                        (when (or (%float-nan-p column-sum)
                                  (%float-infinity-p column-sum))
                          (error 'numerical-instability-error
                                 :location (list :mode mode :column ri)
                                 :value column-sum
                                 :operation "factor column sum"))
                        (if (> column-sum 0.0d0)
                            (let ((weight (* (aref lambda-vector ri) column-sum)))
                              ;; The weight is a product across modes, so it can
                              ;; overflow even when no single column sum does.
                              (when (or (%float-nan-p weight)
                                        (%float-infinity-p weight))
                                (error 'numerical-instability-error
                                       :location (list :component ri)
                                       :value weight
                                       :operation "component weight"))
                              (setf (aref lambda-vector ri) weight)
                              (loop for i from 0 below (array-dimension matrix 0)
                                    do (setf (aref matrix i ri)
                                             (/ (aref matrix i ri) column-sum))))
                            (setf (aref lambda-vector ri) 0.0d0))))))
  lambda-vector)

(defun %absorb-lambda (factor-matrix-vector lambda-vector)
  "Fold LAMBDA-VECTOR back into mode 0 of FACTOR-MATRIX-VECTOR.

SDOT and SPARSE-KL-DIVERGENCE read the factors alone, so the weights have to
live somewhere in them. Putting the whole weight on mode 0 leaves every other
mode with unit-sum columns, which is the normalized form, and keeps the returned
factors directly usable without a separate lambda argument."
  (let ((matrix (svref factor-matrix-vector 0)))
    (declare (type (simple-array double-float) matrix))
    (loop for ri from 0 below (array-dimension matrix 1)
          do (let ((weight (aref lambda-vector ri)))
               (loop for i from 0 below (array-dimension matrix 0)
                     do (setf (aref matrix i ri) (* (aref matrix i ri) weight))))))
  factor-matrix-vector)

(defmacro %with-float-traps-masked (&body body)
  "Run BODY with the IEEE traps masked where the implementation supports it.

The optimizer detects NaN and infinity itself and reports
NUMERICAL-INSTABILITY-ERROR naming the mode, row and column that went bad, which
is far more useful than an implementation's own floating-point condition. That
only works if the arithmetic is allowed to produce the bad value rather than
trapping on it, so the traps are masked over the iteration and %CHECK-FACTOR-VALUES
is what actually reports the failure. Implementations that do not trap by default
need nothing here."
  #+sbcl `(sb-int:with-float-traps-masked (:invalid :overflow :divide-by-zero)
            ,@body)
  #-sbcl `(progn ,@body))

(defun %check-factor-values (factor-matrix-vector)
  "Signal NUMERICAL-INSTABILITY-ERROR on the first NaN or infinite factor entry.

Scanning costs O(R * sum_m I_m), the size of the factors themselves, which is
small beside a sweep's O(nnz * R * n-modes). It runs before the iteration starts
and again after each sweep's updates, so a bad value is reported before it can
spread through normalization, SDOT and the loss."
  (loop for mode from 0 below (length factor-matrix-vector)
        do (let ((matrix (svref factor-matrix-vector mode)))
             (loop for i from 0 below (array-dimension matrix 0)
                   do (loop for ri from 0 below (array-dimension matrix 1)
                            do (let ((value (aref matrix i ri)))
                                 (when (or (%float-nan-p value)
                                           (%float-infinity-p value))
                                   (error 'numerical-instability-error
                                          :location (list :mode mode :row i :column ri)
                                          :value value
                                          :operation "factor matrix update"))))))))

(defun %check-factor-health (factor-matrix-vector lambda-vector
                             &key (dead-component-threshold 1.0d-10)
                                  (on-dead-component :warn))
  "Report factors that have gone numerically bad.

A NaN or infinite entry means the fit has broken down and every downstream
number is meaningless, so it always signals NUMERICAL-INSTABILITY-ERROR.

A dead component -- one whose weight has collapsed to zero -- is different: it
usually means the requested rank is larger than the data supports, which is
ordinary and is exactly what a rank sweep is looking for. Signalling an error
there would break SELECT-RANK, so ON-DEAD-COMPONENT chooses:

  :warn   (default) signal a warning and continue
  :error  signal NUMERICAL-INSTABILITY-ERROR
  :ignore say nothing

Returns the list of dead component indices."
  (%check-factor-values factor-matrix-vector)
  (let ((dead (loop for ri from 0 below (length lambda-vector)
                    when (< (aref lambda-vector ri) dead-component-threshold)
                      collect ri)))
    (when dead
      (ecase on-dead-component
        (:ignore nil)
        (:warn (warn "~D of ~D components collapsed to zero weight (below ~,2E): ~{~D~^, ~}. ~
The data may not support this rank."
                     (length dead) (length lambda-vector)
                     dead-component-threshold dead))
        (:error (error 'numerical-instability-error
                       :location (list :dead-components dead)
                       :value (aref lambda-vector (first dead))
                       :operation "component weight"))))
    dead))

(defun decomposition-inner (n-cycle X-indices-matrix X-value-vector X^-value-vector
                            factor-matrix-vector numerator-tmp denominator-tmp
                            &key verbose convergence-threshold convergence-window
                                 (kkt-tolerance 1.0d-4)
                                 (kappa 1.0d-2) (kappa-tolerance 1.0d-10)
                                 lambda-vector
                                 (dead-component-threshold 1.0d-10)
                                 (on-dead-component :warn))
  "Run up to N-CYCLE outer iterations of multiplicative updates.

One outer iteration is a full sweep: every mode is updated once, in order, with
X^-VALUE-VECTOR refreshed between modes so each update sees the current model.
Counting a single mode as an iteration, as this used to, made N-CYCLE mean
different amounts of work for tensors of different order and made the
convergence window compare points that were only partly updated.

At the end of each sweep the columns are normalized and the scale collected into
LAMBDA-VECTOR, then folded back into mode 0 (see %NORMALIZE-FACTORS and
%ABSORB-LAMBDA), so the model is unchanged but its scale is explicit.

Two convergence tests run, and either stops the sweep:

  KKT residual  max over all entries of |min(A, gradient)|, which is 0 exactly
                at a stationary point of the non-negativity constrained problem.
                Compared against KKT-TOLERANCE; pass 0 to disable.
  moving average the older relative-change test on a window of KL values, kept
                for callers that use it. Enabled only by CONVERGENCE-THRESHOLD.

The moving-average test on its own is weak: averaging over a window dilutes the
step-to-step change, so a larger window reports convergence sooner, including on
runs that have not converged. The KKT residual measures the thing that actually
defines a solution, which is why it is on by default.

KAPPA and KAPPA-TOLERANCE control the inadmissible-zero fix described in UPDATE;
the first sweep runs without it, matching Chi and Kolda.

DEAD-COMPONENT-THRESHOLD and ON-DEAD-COMPONENT are passed to
%CHECK-FACTOR-HEALTH once the sweep ends.

N-CYCLE of 0 is legal and runs no updates, but the initial model is still put
into the returned representation: columns normalized, weights filled in and the
loss computed against them.

Returns six values:
  1. Number of outer iterations executed
  2. Final KL divergence value
  3. Vector of KL divergence values, one per outer iteration
  4. T when either convergence test fired
  5. The lambda vector of component weights
  6. The final KKT residual, recomputed against the model being returned

Signals NUMERICAL-INSTABILITY-ERROR if the loss, the KKT residual, a factor
entry or a normalization aggregate leaves the double range. :N-STARTS does not
paper over this: a loss that cannot be represented reflects data whose model mass
overflows, which a different initialization does not change."
  (unless (and (integerp n-cycle) (>= n-cycle 0))
    (error 'invalid-input-error
           :reason :invalid-iteration-budget
           :details (format nil "n-cycle must be a non-negative integer, got ~S" n-cycle)))
  (let* ((n-modes (length factor-matrix-vector))
         (rank (array-dimension (svref factor-matrix-vector 0) 1))
         ;; UPDATE declares these double-float under (safety 0), so a caller's
         ;; plain 0 or single-float must be converted before it gets there.
         (kappa (coerce kappa 'double-float))
         (kappa-tolerance (coerce kappa-tolerance 'double-float))
         (lambda-vector (or lambda-vector
                            (make-array rank :element-type 'double-float
                                             :initial-element 1.0d0)))
         (threshold (and convergence-threshold
                         (coerce convergence-threshold 'double-float)))
         (window (when threshold
                   (let ((w (or convergence-window 5)))
                     (when (or (null w) (<= w 0))
                       (error "CONVERGENCE-WINDOW must be a positive integer."))
                     w)))
         (kl-buffer (when window
                      (make-array window :element-type 'double-float :initial-element 0d0)))
         (kl-count 0)
         (kl-index 0)
         (last-smooth nil)
         (kkt-limit (and kkt-tolerance (coerce kkt-tolerance 'double-float)))
         (kl-history (make-array n-cycle :element-type 'double-float
                                 :initial-element 0d0 :adjustable t :fill-pointer 0))
         (final-kl 0d0)
         (residual 0d0)
         (sweep-residual 0d0)
         (residual-fresh nil)
         (iterations 0)
         (converged-p nil))
    ;; Reject bad factors before any arithmetic touches them, so a caller that
    ;; hands in a NaN gets the documented condition rather than a trap from deep
    ;; inside a specialized loop.
    (%check-factor-values factor-matrix-vector)
    (%with-float-traps-masked
     (block done
      ;; Seed the reconstruction from the initial factors; from here on every
      ;; mode update is followed by an SDOT, so the reconstruction, the factor
      ;; matrices and the reported KL always describe the same state.
      (sdot factor-matrix-vector X-indices-matrix X^-value-vector)
      (loop for iteration from 0 below n-cycle do
        (setf iterations (1+ iteration))
        ;; Cheap screen: the largest violation any mode reported on entry. It
        ;; costs nothing because UPDATE already has the gradients, but it
        ;; describes staggered pre-update states, so it only gates the exact
        ;; check below rather than deciding anything itself.
        (setf sweep-residual 0d0)
        (loop for mode from 0 below n-modes
              do (setf sweep-residual
                       (max sweep-residual
                            (update X-indices-matrix X-value-vector X^-value-vector
                                    factor-matrix-vector mode
                                    numerator-tmp denominator-tmp
                                    :kappa kappa
                                    :kappa-tolerance kappa-tolerance
                                    :allow-scooch (plusp iteration))))
                 (when (< mode (1- n-modes))
                   (sdot factor-matrix-vector X-indices-matrix X^-value-vector)))
        ;; Catch an overflow the sweep just produced, before normalizing divides
        ;; by it and SDOT and the loss carry it further.
        (%check-factor-values factor-matrix-vector)
        (%normalize-factors factor-matrix-vector lambda-vector)
        (%absorb-lambda factor-matrix-vector lambda-vector)
        (sdot factor-matrix-vector X-indices-matrix X^-value-vector)
        (let ((kl-value (%check-loss
                         (sparse-kl-divergence X-indices-matrix X-value-vector
                                               X^-value-vector factor-matrix-vector))))
          (vector-push-extend kl-value kl-history)
          (setf final-kl kl-value)
          (when verbose
            ;; Deliberately not called the KKT residual: this is the free screen,
            ;; measured before each mode's own update, so it describes staggered
            ;; states rather than the normalized model whose KL sits beside it.
            ;; The settled residual costs a pass per mode and is reported once,
            ;; on the closing line.
            (format t "iteration: ~A, kl-divergence: ~A, kkt-screen: ~,3E~%"
                    iterations kl-value sweep-residual))
          (when (and kkt-limit (plusp kkt-limit) (< sweep-residual kkt-limit))
            ;; The screen tripped; confirm against the model that actually came
            ;; out of the sweep before calling it converged.
            (setf residual (%kkt-residual X-indices-matrix X-value-vector
                                          X^-value-vector factor-matrix-vector
                                          numerator-tmp denominator-tmp))
            (setf residual-fresh t)
            (when (< residual kkt-limit)
              (setf converged-p t)
              (return-from done))
            (setf residual-fresh nil))
          (when threshold
            (setf (aref kl-buffer kl-index) kl-value)
            (setf kl-index (mod (1+ kl-index) window))
            (when (< kl-count window)
              (incf kl-count))
            (when (= kl-count window)
              (let ((smooth (/ (loop for idx from 0 below window
                                     sum (aref kl-buffer idx))
                               window)))
                (when last-smooth
                  (let* ((delta (abs (- smooth last-smooth)))
                         (base (max (abs last-smooth) *epsilon*))
                         (ratio (/ delta base)))
                    (when (< ratio threshold)
                      (setf converged-p t)
                      (return-from done))))
                (setf last-smooth smooth)))))))
     ;; An empty budget never reaches the loop body, and the loop body is what
     ;; normalizes the columns, fills in the weights and computes the loss. Do it
     ;; once here so a caller gets the documented representation either way,
     ;; rather than raw initial factors with a weight vector of all ones.
     (when (zerop iterations)
       (%normalize-factors factor-matrix-vector lambda-vector)
       (%absorb-lambda factor-matrix-vector lambda-vector)
       (sdot factor-matrix-vector X-indices-matrix X^-value-vector)
       (setf final-kl (%check-loss
                       (sparse-kl-divergence X-indices-matrix X-value-vector
                                             X^-value-vector factor-matrix-vector))))
     ;; The settled residual runs CALC-NUMERATOR, which divides an observed count
     ;; by the reconstruction, so it can overflow on the same inputs the sweep
     ;; can. It stays inside the mask for the same reason the sweep does: the
     ;; library reports what went wrong, rather than letting an implementation
     ;; floating-point condition escape from a specialized loop.
     (unless residual-fresh
       (setf residual (%kkt-residual X-indices-matrix X-value-vector X^-value-vector
                                     factor-matrix-vector numerator-tmp denominator-tmp))
       ;; The screen is measured before each mode's own update, so a sweep that
       ;; lands on a solution shows a large screen and a small settled residual.
       ;; Deciding only on the screen would return a residual under the tolerance
       ;; while reporting that the run did not converge.
       (when (and kkt-limit (plusp kkt-limit) (< residual kkt-limit))
         (setf converged-p t)))
     ;; A residual outside the double range describes no model, so it is a
     ;; failure rather than a large number worth returning.
     (when (or (%float-nan-p residual) (%float-infinity-p residual))
       (error 'numerical-instability-error
              :location :kkt-residual
              :value residual
              :operation "KKT residual"))
     (%check-factor-health factor-matrix-vector lambda-vector
                           :dead-component-threshold dead-component-threshold
                           :on-dead-component on-dead-component)
     (when verbose
       (format t "final: iterations ~D, kl-divergence ~A, kkt-residual ~,3E, converged ~A~%"
               iterations final-kl residual (and converged-p t))
       (finish-output)))
    (values iterations final-kl kl-history converged-p lambda-vector residual)))

(defstruct mode-spec
  "Metadata describing a single mode (dimension) of the tensor.

NAME           - String name identifying this mode (e.g., \"user\", \"product\")
LABELS         - Vector of category labels for this mode's indices
DISCRETIZATION - Description of how continuous values were discretized
MISSING-LABELS - List of labels representing missing/unknown values
ROLE           - Keyword indicating semantic role (e.g., :purchase, :time)
POSITIVE-LABEL - Label for positive outcome (for binary modes)
NEGATIVE-LABEL - Label for negative outcome (for binary modes)"
  (name nil :type (or null string))
  (labels nil :type (or null simple-vector))
  (discretization "unspecified" :type string)
  (missing-labels nil :type list)
  (role nil :type (or null keyword))
  (positive-label nil :type (or null string))
  (negative-label nil :type (or null string)))

(defstruct (sparse-tensor (:constructor %make-sparse-tensor))
  "Sparse tensor representation with optional domain metadata.

SHAPE   - List of dimension sizes for each mode (e.g., '(100 50 10))
INDICES - 2D fixnum array of shape (nnz, n-modes) containing coordinates
VALUES  - 1D double-float array of observed counts/values at each index
DOMAINS - Optional vector of mode-spec structures describing each mode
AUX     - Optional auxiliary data (e.g., preprocessing metadata, hash tables)"
  (shape nil :type list :read-only t)
  (indices nil :type (simple-array fixnum (* *)) :read-only t)
  (values nil :type (simple-array double-float (*)) :read-only t)
  (domains nil :type (or null simple-vector) :read-only t)
  (aux nil :type t))

(defun decomposition (tensor &key (n-cycle 100) (r 20) verbose
                                  convergence-threshold convergence-window
                                  (kkt-tolerance 1.0d-4)
                                  (kappa 1.0d-2) (kappa-tolerance 1.0d-10)
                                  (n-starts 1)
                                  (dead-component-threshold 1.0d-10)
                                  (on-dead-component :warn))
  "Run multiplicative-update tensor decomposition on sparse data.

TENSOR        - sparse-tensor structure containing shape, indices, and values.
N-CYCLE       - maximum *outer iterations*; defaults to 100. One outer iteration
                updates every mode once. This used to count single-mode updates,
                so the same number now does N-MODES times as much work. Must be a
                non-negative integer; 0 runs no updates but still returns the
                initial model in the normalized representation below.
R             - latent rank shared across factor matrices; defaults to 20.
VERBOSE       - when true, emit per-iteration logs; defaults to NIL. The
                per-iteration line reports KKT-SCREEN, the free in-sweep value,
                not the settled residual; the closing line reports the settled
                one, which is the seventh return value.
CONVERGENCE-THRESHOLD - optional relative tolerance for the moving-average test.
CONVERGENCE-WINDOW    - smoothing window length; defaults to 5.
KKT-TOLERANCE - stop once the largest KKT violation falls below this; defaults
                to 1d-4 as in Chi and Kolda. Pass 0 to run the full budget.
KAPPA, KAPPA-TOLERANCE - inadmissible-zero handling; see UPDATE.
N-STARTS      - how many random initializations to try, keeping the one with the
                lowest final KL; defaults to 1. Multiplicative updates only find
                a local optimum, so a wider rank or a harder tensor benefits
                from more than one.
DEAD-COMPONENT-THRESHOLD, ON-DEAD-COMPONENT - see %CHECK-FACTOR-HEALTH.

The returned factor matrices are normalized: every mode past the first has
unit-sum columns and mode 0 carries the component weights, so SDOT and
SPARSE-KL-DIVERGENCE still read them directly while the weights are available
separately.

Returns seven values:
  1. factor-matrix-vector - the decomposed factor matrices
  2. iterations - number of outer iterations executed by the winning start
  3. final-kl - final KL divergence value
  4. kl-history - vector of KL divergence at each outer iteration
  5. converged-p - T if either convergence test fired, NIL otherwise
  6. lambda-vector - the component weights, summing to the total predicted mass
  7. kkt-residual - the largest KKT violation at the returned point

Signals INVALID-INPUT-ERROR for a non-positive N-STARTS, and
NUMERICAL-INSTABILITY-ERROR if the fit produces NaN or infinite factors."
  (unless (and (integerp n-starts) (plusp n-starts))
    (error 'invalid-input-error
           :reason :invalid-n-starts
           :details (format nil "n-starts must be a positive integer, got ~S" n-starts)))
  (unless (and (integerp n-cycle) (>= n-cycle 0))
    (error 'invalid-input-error
           :reason :invalid-iteration-budget
           :details (format nil "n-cycle must be a non-negative integer, got ~S" n-cycle)))
  (setf kappa (coerce kappa 'double-float))
  (setf kappa-tolerance (coerce kappa-tolerance 'double-float))
  (let* ((x-shape (sparse-tensor-shape tensor))
         (indices (sparse-tensor-indices tensor))
         (values (sparse-tensor-values tensor))
         (n-modes (array-dimension indices 1))
         (x^-value-vector
          (make-array (length values) :element-type 'double-float
                      :initial-element 1.0d0))
         (factor-matrix-vector
          (make-array n-modes :initial-contents
                      (loop for dim from 0 below n-modes
                            collect (make-array (list (nth dim x-shape) r)
                                                :element-type 'double-float))))
         (numerator-tmp
          (make-array n-modes :initial-contents
                      (loop for dim from 0 below n-modes
                            collect (make-array (list (nth dim x-shape) r)
                                                :element-type 'double-float
                                                :initial-element 0.0d0))))
         (denominator-tmp
          (make-array (list n-modes r) :element-type
                      'double-float :initial-element 1.0d0))
         (best nil))
    (dotimes (start n-starts)
      (loop for factor-matrix across factor-matrix-vector
            do (initialize-random-matrix factor-matrix))
      (multiple-value-bind (iterations final-kl kl-history converged-p lambda-vector residual)
          (decomposition-inner n-cycle indices values x^-value-vector
                               factor-matrix-vector numerator-tmp denominator-tmp
                               :verbose verbose
                               :convergence-threshold convergence-threshold
                               :convergence-window convergence-window
                               :kkt-tolerance kkt-tolerance
                               :kappa kappa
                               :kappa-tolerance kappa-tolerance
                               :dead-component-threshold dead-component-threshold
                               :on-dead-component (if (= n-starts 1)
                                                      on-dead-component
                                                      :ignore))
        (when (or (null best) (< final-kl (second best)))
          (setf best (list (%copy-factor-matrices factor-matrix-vector)
                           final-kl iterations kl-history converged-p
                           (copy-seq lambda-vector) residual)))
        (when (and verbose (> n-starts 1))
          (format t "start ~D/~D: final kl ~,6F~%" (1+ start) n-starts final-kl)
          (finish-output))))
    (destructuring-bind (factors final-kl iterations kl-history converged-p
                         lambda-vector residual)
        best
      ;; With several starts the health check runs once, on the fit that won.
      (when (> n-starts 1)
        (%check-factor-health factors lambda-vector
                              :dead-component-threshold dead-component-threshold
                              :on-dead-component on-dead-component))
      (values factors iterations final-kl kl-history converged-p
              lambda-vector residual))))

(defun ranking (label-list factor-matrix r)
  "Return LABEL-LIST paired with weights from FACTOR-MATRIX column R, sorted descending."
  (let ((result (loop for i from 0 below (array-dimension factor-matrix 0)
                      for label in label-list
                      collect (cons label (aref factor-matrix i r)))))
    (sort result (lambda (a b)
                   (> (cdr a) (cdr b))))))
