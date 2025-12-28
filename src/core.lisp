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
           :make-fold-splits
           :cross-validate-rank
           :select-rank
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
           :convergence-failure-error
           :convergence-iterations
           :convergence-final-kl
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

(define-condition convergence-failure-error (tensor-decomposition-error)
  ((iterations :initarg :iterations
               :reader convergence-iterations
               :type fixnum
               :documentation "Number of iterations completed before failure")
   (final-kl :initarg :final-kl
             :reader convergence-final-kl
             :type double-float
             :documentation "Final KL divergence value at failure"))
  (:report (lambda (condition stream)
             (format stream "Decomposition failed to converge after ~D iterations (final KL=~,6F)"
                     (convergence-iterations condition)
                     (convergence-final-kl condition))))
  (:documentation "Signaled when decomposition fails to converge within allowed iterations."))

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

(defun sparse-kl-divergence (X-indices-matrix X-value-vector X^-value-vector)
  "Compute the Kullback–Leibler divergence between sparse counts and their approximation.
When x=0, the x*log(x/x^) term contributes 0 (limit as x→0+), so we only add x^ - x = x^."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) X-indices-matrix)
           (type (simple-array double-float) X-value-vector X^-value-vector))
  (loop for datum-index fixnum from 0 below (array-dimension X-indices-matrix 0)
        sum (let ((x (aref X-value-vector datum-index))
                  (x^ (aref X^-value-vector datum-index)))
              (if (> x 0.0d0)
                  ;; Standard KL: x*log(x/x^) - x + x^
                  (+ (* x (the double-float
                              (log (/ x (+ x^ (the double-float *epsilon*))))))
                     (- x)
                     x^)
                  ;; When x=0: lim_{x->0+} x*log(x/y) = 0, so just add -x + x^ = x^
                  x^))
        double-float))

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
               factor-matrix-vector factor-index numerator-tmp denominator-tmp)
  "Perform one multiplicative update step for a single factor matrix.

Updates the factor matrix at FACTOR-INDEX in place using the multiplicative
update rule: A_new = A_old * (numerator / denominator). This is the core
step of the MU algorithm for non-negative tensor decomposition with KL
divergence objective.

X-INDICES-MATRIX     - Sparse tensor indices
X-VALUE-VECTOR       - Observed counts
X^-VALUE-VECTOR      - Current reconstructed values
FACTOR-MATRIX-VECTOR - Vector of factor matrices (modified in place)
FACTOR-INDEX         - Index of the mode to update
NUMERATOR-TMP        - Temporary storage for numerator computation
DENOMINATOR-TMP      - Temporary storage for denominator computation"
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) x-indices-matrix)
           (type (simple-array double-float) x-value-vector x^-value-vector denominator-tmp)
           (type fixnum factor-index))
  (initialize-matrix (svref numerator-tmp factor-index) 0.0d0)
  (initialize-matrix denominator-tmp 1.0d0)
  (calc-denominator factor-matrix-vector factor-index denominator-tmp)
  (calc-numerator x-indices-matrix x-value-vector x^-value-vector
                  factor-matrix-vector factor-index numerator-tmp)
  (let ((factor-matrix (svref factor-matrix-vector factor-index))
        (numerator-tmp-elem (svref numerator-tmp factor-index)))
    (declare (type (simple-array double-float) factor-matrix numerator-tmp-elem))
    (loop for i from 0 below (array-dimension factor-matrix 0)
          do (loop for ri from 0 below (array-dimension factor-matrix 1)
                   do (setf (aref factor-matrix i ri)
                            (* (aref factor-matrix i ri)
                               (/ (aref numerator-tmp-elem i ri)
                                  (+ (aref denominator-tmp factor-index ri)
                                     (the double-float *epsilon*)))))))))

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

(defun decomposition-inner (n-cycle X-indices-matrix X-value-vector X^-value-vector
                            factor-matrix-vector numerator-tmp denominator-tmp
                            &key verbose convergence-threshold convergence-window)
  "Iteratively update FACTOR-MATRIX-VECTOR up to N-CYCLE steps, honoring convergence controls."
  (block done
    (let* ((threshold (and convergence-threshold
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
           (compute-kl? (or verbose threshold)))
      (loop for i from 0 below n-cycle do
        (sdot factor-matrix-vector X-indices-matrix X^-value-vector)
        (update X-indices-matrix X-value-vector X^-value-vector
          factor-matrix-vector (mod i (length factor-matrix-vector)) numerator-tmp denominator-tmp)
        (let ((kl-value (when compute-kl?
                          (sparse-kl-divergence X-indices-matrix X-value-vector X^-value-vector))))
          (when verbose
            (format t "cycle: ~A, kl-divergence: ~A~%"
                    (1+ i)
                    kl-value))
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
                      (return-from done (values (1+ i))))))
                (setf last-smooth smooth))))))
      (values n-cycle))))

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
                             convergence-threshold convergence-window)
  "Run multiplicative-update tensor decomposition on sparse data.

TENSOR        - sparse-tensor structure containing shape, indices, and values.
N-CYCLE       - maximum iterations to perform; defaults to 100.
R             - latent rank shared across factor matrices; defaults to 20.
VERBOSE       - when true, emit per-iteration KL divergence logs; defaults to NIL.
CONVERGENCE-THRESHOLD - optional relative tolerance for early stopping.
CONVERGENCE-WINDOW    - smoothing window length; defaults to 5.

Returns the factor-matrix vector and the number of iterations executed."
  (let* ((x-shape (sparse-tensor-shape tensor))
         (indices (sparse-tensor-indices tensor))
         (values (sparse-tensor-values tensor))
         (x^-value-vector
          (make-array (length values) :element-type 'double-float
                      :initial-element 1.0d0))
         (factor-matrix-vector
          (make-array (array-dimension indices 1) :initial-contents
                      (loop for dim from 0 below (array-dimension indices 1)
                            collect (make-array (list (nth dim x-shape) r)
                                                :element-type 'double-float))))
         (numerator-tmp
          (make-array (array-dimension indices 1) :initial-contents
                      (loop for dim from 0 below (array-dimension indices 1)
                            collect (make-array (list (nth dim x-shape) r)
                                                :element-type 'double-float
                                                :initial-element 0.0d0))))
         (denominator-tmp
          (make-array (list (array-dimension indices 1) r) :element-type
                      'double-float :initial-element 1.0d0)))
    (loop for factor-matrix across factor-matrix-vector
          do (initialize-random-matrix factor-matrix))
    (multiple-value-bind (iterations)
        (decomposition-inner n-cycle indices values x^-value-vector
         factor-matrix-vector numerator-tmp denominator-tmp :verbose verbose
         :convergence-threshold convergence-threshold :convergence-window
         convergence-window)
      (values factor-matrix-vector iterations))))

(defun ranking (label-list factor-matrix r)
  "Return LABEL-LIST paired with weights from FACTOR-MATRIX column R, sorted descending."
  (let ((result (loop for i from 0 below (array-dimension factor-matrix 0)
                      for label in label-list
                      collect (cons label (aref factor-matrix i r)))))
    (sort result (lambda (a b)
                   (> (cdr a) (cdr b))))))
