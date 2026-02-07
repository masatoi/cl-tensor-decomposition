;;; CORCONDIA (Core Consistency Diagnostic) Tests
;;;
;;; EXPERIMENTAL: These tests are for the CORCONDIA implementation which
;;; is designed for dense tensors. Results on sparse tensors may be unreliable.

(defpackage :cl-tensor-decomposition/corcondia-test
  (:use :cl :rove :cl-tensor-decomposition))

(in-package :cl-tensor-decomposition/corcondia-test)

(defparameter +test-epsilon+ 1.0d-9)

;;; Test data for CORCONDIA integration tests
(defparameter x-shape '(3 4 2))
(defparameter x-indices-matrix
  (make-array '(5 3) :element-type 'fixnum
              :initial-contents '((0 0 0) (1 1 0) (2 2 1) (0 3 1) (1 0 1))))
(defparameter x-value-vector
  (make-array 5 :element-type 'double-float
              :initial-contents '(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0)))

;;; ============================================================
;;; Matrix Operations Tests
;;; ============================================================

(deftest matrix-transpose-correctness
  "Matrix transpose swaps rows and columns correctly."
  (let* ((a (make-array '(2 3) :element-type 'double-float
                        :initial-contents '((1.0d0 2.0d0 3.0d0)
                                            (4.0d0 5.0d0 6.0d0))))
         (at (cl-tensor-decomposition::%matrix-transpose a)))
    (ok (equal '(3 2) (array-dimensions at))
        "Transposed dimensions are swapped")
    (ok (= 1.0d0 (aref at 0 0)) "Element (0,0) correct")
    (ok (= 4.0d0 (aref at 0 1)) "Element (0,1) correct")
    (ok (= 2.0d0 (aref at 1 0)) "Element (1,0) correct")
    (ok (= 5.0d0 (aref at 1 1)) "Element (1,1) correct")
    (ok (= 3.0d0 (aref at 2 0)) "Element (2,0) correct")
    (ok (= 6.0d0 (aref at 2 1)) "Element (2,1) correct")))

(deftest matrix-multiply-correctness
  "Matrix multiplication produces correct result."
  (let* ((a (make-array '(2 3) :element-type 'double-float
                        :initial-contents '((1.0d0 2.0d0 3.0d0)
                                            (4.0d0 5.0d0 6.0d0))))
         (b (make-array '(3 2) :element-type 'double-float
                        :initial-contents '((7.0d0 8.0d0)
                                            (9.0d0 10.0d0)
                                            (11.0d0 12.0d0))))
         (c (cl-tensor-decomposition::%matrix-multiply a b)))
    (ok (equal '(2 2) (array-dimensions c))
        "Result has correct dimensions")
    ;; C[0,0] = 1*7 + 2*9 + 3*11 = 7 + 18 + 33 = 58
    (ok (< (abs (- 58.0d0 (aref c 0 0))) +test-epsilon+) "C[0,0] = 58")
    ;; C[0,1] = 1*8 + 2*10 + 3*12 = 8 + 20 + 36 = 64
    (ok (< (abs (- 64.0d0 (aref c 0 1))) +test-epsilon+) "C[0,1] = 64")
    ;; C[1,0] = 4*7 + 5*9 + 6*11 = 28 + 45 + 66 = 139
    (ok (< (abs (- 139.0d0 (aref c 1 0))) +test-epsilon+) "C[1,0] = 139")
    ;; C[1,1] = 4*8 + 5*10 + 6*12 = 32 + 50 + 72 = 154
    (ok (< (abs (- 154.0d0 (aref c 1 1))) +test-epsilon+) "C[1,1] = 154")))

(deftest matrix-inverse-2x2
  "Matrix inverse works for 2x2 matrix."
  (let* ((a (make-array '(2 2) :element-type 'double-float
                        :initial-contents '((4.0d0 7.0d0)
                                            (2.0d0 6.0d0))))
         (a-inv (cl-tensor-decomposition::%matrix-inverse a)))
    (ok a-inv "Inverse exists for non-singular matrix")
    ;; det(A) = 4*6 - 7*2 = 24 - 14 = 10
    ;; A^-1 = (1/10) * [[6, -7], [-2, 4]]
    (ok (< (abs (- 0.6d0 (aref a-inv 0 0))) +test-epsilon+) "A^-1[0,0] = 0.6")
    (ok (< (abs (- -0.7d0 (aref a-inv 0 1))) +test-epsilon+) "A^-1[0,1] = -0.7")
    (ok (< (abs (- -0.2d0 (aref a-inv 1 0))) +test-epsilon+) "A^-1[1,0] = -0.2")
    (ok (< (abs (- 0.4d0 (aref a-inv 1 1))) +test-epsilon+) "A^-1[1,1] = 0.4")))

(deftest matrix-inverse-singular-returns-nil
  "Matrix inverse returns NIL for singular matrix."
  (let* ((a (make-array '(2 2) :element-type 'double-float
                        :initial-contents '((1.0d0 2.0d0)
                                            (2.0d0 4.0d0)))) ; rows are linearly dependent
         (a-inv (cl-tensor-decomposition::%matrix-inverse a)))
    (ok (null a-inv) "Inverse is NIL for singular matrix")))

(deftest matrix-inverse-identity-check
  "A * A^-1 = I for invertible matrix."
  (let* ((a (make-array '(3 3) :element-type 'double-float
                        :initial-contents '((1.0d0 2.0d0 3.0d0)
                                            (0.0d0 1.0d0 4.0d0)
                                            (5.0d0 6.0d0 0.0d0))))
         (a-inv (cl-tensor-decomposition::%matrix-inverse a))
         (identity (cl-tensor-decomposition::%matrix-multiply a a-inv)))
    (ok a-inv "Inverse exists")
    ;; Check diagonal elements are 1
    (ok (< (abs (- 1.0d0 (aref identity 0 0))) 1d-10) "I[0,0] ≈ 1")
    (ok (< (abs (- 1.0d0 (aref identity 1 1))) 1d-10) "I[1,1] ≈ 1")
    (ok (< (abs (- 1.0d0 (aref identity 2 2))) 1d-10) "I[2,2] ≈ 1")
    ;; Check off-diagonal elements are 0
    (ok (< (abs (aref identity 0 1)) 1d-10) "I[0,1] ≈ 0")
    (ok (< (abs (aref identity 0 2)) 1d-10) "I[0,2] ≈ 0")
    (ok (< (abs (aref identity 1 0)) 1d-10) "I[1,0] ≈ 0")))

(deftest pseudo-inverse-tall-matrix
  "Pseudo-inverse of tall matrix (m > n) satisfies A⁺A = I."
  (let* ((a (make-array '(4 2) :element-type 'double-float
                        :initial-contents '((1.0d0 0.0d0)
                                            (0.0d0 1.0d0)
                                            (1.0d0 0.0d0)
                                            (0.0d0 1.0d0))))
         (a-pinv (cl-tensor-decomposition::%pseudo-inverse a)))
    (ok a-pinv "Pseudo-inverse exists for tall matrix")
    (ok (equal '(2 4) (array-dimensions a-pinv))
        "Pseudo-inverse has dimensions (n x m)")
    ;; A⁺A should be close to identity (2x2)
    (let ((apa (cl-tensor-decomposition::%matrix-multiply a-pinv a)))
      (ok (< (abs (- 1.0d0 (aref apa 0 0))) 1d-10) "A⁺A[0,0] ≈ 1")
      (ok (< (abs (- 1.0d0 (aref apa 1 1))) 1d-10) "A⁺A[1,1] ≈ 1")
      (ok (< (abs (aref apa 0 1)) 1d-10) "A⁺A[0,1] ≈ 0")
      (ok (< (abs (aref apa 1 0)) 1d-10) "A⁺A[1,0] ≈ 0"))))

(deftest pseudo-inverse-wide-matrix
  "Pseudo-inverse of wide matrix (m < n) satisfies AA⁺ = I."
  (let* ((a (make-array '(2 4) :element-type 'double-float
                        :initial-contents '((1.0d0 0.0d0 1.0d0 0.0d0)
                                            (0.0d0 1.0d0 0.0d0 1.0d0))))
         (a-pinv (cl-tensor-decomposition::%pseudo-inverse a)))
    (ok a-pinv "Pseudo-inverse exists for wide matrix")
    (ok (equal '(4 2) (array-dimensions a-pinv))
        "Pseudo-inverse has dimensions (n x m)")
    ;; AA⁺ should be close to identity (2x2)
    (let ((aap (cl-tensor-decomposition::%matrix-multiply a a-pinv)))
      (ok (< (abs (- 1.0d0 (aref aap 0 0))) 1d-10) "AA⁺[0,0] ≈ 1")
      (ok (< (abs (- 1.0d0 (aref aap 1 1))) 1d-10) "AA⁺[1,1] ≈ 1")
      (ok (< (abs (aref aap 0 1)) 1d-10) "AA⁺[0,1] ≈ 0")
      (ok (< (abs (aref aap 1 0)) 1d-10) "AA⁺[1,0] ≈ 0"))))

;;; ============================================================
;;; Core Tensor Computation Tests
;;; ============================================================

(deftest compute-corcondia-from-core-perfect-superdiagonal
  "Perfect superdiagonal core tensor yields CORCONDIA = 100%."
  ;; Create a perfect superdiagonal core tensor for R=2, 3 modes
  ;; Core size = 2^3 = 8, elements are [1,0,0,0,0,0,0,1] in row-major
  (let* ((rank 2)
         (n-modes 3)
         (core (make-array 8 :element-type 'double-float
                           :initial-contents '(1.0d0 0.0d0 0.0d0 0.0d0
                                               0.0d0 0.0d0 0.0d0 1.0d0)))
         (lambda-vec (make-array 2 :element-type 'double-float
                                   :initial-contents '(1.0d0 1.0d0)))
         (score (cl-tensor-decomposition::%compute-corcondia-from-core
                 core rank n-modes lambda-vec)))
    (ok (< (abs (- 100.0d0 score)) +test-epsilon+)
        (format nil "Perfect superdiagonal gives 100%%, got ~,2F%%" score))))

(deftest compute-corcondia-from-core-all-zeros
  "All-zero core tensor yields CORCONDIA = 0% (clipped)."
  (let* ((rank 2)
         (n-modes 3)
         (core (make-array 8 :element-type 'double-float
                           :initial-element 0.0d0))
         (lambda-vec (make-array 2 :element-type 'double-float
                                   :initial-contents '(1.0d0 1.0d0)))
         (score (cl-tensor-decomposition::%compute-corcondia-from-core
                 core rank n-modes lambda-vec)))
    ;; Score = 100 * (1 - (2*1^2)/2) = 100 * (1 - 1) = 0
    (ok (< (abs score) +test-epsilon+)
        (format nil "All-zero core gives 0%%, got ~,2F%%" score))))

(deftest compute-corcondia-from-core-noisy
  "Noisy core tensor yields intermediate CORCONDIA score."
  (let* ((rank 2)
         (n-modes 3)
         ;; Superdiagonal with some noise
         (core (make-array 8 :element-type 'double-float
                           :initial-contents '(0.9d0 0.1d0 0.05d0 0.0d0
                                               0.0d0 0.1d0 0.0d0 0.95d0)))
         (lambda-vec (make-array 2 :element-type 'double-float
                                   :initial-contents '(1.0d0 1.0d0)))
         (score (cl-tensor-decomposition::%compute-corcondia-from-core
                 core rank n-modes lambda-vec)))
    (ok (> score 50.0d0) "Score > 50% for mostly superdiagonal")
    (ok (< score 100.0d0) "Score < 100% due to noise")))

;;; ============================================================
;;; Integration Tests (EXPERIMENTAL - sparse tensor behavior unreliable)
;;; ============================================================

(deftest corcondia-returns-valid-score
  "CORCONDIA returns a score between 0 and 100."
  (let ((tensor (cltd:make-sparse-tensor x-shape x-indices-matrix x-value-vector)))
    (multiple-value-bind (factors iterations)
        (cltd:decomposition tensor :r 2 :n-cycle 50)
      (declare (ignore iterations))
      (let ((score (cltd:corcondia tensor factors)))
        (ok (>= score 0.0d0)
            "CORCONDIA score is non-negative")
        (ok (<= score 100.0d0)
            "CORCONDIA score is at most 100")))))

(deftest corcondia-lower-rank-higher-score
  "Lower rank typically produces higher CORCONDIA score (less overfitting)."
  (let* ((shape '(5 4 3))
         (indices (make-array '(10 3) :element-type 'fixnum
                              :initial-contents '((0 0 0) (1 1 1) (2 2 2)
                                                  (0 1 0) (1 2 1) (2 0 2)
                                                  (3 1 0) (4 2 1) (0 3 2)
                                                  (1 0 0))))
         (values (make-array 10 :element-type 'double-float
                             :initial-contents '(5.0d0 10.0d0 8.0d0
                                                 3.0d0 4.0d0 2.0d0
                                                 1.0d0 6.0d0 7.0d0
                                                 2.0d0)))
         (tensor (cltd:make-sparse-tensor shape indices values)))
    ;; Use fixed random seed for deterministic results
    (let ((*random-state* (cltd:%seed-random-state 42)))
      ;; Decompose with rank 2 and rank 3
      (let* ((factors-r2 (cltd:decomposition tensor :r 2 :n-cycle 100))
             (factors-r3 (let ((*random-state* (cltd:%seed-random-state 42)))
                           (cltd:decomposition tensor :r 3 :n-cycle 100)))
             (score-r2 (cltd:corcondia tensor factors-r2))
             (score-r3 (cltd:corcondia tensor factors-r3)))
        ;; Lower rank should generally have higher CORCONDIA
        ;; (indicating better superdiagonal structure)
        (ok (> score-r2 score-r3)
            (format nil "Rank 2 (~,1F%%) > Rank 3 (~,1F%%)" score-r2 score-r3))))))

(deftest corcondia-verbose-output
  "CORCONDIA with verbose=t prints diagnostic information."
  (let* ((tensor (cltd:make-sparse-tensor x-shape x-indices-matrix x-value-vector))
         (factors (cltd:decomposition tensor :r 2 :n-cycle 30))
         (output (with-output-to-string (*standard-output*)
                   (cltd:corcondia tensor factors :verbose t))))
    (ok (search "pseudo-inverse" output)
        "Verbose output mentions pseudo-inverse computation")
    (ok (search "core tensor" output)
        "Verbose output mentions core tensor")
    (ok (search "CORCONDIA" output)
        "Verbose output includes CORCONDIA score")))
