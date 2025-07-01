(defpackage cltd-test
  (:use :cl
        :cltd
        :prove))
(in-package :cltd-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-tensor-decomposition)' in your Lisp.

(plan nil)

(defparameter X-shape '(2 3 4))
(defparameter X-indices-matrix
  (make-array '(3 3) :element-type 'fixnum
              :initial-contents '((0 1 0) ; The row corresponds one datum
                                  (1 2 3)
                                  (0 0 1))))
(defparameter X-value-vector
  (make-array 3 :element-type 'single-float :initial-contents '(1.0 2.0 3.0)))

(ok (decomposition X-shape X-indices-matrix X-value-vector :n-cycle 100 :R 2 :verbose t))

(finalize)
