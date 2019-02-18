(in-package :cltd)

;; Prepare sparse tensor

(defparameter X-shape '(2 3 4))
(defparameter X-indices-matrix
  (make-array '(3 3) :element-type 'fixnum
              :initial-contents '((0 1 0)
                                  (1 2 3)
                                  (0 0 1))))
(defparameter X-value-vector
  (make-array 3 :element-type 'double-float :initial-contents '(1d0 2d0 3d0)))

;; Decomposition

(decomposition X-shape X-indices-matrix X-value-vector :n-cycle 100 :R 2 :verbose t)
