(in-package :cltd)

(defparameter X-shape '(2 3 4))
(defparameter X-indices-matrix
  (make-array '(3 3) :element-type 'fixnum
              :initial-contents '((0 1 0)
                                  (1 2 3)
                                  (0 0 1))))

(defparameter X (make-array 3 :element-type 'double-float :initial-contents '(1d0 2d0 3d0)))

(decomposition X-shape X-indices-matrix X :n-cycle 100 :R 2 :verbose t)
