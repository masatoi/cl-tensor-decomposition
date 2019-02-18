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

;; #(#2A((1.5338421552034134d0 1.5338421552034134d0)
;;       (0.7669210776017066d0 0.7669210776017066d0))
;;   #2A((0.29262191851701197d0 0.29262191851701197d0)
;;       (0.09754063950567066d0 0.09754063950567066d0)
;;       (0.19508127901134129d0 0.19508127901134129d0))
;;   #2A((0.3713310167833809d0 0.3713310167833809d0)
;;       (1.1139930503501427d0 1.1139930503501427d0)
;;       (0.0d0 0.0d0)
;;       (0.7426620335667619d0 0.7426620335667619d0)))
