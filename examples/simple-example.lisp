(in-package :cltd)

;; Prepare sparse tensor

(defparameter X-shape '(2 3 4))
(defparameter number-of-non-zero-elements 3)
(defparameter X-indices-matrix
  (make-array (list number-of-non-zero-elements
                    (length X-shape))
              :element-type 'fixnum
              :initial-contents '((0 1 0) ; The row corresponds a element of one datum
                                  (1 2 3)
                                  (0 0 1))))
(defparameter X-value-vector
  (make-array number-of-non-zero-elements
              :element-type 'double-float
              :initial-contents '(1.0d0 2.0d0 3.0d0)))

;; Create sparse tensor
(defparameter X-tensor
  (make-sparse-tensor X-shape X-indices-matrix X-value-vector))

;; Decomposition

(decomposition X-tensor :n-cycle 10 :R 2 :verbose t)

#|
cycle: 1, kl-divergence: 12.340305
cycle: 2, kl-divergence: 6.9702463
cycle: 3, kl-divergence: 6.4631243
cycle: 4, kl-divergence: 4.553627
cycle: 5, kl-divergence: 4.044311
cycle: 6, kl-divergence: 3.4903245
cycle: 7, kl-divergence: 2.7311742
cycle: 8, kl-divergence: 2.5847356
cycle: 9, kl-divergence: 2.1521056
cycle: 10, kl-divergence: 1.8600065

#(#2A((0.6189548 0.16031098) (0.0 0.71152395))
  #2A((1.6513237 0.017198939) (0.2164403 0.3754391) (0.0 1.2300345))
  #2A((0.3765476 0.38967007)
      (2.6936934 6.8288343e-4)
      (0.0 0.0)
      (0.0 1.3418932)))
|#
