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
              :initial-contents '(1d0 2d0 3d0)))

;; Decomposition

(decomposition X-shape X-indices-matrix X-value-vector :n-cycle 10 :R 2 :verbose t)

;; cycle: 1, kl-divergence: 3.8123698367538257d0
;; cycle: 2, kl-divergence: 7.008589548052008d0
;; cycle: 3, kl-divergence: 6.417773781756691d0
;; cycle: 4, kl-divergence: 3.5830575816014383d0
;; cycle: 5, kl-divergence: 2.7313355001902684d0
;; cycle: 6, kl-divergence: 1.8856385081647657d0
;; cycle: 7, kl-divergence: 1.1461000700038138d0
;; cycle: 8, kl-divergence: 0.7998436391579001d0
;; cycle: 9, kl-divergence: 0.7485792224097678d0
;; cycle: 10, kl-divergence: 0.7493554240786242d0

;; #(#2A((4.769308790721058d-16 0.6089092386248208d0)
;;       (0.5846002362657854d0 8.941721411873916d-201))
;;   #2A((3.697392892947788d-5 1.638528941882213d0)
;;       (6.457393956786914d-6 0.5461812701566077d0)
;;       (1.2931746888759577d0 1.912174298594477d-59))
;;   #2A((1.6699059512771195d-9 0.7517156616482887d0)
;;       (1.4550066567623938d-8 2.2551469795230017d0)
;;       (0.0d0 0.0d0)
;;       (2.645447975546872d0 7.443966374813667d-109)))
