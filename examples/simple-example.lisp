;;; -*- coding: utf-8; mode: lisp -*-
;;;
;;; Simple Example - Minimal tensor decomposition demo
;;;

(ql:quickload :cl-tensor-decomposition :silent t)

(in-package :cltd)

;; Prepare sparse tensor data

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
The factor matrices are seeded at random, so the exact numbers differ per run.
One representative run:

cycle: 1, kl-divergence: 12.375458236469434d0
cycle: 2, kl-divergence: 11.421030593767115d0
cycle: 3, kl-divergence: 7.654746677336834d0
cycle: 4, kl-divergence: 6.453010665196585d0
cycle: 5, kl-divergence: 4.747575186549028d0
cycle: 6, kl-divergence: 3.3759113009724446d0
cycle: 7, kl-divergence: 2.6266314340873818d0
cycle: 8, kl-divergence: 2.3132773887178866d0
cycle: 9, kl-divergence: 2.252722740899359d0
cycle: 10, kl-divergence: 2.24935167963779d0

#(#2A((0.8126999087904875d0 2.5362270002205497d-17)
      (1.4069910221524954d-98 1.1019555649412494d0))
  #2A((2.0993123211737923d0 4.282613598770222d-6)
      (0.6997614730501149d0 8.21300213668385d-6)
      (3.844096552359546d-29 1.433543101446069d0))
  #2A((0.43959586016845853d0 7.33015268400767d-10)
      (1.3187923563143393d0 1.2654007348018188d-10)
      (0.0d0 0.0d0)
      (1.2942686901905257d-53 1.2660501686271945d0)))

The reported divergence is the generalized KL over *every* coordinate, so it
includes the predicted mass on the 21 implicit zeros of this 2x3x4 tensor, not
just the 3 stored non-zeros.
|#
