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

iteration: 1, kl-divergence: 8.224610189606516d0, kkt-screen: 2.225d+0
iteration: 2, kl-divergence: 4.664015746706738d0, kkt-screen: 7.337d-1
iteration: 3, kl-divergence: 2.355426500948277d0, kkt-screen: 8.964d-1
iteration: 4, kl-divergence: 2.249334277028649d0, kkt-screen: 9.448d-2
iteration: 5, kl-divergence: 2.2493342451375336d0, kkt-screen: 7.524d-7
final: iterations 5, kl-divergence 2.2493342451375336d0, kkt-residual 7.500d-7, converged T

#(#2A((3.9999936666558518d0 1.443397262487504d-48)
      (1.042843956499246d-274 1.9999979999995001d0))
  #2A((0.7500006666702905d0 3.266427031620839d-89)
      (0.24999933332970933d0 1.0442453827305826d-186)
      (0.0d0 1.0d0))
  #2A((0.24999933333020377d0 7.707424075123446d-321)
      (0.7500006666697963d0 1.9823628043681258d-163)
      (0.0d0 0.0d0)
      (0.0d0 1.0d0)))

It stopped after 5 of the 10 allowed iterations because the KKT residual fell
below the default tolerance of 1d-4.

The per-iteration line reports KKT-SCREEN, the value a sweep sees for free before
each mode's own update. The closing line reports the settled residual, measured
against the finished model, which is what convergence is decided on and what
DECOMPOSITION returns as its seventh value.

One iteration is a full sweep over all three modes. Modes 1 and 2 come back with
unit-sum columns and mode 0 carries the component weights, which DECOMPOSITION
also returns separately as its sixth value:

  lambda: #(3.9999936666558518d0 1.9999979999995001d0)

Those sum to 6, the total observed count. The reported divergence is the
generalized KL over *every* coordinate, so it includes the predicted mass on the
21 implicit zeros of this 2x3x4 tensor, not just the 3 stored non-zeros.
|#
