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
           :make-mode-metadata
           :generate-factor-cards
           :write-factor-cards-json
           :write-scenario-report
           :generate-report-artifacts))

(in-package :cl-tensor-decomposition)

(defparameter *epsilon* 0.000001d0)

(defun initialize-matrix (matrix default-value)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float) matrix)
           (type double-float default-value))
  (loop for i fixnum from 0 below (array-dimension matrix 0) do
    (loop for j fixnum from 0 below (array-dimension matrix 1) do
      (setf (aref matrix i j) default-value)))
  matrix)

(defun initialize-random-matrix (matrix)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float) matrix))
  (loop for i fixnum from 0 below (array-dimension matrix 0) do
    (loop for j fixnum from 0 below (array-dimension matrix 1) do
      (setf (aref matrix i j) (random 1.0d0))))
  matrix)

(defun sparse-kl-divergence (X-indices-matrix X-value-vector X^-value-vector)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) X-indices-matrix)
           (type (simple-array double-float) X-value-vector X^-value-vector))
  (loop for  datum-index fixnum from 0 below (array-dimension X-indices-matrix 0)
        sum (+ (* (aref X-value-vector datum-index)
                  (the double-float
                       (log (/ (aref X-value-vector datum-index)
                               (+ (aref X^-value-vector datum-index)
                                  (the double-float *epsilon*))))))
               (- (aref X-value-vector datum-index))
               (aref X^-value-vector datum-index))
        double-float))

(defun calc-denominator (factor-matrix-vector factor-index denominator-tmp)
  (declare (optimize (speed 3) (safety 0))
           (type simple-array factor-matrix-vector)
           (type (simple-array double-float) denominator-tmp)
           (type fixnum factor-index))
  (loop for other-factor-index fixnum from 0 below (length factor-matrix-vector)
        if (not (= factor-index other-factor-index)) do
          (let ((factor-matrix (svref factor-matrix-vector other-factor-index)))
            (declare (type (simple-array double-float) factor-matrix))
            (loop for ri fixnum from 0 below (array-dimension factor-matrix 1) do
              (setf (aref denominator-tmp factor-index ri)
                    (* (aref denominator-tmp factor-index ri)
                       (loop for i fixnum from 0 below (array-dimension factor-matrix 0)
                             sum (aref factor-matrix i ri) double-float)))))))

(defun calc-numerator (X-indices-matrix X-value-vector X^-value-vector
                       factor-matrix-vector factor-index numerator-tmp)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) X-indices-matrix)
           (type (simple-array double-float) X-value-vector X^-value-vector)
           (type simple-array factor-matrix-vector)
           (type fixnum factor-index))
  (loop for datum-index fixnum from 0 below (array-dimension X-indices-matrix 0) do
    (let ((x/x^ (/ (aref X-value-vector datum-index)
                   (+ (aref X^-value-vector datum-index)
                      (the double-float *epsilon*)))))
      (declare (type double-float x/x^))
      (let ((numerator-tmp-elem (svref numerator-tmp factor-index)))
        (declare (type (simple-array double-float) numerator-tmp-elem))
        (loop for ri fixnum from 0 below (array-dimension numerator-tmp-elem 1) do
          (let ((factor-prod 1.0d0))
            (declare (type double-float factor-prod))
            (loop for other-factor-index fixnum from 0 below (length factor-matrix-vector)
                  if (not (= factor-index other-factor-index)) do
                    (let ((factor-matrix (svref factor-matrix-vector other-factor-index)))
                      (declare (type (simple-array double-float) factor-matrix))
                      (setf factor-prod
                            (* factor-prod
                               (aref factor-matrix
                                     (aref X-indices-matrix datum-index other-factor-index)
                                     ri)))))
            (incf (aref numerator-tmp-elem (aref X-indices-matrix datum-index factor-index) ri)
                  (* x/x^ factor-prod))))))))

(defun update (X-indices-matrix X-value-vector X^-value-vector
               factor-matrix-vector factor-index numerator-tmp denominator-tmp)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) X-indices-matrix)
           (type (simple-array double-float) X-value-vector X^-value-vector denominator-tmp)
           (type fixnum factor-index))
  (initialize-matrix (svref numerator-tmp factor-index) 0.0d0)
  (initialize-matrix denominator-tmp 1.0d0)
  (calc-denominator factor-matrix-vector factor-index denominator-tmp)
  (calc-numerator X-indices-matrix X-value-vector X^-value-vector
                  factor-matrix-vector factor-index numerator-tmp)

  (let ((factor-matrix (svref factor-matrix-vector factor-index))
        (numerator-tmp-elem (svref numerator-tmp factor-index)))
    (declare (type (simple-array double-float) factor-matrix numerator-tmp-elem))
    (loop for i from 0 below (array-dimension factor-matrix 0) do
      (loop for ri from 0 below (array-dimension factor-matrix 1) do
        (setf (aref factor-matrix i ri)
              (* (aref factor-matrix i ri)
                 (/ (aref numerator-tmp-elem i ri)
                    (+ (aref denominator-tmp factor-index ri)
                       (the double-float *epsilon*)))))))))

(defun sdot (factor-matrix-vector X-indices-matrix X^-value-vector)
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
                  (when (< (abs (- smooth last-smooth)) threshold)
                    (return-from done (values (1+ i)))))
                (setf last-smooth smooth))))))
      (values n-cycle))))

(defun decomposition (X-shape X-indices-matrix X-value-vector
                      &key (n-cycle 100) (R 20) verbose
                      convergence-threshold convergence-window)
  (let ((X^-value-vector (make-array (length X-value-vector)
                                     :element-type 'double-float
                                     :initial-element 1.0d0))
        (factor-matrix-vector
          (make-array (array-dimension X-indices-matrix 1)
                      :initial-contents
                      (loop for dim from 0 below (array-dimension X-indices-matrix 1)
                            collect (make-array (list (nth dim X-shape) R)
                                                :element-type 'double-float))))
        (numerator-tmp
          (make-array (array-dimension X-indices-matrix 1)
                      :initial-contents
                      (loop for dim from 0 below (array-dimension X-indices-matrix 1)
                            collect (make-array (list (nth dim X-shape) R)
                                                :element-type 'double-float :initial-element 0.0d0))))
        (denominator-tmp (make-array (list (array-dimension X-indices-matrix 1) R)
                                     :element-type 'double-float
                                     :initial-element 1.0d0)))
    (loop for factor-matrix across factor-matrix-vector do
      (initialize-random-matrix factor-matrix))
    (multiple-value-bind (iterations)
        (decomposition-inner n-cycle X-indices-matrix X-value-vector X^-value-vector
                             factor-matrix-vector numerator-tmp denominator-tmp
                             :verbose verbose
                             :convergence-threshold convergence-threshold
                             :convergence-window convergence-window)
      (values factor-matrix-vector iterations))))

(defun ranking (label-list factor-matrix r)
  (let ((result (loop for i from 0 below (array-dimension factor-matrix 0)
                      for label in label-list
                      collect (cons label (aref factor-matrix i r)))))
    (sort result (lambda (a b)
                   (> (cdr a) (cdr b))))))
