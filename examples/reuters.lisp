(ql:quickload :cl-docclass)
(ql:quickload :cl-tensor-decomposition)

(defpackage :reuters
  (:use :cl :cltd :cl-docclass))

(in-package :reuters)

(setf *print-length* 100)

(defun ls (&optional dir)
  (let ((dir (or dir (uiop:getcwd))))
    (append (uiop:subdirectories dir)
            (uiop:directory-files dir))))

(defparameter *data-files*
  (ls #P"/home/wiz/datasets/reuters/"))

(defun split-articles (file)
  (let ((article-list (with-open-file (f file) (read f))))
    (mapcar (lambda (article)
              (format nil "~%~A~%~A~%"
                      (nth 3 article) ; title
                      (nth 4 article) ; body
                      ))
            article-list)))

;;; Make dictionary (word-hash)

(defparameter *word-hash* (make-hash-table :test 'equal))

(load-igo-dict)

(time
 (dolist (file *data-files*)
   (print file)
   (docclass::add-words-to-hash!
    (apply #'concatenate 'string (split-articles file))
    *word-hash*)))

;; Evaluation took:
;;   386.417 seconds of real time
;;   366.476360 seconds of total run time (345.571214 user, 20.905146 system)
;;   [ Run times consist of 42.194 seconds GC time, and 324.283 seconds non-GC time. ]
;;   94.84% CPU
;;   1,310,797,444,477 processor cycles
;;   386,691,772,720 bytes consed

;; *word-hash* is big size hash table
;; #<HASH-TABLE :TEST EQUAL :COUNT 381152 {10217FFE53}>

;; Removes infrequent words
(defparameter *word-hash-compact* (docclass::remove-infrequent-words *word-hash* 100))

;; *word-hash-compact*
;; #<HASH-TABLE :TEST EQUAL :COUNT 31477 {1012747D63}>

;;; Make input data (List of sparse vectors)

(time (defparameter *texts* (alexandria:mappend #'split-articles *data-files*)))

;; Evaluation took:
;;   26.339 seconds of real time
;;   26.341515 seconds of total run time (25.105451 user, 1.236064 system)
;;   [ Run times consist of 1.853 seconds GC time, and 24.489 seconds non-GC time. ]
;;   100.01% CPU
;;   89,349,768,076 processor cycles
;;   9,202,277,216 bytes consed

(time
 (defparameter tf-idf-list
   (docclass::make-tf-idf-list
    *texts*
    *word-hash-compact*)))

;; Evaluation took:
;;   506.485 seconds of real time
;;   506.553062 seconds of total run time (503.292569 user, 3.260493 system)
;;   [ Run times consist of 6.619 seconds GC time, and 499.935 seconds non-GC time. ]
;;   100.01% CPU
;;   1,718,102,364,839 processor cycles
;;   384,466,180,128 bytes consed

;;; Make sparse tensor from sparse-vector list

(defparameter X-shape (list (length tf-idf-list)
                            (hash-table-count *word-hash-compact*)))

(defparameter n-non-zero
  (loop for sv in tf-idf-list sum (clol.vector:sparse-vector-length sv)))
;; density
(* (/ n-non-zero (apply #'* X-shape)) 100.0) ; => 0.4%


(defparameter X-indices-matrix
  ;; Number of non-zero elements x Number of mode
  (make-array (list n-non-zero 2)
              :element-type 'fixnum))

(defparameter X-value-vector
  (make-array n-non-zero :element-type 'single-float))

(let ((nz-index 0))
  (loop for sparse-vec in tf-idf-list
        for i from 0
        do
           (loop for j from 0 below (clol.vector:sparse-vector-length sparse-vec) do
             (setf (aref X-indices-matrix nz-index 0) i)
             (setf (aref X-indices-matrix nz-index 1)
                   (aref (clol.vector:sparse-vector-index-vector sparse-vec) j))
             (setf (aref X-value-vector nz-index)
                   (aref (clol.vector:sparse-vector-value-vector sparse-vec) j))
             (incf nz-index))))

;;; Training
(defparameter R 6)

(time (defparameter factor-matrix-vector
        (decomposition X-shape X-indices-matrix X-value-vector :n-cycle 1 :R R :verbose nil)))

(time (defparameter factor-matrix-vector
        (let ((cltd::*epsilon* 0.1))
          (decomposition X-shape X-indices-matrix X-value-vector :n-cycle 1 :R R :verbose t))))

;; Evaluation took:
;;   7.632 seconds of real time
;;   7.632068 seconds of total run time (7.504242 user, 0.127826 system)
;;   100.00% CPU
;;   25,889,215,642 processor cycles
;;   822,864,256 bytes consed

(time (defparameter factor-matrix-vector
        (decomposition X-shape X-indices-matrix X-value-vector :n-cycle 10 :R R :verbose nil)))

(time (defparameter factor-matrix-vector
        (decomposition X-shape X-indices-matrix X-value-vector :n-cycle 100 :R R :verbose nil)))

;;; Show result
(setf *print-length* 100)
(ql:quickload :clgplot)

;; Plot data x rank matrix
(clgplot:splot-matrix (aref factor-matrix-vector 0))

(defparameter word-vec (make-array (cadr X-shape)))
(maphash (lambda (key value)
           (setf (aref word-vec (car value)) key))
         *word-hash*)

;; Word list corresponding with index of X
(defparameter word-list (coerce word-vec 'list))

;; Word ranking per rank
(loop for i from 0 below R do
  (format t "~%~%R=~A" i)
  (print (ranking word-list (aref factor-matrix-vector 1) i)))
