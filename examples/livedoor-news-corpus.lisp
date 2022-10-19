(ql:quickload :cl-docclass)

(defpackage :livedoor-news-corpus
  (:use :cl :cltd :cl-docclass))

(in-package :livedoor-news-corpus)

(setf *print-length* 100)

(defun ls (&optional dir)
  (let ((dir (or dir (uiop:getcwd))))
    (append (uiop:subdirectories dir)
            (uiop:directory-files dir))))

(defparameter *livedoor-data*
  (list
   (ls #P"/home/wiz/datasets/livedoor/text/kaden-channel/")
   (ls #P"/home/wiz/datasets/livedoor/text/peachy/")
   (ls #P"/home/wiz/datasets/livedoor/text/sports-watch/")
   (ls #P"/home/wiz/datasets/livedoor/text/dokujo-tsushin/")
   (ls #P"/home/wiz/datasets/livedoor/text/livedoor-homme/")
   (ls #P"/home/wiz/datasets/livedoor/text/topic-news/")
   (ls #P"/home/wiz/datasets/livedoor/text/it-life-hack/")
   (ls #P"/home/wiz/datasets/livedoor/text/movie-enter/")
   (ls #P"/home/wiz/datasets/livedoor/text/smax/")))

(defparameter *livedoor-data-files* (alexandria:flatten *livedoor-data*))

;;; Make dictionary (word-hash)

;;; Make input data (List of sparse vectors)

(defvar *tf-idf-list*)
(defvar *word-hash*)

(load-igo-dict)

(time
 (multiple-value-bind (tf-idf-list word-hash)
     (docclass:make-tf-idf-list-from-files *livedoor-data-files*
                                           :infrequent-threshold 10)
   (setf *tf-idf-list* tf-idf-list
         *word-hash* word-hash)))

;;; Make sparse tensor from sparse-vector list

(defparameter X-shape (list (length *livedoor-data-files*) ; 7367
                            (hash-table-count *word-hash*) ; 18372
                            ))

;; Maximum possible count of words over whole documents
(apply #'* X-shape) ; => 135346524

;; Count of words that actually appear
(defparameter n-non-zero
  (loop for sv in *tf-idf-list* sum (clol.vector:sparse-vector-length sv)))
;; => 1373575/135346524 (density: 1%)

(defparameter X-indices-matrix
  ;; Number of non-zero elements x Number of mode
  (make-array (list n-non-zero 2)
              :element-type 'fixnum))

(defparameter X-value-vector
  (make-array n-non-zero :element-type 'single-float))

(let ((nz-index 0))
  (loop for sparse-vec in *tf-idf-list*
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
