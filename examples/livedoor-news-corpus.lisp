(ql:quickload :cl-docclass)

(defpackage :livedoor-news-corpus
  (:use :cl :cltd :cl-docclass :wiz-util))

(in-package :livedoor-news-corpus)

(setf *print-length* 100)

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

(defparameter *word-hash* (make-hash-table :test 'equal))

(time
 (dolist (file *livedoor-data-files*)
   (docclass::add-words-to-hash-from-file! file *word-hash*)))
;; 6.147 seconds of real time

;; Removes infrequent words
(setf *word-hash* (docclass::remove-infrequent-words *word-hash* 10))

;;; Make input data (List of sparse vectors)

(defparameter tf-idf-list
  (docclass::make-tf-idf-list-from-files *livedoor-data-files* *word-hash*))

;;; Make sparse tensor from sparse-vector list

(defparameter X-shape '(7367 18372))

(defparameter n-non-zero
  (loop for sv in tf-idf-list sum (clol.vector:sparse-vector-length sv)))
;; => 1373575 (density: 1%)

(defparameter X-indices-matrix
  ;; Number of non-zero elements x Number of mode
  (make-array (list n-non-zero 2)
              :element-type 'fixnum))

(defparameter X-value-vector
  (make-array n-non-zero :element-type 'double-float))

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

(time (defparameter factor-matrix-vector
        (decomposition X-shape X-indices-matrix X-value-vector :n-cycle 1000 :R 20 :verbose t)))
