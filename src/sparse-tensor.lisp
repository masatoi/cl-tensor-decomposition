(defpackage :cltd.tensor
  (:use :cl)
  (:export
   :tensor :make-tensor :tensor-p :copy-tensor :tref
   :tensor-size :tensor-hash-table :tensor-default-value))

(in-package :cltd.tensor)

(defstruct (tensor (:constructor %make-tensor)
                   (:print-object %print-tensor))
  size index-table value-matrix default-value)

(defun %print-tensor (obj stream)
  (format stream "#S(TENSOR~T:SIZE ~A)"
          (tensor-size obj)))

(defun make-tensor (size-list &key (default-value 0d0))
  (assert (and (listp size-list)
               (every #'integerp size-list)))
  (%make-tensor :size size-list
                :index-table (make-hash-table :test 'equal)
                :default-value default-value))

(defun tref (tensor &rest subscripts)
  (assert (= (length (tensor-size tensor)) (length subscripts)))
  (assert (every #'integerp subscripts))
  (assert (every (lambda (size-dim subscript)
                   (<= 0 subscript (1- size-dim)))
                 (tensor-size tensor)
                 subscripts))
  (gethash subscripts
           (tensor-hash-table tensor)
           (tensor-default-value tensor)))

(defun (setf tref) (new-value tensor &rest subscripts)
  (assert (= (length (tensor-size tensor)) (length subscripts)))
  (assert (every #'integerp subscripts))
  (assert (every (lambda (size-dim subscript)
                   (<= 0 subscript (1- size-dim)))
                 (tensor-size tensor)
                 subscripts))
  (setf
   (gethash subscripts
            (tensor-hash-table tensor)
            (tensor-default-value tensor))
   new-value)
  new-value)

(defparameter sparse-tensor1 (make-tensor '(2 2)))
(setf (tref sparse-tensor1 0 0) 1d0)
(setf (tref sparse-tensor1 1 1) 2d0)

(defparameter dence-tensor1 (make-array '(2 2) :element-type 'double-float
                                               :initial-contents '((1d0 2d0)
                                                                   (3d0 4d0))))

(defun multiply-sparse-and-dence-tensor (sparse-tensor dence-tensor)
  (maphash (lambda (indices val)
             
  )
;; => sparse-tensor (if the element became 0 as a result of multiplication, delete it from hash-teble)


(defun indices= (x y)
  ;; (declare (optimize (speed 3) (safety 0))
  ;;          (type (simple-array fixnum) x y))
  (loop for i fixnum from 0 below (array-dimension x 0) do
    (when (not (= (aref x i) (aref y i)))
      (return-from indices= nil)))
  t)

;; indices: #(0 0 1), sxhash-indices: 1193941380939622490
;; indices: #(0 1 0), sxhash-indices: 1193941380939622490

(defun sxhash-indices (indices)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) indices))
  (let ((result (sxhash (aref indices 0))))
    (declare (type (UNSIGNED-BYTE 62) result))
    (loop for i fixnum from 1 below (length indices) do
      (setf result (logxor result (* (sxhash (aref indices i)) 31))))
    result))

(let ((x (make-array 3 :element-type 'fixnum :initial-contents '(0 0 1))))
  (sxhash-indices x))

(let ((x (make-array 3 :element-type 'fixnum :initial-contents '(0 1 0))))
  (sxhash-indices x))

(sb-ext:define-hash-table-test indices=
    (lambda (x)
      (sxhash-indices x)))

(defparameter indices-table (make-hash-table :test 'indices=))

(let ((x (make-array 3 :element-type 'fixnum :initial-contents '(0 1 2))))
  (print (sxhash-indices x))
  (setf (gethash x indices-table) 11d0)
  (setf (aref x 0) 0
        (aref x 1) 0
        (aref x 2) 0)
  (print (sxhash-indices x))
  (gethash x indices-table))

(let ((x (make-array 3 :element-type 'fixnum :initial-contents '(0 1 2))))
  (gethash x indices-table))

;; equal版ハッシュテーブルとどれだけの速度差があるか

(defparameter sparse-tensor1 (make-tensor '(100 100 100)))

(time
 (loop for i from 0 below 100 do
   (loop for j from 0 below 100 do
     (loop for k from 0 below 100 do
       (setf (tref sparse-tensor1 i j k) (random 1d0))))))

;; Evaluation took:
;;   0.733 seconds of real time
;;   0.733441 seconds of total run time (0.626131 user, 0.107310 system)
;;   [ Run times consist of 0.111 seconds GC time, and 0.623 seconds non-GC time. ]
;;   100.00% CPU
;;   1,460,461,767 processor cycles
;;   211,887,856 bytes consed

(time
 (loop for i from 0 below 100 do
   (loop for j from 0 below 100 do
     (loop for k from 0 below 100 do
       (tref sparse-tensor1 i j k)))))

;; Evaluation took:
;;   0.647 seconds of real time
;;   0.647366 seconds of total run time (0.631370 user, 0.015996 system)
;;   100.00% CPU
;;   1,289,559,352 processor cycles
;;   112,006,448 bytes consed

(defparameter sparse-tensor2 (make-tensor '(100 100 100) :test 'indices=))

(let ((indices (make-array 3 :element-type 'fixnum)))
  (setf (aref indices 0) 0
        (aref indices 1) 0
        (aref indices 2) 0)
  (print (sxhash-indices indices))
  (setf (gethash indices (tensor-hash-table sparse-tensor2)) (random 1d0))
  (setf (aref indices 0) 1
        (aref indices 1) 2
        (aref indices 2) 3)
  (print (sxhash-indices indices))
  (setf (gethash indices (tensor-hash-table sparse-tensor2)) (random 1d0)))

(let ((indices (make-array 3 :element-type 'fixnum)))
  (setf (aref indices 0) 0
        (aref indices 1) 0
        (aref indices 2) 0)
  (print (sxhash-indices indices))
  (gethash indices (tensor-hash-table sparse-tensor2))
  (setf (aref indices 0) 1
        (aref indices 1) 2
        (aref indices 2) 3)
  (print (sxhash-indices indices))
  (gethash indices (tensor-hash-table sparse-tensor2)))

(time
 (let ((indices (make-array 3 :element-type 'fixnum)))
   (loop for i fixnum from 0 below 3 do
     (loop for j fixnum from 0 below 3 do
       (loop for k fixnum from 0 below 3 do
         (setf (aref indices 0) i
               (aref indices 1) j
               (aref indices 2) k)
         (format t "indices: ~A, sxhash-indices: ~A~%" indices (sxhash-indices indices))
         (setf (gethash indices (tensor-hash-table sparse-tensor2)) (random 1d0)))))))

(let ((indices (make-array 3 :element-type 'fixnum)))
   (loop for i fixnum from 0 below 3 do
     (loop for j fixnum from 0 below 3 do
       (loop for k fixnum from 0 below 3 do
         (setf (aref indices 0) i
               (aref indices 1) j
               (aref indices 2) k)
         (print (sxhash-indices indices))
         (print (gethash indices (tensor-hash-table sparse-tensor2)))))))

(let ((indices (make-array 3 :element-type 'fixnum :initial-contents '(0 0 0))))
  (gethash indices (tensor-hash-table sparse-tensor2)))


(defparameter sparse-tensor3 (make-tensor '(100) :test 'indices=))

(time
 (let ((indices (make-array 1 :element-type 'fixnum)))
   (loop for i fixnum from 0 below 3 do
     (setf (aref indices 0) i)
     (format t "indices: ~A, sxhash-indices: ~A~%" indices (sxhash-indices indices))
     (setf (gethash indices (tensor-hash-table sparse-tensor3)) (random 1d0)))))

(let ((indices (make-array 1 :element-type 'fixnum)))
  (loop for i fixnum from 0 below 3 do
    (setf (aref indices 0) i)
    (print (sxhash-indices indices))
    (print (gethash indices (tensor-hash-table sparse-tensor3)))))


(defparameter sparse-tensor3 (make-tensor '(100)))
(defparameter indices (make-array 1 :element-type 'fixnum :initial-element 3))
(setf (gethash indices (tensor-hash-table sparse-tensor3)) (random 1d0))
(gethash indices (tensor-hash-table sparse-tensor3))

(loop for i fixnum from 0 below 3 do
  (let ((rand (random 1d0)))
    (setf (aref indices 0) i)
    (format t "indices: ~A, sxhash-indices: ~A, random: ~A~%"
          indices
          (sxhash-indices indices)
          rand)
  (setf (gethash indices (tensor-hash-table sparse-tensor3)) rand)))

(loop for i fixnum from 0 below 3 do
  (setf (aref indices 0) i)
  (format t "indices: ~A, sxhash-indices: ~A, hash-value: ~A~%"
          indices
          (sxhash-indices indices)
          (gethash indices (tensor-hash-table sparse-tensor3))))

(indices= indices indices2)





;; Evaluation took:
;;   0.038 seconds of real time
;;   0.038320 seconds of total run time (0.038320 user, 0.000000 system)
;;   100.00% CPU
;;   76,324,191 processor cycles
;;   16,023,552 bytes consed

(defparameter dence-tensor (make-array '(100 100 100) :element-type 'double-float))

(time
 (let ((indices (make-array 3 :element-type 'fixnum)))
   (loop for i from 0 below 100 do
     (loop for j from 0 below 100 do
       (loop for k from 0 below 100 do
         (setf (aref indices 0) i
               (aref indices 1) j
               (aref indices 2) k
               (aref dence-tensor i j k)
               (gethash indices (tensor-hash-table sparse-tensor2))))))))

;; Evaluation took:
;;   0.003 seconds of real time
;;   0.003292 seconds of total run time (0.002806 user, 0.000486 system)
;;   100.00% CPU
;;   6,554,228 processor cycles
;;   0 bytes consed

;;; equal比較のハッシュでインデックスのリストから1次元のインデックス番号を出す
;;; row: i j k value となるような行列を生成する(密)
;;; この行列を上から走査していけばOK

;;; ハッシュ: インデックスリスト->通しインデックス、value
;;; matrix: 通しインデックス、value (高速化のため)
;;; 通しインデックスはcountとして勝手に付く？

;;; とりあえずはequal比較のハッシュでテンソル分解が実装できることを確認し、高速化はそれから

;;; CSF2 (Compressed Sparse Fiber Two)
