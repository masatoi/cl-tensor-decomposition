;;; Coupon Purchase Prediction
;;; https://www.kaggle.com/c/coupon-purchase-prediction/overview

(ql:quickload '(:fare-csv :cl-tensor-decomposition))

(defpackage :coupon-purchase-prediction
  (:use :cl :cltd :fare-csv))

(in-package :coupon-purchase-prediction)

(setf *print-length* 100)

(defparameter *dataset-base-dir*
  (merge-pathnames #P"datasets/coupon-purchase-prediction/"
                   (truename #P"~/")))

;;; User

(defparameter *user-csv*
  (read-csv-file (merge-pathnames #P"user_list.csv" *dataset-base-dir*))
  "header: (\"REG_DATE\" \"SEX_ID\" \"AGE\" \"WITHDRAW_DATE\" \"PREF_NAME\" \"USER_ID_hash\")")

(defclass user ()
  ((sex :initarg :sex :accessor sex-of)
   (age :initarg :age :accessor age-of)
   (prefecture :initarg :prefecture :accessor prefecture-of)
   (user-id :initarg :user-id :accessor user-id-of)))

(defun make-user (line)
  (destructuring-bind (reg-date sex-id age withdraw-date pref-name user-id-hash)
      line
    (declare (ignorable reg-date withdraw-date))
    (make-instance 'user
                   :sex (cond ((equal sex-id "m") :male)
                              ((equal sex-id "f") :female)
                              (t (error "Invalid sex-id")))
                   :age (parse-integer age)
                   :prefecture (unless (equal pref-name "")
                                 pref-name)
                   :user-id user-id-hash)))

(defparameter *user-table* (make-hash-table :test 'equal))

(dolist (line (cdr *user-csv*))
  (let ((user (make-user line)))
    (setf (gethash (user-id-of user) *user-table*) user)))

#+(or)
(let (pref-list)
  (maphash (lambda (k v)
             k
             (let ((pref (prefecture-of v)))
               (when (and pref
                          (null (find pref pref-list :test #'equal)))
                 (push pref pref-list))))
           *user-table*)
  (nreverse pref-list))

(defparameter *pref-names*
  '("東京都" "愛知県" "神奈川県" "広島県" "埼玉県" "奈良県" "石川県" "大阪府" "熊本県" "福岡県"
    "北海道" "京都府" "秋田県" "千葉県" "長崎県" "兵庫県" "沖縄県" "三重県" "茨城県" "鹿児島県"
    "宮城県" "静岡県" "和歌山県" "長野県" "岡山県" "栃木県" "滋賀県" "富山県" "佐賀県" "宮崎県"
    "岩手県" "新潟県" "大分県" "山口県" "岐阜県" "群馬県" "福島県" "愛媛県" "香川県" "山梨県"
    "高知県" "島根県" "徳島県" "福井県" "青森県" "山形県" "鳥取県"))

(defparameter *pref-index*
  (make-hash-table :test #'equal))

(setf (gethash nil *pref-index*) 0)
(loop for pref in *pref-names*
      for i from 1 to (length *pref-names*)
      do (setf (gethash pref *pref-index*) i))

;;; Coupon

(defparameter *coupon-csv*
  (read-csv-file (merge-pathnames #P"coupon_list_train.csv" *dataset-base-dir*))
  "header: (\"CAPSULE_TEXT\" \"GENRE_NAME\" \"PRICE_RATE\" \"CATALOG_PRICE\" \"DISCOUNT_PRICE\"
 \"DISPFROM\" \"DISPEND\" \"DISPPERIOD\" \"VALIDFROM\" \"VALIDEND\" \"VALIDPERIOD\"
 \"USABLE_DATE_MON\" \"USABLE_DATE_TUE\" \"USABLE_DATE_WED\" \"USABLE_DATE_THU\"
 \"USABLE_DATE_FRI\" \"USABLE_DATE_SAT\" \"USABLE_DATE_SUN\" \"USABLE_DATE_HOLIDAY\"
 \"USABLE_DATE_BEFORE_HOLIDAY\" \"large_area_name\" \"ken_name\" \"small_area_name\"
 \"COUPON_ID_hash\")")

(defclass coupon ()
  ((genre-name :initarg :genre-name :accessor genre-name-of)
   (price-rate :initarg :price-rate :accessor price-rate-of)
   (large-area-name :initarg :large-area-name :accessor large-area-name-of)
   (ken-name :initarg :ken-name :accessor ken-name-of)
   (small-area-name :initarg :small-area-name :accessor small-area-name-of)
   (coupon-id :initarg :coupon-id :accessor coupon-id-of)))

(defun make-coupon (line)
  (destructuring-bind (capsule-text genre-name price-rate catalog-price discount-price
                       dispfrom dispend dispperiod validfrom validend validperiod
                       usable-date-mon usable-date-tue usable-date-wed usable-date-thu
                       usable-date-fri usable-date-sat usable-date-sun usable-date-holiday
                       usable-date-before-holiday large-area-name ken-name small-area-name
                       coupon-id-hash)
      line
    (declare (ignorable capsule-text catalog-price discount-price dispfrom dispend dispperiod
                        validfrom validend validperiod usable-date-mon usable-date-tue usable-date-wed
                        usable-date-thu usable-date-fri usable-date-sat usable-date-sun
                        usable-date-holiday usable-date-before-holiday))
    (make-instance 'coupon
                   :genre-name genre-name
                   :price-rate (parse-integer price-rate)
                   :large-area-name large-area-name
                   :ken-name ken-name
                   :small-area-name small-area-name
                   :coupon-id coupon-id-hash)))

(defparameter *coupon-table* (make-hash-table :test 'equal))

(dolist (line (cdr *coupon-csv*))
  (let ((coupon (make-coupon line)))
    (setf (gethash (coupon-id-of coupon) *coupon-table*) coupon)))

#+(or)
(let (genre-list)
  (maphash (lambda (k v)
             k
             (let ((genre (genre-name-of v)))
               (unless (find genre genre-list :test #'equal)
                 (push genre genre-list))))
           *coupon-table*)
  (nreverse genre-list))

(defparameter *genre-names*
  '("グルメ" "ヘアサロン" "エステ" "リラクゼーション" "ビューティー" "ネイル・アイ" "宅配" "レッスン"
    "ギフトカード" "その他のクーポン" "レジャー" "ホテル・旅館" "健康・医療"))

(defparameter *genre-index*
  (make-hash-table :test #'equal))

(loop for genre in *genre-names*
      for i from 0 below (length *genre-names*)
      do (setf (gethash genre *genre-index*) i))

;;; Visit

(time
 (defparameter *visit-csv*
   (read-csv-file (merge-pathnames #P"coupon_visit_train.csv" *dataset-base-dir*))
   "header: (\"PURCHASE_FLG\" \"I_DATE\" \"PAGE_SERIAL\" \"REFERRER_hash\" \"VIEW_COUPON_ID_hash\"
 \"USER_ID_hash\" \"SESSION_ID_hash\" \"PURCHASEID_hash\")"))

;; Evaluation took:
;;   32.735 seconds of real time
;;   32.735028 seconds of total run time (31.195912 user, 1.539116 system)
;;   [ Run times consist of 2.815 seconds GC time, and 29.921 seconds non-GC time. ]
;;   100.00% CPU
;;   124,395,548,242 processor cycles
;;   4,140,777,312 bytes consed

(defclass visit ()
  ((purchase-p :initarg :purchase-p :accessor purchase-p)
   (user :initarg :user :accessor user-of)
   (coupon :initarg :coupon :accessor coupon-of)
   (purchase-id :initarg :purchase-id :accessor purchase-id-of)))

(define-condition user-not-found (simple-error)
  ())

(define-condition coupon-not-found (simple-error)
  ())

(defun make-visit (line)
  (destructuring-bind (purchase-flg i-date page-serial referrer-hash view-coupon-id-hash
                       user-id-hash session-id-hash purchaseid-hash)
      line
    (declare (ignorable i-date page-serial referrer-hash session-id-hash))
    (make-instance 'visit
                   :purchase-p (cond ((string= purchase-flg "0") nil)
                                     ((string= purchase-flg "1") t)
                                     (t (error "Invalid purchase-flg")))
                   :user (or (gethash user-id-hash *user-table*)
                             (error 'user-not-found))
                   :coupon (or (gethash view-coupon-id-hash *coupon-table*)
                               (error 'coupon-not-found))
                   :purchase-id purchaseid-hash)))

;;; Construct Sparse Tensor

(defparameter *X-shape* '(2 2 10 48 13 10 48))

(defun convert-tensor-indices (line)
  (let* ((visit (make-visit line))
         (user (user-of visit))
         (coupon (coupon-of visit))
         (purchase-i (if (purchase-p visit) 0 1)) ; 2
         (sex-i (if (eq (sex-of user) :male) 0 1)) ; 2
         (age-i (min 9 (floor (age-of user) 10))) ; 10
         (user-pref-i (gethash (prefecture-of user) *pref-index*)) ; 48
         (genre-i (gethash (genre-name-of coupon) *genre-index*)) ; 13
         (price-rate-i (min 9 (floor (price-rate-of coupon) 10))) ; 10
         (coupon-pref-i (gethash (ken-name-of coupon) *pref-index*))) ; 48
    (list purchase-i sex-i age-i user-pref-i genre-i price-rate-i coupon-pref-i)))

(defparameter *indices-table-for-count-dence-elem* (make-hash-table :test #'equal))

(time
 (dolist (line (cdr *visit-csv*))
   (handler-case
       (let ((indices (convert-tensor-indices line)))
         (setf (gethash indices *indices-table-for-count-dence-elem*)
               (if (gethash indices *indices-table-for-count-dence-elem*)
                   (1+ (gethash indices *indices-table-for-count-dence-elem*))
                   1)))
     (user-not-found (e) e)
     (coupon-not-found (e) e))))

(defparameter *number-of-non-zero-elements* (hash-table-count *indices-table-for-count-dence-elem*))

(defparameter *X-indices-matrix*
  (make-array (list *number-of-non-zero-elements*
                    (length *X-shape*))
              :element-type 'fixnum
              :initial-element 0))

(defparameter *X-value-vector*
  (make-array *number-of-non-zero-elements*
              :element-type 'single-float
              :initial-element 0.0))

(defun set-row (array i row)
  (loop for j from 0
        for elem in row
        do (setf (aref array i j) elem)))

(time
 (let ((i 0))
   (maphash (lambda (indices cnt)
              (set-row *X-indices-matrix* i indices)
              (setf (aref *x-value-vector* i) (* cnt 1.0))
              (incf i))
            *indices-table-for-count-dence-elem*)))

;;; Run

(defparameter *result*
  (decomposition *X-shape* *X-indices-matrix* *X-value-vector* :n-cycle 2000 :R 10 :verbose t))

;;; Visualize

(defun report (result)
  (let* ((indent 4)
         (rank (array-dimension (aref *result* 0) 1))
         (label-features '("purchase" "sex" "age" "user-pref" "genre" "price-rate" "coupon-pref"))
         (one-tenth '("0-10" "10-20" "20-30" "30-40" "40-50" "50-60" "60-70" "70-80" "80-90" "90-100"))
         (label-values `(("purchase" "not-purchase")
                         ("male" "female")
                         ,one-tenth
                         ("none" ,@*pref-names*)
                         ,*genre-names*
                         ,one-tenth
                         ("none" ,@*pref-names*))))
    (dotimes (r rank)
      (format t "~%=== rank=~A ==============================================~%" r)
      (loop for feature-name in label-features
            for values-name in label-values
            for i from 0
            do (format t "~%[~A, rank=~A]~%" feature-name r)
               (loop for pair in (ranking values-name (aref result i) r)
                     for i from 0 below 10
                     do (format t "~VT~A~vT~A~%" indent (car pair) 20 (cdr pair)))))))

(report *result*)
