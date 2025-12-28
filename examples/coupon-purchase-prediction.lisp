;;; Coupon Purchase Prediction
;;; https://www.kaggle.com/c/coupon-purchase-prediction/overview

(ql:quickload '(:fare-csv :cl-tensor-decomposition :cl-json))

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
              :element-type 'double-float
              :initial-element 0d0))

(defun set-row (array i row)
  (loop for j from 0
        for elem in row
        do (setf (aref array i j) elem)))

(time
 (let ((i 0))
   (maphash (lambda (indices cnt)
              (set-row *X-indices-matrix* i indices)
              (setf (aref *x-value-vector* i) (* cnt 1.0d0))
              (incf i))
            *indices-table-for-count-dence-elem*)))

;;; Run

;; Create sparse tensor
(defparameter *X-tensor*
  (cltd:make-sparse-tensor *X-shape* *X-indices-matrix* *X-value-vector*))

(defparameter *result*
  (cltd:decomposition *X-tensor*
                 :n-cycle 2000
                 :R 20
                 :verbose t
                 :convergence-threshold 1d-5
                 :convergence-window 100))

(defparameter *age-buckets*
  '("0-10" "10-20" "20-30" "30-40" "40-50" "50-60" "60-70" "70-80" "80-90" "90-100"))

(defparameter *prefecture-labels*
  (cons "none" *pref-names*))

(defparameter *coupon-mode-metadata*
  (list (cltd:make-mode-metadata :purchase '("purchase" "not_purchase")
                                 :role :purchase
                                 :positive-label "purchase"
                                 :negative-label "not_purchase"
                                 :discretization "binary")
        (cltd:make-mode-metadata "sex" '("male" "female")
                                 :discretization "binary")
        (cltd:make-mode-metadata "age" *age-buckets*
                                 :discretization "decade")
        (cltd:make-mode-metadata "user_prefecture" *prefecture-labels*
                                 :discretization "prefecture")
        (cltd:make-mode-metadata "genre" *genre-names*
                                 :discretization "genre catalog")
        (cltd:make-mode-metadata "price_rate" *age-buckets*
                                 :discretization "decile")
        (cltd:make-mode-metadata "coupon_prefecture" *prefecture-labels*
                                 :discretization "prefecture")))

;;; Visualize

(defun report (result)
  (let* ((indent 4)
         (rank (array-dimension (aref result 0) 1))
         (label-features '("purchase" "sex" "age" "user-pref" "genre" "price-rate" "coupon-pref"))
         (label-values `(("purchase" "not-purchase")
                         ("male" "female")
                         ,*age-buckets*
                         ,*prefecture-labels*
                         ,*genre-names*
                         ,*age-buckets*
                         ,*prefecture-labels*)))
    (dotimes (r rank)
      (format t "~%=== rank=~A ==============================================~%" r)
      (loop for feature-index from 0
            for feature-name in label-features
            for labels in label-values do
        (format t "~%[~A, rank=~A]~%" feature-name r)
        (loop for pair in (ranking labels (aref result feature-index) r)
              for i from 0 below 10
              do (format t "~VT~A~V@T~,6F~%" indent (car pair) (+ indent 22) (cdr pair)))))))

;;; Model selection example -------------------------------------------------

(defun cross-validate-rank-example (&key (ranks '(10 15 20)) (k 3) (n-cycle 2000))
  "Run cross-validation on the coupon tensor and return the best rank entry and all fold scores."
  (let ((random-state (make-random-state t)))
    (multiple-value-bind (best results)
        (cltd:select-rank *X-indices-matrix* *X-value-vector* ranks
                          :k k
                          :n-cycle n-cycle
                          :random-state random-state
                          :convergence-threshold 1d-5
                          :convergence-window 10)
      (format t "Best rank ~A with mean KL ~,6F~%"
              (cdr (assoc :rank best))
              (cdr (assoc :mean best)))
      (values best results))))

(cross-validate-rank-example)

;;; JSON export example -----------------------------------------------------

(defun export-factor-cards-as-json (factor-matrix-vector &key (json-path #P"factor_cards.json") (report-path #P"factor_report.md"))
  "Generate scenario cards for FACTOR-MATRIX-VECTOR and export them to JSON and Markdown."
  (cltd:generate-report-artifacts factor-matrix-vector
                                  *X-indices-matrix*
                                  *X-value-vector*
                                  *coupon-mode-metadata*
                                  :factor-json-path json-path
                                  :report-path report-path
                                  :json-serializer (lambda (cards stream)
                                                     (write-string (cl-json:encode-json-to-string cards)
                                                                   stream)))
  (format t "Wrote ~A and ~A~%" json-path report-path)
  (values json-path report-path))

(report *result*)

(export-factor-cards-as-json *result* :json-path #P"/tmp/factor_cards.json" :report-path #P"/tmp/factor_report.md")

#|
=== rank=0 ==============================================

[purchase, rank=0]
    not-purchase    3.079002
    purchase        0.4349609

[sex, rank=0]
    female          0.8010475
    male            0.7593578

[age, rank=0]
    40-50           1.1960298
    30-40           1.1132455
    50-60           0.82149446
    20-30           0.40952933
    60-70           0.34040558
    70-80           0.05580489
    10-20           0.011177097
    80-90           8.082997e-4
    0-10            0.0
    90-100          0.0

[user-pref, rank=0]
    none            3.3347363
    東京都             1.7158562
    大阪府             1.4404967
    神奈川県            1.0824491
    兵庫県             0.7333966
    埼玉県             0.5882475
    愛知県             0.56628925
    千葉県             0.56172997
    福岡県             0.538736
    北海道             0.4781248

[genre, rank=0]
    その他のクーポン        2.1869242
    ギフトカード          1.4713569
    エステ             0.01519537
    健康・医療           0.011658345
    レッスン            0.008417825
    リラクゼーション        0.001623578
    ネイル・アイ          8.6628273e-4
    宅配              8.886518e-6
    グルメ             2.0610418e-27
    レジャー            1.4012985e-45

[price-rate, rank=0]
    90-100          1.625797
    80-90           1.0135443
    50-60           0.66415846
    60-70           0.31206876
    70-80           0.24187072
    30-40           0.03620361
    40-50           0.022707935
    0-10            0.0
    10-20           0.0
    20-30           0.0

[coupon-pref, rank=0]
    東京都             31.748325
    大阪府             0.3641074
    島根県             0.04550483
    静岡県             0.023730047
    栃木県             0.003693065
    新潟県             0.0031939363
    神奈川県            1.2296704e-24
    石川県             2.0197024e-36
    京都府             8.332121e-42
    三重県             2.2981295e-43

=== rank=1 ==============================================

[purchase, rank=1]
    not-purchase    2.981369
    purchase        0.063503616

[sex, rank=1]
    male            0.8370523
    female          0.68383723

[age, rank=1]
    50-60           0.9601833
    40-50           0.8527379
    60-70           0.5729621
    30-40           0.538319
    20-30           0.15644422
    70-80           0.09121506
    80-90           0.005522284
    10-20           1.5285138e-4
    0-10            0.0
    90-100          0.0

[user-pref, rank=1]
    東京都             4.0597935
    none            3.0463896
    神奈川県            2.419627
    埼玉県             1.8818496
    千葉県             1.3586315
    静岡県             0.9062907
    愛知県             0.8147225
    宮城県             0.4904375
    群馬県             0.48673418
    長野県             0.46298975

[genre, rank=1]
    ホテル・旅館          6.314542
    レジャー            0.70662194
    宅配              0.054441355
    その他のクーポン        0.012927354
    リラクゼーション        0.01164182
    グルメ             0.004965934
    ビューティー          0.0013512169
    ヘアサロン           1.4012985e-45
    エステ             1.4012985e-45
    ネイル・アイ          0.0

[price-rate, rank=1]
    50-60           3.6478288
    60-70           0.12170476
    30-40           0.0065523405
    70-80           4.3856901e-10
    0-10            0.0
    10-20           0.0
    20-30           0.0
    40-50           0.0
    80-90           0.0
    90-100          0.0

[coupon-pref, rank=1]
    静岡県             6.2670283
    長野県             4.7565427
    東京都             2.4700801
    栃木県             1.9108007
    群馬県             1.880465
    神奈川県            1.8226742
    千葉県             1.6661019
    山梨県             1.4967185
    岩手県             0.83617646
    福島県             0.83594805

=== rank=2 ==============================================

[purchase, rank=2]
    not-purchase    0.4103063
    purchase        0.027932255

[sex, rank=2]
    female          0.18003671
    male            0.10347374

[age, rank=2]
    40-50           3.5012171
    30-40           3.0318954
    50-60           2.517917
    60-70           0.87232393
    20-30           0.78525645
    70-80           0.11174877
    10-20           0.004670175
    0-10            0.0
    80-90           0.0
    90-100          0.0

[user-pref, rank=2]
    none            4.8840437
    東京都             3.6206143
    大阪府             2.7058198
    神奈川県            2.26269
    兵庫県             1.4432745
    福岡県             1.3929698
    千葉県             1.1997132
    埼玉県             1.0718876
    愛知県             0.9855538
    広島県             0.6275783

[genre, rank=2]
    その他のクーポン        5.8857937
    宅配              0.6704814
    ホテル・旅館          0.26240727
    レッスン            0.15769851
    ヘアサロン           0.1529289
    リラクゼーション        0.1426837
    ネイル・アイ          0.03172847
    レジャー            0.02809983
    エステ             0.0065976
    ビューティー          0.0026236298

[price-rate, rank=2]
    60-70           5.435297
    50-60           0.73237467
    30-40           0.08306761
    40-50           0.053853106
    70-80           0.0023165725
    80-90           1.79886e-5
    0-10            0.0
    10-20           0.0
    20-30           0.0
    90-100          0.0

[coupon-pref, rank=2]
    東京都             5.2416987
    大阪府             2.8708909
    広島県             2.6545663
    北海道             2.628253
    千葉県             2.373766
    福岡県             1.0658408
    新潟県             0.8867933
    神奈川県            0.5243124
    愛知県             0.3890164
    兵庫県             0.34138647

=== rank=3 ==============================================

[purchase, rank=3]
    not-purchase    2.5926776
    purchase        0.12446049

[sex, rank=3]
    male            0.20536199
    female          0.17572413

[age, rank=3]
    40-50           0.8292033
    50-60           0.81039244
    30-40           0.44913006
    60-70           0.38886347
    20-30           0.17468628
    70-80           0.053830404
    10-20           0.0018016668
    80-90           1.6614732e-4
    0-10            0.0
    90-100          0.0

[user-pref, rank=3]
    神奈川県            10.824339
    東京都             5.437464
    none            5.2887416
    埼玉県             1.7494075
    千葉県             1.5446281
    静岡県             0.60542023
    群馬県             0.2901937
    大阪府             0.2261047
    兵庫県             0.22398376
    茨城県             0.22045036

[genre, rank=3]
    グルメ             3.7718928
    レジャー            1.1240114
    ホテル・旅館          0.8074714
    リラクゼーション        0.14771678
    ギフトカード          0.10080114
    宅配              0.07470724
    その他のクーポン        0.04474916
    健康・医療           0.034750346
    ヘアサロン           0.023969376
    レッスン            0.0048777694

[price-rate, rank=3]
    50-60           4.0969605
    60-70           0.9022512
    40-50           0.010152349
    70-80           0.008844246
    30-40           0.0031867216
    0-10            0.0
    10-20           0.0
    20-30           0.0
    80-90           0.0
    90-100          0.0

[coupon-pref, rank=3]
    神奈川県            47.203316
    沖縄県             0.68828493
    東京都             0.5175229
    大阪府             0.4128029
    埼玉県             0.39717573
    愛知県             0.31403616
    愛媛県             0.10012953
    茨城県             0.09563994
    新潟県             0.08544626
    京都府             0.07975815

=== rank=4 ==============================================

[purchase, rank=4]
    not-purchase    3.45811
    purchase        0.14934127

[sex, rank=4]
    female          0.6346612
    male            0.6333355

[age, rank=4]
    40-50           1.5802447
    50-60           1.326805
    30-40           0.8808782
    60-70           0.6420881
    20-30           0.1963959
    70-80           0.08858847
    10-20           0.003437625
    80-90           2.8015268e-19
    0-10            0.0
    90-100          0.0

[user-pref, rank=4]
    大阪府             6.886573
    none            3.0774248
    兵庫県             2.803658
    京都府             0.9922955
    奈良県             0.8324789
    滋賀県             0.34504706
    徳島県             0.13421893
    愛知県             0.11025647
    和歌山県            0.10932996
    三重県             0.103030704

[genre, rank=4]
    グルメ             3.842646
    レジャー            0.233377
    ホテル・旅館          0.110126644
    リラクゼーション        0.04854976
    その他のクーポン        0.014972425
    ネイル・アイ          0.008124924
    ビューティー          0.0029659774
    ギフトカード          0.0013666553
    宅配              7.747373e-4
    エステ             6.8308075e-4

[price-rate, rank=4]
    50-60           4.685358
    60-70           0.23432723
    70-80           0.031470004
    80-90           0.01772631
    40-50           0.0015895292
    0-10            0.0
    10-20           0.0
    20-30           0.0
    30-40           0.0
    90-100          0.0

[coupon-pref, rank=4]
    大阪府             20.622366
    兵庫県             4.4580116
    京都府             2.7461832
    愛知県             0.1396182
    滋賀県             0.12022638
    福岡県             0.05978644
    沖縄県             0.04700112
    奈良県             0.042678647
    北海道             0.0067649917
    和歌山県            0.006012747

=== rank=5 ==============================================

[purchase, rank=5]
    not-purchase    1.6127158
    purchase        0.10749078

[sex, rank=5]
    female          1.0881672
    male            0.62291217

[age, rank=5]
    40-50           1.364204
    50-60           1.2373405
    30-40           0.8801398
    60-70           0.3271085
    20-30           0.22839163
    70-80           0.044158284
    10-20           0.0033717526
    80-90           1.8004063e-4
    0-10            0.0
    90-100          0.0

[user-pref, rank=5]
    none            3.5559318
    東京都             2.6096544
    大阪府             2.0550156
    神奈川県            1.5344347
    兵庫県             1.0546242
    愛知県             1.052187
    千葉県             0.9005806
    福岡県             0.87186354
    埼玉県             0.81077796
    静岡県             0.4096967

[genre, rank=5]
    宅配              8.678864
    その他のクーポン        0.08887227
    レジャー            0.036425106
    ギフトカード          0.012095822
    エステ             0.0058339234
    ホテル・旅館          0.004856442
    リラクゼーション        3.198697e-8
    グルメ             2.0163602e-21
    レッスン            5.605194e-45
    ヘアサロン           1.4012985e-45

[price-rate, rank=5]
    50-60           3.9564214
    60-70           1.0311604
    30-40           0.018144736
    70-80           5.2151206e-17
    80-90           2.0054838e-17
    40-50           1.146038e-29
    90-100          4.4910214e-41
    0-10            0.0
    10-20           0.0
    20-30           0.0

[coupon-pref, rank=5]
    東京都             10.541425
    大阪府             5.754057
    埼玉県             3.6680355
    北海道             2.3592927
    兵庫県             2.2619767
    神奈川県            2.173582
    宮城県             1.5161151
    福岡県             1.4258493
    愛知県             1.1175853
    京都府             0.7905531

=== rank=6 ==============================================

[purchase, rank=6]
    not-purchase    2.1213174
    purchase        0.17557292

[sex, rank=6]
    male            0.5227407
    female          1.7329959e-6

[age, rank=6]
    60-70           2.079369
    50-60           0.5097737
    70-80           0.48312253
    30-40           0.04991501
    20-30           0.0146722905
    80-90           0.00655239
    10-20           0.0025539733
    40-50           7.0082556e-6
    0-10            0.0
    90-100          0.0

[user-pref, rank=6]
    none            2.7296784
    神奈川県            1.9996988
    東京都             1.979251
    兵庫県             1.4069945
    大阪府             1.2635912
    埼玉県             1.168914
    福岡県             1.0164199
    愛知県             0.9389229
    千葉県             0.869542
    静岡県             0.65171623

[genre, rank=6]
    宅配              3.1982136
    ホテル・旅館          0.8516569
    グルメ             0.29987502
    その他のクーポン        0.19710779
    レジャー            0.024832645
    ギフトカード          0.020405164
    エステ             0.0037570328
    リラクゼーション        0.002499126
    レッスン            0.0015697235
    ヘアサロン           3.924812e-6

[price-rate, rank=6]
    50-60           2.9025202
    60-70           0.8593236
    70-80           0.40875667
    80-90           0.05988241
    90-100          0.013171699
    30-40           0.010466184
    40-50           0.0057237335
    10-20           0.0013635228
    0-10            2.5394334e-31
    20-30           0.0

[coupon-pref, rank=6]
    東京都             14.301531
    大阪府             4.2522216
    神奈川県            3.485111
    北海道             2.7612596
    兵庫県             2.7073684
    埼玉県             2.1954806
    福岡県             1.7615194
    宮城県             1.0213281
    愛知県             0.9846245
    京都府             0.7492894

=== rank=7 ==============================================

[purchase, rank=7]
    not-purchase    1.8675365
    purchase        0.1026561

[sex, rank=7]
    female          0.7229661
    male            0.20527883

[age, rank=7]
    40-50           0.8961519
    30-40           0.8091657
    50-60           0.50470257
    20-30           0.27545226
    60-70           0.11162681
    70-80           0.01661025
    10-20           0.0034852875
    0-10            0.0
    80-90           0.0
    90-100          0.0

[user-pref, rank=7]
    大阪府             6.7557974
    none            4.2355385
    兵庫県             2.7846448
    京都府             0.93310744
    奈良県             0.63229334
    滋賀県             0.12490229
    和歌山県            0.07999811
    岐阜県             0.044369124
    石川県             0.033913452
    三重県             0.02809794

[genre, rank=7]
    ヘアサロン           1.1176549
    エステ             0.5882067
    ネイル・アイ          0.4177279
    ギフトカード          0.37809876
    リラクゼーション        0.35210955
    ホテル・旅館          0.3099347
    レッスン            0.13658471
    宅配              0.083983965
    その他のクーポン        0.06864868
    グルメ             0.043233678

[price-rate, rank=7]
    60-70           1.9461124
    70-80           1.8797637
    50-60           0.87213105
    80-90           0.5239301
    90-100          0.42128184
    0-10            0.017737111
    10-20           0.0
    20-30           0.0
    30-40           0.0
    40-50           0.0

[coupon-pref, rank=7]
    大阪府             28.35483
    兵庫県             5.035502
    京都府             2.3022516
    奈良県             0.57309836
    愛知県             0.08778128
    石川県             0.031487163
    沖縄県             0.00883578
    島根県             0.008042143
    新潟県             0.0032959313
    栃木県             0.0018497795

=== rank=8 ==============================================

[purchase, rank=8]
    not-purchase    3.2986133
    purchase        0.1371167

[sex, rank=8]
    male            1.2524431
    female          0.58669347

[age, rank=8]
    50-60           2.2135477
    40-50           1.6354562
    60-70           1.1705741
    30-40           0.87919927
    20-30           0.33436808
    70-80           0.15940519
    10-20           0.0021482809
    80-90           7.5110974e-4
    0-10            0.0
    90-100          0.0

[user-pref, rank=8]
    東京都             9.0457325
    none            3.4241815
    神奈川県            3.0157397
    千葉県             2.232021
    埼玉県             2.1527436
    栃木県             0.37205023
    静岡県             0.33995098
    群馬県             0.31717426
    茨城県             0.28594273
    大阪府             0.19336629

[genre, rank=8]
    グルメ             4.4183807
    ホテル・旅館          0.21723093
    リラクゼーション        0.13933443
    ギフトカード          0.049644854
    レジャー            0.01940004
    ヘアサロン           0.009190674
    レッスン            0.009041174
    宅配              0.004508799
    ネイル・アイ          0.0030959805
    健康・医療           0.0019516915

[price-rate, rank=8]
    50-60           2.6583722
    60-70           0.41545764
    70-80           0.07500687
    20-30           0.010142136
    80-90           0.009156286
    10-20           0.002935366
    30-40           0.0019952247
    90-100          3.8838462e-4
    0-10            0.0
    40-50           0.0

[coupon-pref, rank=8]
    東京都             16.178572
    千葉県             0.2289457
    埼玉県             0.14525874
    京都府             0.03872085
    栃木県             0.031116966
    福岡県             0.011177518
    宮城県             0.009315974
    愛知県             0.007777589
    大阪府             0.0065482613
    神奈川県            0.0027912166

=== rank=9 ==============================================

[purchase, rank=9]
    not-purchase    0.7426836
    purchase        0.049230415

[sex, rank=9]
    female          0.2058064
    male            0.08193204

[age, rank=9]
    50-60           2.4393766
    40-50           2.3731654
    30-40           1.5109996
    60-70           0.643929
    20-30           0.35258776
    70-80           0.044536274
    10-20           0.0020583034
    80-90           2.157057e-36
    0-10            0.0
    90-100          0.0

[user-pref, rank=9]
    none            5.6600065
    東京都             3.2259004
    大阪府             2.4517326
    神奈川県            1.8760259
    福岡県             1.5814757
    兵庫県             1.4837201
    愛知県             1.3581933
    埼玉県             1.0422765
    千葉県             0.9995566
    北海道             0.6567021

[genre, rank=9]
    宅配              8.372576
    レッスン            0.51281303
    その他のクーポン        0.37510028
    ヘアサロン           0.18091217
    エステ             0.0329418
    レジャー            0.02091986
    ビューティー          0.01709746
    ネイル・アイ          0.013438089
    リラクゼーション        0.007901077
    グルメ             2.4894173e-12

[price-rate, rank=9]
    70-80           6.529599
    50-60           2.3560503
    60-70           0.8136956
    90-100          0.3791653
    30-40           0.059591617
    80-90           7.676405e-8
    40-50           6.5752087e-12
    0-10            2.6497498e-31
    10-20           5.3269225e-32
    20-30           0.0

[coupon-pref, rank=9]
    東京都             19.832232
    福岡県             2.9304652
    神奈川県            1.3808559
    北海道             0.75341946
    愛知県             0.7294333
    兵庫県             0.602459
    埼玉県             0.5288675
    香川県             0.3693826
    宮城県             0.31185704
    千葉県             0.19176215

=== rank=10 ==============================================

[purchase, rank=10]
    not-purchase    0.5227111
    purchase        0.031539924

[sex, rank=10]
    female          0.50448775
    male            0.33790812

[age, rank=10]
    40-50           0.92763764
    50-60           0.83827174
    30-40           0.7935095
    60-70           0.2882469
    20-30           0.22625127
    70-80           0.03233026
    10-20           0.0046464596
    80-90           9.7916345e-5
    0-10            0.0
    90-100          0.0

[user-pref, rank=10]
    none            2.82136
    東京都             2.130447
    大阪府             1.6665866
    神奈川県            1.1473355
    兵庫県             0.8453065
    千葉県             0.7311485
    福岡県             0.6473641
    埼玉県             0.64225507
    愛知県             0.6177079
    静岡県             0.32979757

[genre, rank=10]
    その他のクーポン        4.6957383
    レジャー            0.019294877
    宅配              1.856415e-4
    グルメ             2.0819155e-6
    エステ             2.1009123e-7
    リラクゼーション        3.3427925e-26
    レッスン            5.929663e-29
    ホテル・旅館          5.602069e-40
    ギフトカード          8.7313506e-41
    ビューティー          2.802597e-45

[price-rate, rank=10]
    50-60           3.5288363
    70-80           0.73247695
    80-90           0.18698665
    60-70           4.014965e-10
    30-40           8.946779e-24
    0-10            0.0
    10-20           0.0
    20-30           0.0
    40-50           0.0
    90-100          0.0

[coupon-pref, rank=10]
    大阪府             20.99673
    北海道             9.330911
    新潟県             9.167889
    東京都             8.523857
    愛知県             7.2928762
    滋賀県             5.8158264
    栃木県             4.9575067
    静岡県             4.233865
    福岡県             3.143096
    神奈川県            2.5456128

=== rank=11 ==============================================

[purchase, rank=11]
    not-purchase    2.368679
    purchase        0.04166739

[sex, rank=11]
    male            0.6780525
    female          0.64395624

[age, rank=11]
    50-60           0.83590883
    40-50           0.78497237
    30-40           0.50612044
    60-70           0.34175792
    20-30           0.15316452
    70-80           0.04597939
    10-20           2.8231437e-4
    80-90           1.9948898e-4
    0-10            0.0
    90-100          0.0

[user-pref, rank=11]
    none            2.3654706
    東京都             1.9189594
    大阪府             1.2443249
    神奈川県            1.0779135
    埼玉県             0.78415316
    千葉県             0.76596785
    愛知県             0.74045897
    兵庫県             0.6872939
    静岡県             0.34310248
    福岡県             0.3153428

[genre, rank=11]
    ホテル・旅館          7.0860157
    リラクゼーション        0.12893108
    レジャー            0.11435959
    レッスン            0.049735434
    ギフトカード          0.029619271
    ヘアサロン           0.0065508056
    その他のクーポン        0.00631949
    エステ             0.004619048
    ビューティー          2.3331918e-4
    宅配              2.2757003e-9

[price-rate, rank=11]
    60-70           1.9160668
    70-80           1.3227689
    50-60           0.78882194
    80-90           0.007618951
    30-40           2.1117438e-13
    0-10            0.0
    10-20           0.0
    20-30           0.0
    40-50           0.0
    90-100          0.0

[coupon-pref, rank=11]
    東京都             8.234593
    千葉県             5.4019246
    大阪府             4.69263
    沖縄県             2.9565864
    京都府             2.038615
    神奈川県            1.840804
    愛知県             0.95421684
    奈良県             0.49869052
    岐阜県             0.48182017
    長野県             0.46303374

=== rank=12 ==============================================

[purchase, rank=12]
    not-purchase    1.7790383
    purchase        0.08799789

[sex, rank=12]
    female          0.9192852
    male            0.13077

[age, rank=12]
    40-50           1.255884
    30-40           1.1320415
    50-60           0.7805519
    20-30           0.6919948
    60-70           0.14019884
    70-80           0.018964928
    10-20           0.0030333418
    0-10            0.0
    80-90           0.0
    90-100          0.0

[user-pref, rank=12]
    none            6.112432
    東京都             5.6847744
    神奈川県            2.0115228
    千葉県             1.3049008
    埼玉県             1.2211661
    愛知県             0.9804569
    静岡県             0.22441356
    茨城県             0.14815386
    栃木県             0.11393816
    山梨県             0.10184002

[genre, rank=12]
    ヘアサロン           1.1749167
    エステ             0.9213106
    ネイル・アイ          0.653298
    リラクゼーション        0.64976054
    レッスン            0.3967426
    宅配              0.3163204
    その他のクーポン        0.1250148
    健康・医療           0.05912288
    ビューティー          0.04423595
    レジャー            0.018880622

[price-rate, rank=12]
    70-80           1.7881105
    60-70           1.0765679
    80-90           0.56746215
    90-100          0.16117682
    50-60           0.05946502
    0-10            0.0
    10-20           0.0
    20-30           0.0
    30-40           0.0
    40-50           0.0

[coupon-pref, rank=12]
    東京都             27.445019
    愛知県             3.0111585
    神奈川県            3.0019023
    千葉県             1.2072629
    埼玉県             0.6553709
    大阪府             0.28160945
    北海道             0.24256286
    福岡県             0.18230008
    静岡県             0.15225294
    岐阜県             0.06377625

=== rank=13 ==============================================

[purchase, rank=13]
    not-purchase    0.95123076
    purchase        0.04885135

[sex, rank=13]
    male            0.68767154
    female          0.6414804

[age, rank=13]
    40-50           1.5596884
    50-60           1.4194573
    30-40           1.1559424
    60-70           0.5950353
    20-30           0.28952104
    10-20           5.8593036e-4
    70-80           4.0820253e-4
    0-10            0.0
    80-90           0.0
    90-100          0.0

[user-pref, rank=13]
    北海道             15.307106
    none            2.7543476
    愛知県             0.28000665
    東京都             0.24345791
    青森県             0.1354792
    神奈川県            0.12932745
    埼玉県             0.12839332
    大阪府             0.06865369
    宮城県             0.05637947
    岩手県             0.052394748

[genre, rank=13]
    グルメ             1.9483938
    ホテル・旅館          1.325881
    宅配              0.2372769
    レジャー            0.16682588
    リラクゼーション        0.120315194
    その他のクーポン        0.11254368
    ヘアサロン           0.09675026
    エステ             0.0690159
    ネイル・アイ          0.017901484
    レッスン            0.011181501

[price-rate, rank=13]
    50-60           2.6156569
    60-70           0.24996698
    70-80           0.10146539
    80-90           0.05404152
    90-100          0.012714724
    0-10            0.0
    10-20           0.0
    20-30           0.0
    30-40           0.0
    40-50           0.0

[coupon-pref, rank=13]
    北海道             21.880049
    東京都             1.1495992
    宮城県             0.07079647
    沖縄県             0.06174815
    岩手県             0.05447035
    福岡県             0.036069255
    大阪府             0.025307503
    広島県             0.017780438
    福島県             0.016662702
    山形県             0.016363015

=== rank=14 ==============================================

[purchase, rank=14]
    not-purchase    1.3526695
    purchase        0.04300598

[sex, rank=14]
    female          1.6264105
    male            0.2202676

[age, rank=14]
    40-50           3.2308183
    30-40           2.0489502
    50-60           1.5923942
    20-30           0.8654902
    60-70           0.0057975836
    10-20           0.0026954764
    70-80           3.6431285e-23
    0-10            0.0
    80-90           0.0
    90-100          0.0

[user-pref, rank=14]
    東京都             8.791859
    none            6.7478786
    神奈川県            2.6421974
    埼玉県             2.3592439
    千葉県             1.9441957
    栃木県             0.124795035
    兵庫県             0.12074614
    大阪府             0.115471676
    茨城県             0.08721318
    新潟県             0.077611335

[genre, rank=14]
    グルメ             4.2930503
    リラクゼーション        0.5824178
    宅配              0.47252098
    ホテル・旅館          0.43476334
    ネイル・アイ          0.32848996
    ギフトカード          0.13608065
    レッスン            0.13408977
    ヘアサロン           0.12813742
    その他のクーポン        0.124977954
    レジャー            0.07328253

[price-rate, rank=14]
    50-60           2.4674888
    60-70           0.5473085
    70-80           0.07248852
    80-90           0.0032009364
    30-40           0.0022681586
    90-100          0.0016217515
    10-20           1.8544165e-4
    20-30           2.9287865e-23
    0-10            0.0
    40-50           0.0

[coupon-pref, rank=14]
    東京都             13.484595
    神奈川県            0.6115774
    千葉県             0.30682975
    埼玉県             0.16422862
    福岡県             0.11658129
    北海道             0.042011086
    大阪府             0.023969756
    沖縄県             0.012238273
    広島県             0.010203229
    京都府             0.007866103

=== rank=15 ==============================================

[purchase, rank=15]
    not-purchase    1.3840476
    purchase        0.05286656

[sex, rank=15]
    male            0.40603286
    female          0.33881614

[age, rank=15]
    50-60           0.9839888
    40-50           0.8108464
    60-70           0.44133085
    30-40           0.42642552
    20-30           0.11595425
    70-80           0.08036094
    10-20           0.013466829
    0-10            0.0
    80-90           0.0
    90-100          0.0

[user-pref, rank=15]
    広島県             3.120223
    none            2.1195738
    愛媛県             1.2863604
    岡山県             1.0316815
    兵庫県             0.93768555
    香川県             0.66445416
    大阪府             0.63611424
    山口県             0.60725176
    徳島県             0.52196616
    福岡県             0.5051045

[genre, rank=15]
    ホテル・旅館          4.695799
    グルメ             0.8259924
    レジャー            0.63746685
    宅配              0.04916255
    リラクゼーション        0.02670276
    ヘアサロン           0.019993486
    レッスン            0.0063774996
    エステ             0.0032248881
    その他のクーポン        1.4798732e-9
    ネイル・アイ          5.662746e-18

[price-rate, rank=15]
    50-60           5.54248
    60-70           0.3183474
    70-80           0.11192238
    80-90           0.015879806
    0-10            0.0
    10-20           0.0
    20-30           0.0
    30-40           0.0
    40-50           0.0
    90-100          0.0

[coupon-pref, rank=15]
    広島県             5.002689
    岡山県             4.8674664
    愛媛県             3.819457
    山口県             2.2871482
    香川県             2.048055
    兵庫県             1.9058306
    大分県             1.2801807
    島根県             1.1209284
    東京都             1.0994488
    熊本県             1.0699294

=== rank=16 ==============================================

[purchase, rank=16]
    not-purchase    2.2071648
    purchase        0.030599115

[sex, rank=16]
    male            0.23976274
    female          0.22294049

[age, rank=16]
    50-60           1.497894
    40-50           1.2175779
    60-70           0.7949202
    30-40           0.65249264
    70-80           0.14198731
    20-30           0.121211365
    10-20           1.914623e-4
    80-90           1.4012985e-45
    0-10            0.0
    90-100          0.0

[user-pref, rank=16]
    大阪府             8.310363
    none            6.348709
    兵庫県             4.6711187
    愛知県             3.4782143
    京都府             2.206215
    奈良県             1.6127536
    滋賀県             1.0965009
    三重県             1.0877019
    岐阜県             0.8397209
    東京都             0.702386

[genre, rank=16]
    ホテル・旅館          7.5691876
    レジャー            0.3448579
    宅配              0.13649517
    その他のクーポン        0.0074250787
    リラクゼーション        0.0017195438
    グルメ             4.4034434e-5
    ヘアサロン           0.0
    エステ             0.0
    ビューティー          0.0
    ネイル・アイ          0.0

[price-rate, rank=16]
    50-60           5.29018
    60-70           0.12799548
    70-80           3.577729e-23
    80-90           1.4012985e-45
    0-10            0.0
    10-20           0.0
    20-30           0.0
    30-40           0.0
    40-50           0.0
    90-100          0.0

[coupon-pref, rank=16]
    兵庫県             3.0401216
    三重県             2.685706
    長野県             1.4876926
    石川県             1.3615047
    岐阜県             1.0510179
    京都府             0.97669965
    福井県             0.96898156
    岡山県             0.9614938
    静岡県             0.95499766
    和歌山県            0.9178504

=== rank=17 ==============================================

[purchase, rank=17]
    not-purchase    2.1686087
    purchase        0.1401261

[sex, rank=17]
    female          0.31995893
    male            0.18927184

[age, rank=17]
    50-60           1.3681254
    40-50           1.156638
    30-40           0.71320134
    60-70           0.53779966
    20-30           0.17995061
    70-80           0.059219707
    10-20           8.4013224e-4
    80-90           1.4000862e-4
    0-10            0.0
    90-100          0.0

[user-pref, rank=17]
    none            3.9989574
    東京都             2.3662004
    大阪府             1.4447092
    神奈川県            1.3954381
    兵庫県             0.9889343
    愛知県             0.96942824
    千葉県             0.77332866
    福岡県             0.7720736
    埼玉県             0.7235036
    広島県             0.43233243

[genre, rank=17]
    宅配              7.534885
    レッスン            0.32018176
    エステ             0.10377104
    レジャー            0.033934064
    健康・医療           0.025387578
    グルメ             0.0060992558
    リラクゼーション        0.0041571446
    その他のクーポン        5.118636e-8
    ネイル・アイ          1.348504e-10
    ギフトカード          1.6775477e-28

[price-rate, rank=17]
    80-90           4.3227882
    70-80           1.4451641
    60-70           0.92241496
    50-60           0.3108305
    0-10            0.28807923
    90-100          0.18659206
    10-20           0.17024909
    40-50           0.14367303
    30-40           0.032832243
    20-30           0.0

[coupon-pref, rank=17]
    東京都             18.458841
    大阪府             2.3106465
    愛知県             0.9333674
    埼玉県             0.62400097
    岐阜県             0.56252456
    神奈川県            0.29084638
    兵庫県             0.27102277
    福岡県             0.26053324
    岡山県             0.22566587
    北海道             0.2107702

=== rank=18 ==============================================

[purchase, rank=18]
    not-purchase    1.7912629
    purchase        0.08927526

[sex, rank=18]
    female          0.46719456
    male            0.45835125

[age, rank=18]
    50-60           1.1657495
    40-50           1.0928315
    30-40           0.7393619
    60-70           0.41674086
    20-30           0.17924094
    70-80           0.08005629
    10-20           0.0025910537
    0-10            0.0
    80-90           0.0
    90-100          0.0

[user-pref, rank=18]
    愛知県             9.740398
    none            4.6195827
    宮城県             1.6790972
    岐阜県             1.4939338
    静岡県             1.198481
    三重県             1.0136162
    新潟県             0.7516665
    岩手県             0.3867537
    長野県             0.21327107
    福島県             0.20521982

[genre, rank=18]
    グルメ             3.1488037
    ホテル・旅館          0.90022254
    レジャー            0.46550462
    リラクゼーション        0.18945444
    ヘアサロン           0.09675853
    その他のクーポン        0.078181304
    宅配              0.051834285
    ネイル・アイ          0.03724882
    レッスン            0.0122079635
    エステ             3.216022e-40

[price-rate, rank=18]
    50-60           3.542035
    60-70           0.3179787
    70-80           0.060131595
    80-90           0.020806817
    90-100          0.0063272505
    0-10            0.0
    10-20           0.0
    20-30           0.0
    30-40           0.0
    40-50           0.0

[coupon-pref, rank=18]
    愛知県             11.42809
    宮城県             2.157299
    新潟県             0.8626758
    岐阜県             0.5836112
    静岡県             0.56461203
    京都府             0.27103737
    石川県             0.22086239
    岩手県             0.15514746
    大阪府             0.15321568
    沖縄県             0.1465497

=== rank=19 ==============================================

[purchase, rank=19]
    not-purchase    1.7720279
    purchase        0.06624286

[sex, rank=19]
    male            0.4222375
    female          0.28785437

[age, rank=19]
    50-60           0.9933285
    40-50           0.84622896
    30-40           0.6148684
    60-70           0.59917617
    20-30           0.20896131
    70-80           0.1033106
    10-20           5.8408955e-4
    80-90           2.0276357e-5
    0-10            0.0
    90-100          0.0

[user-pref, rank=19]
    福岡県             9.2037325
    none            3.437759
    熊本県             1.6619412
    大分県             1.367689
    長崎県             1.2377743
    宮崎県             0.7646401
    山口県             0.62219054
    佐賀県             0.59637713
    鹿児島県            0.44276455
    沖縄県             0.42060307

[genre, rank=19]
    ホテル・旅館          5.2219844
    グルメ             2.4827518
    レジャー            0.4094028
    ヘアサロン           0.075165674
    リラクゼーション        0.033994943
    ネイル・アイ          0.021587353
    エステ             0.017671699
    ギフトカード          0.0068943733
    宅配              0.0038348772
    その他のクーポン        0.00285259

[price-rate, rank=19]
    50-60           3.6542368
    60-70           0.42968315
    70-80           0.051241327
    80-90           0.023783898
    90-100          0.0019105313
    0-10            0.0
    10-20           0.0
    20-30           0.0
    30-40           0.0
    40-50           0.0

[coupon-pref, rank=19]
    福岡県             13.348669
    大分県             5.5419436
    熊本県             4.9312096
    長崎県             2.9952428
    佐賀県             1.6498284
    鹿児島県            1.3934722
    宮崎県             1.1432154
    沖縄県             0.93378395
    山口県             0.68672574
    東京都             0.64897496
|#
