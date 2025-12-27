(in-package :cltd)

(defparameter X clml.clustering.nmf::datamatrix)
(array-dimensions X) ; => (100 1202)

(defparameter X^ (make-array (array-dimensions X)
                             :element-type 'double-float
                             :initial-element 1d0))

(defparameter R 20)

(defparameter A (make-array (list (array-dimension X 0) R)
                            :element-type 'double-float
                            :initial-element 0d0))

(defparameter B (make-array (list (array-dimension X 1) R)
                            :element-type 'double-float
                            :initial-element 0d0))

(defun init-random (X)
  (loop for i from 0 below (array-dimension X 0) do
    (loop for j from 0 below (array-dimension X 1) do
      (setf (aref X i j) (random 1d0)))))

(init-random A)
(init-random B)

(defun dot! (A B X^)
  (loop for i from 0 below (array-dimension A 0) do
    (loop for j from 0 below (array-dimension B 0) do
      (setf (aref X^ i j) 0d0)
      (loop for r from 0 below (array-dimension A 1) do
        (incf (aref X^ i j) (* (aref A i r)
                               (aref B j r)))))))

(time (dot! A B X^))

(defun update-A (A B X X^)
  (loop for i from 0 below (array-dimension A 0) do
    (loop for r from 0 below (array-dimension A 1) do
      (let ((numerator 0d0)
            (denominator 0d0))
        (loop for j from 0 below (array-dimension B 0) do
          (incf numerator   (* (/ (aref X i j) (aref X^ i j)) (aref B j r)))
          (incf denominator (aref B j r)))
        (setf (aref A i r) (* (aref A i r) (/ numerator denominator)))))))

(defun update-B (B A X X^)
  (loop for j from 0 below (array-dimension B 0) do
    (loop for r from 0 below (array-dimension B 1) do
      (let ((numerator 0d0)
            (denominator 0d0))
        (loop for i from 0 below (array-dimension A 0) do
          (incf numerator   (* (/ (aref X i j) (aref X^ i j)) (aref A i r))) ; ここのアクセス順さえ変えれば他は全く同じ構造
          (incf denominator (aref A i r)))
        (setf (aref B j r) (* (aref B j r) (/ numerator denominator)))))))

(dot! A B X^)
(update-A A B X X^)
(dot! A B X^)
(update-B B A X X^)

(defun square (x)
  (* x x))

(defun rmse (X X^)
  (let ((sum 0d0))
    (loop for i from 0 below (array-dimension X 0) do
      (loop for j from 0 below (array-dimension X 1) do
        (incf sum (square (- (aref X i j) (aref X^ i j))))))
    (/ sum (* (array-dimension X 0)
              (array-dimension X 1)))))

(defparameter *result* nil)

(defun kl-divergence (X X^)
  (let ((sum 0d0))
    (loop for i from 0 below (array-dimension X 0) do
      (loop for j from 0 below (array-dimension X 1) do
        (incf sum
              (+ (* (aref X i j)
                    (log (+ (/ (aref X i j)
                               (+ (aref X^ i j)))
                            0.000001d0)))
                 (- (aref X i j))
                 (aref X^ i j)))))
    sum))

(kl-divergence X X^)

(progn
  (init-random A)
  (init-random B)
  (loop for i from 1 to 100 do
    (dot! A B X^)
    (update-A A B X X^)
    (dot! A B X^)
    (update-B B A X X^)
    (format t "cycle: ~A, kl-divergence: ~A, rmse: ~A~%" i (kl-divergence X X^) (rmse X X^))))

;; clmlのnmfでコスト関数を :euclidean :kl が選べる

(ql:quickload :wiz-util)
(dot! clml.clustering.nmf::*A*
      (wiz:transpose-matrix clml.clustering.nmf::*B*)
      X^)

(print (kl-divergence X X^))
;; klを選んだとき: 645.5766511993866d0

;; コスト関数を選ぶ基準
;; 訪問回数などの大きな値を取る可能性があるとき => KL
;; 0~100のスコアなど決まった範囲内に収まるとき => ユークリッド距離

;; 前者の方がスパース化させやすい

(ql:quickload :clgplot)
(clgp:plot '(1751.5153246320074d0 1473.477224827758d0 1342.6129596540945d0
             1147.2434585062472d0 963.5574663982889d0 848.4302548097248d0
             785.6906613452702d0 750.0925772304598d0 726.3712278199024d0
             706.9323607795768d0 692.2988092695742d0 682.5463920357895d0
             676.8847168221818d0 674.0177792894322d0 672.0591638789219d0
             670.6887820806649d0 669.6556400487817d0 668.8738236997931d0
             668.2277017325328d0 667.5457399715638d0 666.788427060137d0 666.0575203497308d0
             665.4826506642685d0 664.9549993691363d0 664.4338931957685d0
             664.0718808479198d0 663.8191885263018d0 663.5778325577647d0
             663.3354657603993d0 663.1069269220804d0 662.8983048108473d0
             662.7055593619659d0 662.5364390070786d0 662.3787062024351d0
             662.2134756280385d0 662.0448846520924d0 661.9180496494536d0
             661.8384467931974d0 661.7752500792291d0 661.7117732409273d0
             661.6217406920501d0 661.4360017554966d0 661.2292348682998d0
             661.1650421302293d0 661.1287544624746d0 661.0917061702569d0
             661.0525998182508d0 661.0115666932973d0 660.9497694066536d0 660.839106158608d0
             660.7314390285379d0 660.6807847295388d0 660.6541518512497d0
             660.6362775575686d0 660.6219901178126d0 660.6093043959631d0
             660.5970073558124d0 660.5853333860229d0 660.5747271631924d0
             660.5634727884445d0 660.549814492441d0 660.5360549915237d0 660.5248120311277d0
             660.5127896876181d0 660.4974209440203d0 660.4788637101539d0
             660.4595657055058d0 660.4405858036102d0 660.4197218589145d0
             660.3935315004102d0 660.361179467375d0 660.3288780431717d0 660.3039861079473d0
             660.2860612082887d0 660.271060924296d0 660.2581328567841d0 660.2491194795418d0
             660.2435880841508d0 660.2395646950619d0 660.2360788653659d0 660.23306937935d0
             660.2304132193831d0 660.2274271757925d0 660.2230606571312d0 660.216353228183d0
             660.206705053309d0 660.1928690443671d0 660.1716594787246d0 660.1444224338596d0
             660.118052083055d0 660.089966462521d0 660.0584510604491d0 660.0243138225775d0
             659.9854973095757d0 659.9440539506154d0 659.9068098292889d0
             659.8794051127354d0 659.862043289094d0 659.8513385831167d0 659.8436293020626d0)
           :y-label "KL divergence"
           :x-label "cycle"
           ;:output "/home/wiz/tmp/kl-divergence.png"
           )

;;; sparse

;; (ql:quickload :spartns)

;; (use-package :spartns)

;; (locally (declare #+sbcl(sb-ext:muffle-conditions style-warning))
;;   (defspartn 2dmatrix
;;     :representation (spartns:hash spartns:array)
;;     :non-zero       (80 849)
;;     :element-type   double-float
;;     :sparse-element 0d0))

;; (defparameter sparse-X (make-2dmatrix))
;; (defparameter sparse-X^ (make-2dmatrix))

;; (defparameter cnt 0)

;; (loop for i from 0 below (array-dimension X 0) do
;;   (loop for j from 0 below (array-dimension X 1) do
;;     (when (> (aref X i j) 1.1102230246251568d-16)
;;       (incf cnt)
;;       (set-2dmatrix sparse-X i j (aref X i j)))))

;; 4266
;; (* 80 849) ; 67920 (6.2%)

;; (defun sparse-dot! (A B X^)
;;   (w/fast-traversals ((2dmatrix A i j val-a)
;;                       (2dmatrix B j k val-b))
;;       (set-2dmatrix X^ i k
;;                     (+ (get-2dmatrix X^ i k) (* val-a val-b)))))

;; (defun update-A (A B X X^)
;;   (do-fast-traversal '

;;   (w/fast-traversals ((2dmatrix A i j val-a)
;;                       (2dmatrix B j k val-b)
;;                       (2dmatrix X l m val-b))
;;       (set-2dmatrix X^ i k
;;                     (+ (get-2dmatrix X^ i k) (* val-a val-b))))


;;   (loop for i from 0 below (array-dimension A 0) do
;;     (loop for r from 0 below (array-dimension A 1) do
;;       (let ((numerator 0d0)
;;             (denominator 0d0))
;;         (loop for j from 0 below (array-dimension B 0) do
;;           (incf numerator   (* (/ (aref X i j) (aref X^ i j)) (aref B j r)))
;;           (incf denominator (aref B j r)))
;;         (setf (aref A i r) (* (aref A i r) (/ numerator denominator)))))))

;; (defun update-B (B A X X^)
;;   (loop for j from 0 below (array-dimension B 0) do
;;     (loop for r from 0 below (array-dimension B 1) do
;;       (let ((numerator 0d0)
;;             (denominator 0d0))
;;         (loop for i from 0 below (array-dimension A 0) do
;;           (incf numerator   (* (/ (aref X i j) (aref X^ i j)) (aref A i r))) ; ここのアクセス順さえ変えれば他は全く同じ構造
;;           (incf denominator (aref A i r)))
;;         (setf (aref B j r) (* (aref B j r) (/ numerator denominator)))))))

;;; use spartanian

;;; 一定以上小さいものには0を代入

(in-package :clml.clustering.nmf)
(defparameter datamatrix (make-document-term-matrix corpus-dataset))

(in-package :cltd)
(defparameter X clml.clustering.nmf::datamatrix)

(loop for i from 0 below (array-dimension X 0) do
  (loop for j from 0 below (array-dimension X 1) do
    (when (<= (aref X i j) 1.1102230246251568d-16)
      (setf (aref X i j) 0d0))))

(aref X 0 0)

(let ((cnt 0))
  (loop for i from 0 below (array-dimension X 0) do
    (loop for j from 0 below (array-dimension X 1) do
      (when (> (aref X i j) 0d0)
        (incf cnt))))
  cnt)

;; => 4852
(array-dimensions X) ; => (100 1202)

(defparameter non-zero-size 4852)
(defparameter n-factor 2)
(defparameter R 3)
(defparameter X-indices-matrix (make-array (list non-zero-size n-factor) :element-type 'fixnum))
(defparameter X-value-vector (make-array non-zero-size :element-type 'double-float))
(defparameter X^-value-vector (make-array non-zero-size :element-type 'double-float :initial-element 1d0))

;; X -> sparse-X-indices-matrix, sparse-X-value-vector
(let ((cnt 0))
  (loop for i from 0 below (array-dimension X 0) do
    (loop for j from 0 below (array-dimension X 1) do
      (when (> (aref X i j) 0d0)
        (loop for dim from 0 below n-factor
              for index in (list i j)
              do (setf (aref X-indices-matrix cnt dim) index
                       (aref X-value-vector cnt) (aref X i j)))
        (incf cnt)))))

(array-dimensions X) ; => (100 1202)

(defparameter A (make-array (list (array-dimension X 0) R)
                            :element-type 'double-float
                            :initial-element 0d0))

(defparameter B (make-array (list (array-dimension X 1) R)
                            :element-type 'double-float
                            :initial-element 0d0))
(init-random A)
(init-random B)

(defparameter factor-matrix-vector (vector A B))

(defun dot2! (factor-matrix-vector sparse-X-indices-matrix sparse-X^-value-vector)
  (let ((R (array-dimension (svref factor-matrix-vector 0) 1)))
    (loop for datum-index from 0 below (array-dimension sparse-X-indices-matrix 0) do
      (setf (aref sparse-X^-value-vector datum-index)
            (loop for ri from 0 below R
                  sum (let ((prod 1d0))
                        (loop for factor-index from 0 below n-factor do
                          (setf prod (* prod (aref (aref factor-matrix-vector factor-index)
                                                   (aref sparse-X-indices-matrix datum-index factor-index)
                                                   ri))))
                        prod))))))

(defun dot3! (factor-matrix-vector X-indices-matrix X^-value-vector)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) X-indices-matrix)
           (type (simple-array double-float) X^-value-vector))
  (let ((R (array-dimension (svref factor-matrix-vector 0) 1)))
    (declare (type fixnum R))
    (loop for datum-index fixnum from 0 below (array-dimension X-indices-matrix 0) do
      (setf (aref X^-value-vector datum-index)
            (loop for ri fixnum from 0 below R
                  sum (let ((prod 1d0))
                        (declare (type double-float prod))
                        (loop for factor-index fixnum from 0 below n-factor do
                          (let ((factor-matrix (aref factor-matrix-vector factor-index)))
                            (declare (type (simple-array double-float) factor-matrix))
                            (setf prod
                                  (* prod (aref factor-matrix
                                                (aref X-indices-matrix datum-index factor-index)
                                                ri)))))
                        prod)
                  double-float)))))

(time (dot2! factor-matrix-vector sparse-X-indices-matrix sparse-X^-value-vector))
(time (dot3! factor-matrix-vector sparse-X-indices-matrix sparse-X^-value-vector))

(ql:quickload :cl-debug-print)
(cl-syntax:use-syntax cl-debug-print:debug-print-syntax)

(defun datum-indices-value (datum-index X-indices-matrix X-value-vector)
  (format t "indices: ")
  (loop for j from 0 below n-factor do
    (format t "~A " (aref X-indices-matrix datum-index j)))
  (format t "~%value: ~A~%" (aref X-value-vector datum-index)))

;; 次の2つが一致することを確認(sparse版がdence版と同じ結果
(datum-indices-value 0 X-indices-matrix X^-value-vector)
;; indices: 0 1
;; value: 0.49650179820894935d0
(aref X^ 0 1) ; => 0.49650179820894935d0

(datum-indices-value 1 sparse-X-indices-matrix sparse-X^-value-vector)
;; indices: 0 81
;; value: 0.4839826097016188d0
(aref X^ 0 81) ; => 0.4839826097016188d0

(datum-indices-value 100 sparse-X-indices-matrix sparse-X^-value-vector)
;; indices: 2 271
;; value: 0.4016027188061294d0
(aref X^ 2 271) ; => 0.4016027188061294d0


(defparameter numerator-tmp
  (vector (make-array (list (array-dimension X 0) R)
                      :element-type 'double-float
                      :initial-element 0d0)
          (make-array (list (array-dimension X 1) R)
                      :element-type 'double-float
                      :initial-element 0d0)))

(defparameter denominator-tmp (make-array (list (length numerator-tmp) R)
                                          :element-type 'double-float
                                          :initial-element 1d0))

(defun calc-denominator (factor-matrix-vector denominator-tmp large-R)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float) denominator-tmp)
           (type fixnum large-R))
  (loop for factor-index fixnum from 0 below (length factor-matrix-vector) do
    (loop for other-factor-index fixnum from 0 below (length factor-matrix-vector)
          if (not (= factor-index other-factor-index)) do
            (let ((factor-matrix (svref factor-matrix-vector other-factor-index)))
              (declare (type (simple-array double-float) factor-matrix))
              (loop for r fixnum from 0 below large-R do
                (setf (aref denominator-tmp factor-index r)
                      (* (aref denominator-tmp factor-index r)
                         (loop for i fixnum from 0 below (array-dimension factor-matrix 0)
                               sum (aref factor-matrix i r) double-float))))))))

(time (calc-denominator factor-matrix-vector denominator-tmp R))

(defun calc-numerator (X-indices-matrix X-value-vector X^-value-vector
                       factor-matrix-vector numerator-tmp large-R)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) X-indices-matrix)
           (type (simple-array double-float) X-value-vector X^-value-vector)
           (type fixnum large-R))
  (loop for datum-index fixnum from 0 below (array-dimension X-indices-matrix 0) do
    (let ((x/x^ (/ (aref X-value-vector datum-index) (aref X^-value-vector datum-index))))
      (declare (type double-float x/x^))
      (loop for factor-index fixnum from 0 below (length factor-matrix-vector) do
        (let ((factor-tmp (svref numerator-tmp factor-index)))
          (declare (type (simple-array double-float) factor-tmp))
          (loop for r fixnum from 0 below large-R do
            (let ((factor-prod 1d0))
              (declare (type double-float factor-prod))
              (loop for other-factor-index fixnum from 0 below (length factor-matrix-vector)
                    if (not (= factor-index other-factor-index)) do
                      (let ((factor-matrix (svref factor-matrix-vector other-factor-index)))
                        (declare (type (simple-array double-float) factor-matrix))
                        (setf factor-prod
                              (* factor-prod
                                 (aref factor-matrix
                                       (aref X-indices-matrix datum-index other-factor-index)
                                       r)))))
              (incf (aref factor-tmp (aref X-indices-matrix datum-index factor-index) r)
                    (* x/x^ factor-prod)))))))))

(time (calc-numerator sparse-X-indices-matrix sparse-X-value-vector sparse-X^-value-vector
                      factor-matrix-vector numerator-tmp R))

(defun initialize-matrix (matrix default-value)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float) matrix)
           (type double-float default-value))
  (loop for i from 0 below (array-dimension matrix 0) do
    (loop for j from 0 below (array-dimension matrix 1) do
      (setf (aref matrix i j) default-value))))

(defun update (X-indices-matrix X-value-vector X^-value-vector
               factor-matrix-vector numerator-tmp denominator-tmp large-R)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) X-indices-matrix)
           (type (simple-array double-float) X-value-vector X^-value-vector denominator-tmp)
           (type fixnum large-R))

  (initialize-matrix denominator-tmp 1d0)

  (let ((n-factor (length factor-matrix-vector)))
    (loop for factor-index from 0 below n-factor do
      (let ((factor-tmp (svref numerator-tmp factor-index)))
        (declare (type (simple-array double-float) factor-tmp))
        (initialize-matrix factor-tmp 0d0)))

    (calc-denominator factor-matrix-vector denominator-tmp large-R)
    (calc-numerator X-indices-matrix X-value-vector X^-value-vector
                    factor-matrix-vector numerator-tmp large-R)

    (loop for factor-index from 0 below n-factor do
      (let ((factor-matrix (svref factor-matrix-vector factor-index))
            (factor-tmp (svref numerator-tmp factor-index)))
        (declare (type (simple-array double-float) factor-matrix factor-tmp))
        (loop for i from 0 below (array-dimension factor-matrix 0) do
          (loop for r from 0 below (array-dimension factor-matrix 1) do
            (setf (aref factor-matrix i r)
                  (* (aref factor-matrix i r)
                     (/ (aref factor-tmp i r)
                        (aref denominator-tmp factor-index r))))))))))

(time (update sparse-X-indices-matrix sparse-X-value-vector sparse-X^-value-vector
        factor-matrix-vector numerator-tmp denominator-tmp R))

(defun sdot (factor-matrix-vector X-indices-matrix X^-value-vector large-R)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) X-indices-matrix)
           (type (simple-array double-float) X^-value-vector)
           (type fixnum large-R))
  (loop for datum-index fixnum from 0 below (array-dimension X-indices-matrix 0) do
    (setf (aref X^-value-vector datum-index)
          (loop for ri fixnum from 0 below large-R
                sum (let ((prod 1d0))
                      (declare (type double-float prod))
                      (loop for factor-index fixnum from 0 below (length factor-matrix-vector) do
                        (let ((factor-matrix (svref factor-matrix-vector factor-index)))
                          (declare (type (simple-array double-float) factor-matrix))
                          (setf prod
                                (* prod (aref factor-matrix
                                              (aref X-indices-matrix datum-index factor-index)
                                              ri)))))
                      prod)
                double-float))))

(defun sparse-kl-divergence (X-indices-matrix X-value-vector X^-value-vector)
  (loop for datum-index from 0 below (array-dimension X-indices-matrix 0)
        sum (+ (* (aref X-value-vector datum-index)
                  (log (+ (/ (aref X-value-vector datum-index)
                             (aref X^-value-vector datum-index))
                          0.000001d0)))
               (- (aref X-value-vector datum-index))
               (aref X^-value-vector datum-index))))

(init-random A)
(init-random B)
(setf factor-matrix-vector (vector A B))

(loop for i from 1 to 100 do
  (sdot factor-matrix-vector X-indices-matrix X^-value-vector R)
  (update X-indices-matrix X-value-vector X^-value-vector factor-matrix-vector numerator-tmp denominator-tmp R)
  (format t "cycle: ~A, kl-divergence: ~A~%" i
          (sparse-kl-divergence X-indices-matrix X-value-vector X^-value-vector)))

;;; simple example

(defparameter A (make-array '(2 2)
                            :element-type 'double-float
                            :initial-contents '((1d0 2d0)
                                                (3d0 4d0))))

(defparameter B (make-array '(3 2)
                            :element-type 'double-float
                            :initial-contents '((1d0 2d0)
                                                (3d0 4d0)
                                                (5d0 6d0))))

(defparameter C (make-array '(4 2)
                            :element-type 'double-float
                            :initial-contents '((1d0 2d0)
                                                (3d0 4d0)
                                                (5d0 6d0)
                                                (7d0 8d0))))

(defparameter factor-matrix-vector (vector A B C))

(defparameter R 2)

(defparameter denominator-tmp (make-array (list (length '(A B C)) R)
                                          :element-type 'double-float
                                          :initial-element 1d0))

(calc-denominator factor-matrix-vector denominator-tmp R)
;; #2A((144.0d0 240.0d0) (64.0d0 120.0d0) (36.0d0 72.0d0))

(defparameter X-indices-matrix
  (make-array '(3 3) :element-type 'fixnum
              :initial-contents '((0 1 0)
                                  (1 2 3)
                                  (0 0 1))))

(defparameter X (make-array 3 :element-type 'double-float :initial-contents '(1d0 2d0 3d0)))

(defparameter X^ (make-array 3 :element-type 'double-float :initial-element 1d0))

(defparameter numerator-tmp
  (vector (make-array '(2 2) :element-type 'double-float :initial-element 0d0)
          (make-array '(3 2) :element-type 'double-float :initial-element 0d0)
          (make-array '(4 2) :element-type 'double-float :initial-element 0d0)))

(calc-numerator X-indices-matrix X X^ factor-matrix-vector numerator-tmp R)
;; #(#2A((12.0d0 32.0d0) (70.0d0 96.0d0))
;;   #2A((9.0d0 24.0d0) (1.0d0 4.0d0) (42.0d0 64.0d0))
;;   #2A((3.0d0 8.0d0) (3.0d0 12.0d0) (0.0d0 0.0d0) (30.0d0 48.0d0)))

(sdot factor-matrix-vector X-indices-matrix X^ R)
(update X-indices-matrix X X^ factor-matrix-vector numerator-tmp denominator-tmp R)


(loop for i from 1 to 10 do
  (sdot factor-matrix-vector X-indices-matrix X^ R)
  (update X-indices-matrix X X^ factor-matrix-vector numerator-tmp denominator-tmp R)
  (format t "cycle: ~A, kl-divergence: ~A~%" i
          (sparse-kl-divergence X-indices-matrix X X^)))
