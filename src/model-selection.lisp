(in-package :cl-tensor-decomposition)

(defun make-fold-splits (indices counts k &key (random-state *random-state*))
  (declare (ignore counts))
  (let* ((nnz (array-dimension indices 0))
         (order (make-array nnz :element-type 'fixnum)))
    (loop for i from 0 below nnz do
      (setf (aref order i) i))
    (let ((*random-state* random-state))
      (loop for i from 0 below nnz do
        (let* ((j (+ i (random (- nnz i))))
               (tmp (aref order i)))
          (setf (aref order i) (aref order j))
          (setf (aref order j) tmp))))
    (loop with fold-size = (ceiling nnz k)
          for fold-index from 0 below k collect
            (loop for offset from (* fold-index fold-size)
                  below (min nnz (* (1+ fold-index) fold-size))
                  collect (aref order offset)))))

(defun %subset-tensor (indices counts subset)
  (let* ((subset-size (length subset))
         (dimension (array-dimension indices 1))
         (sub-indices (make-array (list subset-size dimension)
                                  :element-type 'fixnum))
         (sub-counts (make-array subset-size :element-type 'double-float)))
    (loop for pos from 0 below subset-size
          for original-index in subset do
            (loop for dim from 0 below dimension do
              (setf (aref sub-indices pos dim)
                    (aref indices original-index dim)))
            (setf (aref sub-counts pos)
                  (coerce (aref counts original-index) 'double-float)))
    (values sub-indices sub-counts)))

(defun %tensor-dimensions (indices)
  (loop for dim from 0 below (array-dimension indices 1)
        collect (loop for row from 0 below (array-dimension indices 0)
                      maximize (aref indices row dim))))

(defun %build-shape (indices)
  (mapcar #'1+ (%tensor-dimensions indices)))

(defun %complement-subset (nnz subset)
  (let ((mask (make-array nnz :element-type 'bit :initial-element 0)))
    (dolist (idx subset)
      (setf (aref mask idx) 1))
    (loop for i from 0 below nnz
          unless (= (aref mask i) 1)
            collect i)))

(defun %evaluate-fold (shape train-indices train-counts valid-indices valid-counts rank
                      &key (n-cycle 100) convergence-threshold convergence-window
                           (evaluation-function #'sparse-kl-divergence)
                           verbose)
  (multiple-value-bind (factor-matrix-vector iterations)
      (decomposition shape
                     train-indices
                     train-counts
                     :R rank
                     :n-cycle n-cycle
                     :convergence-threshold convergence-threshold
                     :convergence-window convergence-window
                     :verbose verbose)
    (declare (ignore iterations))
    (let ((approx (make-array (length valid-counts)
                              :element-type 'double-float
                              :initial-element 0d0)))
      (sdot factor-matrix-vector valid-indices approx)
      (funcall evaluation-function valid-indices valid-counts approx))))

(defun cross-validate-rank (indices counts ranks &key (k 5)
                                    (n-cycle 100)
                                    convergence-threshold
                                    convergence-window
                                    (evaluation-function #'sparse-kl-divergence)
                                    random-state
                                    verbose)
  (let* ((nnz (array-dimension indices 0))
         (shape (%build-shape indices))
         (folds (if random-state
                    (make-fold-splits indices counts k :random-state random-state)
                    (make-fold-splits indices counts k)))
         (total (* (length ranks) (length folds)))
         (completed 0)
         (results '()))
    (dolist (rank ranks)
      (let ((fold-scores '()))
        (dolist (subset folds)
          (multiple-value-bind (valid-indices valid-counts)
              (%subset-tensor indices counts subset)
            (multiple-value-bind (train-indices train-counts)
                (%subset-tensor indices counts (%complement-subset nnz subset))
              (push (%evaluate-fold shape
                                     train-indices train-counts
                                     valid-indices valid-counts rank
                                     :n-cycle n-cycle
                                     :convergence-threshold convergence-threshold
                                     :convergence-window convergence-window
                                     :evaluation-function evaluation-function
                                     :verbose verbose)
                    fold-scores)
               (incf completed)
               (let* ((ratio (/ completed (max 1 total)))
                      (percent (* 100d0 (coerce ratio 'double-float))))
                 (format t "Cross-validation progress ~D/~D (~,2F%%)~%"
                         completed total percent)
                 (finish-output)))))
        (let* ((mean (/ (reduce #'+ fold-scores) (length fold-scores)))
               (variance (if (> (length fold-scores) 1)
                             (/ (reduce #'+ fold-scores
                                        :key (lambda (score)
                                               (expt (- score mean) 2)))
                                (1- (length fold-scores)))
                             0d0))
               (std (sqrt variance)))
          (push (list (cons :rank rank)
                      (cons :mean mean)
                      (cons :std std)
                      (cons :scores (nreverse fold-scores)))
                results))))
    (nreverse results)))

(defun select-rank (indices counts ranks &key (k 5)
                             (n-cycle 100)
                             convergence-threshold
                             convergence-window
                             (evaluation-function #'sparse-kl-divergence)
                             random-state
                             verbose)
  (let* ((cv-key-args (list :k k
                            :n-cycle n-cycle
                            :convergence-threshold convergence-threshold
                            :convergence-window convergence-window
                            :evaluation-function evaluation-function
                            :verbose verbose))
         (cv-key-args (if random-state
                          (append cv-key-args (list :random-state random-state))
                          cv-key-args))
         (cv-results (apply #'cross-validate-rank
                            indices counts ranks cv-key-args)))
    (values (car (sort cv-results #'<
                       :key (lambda (result)
                              (cdr (assoc :mean result)))))
            cv-results)))
