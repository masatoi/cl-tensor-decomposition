(in-package :cl-tensor-decomposition)

(defparameter *reporting-epsilon* 1.0d-8)

(defun %normalize-mode-name (name)
  (typecase name
    (string name)
    (symbol (string-downcase (symbol-name name)))
    (t (princ-to-string name))))

(defun %normalize-label (value)
  (typecase value
    (string value)
    (symbol (string-downcase (symbol-name value)))
    (number (princ-to-string value))
    (character (string value))
    (t (princ-to-string value))))

(defun round-to (value &optional (decimals 3))
  (let* ((factor (expt 10d0 decimals))
         (rounded (/ (round (* value factor)) factor)))
    (coerce rounded 'double-float)))

(defstruct mode-spec
  name
  labels
  discretization
  missing-labels
  role
  positive-label
  negative-label)

(defun make-mode-metadata (name labels &key (discretization "unspecified")
                                         (missing-labels nil)
                                         role
                                         positive-label
                                         negative-label)
  (let* ((labels-vector (if (typep labels 'vector)
                            labels
                            (coerce labels '(simple-vector))))
         (normalized-labels (make-array (length labels-vector)
                                        :element-type 'string
                                        :initial-element "")))
    (loop for i from 0 below (length labels-vector) do
      (setf (aref normalized-labels i)
            (%normalize-label (aref labels-vector i))))
    (make-mode-spec :name (%normalize-mode-name name)
                    :labels normalized-labels
                    :discretization discretization
                    :missing-labels (mapcar #'%normalize-label missing-labels)
                    :role (cond ((null role) nil)
                                ((keywordp role) role)
                                ((symbolp role)
                                 (intern (string-upcase (symbol-name role)) :keyword))
                                ((stringp role)
                                 (intern (string-upcase role) :keyword))
                                (t role))
                    :positive-label (and positive-label
                                         (%normalize-label positive-label))
                    :negative-label (and negative-label
                                         (%normalize-label negative-label)))))

(defun %coerce-mode-spec (metadata)
  (cond
    ((typep metadata 'mode-spec) metadata)
    ((and (listp metadata) (getf metadata :name))
     (make-mode-metadata (getf metadata :name)
                         (getf metadata :labels)
                         :discretization (getf metadata :discretization "unspecified")
                         :missing-labels (getf metadata :missing-labels)
                         :role (getf metadata :role)
                         :positive-label (getf metadata :positive-label)
                         :negative-label (getf metadata :negative-label)))
    (t (error "Invalid mode metadata: ~S" metadata))))

(defun ensure-mode-specs (metadata factor-matrix-vector)
  (let* ((num-modes (length factor-matrix-vector))
         (specs (make-array num-modes)))
    (unless (= num-modes (length metadata))
      (error "Metadata count (~A) does not match factor matrices (~A)."
             (length metadata) num-modes))
    (loop for idx from 0 below num-modes do
      (let* ((spec (%coerce-mode-spec (nth idx metadata)))
             (labels (mode-spec-labels spec))
             (matrix (svref factor-matrix-vector idx))
             (rows (array-dimension matrix 0)))
        (unless (= rows (length labels))
          (error "Mode ~A expects ~A labels but metadata provides ~A."
                 (mode-spec-name spec) rows (length labels)))
        (unless (mode-spec-positive-label spec)
          (setf (mode-spec-positive-label spec) (aref labels 0)))
        (when (and (eq (mode-spec-role spec) :purchase)
                   (not (mode-spec-negative-label spec))
                   (> (length labels) 1))
          (setf (mode-spec-negative-label spec) (aref labels 1)))
        (unless (position (mode-spec-positive-label spec) labels :test #'string=)
          (error "Positive label ~A not found in mode ~A."
                 (mode-spec-positive-label spec) (mode-spec-name spec)))
        (when (mode-spec-negative-label spec)
          (unless (position (mode-spec-negative-label spec) labels :test #'string=)
            (error "Negative label ~A not found in mode ~A."
                   (mode-spec-negative-label spec) (mode-spec-name spec))))
        (setf (svref specs idx) spec)))
    specs))

(defun normalize-factor-matrices (factor-matrix-vector epsilon)
  (let* ((num-modes (length factor-matrix-vector))
         (normalized (make-array num-modes))
         (column-sums (make-array num-modes)))
    (loop for mode-index from 0 below num-modes do
      (let* ((matrix (svref factor-matrix-vector mode-index))
             (rows (array-dimension matrix 0))
             (cols (array-dimension matrix 1))
             (normalized-matrix (make-array (array-dimensions matrix)
                                            :element-type 'double-float))
             (sums (make-array cols :element-type 'double-float)))
        (loop for r from 0 below cols do
          (let ((sum 0d0))
            (loop for i from 0 below rows do
              (let ((value (coerce (aref matrix i r) 'double-float)))
                (setf (aref normalized-matrix i r) value)
                (incf sum value)))
            (setf sum (if (> sum 0d0) sum epsilon))
            (setf (aref sums r) sum)
            (loop for i from 0 below rows do
              (setf (aref normalized-matrix i r)
                    (/ (aref normalized-matrix i r) sum)))))
        (setf (svref normalized mode-index) normalized-matrix)
        (setf (svref column-sums mode-index) sums)))
    (values normalized column-sums)))

(defun compute-lambda-vector (column-sums &optional lambda)
  (let* ((num-modes (length column-sums))
         (cols (length (svref column-sums 0)))
         (lambda-vector (make-array cols :element-type 'double-float)))
    (loop for r from 0 below cols do
      (let ((value (if lambda
                       (etypecase lambda
                         (vector (aref lambda r))
                         (list (nth r lambda)))
                       1d0)))
        (loop for mode-index from 0 below num-modes do
          (setf value (* value (aref (svref column-sums mode-index) r))))
        (setf (aref lambda-vector r) (coerce value 'double-float))))
    lambda-vector))

(defun estimate-marginals (mode-specs X-indices-matrix X-value-vector epsilon)
  (let* ((num-modes (length mode-specs))
         (nnz (array-dimension X-indices-matrix 0))
         (totals (make-array num-modes))
         (total-count 0d0))
    (loop for mode-index from 0 below num-modes do
      (let* ((labels (mode-spec-labels (svref mode-specs mode-index)))
             (counts (make-array (length labels)
                                 :element-type 'double-float
                                 :initial-element 0d0)))
        (setf (svref totals mode-index) counts)))
    (loop for row from 0 below nnz do
      (let ((count (coerce (aref X-value-vector row) 'double-float)))
        (incf total-count count)
        (loop for mode-index from 0 below num-modes do
          (let ((idx (aref X-indices-matrix row mode-index)))
            (incf (aref (svref totals mode-index) idx) count)))))
    (loop for mode-index from 0 below num-modes do
      (let* ((counts (svref totals mode-index))
             (length (length counts))
             (sum 0d0))
        (loop for i from 0 below length do
          (incf (aref counts i) epsilon)
          (incf sum (aref counts i)))
        (loop for i from 0 below length do
          (setf (aref counts i) (if (> sum 0d0)
                                    (/ (aref counts i) sum)
                                    (/ 1d0 length))))))
    totals))

(defun compute-lift-matrices (prob-matrices marginals epsilon)
  (let* ((num-modes (length prob-matrices))
         (lift-matrices (make-array num-modes)))
    (loop for mode-index from 0 below num-modes do
      (let* ((prob (svref prob-matrices mode-index))
             (rows (array-dimension prob 0))
             (cols (array-dimension prob 1))
             (baseline (svref marginals mode-index))
             (lift (make-array (array-dimensions prob)
                               :element-type 'double-float)))
        (loop for r from 0 below cols do
          (loop for i from 0 below rows do
            (let ((base (max epsilon (aref baseline i)))
                  (p (aref prob i r)))
              (setf (aref lift i r) (/ p base)))))
        (setf (svref lift-matrices mode-index) lift)))
    lift-matrices))

(defun compute-coherence-vectors (prob-matrices)
  (let* ((num-modes (length prob-matrices))
         (coherence (make-array num-modes)))
    (loop for mode-index from 0 below num-modes do
      (let* ((matrix (svref prob-matrices mode-index))
             (rows (array-dimension matrix 0))
             (cols (array-dimension matrix 1))
             (coherence-vector (make-array cols :element-type 'double-float)))
        (if (<= rows 1)
            (loop for r from 0 below cols do
              (setf (aref coherence-vector r) 1d0))
            (let ((log-dim (log (coerce rows 'double-float))))
              (loop for r from 0 below cols do
                (let ((entropy 0d0))
                  (loop for i from 0 below rows do
                    (let ((p (aref matrix i r)))
                      (when (> p 0d0)
                        (decf entropy (* p (log p))))))
                  (setf (aref coherence-vector r)
                        (max 0d0 (- 1d0 (/ entropy log-dim))))))))
        (setf (svref coherence mode-index) coherence-vector)))
    coherence))

(defun compute-coverage (lambda-vector prob-matrices X-indices-matrix X-value-vector)
  (let* ((nnz (array-dimension X-indices-matrix 0))
         (num-modes (length prob-matrices))
         (cols (length lambda-vector))
         (counts (make-array cols :element-type 'double-float :initial-element 0d0))
         (scores (make-array cols :element-type 'double-float))
         (total 0d0))
    (loop for row from 0 below nnz do
      (let ((count (coerce (aref X-value-vector row) 'double-float))
            (denominator 0d0))
        (incf total count)
        (loop for r from 0 below cols do
          (let ((score (aref lambda-vector r)))
            (loop for mode-index from 0 below num-modes do
              (let* ((index (aref X-indices-matrix row mode-index))
                     (matrix (svref prob-matrices mode-index)))
                (setf score (* score (aref matrix index r)))))
            (setf (aref scores r) score)
            (incf denominator score)))
        (when (> denominator 0d0)
          (loop for r from 0 below cols do
            (let ((resp (/ (aref scores r) denominator)))
              (incf (aref counts r) (* count resp)))))))
    (let ((shares (make-array cols :element-type 'double-float)))
      (loop for r from 0 below cols do
        (setf (aref shares r)
              (if (> total 0d0)
                  (/ (aref counts r) total)
                  0d0)))
      (values counts shares total))))

(defun salient-entry->alist (label prob lift)
  (list (cons :value label)
        (cons :prob (round-to prob))
        (cons :lift (round-to lift))))

(defun compute-salient-entries (mode-spec prob-matrix lift-matrix factor-index target)
  (let* ((labels (mode-spec-labels mode-spec))
         (rows (length labels))
         (entries (loop for i from 0 below rows collect
                        (list :label (aref labels i)
                              :prob (aref prob-matrix i factor-index)
                              :lift (aref lift-matrix i factor-index))))
         (sorted (sort (copy-list entries) #'>
                       :key (lambda (entry) (getf entry :prob))))
         (missing-ignored nil)
         (accum 0d0)
         (selected '()))
    (dolist (entry sorted)
      (let ((label (getf entry :label)))
        (if (member label (mode-spec-missing-labels mode-spec) :test #'string=)
            (setf missing-ignored t)
            (progn
              (push entry selected)
              (incf accum (getf entry :prob))
              (when (>= accum target)
                (return))))))
    (values (mapcar (lambda (entry)
                      (salient-entry->alist (getf entry :label)
                                            (getf entry :prob)
                                            (getf entry :lift)))
                    (nreverse selected))
            missing-ignored)))

(defun compute-negative-entries (mode-spec prob-matrix lift-matrix factor-index)
  (let* ((labels (mode-spec-labels mode-spec))
         (rows (length labels))
         (candidates (loop for i from 0 below rows
                           for label = (aref labels i)
                           for prob = (aref prob-matrix i factor-index)
                           for lift = (aref lift-matrix i factor-index)
                           when (and (< lift 1d0)
                                     (not (member label (mode-spec-missing-labels mode-spec)
                                                  :test #'string=)))
                             collect (list :label label :prob prob :lift lift))))
    (setf candidates (sort candidates #'<
                           :key (lambda (entry) (getf entry :lift))))
    (mapcar (lambda (entry)
              (list (cons :value (getf entry :label))
                    (cons :prob (round-to (getf entry :prob)))
                    (cons :lift (round-to (getf entry :lift)))))
            (subseq candidates 0 (min 3 (length candidates))))))

(defun build-mode-summary (mode-spec prob-matrix lift-matrix coherence factor-index target)
  (multiple-value-bind (salient missing?)
      (compute-salient-entries mode-spec prob-matrix lift-matrix factor-index target)
    (list (cons :name (mode-spec-name mode-spec))
          (cons :role (mode-spec-role mode-spec))
          (cons :salient salient)
          (cons :negatives (compute-negative-entries mode-spec prob-matrix lift-matrix factor-index))
          (cons :coherence (round-to coherence))
          (cons :discretization (mode-spec-discretization mode-spec))
          (cons :missing-ignored? missing?))))

(defun summary-ref (summary key)
  (cdr (assoc key summary)))

(defun summaries->salient-alist (summaries)
  (loop for summary in summaries collect
        (cons (summary-ref summary :name)
              (summary-ref summary :salient))))

(defun summaries->negatives-alist (summaries)
  (loop for summary in summaries collect
        (cons (summary-ref summary :name)
              (summary-ref summary :negatives))))

(defun summaries->coherence-alist (summaries)
  (loop for summary in summaries collect
        (cons (summary-ref summary :name)
              (summary-ref summary :coherence))))

(defun summaries->discretization-alist (summaries)
  (loop for summary in summaries collect
        (cons (summary-ref summary :name)
              (summary-ref summary :discretization))))

(defun summaries->role-alist (summaries)
  (loop for summary in summaries collect
        (cons (summary-ref summary :name)
              (summary-ref summary :role))))

(defun purchase-bias-for-factor (summaries mode-specs prob-matrices factor-index)
  (let ((purchase-summary (find :purchase summaries
                                :key (lambda (summary)
                                       (summary-ref summary :role)))))
    (if (null purchase-summary)
        (list (cons :purchase 0d0) (cons :not_purchase 0d0))
        (let* ((mode-name (summary-ref purchase-summary :name))
               (mode-index (position mode-name
                                     (loop for spec across mode-specs collect (mode-spec-name spec))
                                     :test #'string=))
               (spec (svref mode-specs mode-index))
               (matrix (svref prob-matrices mode-index))
               (positive-index (position (mode-spec-positive-label spec)
                                         (mode-spec-labels spec)
                                         :test #'string=))
               (negative-index (and (mode-spec-negative-label spec)
                                    (position (mode-spec-negative-label spec)
                                              (mode-spec-labels spec)
                                              :test #'string=))))
          (list (cons :purchase (if positive-index
                                    (round-to (aref matrix positive-index factor-index))
                                    0d0))
                (cons :not_purchase (if negative-index
                                        (round-to (aref matrix negative-index factor-index))
                                        0d0)))))))

(defun build-card-alist (factor-index lambda-vector coverage-counts coverage-shares
                        summaries missing? purchase-bias
                        &key kl-contribution contribution-rank)
  "Build an alist representing a single factor card.
When KL-CONTRIBUTION is provided, includes :kl_contribution field.
When CONTRIBUTION-RANK is provided, includes :contribution_rank field."
  (let ((base-alist
          (list (cons :factor_id factor-index)
                (cons :lambda (round-to (aref lambda-vector factor-index)))
                (cons :coverage (list (cons :count_est (round-to (aref coverage-counts factor-index)))
                                      (cons :share (round-to (aref coverage-shares factor-index)))))
                (cons :purchase_bias purchase-bias)
                (cons :salient (summaries->salient-alist summaries))
                (cons :negatives (summaries->negatives-alist summaries))
                (cons :coherence (summaries->coherence-alist summaries))
                (cons :notes (list (cons :missing_labels_ignored missing?)
                                   (cons :discretization (summaries->discretization-alist summaries))))
                (cons :mode_roles (summaries->role-alist summaries))
                (cons :mode_summaries summaries))))
    ;; Add diagnostics if provided
    (when kl-contribution
      (push (cons :kl_contribution (round-to kl-contribution)) base-alist))
    (when contribution-rank
      (push (cons :contribution_rank contribution-rank) base-alist))
    base-alist))

(defun generate-factor-cards
       (factor-matrix-vector x-indices-matrix x-value-vector metadata
        &key lambda population-marginals (target-coverage 0.8d0)
        (epsilon *reporting-epsilon*) (include-diagnostics nil))
  "Generate factor cards from decomposition results.

When INCLUDE-DIAGNOSTICS is T, returns an alist with:
  :model_diagnostics - Model-level diagnostic metrics
  :factors - List of factor cards (each card includes :kl_contribution)

When INCLUDE-DIAGNOSTICS is NIL (default), returns just the list of factor cards
for backward compatibility."
  (let* ((mode-specs (ensure-mode-specs metadata factor-matrix-vector)))
    (multiple-value-bind (prob-matrices column-sums)
        (normalize-factor-matrices factor-matrix-vector epsilon)
      (let* ((lambda-vector (compute-lambda-vector column-sums lambda))
             (marginals
              (or population-marginals
                  (estimate-marginals mode-specs x-indices-matrix
                   x-value-vector epsilon)))
             (lift-matrices
              (compute-lift-matrices prob-matrices marginals epsilon))
             (coherence-vectors (compute-coherence-vectors prob-matrices))
             (cards 'nil)
             (kl-contributions nil)
             (contribution-ranking nil)
             (model-diagnostics nil))
        (when include-diagnostics
          (setf kl-contributions
                  (compute-factor-kl-contributions factor-matrix-vector
                   x-indices-matrix x-value-vector :epsilon epsilon))
          (setf contribution-ranking
                  (rank-factors-by-contribution kl-contributions))
          (let* ((x^-value-vector
                  (make-array (length x-value-vector) :element-type
                              'double-float :initial-element 0.0d0))
                 (_
                  (sdot factor-matrix-vector x-indices-matrix x^-value-vector))
                 (kl-divergence
                  (sparse-kl-divergence x-indices-matrix x-value-vector
                   x^-value-vector))
                 (similarity-matrix
                  (compute-factor-similarity-matrix factor-matrix-vector))
                 (similar-pairs
                  (extract-similar-factor-pairs similarity-matrix :threshold
                   0.7d0))
                 (redundancy-score
                  (compute-factor-redundancy-score similarity-matrix :threshold
                   0.7d0))
                 (responsibilities
                  (compute-observation-responsibilities factor-matrix-vector
                   x-indices-matrix :epsilon epsilon))
                 (responsibility-stats
                  (responsibility-stats->alist responsibilities
                   x-value-vector))
                 (residuals
                  (compute-observation-residuals factor-matrix-vector
                   x-indices-matrix x-value-vector :epsilon epsilon))
                 (residual-stats
                  (residual-stats->alist residuals x-value-vector)))
            (declare (ignore _))
            (multiple-value-bind (exclusivity overlap)
                (compute-factor-exclusivity responsibilities x-value-vector)
              (setf model-diagnostics
                      (list (cons :kl_divergence (round-to kl-divergence 6))
                            (cons :factor_similarity
                                  (list (cons :similar_pairs similar-pairs)
                                        (cons :redundancy_score
                                              (round-to redundancy-score))))
                            (cons :exclusivity (round-to exclusivity))
                            (cons :overlap (round-to overlap))
                            (cons :responsibility_stats responsibility-stats)
                            (cons :residual_stats residual-stats)
                            (cons :kl_contributions
                                  (kl-contributions->alist
                                   kl-contributions :normalize t)))))))
        (multiple-value-bind (coverage-counts coverage-shares)
            (compute-coverage lambda-vector prob-matrices x-indices-matrix
             x-value-vector)
          (loop for factor-index from 0 below (length lambda-vector)
                do (let* ((summaries 'nil) (missing? nil))
                     (loop for mode-index from 0 below (length mode-specs)
                           do (let* ((summary
                                      (build-mode-summary
                                       (svref mode-specs mode-index)
                                       (svref prob-matrices mode-index)
                                       (svref lift-matrices mode-index)
                                       (aref
                                        (svref coherence-vectors mode-index)
                                        factor-index)
                                       factor-index target-coverage)))
                                (when (summary-ref summary :missing-ignored?)
                                  (setf missing? t))
                                (push summary summaries)))
                     (setf summaries (nreverse summaries))
                     (let ((purchase-bias
                            (purchase-bias-for-factor summaries mode-specs
                             prob-matrices factor-index)))
                       (push
                        (if include-diagnostics
                            (build-card-alist factor-index lambda-vector
                             coverage-counts coverage-shares summaries missing?
                             purchase-bias :kl-contribution
                             (aref kl-contributions factor-index)
                             :contribution-rank
                             (position factor-index contribution-ranking :key
                                       #'car))
                            (build-card-alist factor-index lambda-vector
                             coverage-counts coverage-shares summaries missing?
                             purchase-bias))
                        cards))))
          (if include-diagnostics
              (list (cons :model_diagnostics model-diagnostics)
                    (cons :factors (nreverse cards)))
              (nreverse cards)))))))

(defun write-factor-cards-json (cards path &key serializer)
  (unless serializer
    (error "Provide :serializer function that writes CARDS as JSON to the stream."))
  (let ((pathname (pathname path)))
    (with-open-file (out pathname :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
      (funcall serializer cards out))))

(defun card-ref (card key)
  (cdr (assoc key card)))

(defun find-summary-by-role (card role)
  (find role (card-ref card :mode_summaries)
        :key (lambda (summary) (summary-ref summary :role))
        :test #'eq))

(defun first-salient-entry (summary)
  (first (summary-ref summary :salient)))

(defun entry-ref (entry key)
  (cdr (assoc key entry)))

(defun headline-title (card)
  (let ((segments '()))
    (let ((purchase-summary (find-summary-by-role card :purchase)))
      (when purchase-summary
        (let ((entry (first-salient-entry purchase-summary)))
          (when entry
            (push (entry-ref entry :value) segments)))))
    (let* ((summaries (remove (find-summary-by-role card :purchase)
                              (card-ref card :mode_summaries)
                              :test #'eq))
           (ordered (sort (copy-list summaries) #'>
                          :key (lambda (summary)
                                 (let ((entry (first-salient-entry summary)))
                                   (if entry
                                       (max 0d0 (- (entry-ref entry :lift) 1d0))
                                       -1d0))))))
      (dolist (summary ordered)
        (when (< (length segments) 3)
          (let ((entry (first-salient-entry summary)))
            (when entry
              (push (entry-ref entry :value) segments))))))
    (if (null segments)
        (format nil "Factor ~A" (1+ (card-ref card :factor_id)))
        (format nil "~{~a~^ × ~}" (nreverse segments)))))

(defun summary-line (card)
  (let* ((coverage (card-ref card :coverage))
         (share (cdr (assoc :share coverage)))
         (purchase (cdr (assoc :purchase (card-ref card :purchase_bias))))
         (not-purchase (cdr (assoc :not_purchase (card-ref card :purchase_bias))))
         (top-signals (loop for summary in (card-ref card :mode_summaries)
                            for entry = (first-salient-entry summary)
                            when entry
                              collect (format nil "~a → ~a"
                                              (summary-ref summary :name)
                                              (entry-ref entry :value)))))
    (format nil "Share ~,3F; purchase ~,3F / not_purchase ~,3F. Top signals: ~{~a~^, ~}."
            share purchase not-purchase
            (subseq top-signals 0 (min 3 (length top-signals))))))

(defun evidence-line (card)
  (let ((segments '()))
    (dolist (summary (card-ref card :mode_summaries))
      (let ((entry (first-salient-entry summary)))
        (when entry
          (push (format nil "~a (prob ~,3F, lift ~,3F)"
                        (entry-ref entry :value)
                        (entry-ref entry :prob)
                        (entry-ref entry :lift))
                segments))))
    (if segments
        (format nil "~{~a~^; ~}" (nreverse (subseq segments 0 (min 3 (length segments)))))
        "No salient categories identified.")))

(defun recommendations-line (card)
  (let ((segments '()))
    (dolist (summary (card-ref card :mode_summaries))
      (let ((entry (first-salient-entry summary)))
        (when entry
          (push (format nil "Emphasize ~a for ~a"
                        (entry-ref entry :value)
                        (summary-ref summary :name))
                segments))))
    (if segments
        (format nil "~{~a~^; ~}" (nreverse (subseq segments 0 (min 2 (length segments)))))
        "Maintain baseline messaging; no dominant focus.")))

(defun caveats-line (card)
  (let ((segments '()))
    (dolist (summary (card-ref card :mode_summaries))
      (when (< (summary-ref summary :coherence) 0.5d0)
        (push (format nil "~a coherence ~,3F indicates diffused preference"
                      (summary-ref summary :name)
                      (summary-ref summary :coherence))
              segments))
      (let ((neg (first (summary-ref summary :negatives))))
        (when neg
          (push (format nil "Avoid ~a (prob ~,3F)"
                        (entry-ref neg :value)
                        (entry-ref neg :prob))
                segments))))
    (when (cdr (assoc :missing_labels_ignored (card-ref card :notes)))
      (push "Missing labels were excluded from salient sets." segments))
    (if segments
        (format nil "~{~a~^; ~}" (nreverse segments))
        "No significant caveats surfaced.")))

(defun factor-report-markdown-string (cards)
  (with-output-to-string (out)
    (write-string "# Scenario Cards\n\n" out)
    (dolist (card cards)
      (format out "## Factor ~A — ~A~%~%"
              (1+ (card-ref card :factor_id))
              (headline-title card))
      (format out "**Summary:** ~A~%~%" (summary-line card))
      (format out "**Evidence:** ~A~%~%" (evidence-line card))
      (format out "**Recommendations:** ~A~%~%" (recommendations-line card))
      (format out "**Caveats:** ~A~%~%" (caveats-line card))
      (write-char #\Newline out))))

(defun write-scenario-report (cards path)
  (let ((pathname (pathname path)))
    (with-open-file (out pathname :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
      (write-string (factor-report-markdown-string cards) out))))

(defun generate-report-artifacts (factor-matrix-vector X-indices-matrix X-value-vector metadata
                                &key lambda population-marginals
                                     (target-coverage 0.8d0)
                                     (epsilon *reporting-epsilon*)
                                     (factor-json-path "factor_cards.json")
                                     (report-path "report.md")
                                     json-serializer
                                     (include-diagnostics nil))
  "Generate factor cards, JSON output, and markdown report.

When INCLUDE-DIAGNOSTICS is T, the returned result and JSON output include
model-level diagnostics (:model_diagnostics) and factor cards with KL contributions.

The markdown report is generated from factor cards only (diagnostics are not
included in the markdown format)."
  (let ((result (generate-factor-cards factor-matrix-vector
                                       X-indices-matrix
                                       X-value-vector
                                       metadata
                                       :lambda lambda
                                       :population-marginals population-marginals
                                       :target-coverage target-coverage
                                       :epsilon epsilon
                                       :include-diagnostics include-diagnostics)))
    ;; Extract cards for markdown report (handle both formats)
    (let ((cards (if include-diagnostics
                     (cdr (assoc :factors result))
                     result)))
      (when json-serializer
        (write-factor-cards-json result factor-json-path :serializer json-serializer))
      (write-scenario-report cards report-path)
      result)))
