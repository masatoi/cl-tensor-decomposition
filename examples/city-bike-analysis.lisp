(ql:quickload '(:cl-tensor-decomposition :fare-csv :local-time :cl-json))

(defpackage :city-bike-analysis
  (:use :cl :cltd)
  (:import-from :cl-json :encode-json-to-string)
  (:import-from :local-time :parse-timestring :timestamp-day-of-week :timestamp-hour)
  (:import-from :uiop :ensure-directories-exist :ensure-directory-pathname
                :pathname-directory-pathname)
  (:export :run))

(in-package :city-bike-analysis)

(defparameter *dataset-root*
  (merge-pathnames #P"datasets/city-bike/" (truename #P"~/"))
  "Base directory that should contain decompressed Citi Bike tripdata CSV files.")

(defparameter *row-limit* nil
  "Optional limit on the number of valid rides to ingest. Useful for quick smoke runs.")

(defparameter *top-start-stations* 60
  "Number of busiest start stations to track explicitly; the remainder go into an \"other\" bucket.")

(defparameter *rank* 6
  "Latent rank used for the tensor decomposition.")

(defparameter *n-cycle* 80
  "Maximum number of update cycles for the multiplicative update solver.")

(defparameter *convergence-threshold* 1d-4
  "Relative change threshold on the smoothed KL divergence to stop early.")

(defparameter *convergence-window* 6
  "Window length used when smoothing KL divergence for convergence detection.")

(defparameter *report-subdir* #P"reports/"
  "Relative subdirectory (under the dataset folder) where artifacts will be written.")

(defparameter *report-json-filename* #P"city-bike-factor-cards.json")
(defparameter *report-markdown-filename* #P"city-bike-report.md")

(defparameter +other-station-label+ "Other start stations")
(defparameter +day-labels+ '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(defparameter +hour-labels+
  (loop for hour from 0 below 24
        collect (format nil "~2,'0D:00" hour)))

(defun blank-string-p (value)
  (or (null value)
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) value)))))

(defun clean-string (value)
  (cond
    ((stringp value) (string-trim '(#\Space #\Tab #\Newline #\Return) value))
    ((null value) "")
    (t (clean-string (princ-to-string value)))))

(defun sanitize-station (value)
  (let ((trimmed (clean-string value)))
    (if (blank-string-p trimmed)
        "Unknown station"
        trimmed)))

(defun canonicalize-member (value)
  (let* ((trimmed (clean-string value))
         (lower (string-downcase trimmed)))
    (cond
      ((string= lower "member") "member")
      ((string= lower "casual") "casual")
      (t nil))))

(defun canonicalize-rideable (value)
  (let* ((trimmed (clean-string value)))
    (if (blank-string-p trimmed)
        nil
        (string-downcase trimmed))))

(defun humanize-token (value)
  (string-capitalize (substitute #\Space #\_ (string-downcase value))))

(defun percent-of (part total)
  (if (or (zerop total) (zerop part))
      0d0
      (/ (coerce part 'double-float) (coerce total 'double-float))))

(defun locate-tripdata (&optional dataset-path)
  (cond
    ((and dataset-path (probe-file dataset-path))
     (truename dataset-path))
    (dataset-path
     (error "Tripdata file ~A was not found. Ensure the CSV is present and decompressed."
            dataset-path))
    (t
     (let* ((pattern (merge-pathnames #P"*.csv" *dataset-root*))
            (candidates (directory pattern)))
       (when (null candidates)
         (error "No CSV files found under ~A. Download a Citi Bike tripdata CSV (e.g., 2024-01-citibike-tripdata.csv) and place it there."
                *dataset-root*))
       (car (sort candidates #'string> :key #'namestring))))))

(defun read-tripdata (path)
  (let* ((rows (fare-csv:read-csv-file path))
         (header (mapcar #'string-downcase (car rows)))
         (body (cdr rows)))
    (values header body)))

(defun column-index (header column-name)
  (or (position column-name header :test #'string=)
      (error "Column ~A not present in dataset header ~A." column-name header)))

(defun %try-parse-timestring (text)
  (handler-case
      (parse-timestring text)
    (error () nil)))

(defun parse-start-timestamp (value)
  (let* ((raw (clean-string value)))
    (when (not (blank-string-p raw))
      (or (%try-parse-timestring raw)
          (%try-parse-timestring (substitute #\T #\Space raw))))))

(defun collect-trip-counts (rows header &key row-limit)
  (let* ((start-idx (column-index header "start_station_name"))
         (started-at-idx (column-index header "started_at"))
         (member-idx (column-index header "member_casual"))
         (rideable-idx (column-index header "rideable_type"))
         (counts (make-hash-table :test 'equal))
         (station-counts (make-hash-table :test 'equal))
         (rideable-counts (make-hash-table :test 'equal))
         (member-counts (make-hash-table :test 'equal))
         (processed 0)
         (skipped 0))
    (dolist (row rows)
      (when (and row-limit (>= processed row-limit))
        (return))
      (let ((timestamp (parse-start-timestamp (nth started-at-idx row))))
        (if (null timestamp)
            (incf skipped)
            (let* ((station (sanitize-station (nth start-idx row)))
                   (member (canonicalize-member (nth member-idx row)))
                   (rideable (canonicalize-rideable (nth rideable-idx row))))
              (if (or (null member) (null rideable))
                  (incf skipped)
                  (let* ((dow (timestamp-day-of-week timestamp))
                         (hour (timestamp-hour timestamp))
                         (key (list dow hour station member rideable)))
                    (incf processed)
                    (incf (gethash key counts 0))
                    (incf (gethash station station-counts 0))
                    (incf (gethash rideable rideable-counts 0))
                    (incf (gethash member member-counts 0))))))))
    (values counts station-counts rideable-counts member-counts processed skipped)))

(defun compute-station-labels (station-counts top-n)
  (let ((pairs '()))
    (maphash (lambda (name count)
               (push (cons name count) pairs))
             station-counts)
    (setf pairs (sort pairs #'> :key #'cdr))
    (let* ((selected (subseq pairs 0 (min top-n (length pairs))))
           (labels (cons +other-station-label+ (mapcar #'car selected)))
           (mapping (make-hash-table :test 'equal)))
      (loop for name in (mapcar #'car selected)
            for idx from 1 do
            (setf (gethash name mapping) idx))
      (values labels mapping selected))))

(defun compute-rideable-labels (rideable-counts)
  (let ((pairs '()))
    (maphash (lambda (name count)
               (push (cons name count) pairs))
             rideable-counts)
    (setf pairs (sort pairs #'> :key #'cdr))
    (let ((labels (mapcar (lambda (pair) (humanize-token (car pair))) pairs))
          (mapping (make-hash-table :test 'equal)))
      (loop for pair in pairs
            for idx from 0 do
            (setf (gethash (car pair) mapping) idx))
      (values labels mapping pairs))))

(defun compute-member-labels (member-counts)
  (let ((order '("member" "casual")))
    (dolist (name order)
      (when (null (gethash name member-counts))
        (warn "Encountered member type ~A with zero observations." name)))
    (let ((labels '())
          (mapping (make-hash-table :test 'equal))
          (pairs '()))
      (loop for name in order
            for idx from 0 do
            (push (humanize-token name) labels)
            (setf (gethash name mapping) idx)
            (push (cons name (gethash name member-counts 0)) pairs))
      (values (nreverse labels)
              mapping
              (nreverse pairs)))))

(defun aggregate-final-counts (counts station-map member-map rideable-map)
  (let ((final (make-hash-table :test 'equal)))
    (maphash (lambda (key value)
               (destructuring-bind (dow hour station member rideable) key
                 (let* ((station-index (or (gethash station station-map) 0))
                        (member-index (gethash member member-map))
                        (rideable-index (gethash rideable rideable-map)))
                   (when (and member-index rideable-index)
                     (let ((final-key (list dow hour station-index member-index rideable-index)))
                       (incf (gethash final-key final 0) value))))))
             counts)
    final))

(defun counts->tensor (counts num-modes)
  (let* ((nnz (hash-table-count counts))
         (indices (make-array (list nnz num-modes) :element-type 'fixnum))
         (values (make-array nnz :element-type 'double-float))
         (row 0))
    (maphash (lambda (key value)
               (loop for mode from 0 below num-modes do
                 (setf (aref indices row mode) (nth mode key)))
               (setf (aref values row) (coerce value 'double-float))
               (incf row))
             counts)
    (values indices values)))

(defun build-mode-metadata-list (station-labels member-labels rideable-labels)
  (list
   (make-mode-metadata "Day of week" +day-labels+
                       :role :temporal
                       :discretization "day-of-week")
   (make-mode-metadata "Hour of day" +hour-labels+
                       :role :temporal
                       :discretization "hour-of-day")
   (make-mode-metadata "Start station segment" station-labels
                       :role :location
                       :discretization "top-start-stations")
   (make-mode-metadata "Rider type" member-labels
                       :role :segment
                       :positive-label (first member-labels)
                       :negative-label (second member-labels)
                       :discretization "membership")
   (make-mode-metadata "Bike type" rideable-labels
                       :role :equipment
                       :discretization "rideable-type")))

(defun prepare-city-bike-tensor (&key dataset-path row-limit top-stations)
  (let ((path (locate-tripdata dataset-path)))
    (multiple-value-bind (header rows)
        (read-tripdata path)
      (multiple-value-bind (counts station-counts rideable-counts member-counts processed skipped)
          (collect-trip-counts rows header :row-limit row-limit)
        (when (zerop processed)
          (error "No valid records found after preprocessing."))
        (multiple-value-bind (station-labels station-map top-station-pairs)
            (compute-station-labels station-counts top-stations)
          (multiple-value-bind (rideable-labels rideable-map rideable-pairs)
              (compute-rideable-labels rideable-counts)
            (multiple-value-bind (member-labels member-map member-pairs)
                (compute-member-labels member-counts)
              (let* ((final-counts (aggregate-final-counts counts station-map member-map rideable-map))
                     (num-modes 5)
                     (nnz (hash-table-count final-counts))
                     (total-trips (loop for val being the hash-values of final-counts sum val))
                     (x-shape (list 7
                                    24
                                    (length station-labels)
                                    (length member-labels)
                                    (length rideable-labels)))
                     (others-count (max 0 (- processed
                                             (loop for pair in top-station-pairs sum (cdr pair))))))
                (multiple-value-bind (indices values)
                    (counts->tensor final-counts num-modes)
                  (values x-shape
                          indices
                          values
                          (build-mode-metadata-list station-labels member-labels rideable-labels)
                          (list :dataset-path path
                                :processed processed
                                :skipped skipped
                                :nnz nnz
                                :density (percent-of nnz (apply #'* x-shape))
                                :total-trips total-trips
                                :station-labels station-labels
                                :rideable-labels rideable-labels
                                :member-labels member-labels
                                :top-stations top-station-pairs
                                :rideable-counts rideable-pairs
                                :member-counts member-pairs
                                :others-count others-count)))))))))))

(defun print-preprocessing-summary (summary &key (top-limit 5))
  (let* ((processed (getf summary :processed))
         (skipped (getf summary :skipped))
         (density (getf summary :density))
         (nnz (getf summary :nnz))
         (top-stations (getf summary :top-stations))
         (others (getf summary :others-count))
         (rideable (getf summary :rideable-counts))
         (members (getf summary :member-counts)))
    (format t "~&[Preprocessing] Valid trips: ~:D (skipped ~:D). Non-zero tensor cells: ~:D (density ~,4F).~%"
            processed skipped nnz density)
    (when top-stations
      (let ((limit (min top-limit (length top-stations))))
        (format t "~&[Start Stations] Top ~D buckets (plus ~A):~%" limit +other-station-label+)
        (loop for pair in top-stations
              for idx from 1
              while (<= idx limit) do
              (format t "  ~2D. ~A — ~,2F%% (~:D trips)~%"
                      idx (car pair)
                      (* 100d0 (percent-of (cdr pair) processed))
                      (cdr pair)))
        (format t "  Other bucket (~A) — ~,2F%% (~:D trips)~%"
                +other-station-label+
                (* 100d0 (percent-of others processed))
                others)))
    (when rideable
      (format t "~&[Rideable Mix]~%")
      (dolist (pair rideable)
        (format t "  ~A — ~,2F%% (~:D trips)~%"
                (humanize-token (car pair))
                (* 100d0 (percent-of (cdr pair) processed))
                (cdr pair))))
    (when members
      (format t "~&[Rider Types]~%")
      (dolist (pair members)
        (format t "  ~A — ~,2F%% (~:D trips)~%"
                (humanize-token (car pair))
                (* 100d0 (percent-of (cdr pair) processed))
                (cdr pair))))))

(defun alist-value (alist key)
  (cdr (assoc key alist)))

(defun print-card-preview (cards &key (limit 3))
  (format t "~&[Factor Preview] Showing top ~D factors.~%" (min limit (length cards)))
  (loop for card in cards
        for idx from 1
        while (<= idx limit) do
        (let* ((factor-id (1+ (alist-value card :factor_id)))
               (coverage (alist-value (alist-value card :coverage) :share)))
          (format t "~&Factor ~D — share ~,3F~%" factor-id coverage)
          (dolist (summary (alist-value card :mode_summaries))
            (let* ((name (alist-value summary :name))
                   (salient (alist-value summary :salient))
                   (entry (car salient)))
              (when entry
                (format t "  ~A ⇒ ~A (prob ~,3F, lift ~,3F)~%"
                        name
                        (alist-value entry :value)
                        (alist-value entry :prob)
                        (alist-value entry :lift))))))
          (let ((negatives (alist-value card :negatives)))
            (when negatives
              (let ((first-neg (cadar negatives)))
                (when first-neg
                  (format t "  Caveat: avoid ~A (prob ~,3F)~%"
                          (alist-value first-neg :value)
                          (alist-value first-neg :prob))))))))

(defun write-cards-json (cards stream)
  (write-string (encode-json-to-string cards) stream))

(defun run (&key dataset-path
                 (row-limit *row-limit*)
                 (rank *rank*)
                 (top-stations *top-start-stations*)
                 (n-cycle *n-cycle*)
                 (convergence-threshold *convergence-threshold*)
                 (convergence-window *convergence-window*)
                 (verbose nil))
  (let* ((path (locate-tripdata dataset-path)))
    (format t "~&[City Bike] Using dataset: ~A~%" path)
    (when row-limit
      (format t "[City Bike] Row limit enabled: ingesting first ~:D valid trips.~%" row-limit))
    (multiple-value-bind (x-shape indices values metadata summary)
        (prepare-city-bike-tensor :dataset-path path
                                  :row-limit row-limit
                                  :top-stations top-stations)
      (print-preprocessing-summary summary)
      (format t "~&[Decomposition] Rank=~D, cycles=~D, convergence-threshold=~E~%"
              rank n-cycle convergence-threshold)
      (let ((tensor (cltd:make-sparse-tensor x-shape indices values)))
        (multiple-value-bind (factor-matrix-vector iterations)
            (cltd:decomposition tensor
                           :R rank
                           :n-cycle n-cycle
                           :convergence-threshold convergence-threshold
                           :convergence-window convergence-window
                           :verbose verbose)
        (format t "[Decomposition] Completed after ~D iterations.~%" iterations)
        (let* ((dataset-dir (pathname-directory-pathname path))
               (report-dir (ensure-directory-pathname (merge-pathnames *report-subdir* dataset-dir))))
          (ensure-directories-exist report-dir)
          (let* ((json-path (merge-pathnames *report-json-filename* report-dir))
                 (report-path (merge-pathnames *report-markdown-filename* report-dir))
                 (cards (generate-report-artifacts factor-matrix-vector
                                                   indices
                                                   values
                                                   metadata
                                                   :factor-json-path json-path
                                                   :report-path report-path
                                                   :json-serializer #'write-cards-json)))
            (format t "[Artifacts] JSON written to ~A~%" (namestring json-path))
            (format t "[Artifacts] Markdown report written to ~A~%" (namestring report-path))
            (print-card-preview cards :limit 3)
            (values factor-matrix-vector cards json-path report-path))))))))

(run)
