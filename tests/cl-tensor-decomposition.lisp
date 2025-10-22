(defpackage cltd-test
  (:use :cl
        :cltd
        :prove))
(in-package :cltd-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-tensor-decomposition)' in your Lisp.

(plan nil)

(defparameter X-shape '(2 3 4))
(defparameter X-indices-matrix
  (make-array '(3 3) :element-type 'fixnum
              :initial-contents '((0 1 0) ; The row corresponds one datum
                                  (1 2 3)
                                  (0 0 1))))
(defparameter X-value-vector
  (make-array 3 :element-type 'double-float :initial-contents '(1.0d0 2.0d0 3.0d0)))

(ok (decomposition X-shape X-indices-matrix X-value-vector :n-cycle 100 :R 2 :verbose t))

(let* ((mode0 (make-array '(2 2) :element-type 'double-float
                          :initial-contents '((0.6d0 0.2d0)
                                              (0.4d0 0.8d0))))
       (mode1 (make-array '(3 2) :element-type 'double-float
                          :initial-contents '((0.7d0 0.2d0)
                                              (0.2d0 0.3d0)
                                              (0.1d0 0.5d0))))
       (factor-matrices (make-array 2 :initial-contents (list mode0 mode1)))
       (indices (make-array '(4 2) :element-type 'fixnum
                            :initial-contents '((0 0)
                                                (0 1)
                                                (1 0)
                                                (1 2))))
       (counts (make-array 4 :element-type 'double-float
                           :initial-contents '(30d0 10d0 5d0 15d0)))
       (metadata (list
                  (cltd:make-mode-metadata :purchase '("purchase" "not_purchase")
                                           :role :purchase
                                           :positive-label "purchase"
                                           :negative-label "not_purchase"
                                           :discretization "binary")
                  (cltd:make-mode-metadata "genre" '("gourmet" "beauty" "travel")
                                           :discretization "manual top3"))))
  (let ((cards (cltd:generate-factor-cards factor-matrices indices counts metadata)))
    (ok (= (length cards) 2) "Generated cards for each factor")
    (let ((share-sum (loop for card in cards
                           sum (cdr (assoc :share (cdr (assoc :coverage card)))))))
      (ok (< (abs (- share-sum 1d0)) 0.02d0) "Coverage shares sum to ~1"))
    (let ((markdown (cltd::factor-report-markdown-string cards)))
      (ok (search "## Factor 1" markdown) "Markdown report includes headings"))
    (uiop:with-temporary-file (:pathname json-path :suffix "json")
      (cltd:write-factor-cards-json cards json-path
                                    :serializer (lambda (data stream)
                                                  (write data :stream stream :readably t)))
      (ok (probe-file json-path) "factor_cards.json written"))
    (uiop:with-temporary-file (:pathname report-path :suffix "md")
      (cltd:write-scenario-report cards report-path)
      (ok (and (probe-file report-path)
               (search "Scenario Cards" (uiop:read-file-string report-path)))
          "report.md written with content"))))

(multiple-value-bind (result-vec iterations)
    (cltd:decomposition X-shape X-indices-matrix X-value-vector
                        :n-cycle 100
                        :R 2
                        :convergence-threshold 1d6
                        :convergence-window 3)
  (declare (ignore result-vec))
  (ok (< iterations 100) "Convergence threshold stops early")
  (ok (>= iterations 3) "At least window iterations executed"))

(finalize)
