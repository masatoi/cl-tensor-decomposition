;;; -*- coding: utf-8; mode: lisp -*-
;;;
;;; Palmer Penguins Analysis Example
;;;
;;; A minimal example demonstrating tensor decomposition on the Palmer Penguins
;;; dataset. This dataset is small enough for CI testing while providing
;;; meaningful interpretable results.
;;;
;;; Dataset: Palmer Archipelago (Antarctica) Penguin Data
;;; License: CC0 (Public Domain)
;;; Source: https://allisonhorst.github.io/palmerpenguins/
;;;
;;; Tensor structure: species(3) x island(3) x sex(2)
;;; - Species: Adelie, Chinstrap, Gentoo
;;; - Island: Torgersen, Biscoe, Dream
;;; - Sex: female, male
;;;
;;; The aggregated counts represent the number of penguins observed for each
;;; combination of species, island, and sex.

(ql:quickload '(:cl-tensor-decomposition :fare-csv) :silent t)

(defpackage :palmer-penguins-analysis
  (:use :cl :cltd)
  (:export :run :*penguins-data*))

(in-package :palmer-penguins-analysis)

;;; ==========================================================================
;;; Embedded Dataset (aggregated from Palmer Penguins)
;;; ==========================================================================
;;; Original data has 344 observations. After removing NA values for sex (11 rows)
;;; and aggregating by species/island/sex, we get 13 non-zero cells.

(defparameter *species-labels* '("Adelie" "Chinstrap" "Gentoo"))
(defparameter *island-labels* '("Biscoe" "Dream" "Torgersen"))
(defparameter *sex-labels* '("female" "male"))

(defparameter *x-shape*
  (list (length *species-labels*)
        (length *island-labels*)
        (length *sex-labels*)))

;;; Aggregated count data: (species-idx island-idx sex-idx count)
;;; Derived from palmerpenguins R package data
(defparameter *penguins-data*
  '(;; Adelie penguins
    (0 0 0 22)   ; Adelie, Biscoe, female
    (0 0 1 22)   ; Adelie, Biscoe, male
    (0 1 0 27)   ; Adelie, Dream, female
    (0 1 1 28)   ; Adelie, Dream, male
    (0 2 0 24)   ; Adelie, Torgersen, female
    (0 2 1 23)   ; Adelie, Torgersen, male
    ;; Chinstrap penguins (only on Dream island)
    (1 1 0 34)   ; Chinstrap, Dream, female
    (1 1 1 34)   ; Chinstrap, Dream, male
    ;; Gentoo penguins (only on Biscoe island)
    (2 0 0 58)   ; Gentoo, Biscoe, female
    (2 0 1 61))) ; Gentoo, Biscoe, male

(defun build-tensor-from-data (data n-modes)
  "Convert list of (idx0 idx1 ... count) to sparse tensor representation."
  (let* ((nnz (length data))
         (indices (make-array (list nnz n-modes) :element-type 'fixnum))
         (values (make-array nnz :element-type 'double-float)))
    (loop for entry in data
          for i from 0
          do (loop for mode from 0 below n-modes
                   do (setf (aref indices i mode) (nth mode entry)))
             (setf (aref values i)
                   (coerce (nth n-modes entry) 'double-float)))
    (values indices values)))

(defun build-mode-metadata ()
  "Build mode metadata for the penguins dataset."
  (list (make-mode-metadata "species" *species-labels*)
        (make-mode-metadata "island" *island-labels*)
        (make-mode-metadata "sex" *sex-labels*)))

(defun print-factor-summary (cards)
  "Print a summary of factor cards to stdout."
  (format t "~%=== Palmer Penguins Factor Analysis ===~%~%")
  (dolist (card cards)
    (let ((factor-id (cdr (assoc :factor_id card)))
          (lambda (cdr (assoc :lambda card)))
          (coverage (cdr (assoc :coverage card)))
          (salient (cdr (assoc :salient card))))
      (format t "Factor ~D (lambda=~,1F, coverage=~,1F%):~%"
              factor-id lambda
              (* 100 (cdr (assoc :share coverage))))
      ;; Print salient entries for each mode
      (dolist (mode-entry salient)
        (let ((mode-name (car mode-entry))
              (entries (cdr mode-entry)))
          (format t "  ~A: " mode-name)
          (format t "~{~A~^, ~}~%"
                  (loop for entry in entries
                        collect (format nil "~A (~,0F%)"
                                        (cdr (assoc :value entry))
                                        (* 100 (cdr (assoc :prob entry))))))))
      (format t "~%"))))

(defun run (&key (rank 2) (n-cycle 50) (verbose nil)
                 (output-json nil) (output-markdown nil))
  "Run tensor decomposition on Palmer Penguins data.

RANK          - Number of latent factors (default 2)
N-CYCLE       - Maximum iterations (default 50)
VERBOSE       - Print decomposition progress
OUTPUT-JSON   - Path to write factor cards JSON (optional)
OUTPUT-MARKDOWN - Path to write markdown report (optional)

Returns the factor cards alist."
  (format t "Loading Palmer Penguins dataset...~%")
  (format t "  Shape: ~{~D~^ x ~} (~D non-zero cells)~%"
          *x-shape* (length *penguins-data*))

  ;; Build tensor
  (multiple-value-bind (x-indices x-values)
      (build-tensor-from-data *penguins-data* (length *x-shape*))

    ;; Validate input
    (validate-input-data *x-shape* x-indices x-values)
    (format t "  Total observations: ~,0F~%" (reduce #'+ x-values))

    ;; Run decomposition
    (format t "~%Running decomposition (rank=~D)...~%" rank)
    (multiple-value-bind (factor-matrices iterations)
        (decomposition *x-shape* x-indices x-values
                       :r rank
                       :n-cycle n-cycle
                       :convergence-threshold 1d-5
                       :convergence-window 5
                       :verbose verbose)

      (format t "Converged in ~D iterations~%" iterations)

      ;; Compute KL divergence
      (let ((kl (sparse-kl-divergence x-indices x-values factor-matrices)))
        (format t "Final KL divergence: ~,4F~%" kl))

      ;; Generate factor cards
      (let* ((metadata (build-mode-metadata))
             (cards (generate-factor-cards
                     factor-matrices x-indices x-values metadata
                     :include-diagnostics nil)))

        ;; Print summary
        (print-factor-summary cards)

        ;; Optional: write JSON output
        (when output-json
          (with-open-file (out output-json
                               :direction :output
                               :if-exists :supersede)
            ;; Simple JSON-like output (for full JSON, use cl-json)
            (format out "~S~%" cards))
          (format t "Factor cards written to ~A~%" output-json))

        ;; Optional: write markdown report
        (when output-markdown
          (write-scenario-report cards output-markdown)
          (format t "Markdown report written to ~A~%" output-markdown))

        cards))))

;;; ==========================================================================
;;; Run the example
;;; ==========================================================================

(format t "~%To run the analysis:~%")
(format t "  (palmer-penguins-analysis:run)~%")
(format t "  (palmer-penguins-analysis:run :rank 3 :verbose t)~%")
(format t "  (palmer-penguins-analysis:run :output-markdown #P\"penguins-report.md\")~%~%")

;; Uncomment to run automatically:
;; (run)
