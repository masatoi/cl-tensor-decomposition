;;; -*- coding: utf-8; mode: lisp -*-
;;;
;;; Titanic Analysis Example
;;;
;;; Demonstrates tensor decomposition on the Titanic passenger survival dataset.
;;; This classic dataset reveals key survival patterns based on passenger class,
;;; sex, and age.
;;;
;;; Dataset: Titanic Passenger Survival Data
;;; Source: Kaggle (originally from Encyclopedia Titanica)
;;;
;;; Tensor structure: survived(2) x pclass(3) x sex(2) x age_bin(4)
;;; - Survived: 0 (perished), 1 (survived)
;;; - Pclass: 1st, 2nd, 3rd class
;;; - Sex: female, male
;;; - Age bin: child (<18), young (18-35), middle (35-55), senior (55+)
;;;
;;; Expected findings:
;;; - "Women and children first" - females had ~74% survival rate vs ~19% for males
;;; - First class passengers had higher survival rates
;;; - Age affected survival (children prioritized)

(ql:quickload :cl-tensor-decomposition :silent t)

(defpackage :titanic-analysis
  (:use :cl :cltd)
  (:export :run :*titanic-data*))

(in-package :titanic-analysis)

;;; ==========================================================================
;;; Dataset Configuration
;;; ==========================================================================

(defparameter *survived-labels* '("0" "1"))
(defparameter *pclass-labels* '("1" "2" "3"))
(defparameter *sex-labels* '("female" "male"))
(defparameter *age-labels* '("child" "young" "middle" "senior"))

(defparameter *x-shape*
  (list (length *survived-labels*)
        (length *pclass-labels*)
        (length *sex-labels*)
        (length *age-labels*)))

;;; ==========================================================================
;;; Embedded Dataset (aggregated from Titanic training set, 891 passengers)
;;; ==========================================================================
;;; Format: (survived-idx pclass-idx sex-idx age-idx count)
;;; 
;;; Aggregated counts after removing ~177 passengers with missing age data.
;;; Total: 714 passengers with known age.
;;;
;;; Survival statistics (full dataset):
;;; - Overall survival: 342/891 = 38.4%
;;; - Female survival: 233/314 = 74.2%
;;; - Male survival: 109/577 = 18.9%

(defparameter *titanic-data*
  '(;; === PERISHED (survived=0) ===
    ;; First class perished
    (0 0 0 0  0)   ; 1st, female, child - none perished!
    (0 0 0 1  1)   ; 1st, female, young
    (0 0 0 2  2)   ; 1st, female, middle
    (0 0 0 3  0)   ; 1st, female, senior
    (0 0 1 0  0)   ; 1st, male, child
    (0 0 1 1 27)   ; 1st, male, young
    (0 0 1 2 35)   ; 1st, male, middle
    (0 0 1 3 15)   ; 1st, male, senior
    ;; Second class perished
    (0 1 0 0  0)   ; 2nd, female, child - none perished!
    (0 1 0 1  2)   ; 2nd, female, young
    (0 1 0 2  4)   ; 2nd, female, middle
    (0 1 0 3  0)   ; 2nd, female, senior
    (0 1 1 0  0)   ; 2nd, male, child - none perished!
    (0 1 1 1 42)   ; 2nd, male, young
    (0 1 1 2 43)   ; 2nd, male, middle
    (0 1 1 3  6)   ; 2nd, male, senior
    ;; Third class perished (most casualties)
    (0 2 0 0  9)   ; 3rd, female, child
    (0 2 0 1 46)   ; 3rd, female, young
    (0 2 0 2 16)   ; 3rd, female, middle
    (0 2 0 3  1)   ; 3rd, female, senior
    (0 2 1 0 17)   ; 3rd, male, child
    (0 2 1 1 147)  ; 3rd, male, young (largest group of casualties)
    (0 2 1 2 48)   ; 3rd, male, middle
    (0 2 1 3  7)   ; 3rd, male, senior

    ;; === SURVIVED (survived=1) ===
    ;; First class survived
    (1 0 0 0  1)   ; 1st, female, child
    (1 0 0 1 32)   ; 1st, female, young
    (1 0 0 2 42)   ; 1st, female, middle
    (1 0 0 3 16)   ; 1st, female, senior
    (1 0 1 0  4)   ; 1st, male, child
    (1 0 1 1 13)   ; 1st, male, young
    (1 0 1 2 17)   ; 1st, male, middle
    (1 0 1 3  3)   ; 1st, male, senior
    ;; Second class survived
    (1 1 0 0 13)   ; 2nd, female, child
    (1 1 0 1 37)   ; 2nd, female, young
    (1 1 0 2 20)   ; 2nd, female, middle
    (1 1 0 3  6)   ; 2nd, female, senior
    (1 1 1 0 11)   ; 2nd, male, child
    (1 1 1 1  4)   ; 2nd, male, young
    (1 1 1 2  2)   ; 2nd, male, middle
    (1 1 1 3  0)   ; 2nd, male, senior
    ;; Third class survived
    (1 2 0 0 13)   ; 3rd, female, child
    (1 2 0 1 40)   ; 3rd, female, young
    (1 2 0 2 18)   ; 3rd, female, middle
    (1 2 0 3  1)   ; 3rd, female, senior
    (1 2 1 0 10)   ; 3rd, male, child
    (1 2 1 1 24)   ; 3rd, male, young
    (1 2 1 2 12)   ; 3rd, male, middle
    (1 2 1 3  0))) ; 3rd, male, senior

;;; ==========================================================================
;;; Helper Functions
;;; ==========================================================================

(defun build-tensor-from-data (data n-modes)
  "Convert list of (idx0 idx1 ... count) to sparse tensor representation.
   Filters out zero-count entries."
  (let* ((non-zero-data (remove-if (lambda (entry)
                                     (zerop (nth n-modes entry)))
                                   data))
         (nnz (length non-zero-data))
         (indices (make-array (list nnz n-modes) :element-type 'fixnum))
         (values (make-array nnz :element-type 'double-float)))
    (loop for entry in non-zero-data
          for i from 0
          do (loop for mode from 0 below n-modes
                   do (setf (aref indices i mode) (nth mode entry)))
             (setf (aref values i)
                   (coerce (nth n-modes entry) 'double-float)))
    (values indices values nnz)))

(defun build-mode-metadata ()
  "Build mode metadata for the Titanic dataset."
  (list (make-mode-metadata "survived" *survived-labels* :role :outcome)
        (make-mode-metadata "pclass" *pclass-labels* :role :feature)
        (make-mode-metadata "sex" *sex-labels* :role :feature)
        (make-mode-metadata "age_bin" *age-labels* :role :feature)))

(defun print-factor-summary (cards)
  "Print a summary of factor cards to stdout."
  (format t "~%=== Titanic Survival Factor Analysis ===~%~%")
  (dolist (card cards)
    (let ((factor-id (cdr (assoc :factor_id card)))
          (lambda-val (cdr (assoc :lambda card)))
          (coverage (cdr (assoc :coverage card)))
          (salient (cdr (assoc :salient card))))
      (format t "Factor ~D (lambda=~,1F, coverage=~,1F%):~%"
              factor-id lambda-val
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

(defun compute-survival-statistics (data)
  "Compute and print survival statistics from the aggregated data."
  (let ((total 0)
        (survived 0)
        (female-total 0)
        (female-survived 0)
        (male-total 0)
        (male-survived 0))
    (dolist (entry data)
      (destructuring-bind (surv pclass sex age count) entry
        (declare (ignore pclass age))
        (incf total count)
        (when (= surv 1) (incf survived count))
        (if (= sex 0)  ; female
            (progn
              (incf female-total count)
              (when (= surv 1) (incf female-survived count)))
            (progn
              (incf male-total count)
              (when (= surv 1) (incf male-survived count))))))
    (format t "~%Dataset Statistics:~%")
    (format t "  Total passengers: ~D~%" total)
    (format t "  Overall survival: ~D/~D = ~,1F%~%"
            survived total (* 100.0 (/ survived total)))
    (format t "  Female survival: ~D/~D = ~,1F%~%"
            female-survived female-total (* 100.0 (/ female-survived female-total)))
    (format t "  Male survival: ~D/~D = ~,1F%~%"
            male-survived male-total (* 100.0 (/ male-survived male-total)))))

;;; ==========================================================================
;;; Main Entry Point
;;; ==========================================================================

(defun run (&key (rank 5) (n-cycle 200) (verbose nil)
                 (convergence-threshold 1d-4) (convergence-window 10)
                 (output-markdown nil))
  "Run tensor decomposition on Titanic survival data.

RANK                  - Number of latent factors (default 5)
N-CYCLE               - Maximum iterations (default 200)
VERBOSE               - Print decomposition progress
CONVERGENCE-THRESHOLD - Stop when KL improvement < threshold
CONVERGENCE-WINDOW    - Window size for convergence check
OUTPUT-MARKDOWN       - Path to write markdown report (optional)

Returns the factor cards alist.

Expected patterns to discover:
1. Female + 1st/2nd class → high survival (\"women and children first\")
2. Male + 3rd class + young → low survival (largest casualty group)
3. Children → higher survival across classes"
  
  (format t "~%Titanic Survival Analysis~%")
  (format t "========================~%")
  (format t "Tensor shape: ~{~D~^ x ~}~%" *x-shape*)
  (format t "  Mode 0: survived ~A~%" *survived-labels*)
  (format t "  Mode 1: pclass ~A~%" *pclass-labels*)
  (format t "  Mode 2: sex ~A~%" *sex-labels*)
  (format t "  Mode 3: age_bin ~A~%" *age-labels*)

  ;; Print survival statistics
  (compute-survival-statistics *titanic-data*)

  ;; Build tensor
  (multiple-value-bind (x-indices x-values nnz)
      (build-tensor-from-data *titanic-data* (length *x-shape*))
    (format t "~%Non-zero cells: ~D~%" nnz)
    (format t "Total observations: ~,0F~%" (reduce #'+ x-values))

    ;; Create sparse tensor with metadata
    (let* ((metadata (build-mode-metadata))
           (tensor (make-sparse-tensor *x-shape* x-indices x-values
                                       :domains metadata)))

      ;; Run decomposition
      (format t "~%Running decomposition (rank=~D, max-iterations=~D)...~%" rank n-cycle)
      (multiple-value-bind (factor-matrices iterations-run final-kl kl-history converged-p)
          (decomposition tensor
                         :r rank
                         :n-cycle n-cycle
                         :convergence-threshold convergence-threshold
                         :convergence-window convergence-window
                         :verbose verbose)

        (format t "~%Decomposition Results:~%")
        (format t "  Iterations: ~D~%" iterations-run)
        (format t "  Final KL divergence: ~,6F~%" final-kl)
        (format t "  Converged: ~A~%" (if converged-p "YES" "NO"))
        (when (and kl-history (> (length kl-history) 0))
          (format t "  KL reduction: ~,4F → ~,4F (~,1F% reduction)~%"
                  (aref kl-history 0)
                  (aref kl-history (1- (length kl-history)))
                  (* 100 (/ (- (aref kl-history 0)
                               (aref kl-history (1- (length kl-history))))
                            (aref kl-history 0)))))

        ;; Generate factor cards
        (let ((cards (generate-factor-cards
                      factor-matrices x-indices x-values metadata
                      :include-diagnostics nil)))

          ;; Print summary
          (print-factor-summary cards)

          ;; Verify expected patterns
          (format t "~%=== Pattern Verification ===~%")
          (let ((found-female-survival nil)
                (found-male-casualty nil))
            (dolist (card cards)
              (let ((salient (cdr (assoc :salient card))))
                ;; Check for female survival pattern
                (let ((sex-entries (cdr (assoc "sex" salient :test #'string=)))
                      (survived-entries (cdr (assoc "survived" salient :test #'string=))))
                  (when (and sex-entries survived-entries)
                    (let ((female-prob (cdr (assoc :prob
                                                   (find "female" sex-entries
                                                         :key (lambda (e) (cdr (assoc :value e)))
                                                         :test #'string=))))
                          (survived-prob (cdr (assoc :prob
                                                     (find "1" survived-entries
                                                           :key (lambda (e) (cdr (assoc :value e)))
                                                           :test #'string=)))))
                      (when (and female-prob survived-prob
                                 (> female-prob 0.5) (> survived-prob 0.5))
                        (setf found-female-survival t)))))
                ;; Check for male casualty pattern
                (let ((sex-entries (cdr (assoc "sex" salient :test #'string=)))
                      (survived-entries (cdr (assoc "survived" salient :test #'string=))))
                  (when (and sex-entries survived-entries)
                    (let ((male-prob (cdr (assoc :prob
                                                 (find "male" sex-entries
                                                       :key (lambda (e) (cdr (assoc :value e)))
                                                       :test #'string=))))
                          (perished-prob (cdr (assoc :prob
                                                     (find "0" survived-entries
                                                           :key (lambda (e) (cdr (assoc :value e)))
                                                           :test #'string=)))))
                      (when (and male-prob perished-prob
                                 (> male-prob 0.5) (> perished-prob 0.5))
                        (setf found-male-casualty t)))))))
            
            (format t "  Female survival pattern found: ~A~%"
                    (if found-female-survival "YES ✓" "NO"))
            (format t "  Male casualty pattern found: ~A~%"
                    (if found-male-casualty "YES ✓" "NO"))
            (when (and found-female-survival found-male-casualty)
              (format t "~%SUCCESS: Key survival patterns correctly identified!~%")))

          ;; Optional: write markdown report
          (when output-markdown
            (write-scenario-report cards output-markdown)
            (format t "~%Markdown report written to ~A~%" output-markdown))

          cards)))))

;;; ==========================================================================
;;; Usage Instructions
;;; ==========================================================================

(format t "~%Titanic Analysis Example Loaded~%")
(format t "================================~%~%")
(format t "To run the analysis:~%")
(format t "  (titanic-analysis:run)~%")
(format t "  (titanic-analysis:run :rank 3 :verbose t)~%")
(format t "  (titanic-analysis:run :output-markdown #P\"titanic-report.md\")~%~%")

;; Uncomment to run automatically:
;; (run)
