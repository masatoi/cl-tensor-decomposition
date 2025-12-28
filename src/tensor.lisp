;;; -*- coding:utf-8; mode:lisp -*-

;;;; tensor.lisp - Sparse tensor utilities and constructors
;;;;
;;;; This module provides utilities for creating and working with sparse tensors.
;;;; The mode-spec and sparse-tensor structures are defined in core.lisp.

(in-package :cl-tensor-decomposition)

;;; ============================================================================
;;; Internal Utilities
;;; ============================================================================

(defun %remove-from-plist (plist &rest keys)
  "Remove KEYS and their values from PLIST. Returns a new plist."
  (loop for (key value) on plist by #'cddr
        unless (member key keys :test #'eq)
          collect key and collect value))

;;; ============================================================================
;;; Mode Metadata Utilities
;;; ============================================================================

(defun %normalize-mode-name (name)
  "Convert NAME to a normalized string representation for mode names.
Symbols are downcased, strings pass through unchanged, other types are printed."
  (typecase name
    (string name)
    (symbol (string-downcase (symbol-name name)))
    (t (princ-to-string name))))

(defun %normalize-label (value)
  "Convert VALUE to a normalized string representation for category labels.
Handles strings, symbols (downcased), numbers, and characters."
  (typecase value
    (string value)
    (symbol (string-downcase (symbol-name value)))
    (number (princ-to-string value))
    (character (string value))
    (t (princ-to-string value))))

(defun make-mode-metadata (name labels &key (discretization "unspecified")
                                         (missing-labels nil)
                                         role
                                         positive-label
                                         negative-label)
  "Create a mode-spec structure with normalized names and labels.

NAME             - Name for this mode (string or symbol)
LABELS           - List or vector of category labels for this mode
DISCRETIZATION   - Description of discretization method (default \"unspecified\")
MISSING-LABELS   - List of labels representing missing values
ROLE             - Semantic role keyword (e.g., :purchase, :time)
POSITIVE-LABEL   - Label for positive outcome in binary modes
NEGATIVE-LABEL   - Label for negative outcome in binary modes

Returns a mode-spec structure with normalized string representations."
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

(defun %coerce-domain-spec (spec mode-index)
  "Coerce SPEC to a mode-spec structure.
SPEC can be:
  - A mode-spec structure (returned as-is)
  - NIL (returned as-is)
  - A plist with :name key (converted via make-mode-metadata)
Signals invalid-input-error for other inputs."
  (typecase spec
    (mode-spec spec)
    (null nil)
    (list
     (if (getf spec :name)
         (make-mode-metadata (getf spec :name)
                             (getf spec :labels)
                             :discretization (or (getf spec :discretization) "unspecified")
                             :missing-labels (getf spec :missing-labels)
                             :role (getf spec :role)
                             :positive-label (getf spec :positive-label)
                             :negative-label (getf spec :negative-label))
         (error 'invalid-input-error
                :reason :invalid-domain-spec
                :details (format nil "domains[~D] is a list but missing :name key: ~S"
                                 mode-index spec))))
    (t
     (error 'invalid-input-error
            :reason :invalid-domain-spec
            :details (format nil "domains[~D] must be mode-spec, plist with :name, or NIL: ~S"
                             mode-index spec)))))

;;; ============================================================================
;;; Sparse Tensor Validation
;;; ============================================================================

(defun %validate-domains (domains shape)
  "Validate domain metadata against tensor shape.
DOMAINS must be a vector of mode-spec structures (or NIL entries).
Signals invalid-input-error if validation fails."
  (when domains
    (let ((n-modes (length shape)))
      (unless (= (length domains) n-modes)
        (error 'invalid-input-error
               :reason :domains-mode-mismatch
               :details (format nil "domains has ~D entries but tensor has ~D modes"
                                (length domains) n-modes)))
      (loop for mode from 0 below n-modes
            for domain = (svref domains mode)
            for dim = (nth mode shape)
            when domain do
              (let ((labels (mode-spec-labels domain)))
                (when labels
                  (unless (= (length labels) dim)
                    (error 'invalid-input-error
                           :reason :domain-labels-mismatch
                           :details (format nil "mode ~D (~A) has ~D categories but ~D labels"
                                            mode (mode-spec-name domain)
                                            dim (length labels))))))))))

;;; ============================================================================
;;; Sparse Tensor Constructor
;;; ============================================================================

(defun make-sparse-tensor (shape indices values &key domains aux)
  "Create a validated sparse-tensor structure.

SHAPE   - List of dimension sizes for each mode
INDICES - 2D fixnum array of non-zero coordinates (nnz x n-modes)
VALUES  - 1D double-float array of observed counts
DOMAINS - Optional list/vector of mode-spec or plist with :name key
AUX     - Optional auxiliary data

Validates all inputs for consistency:
- Shape must be a non-empty list of positive integers
- Indices must be within bounds
- Values must be non-negative and finite
- Domains (if provided) must be mode-spec or plist with :name, and match dimensions

Signals invalid-input-error if validation fails."
  ;; Validate core tensor data first
  (validate-input-data shape indices values)
  ;; Convert and validate domains if provided
  (let ((domain-vector
          (when domains
            (let* ((n-modes (length shape))
                   (domains-length (if (listp domains)
                                       (length domains)
                                       (length domains))))
              ;; Check domains length matches mode count
              (unless (= domains-length n-modes)
                (error 'invalid-input-error
                       :reason :domains-mode-mismatch
                       :details (format nil "domains has ~D entries but tensor has ~D modes"
                                        domains-length n-modes)))
              ;; Convert to vector of mode-specs
              (let ((vec (make-array n-modes)))
                (loop for i from 0 below n-modes
                      for spec = (if (listp domains)
                                     (nth i domains)
                                     (svref domains i))
                      do (setf (svref vec i) (%coerce-domain-spec spec i)))
                vec)))))
    ;; Validate domain labels against shape dimensions
    (%validate-domains domain-vector shape)
    ;; Construct
    (%make-sparse-tensor :shape shape
                         :indices indices
                         :values values
                         :domains domain-vector
                         :aux aux)))

;;; ============================================================================
;;; Sparse Tensor Accessors and Utilities
;;; ============================================================================

(defun sparse-tensor-nnz (tensor)
  "Return the number of non-zero entries in TENSOR."
  (length (sparse-tensor-values tensor)))

(defun sparse-tensor-n-modes (tensor)
  "Return the number of modes (dimensions) in TENSOR."
  (length (sparse-tensor-shape tensor)))

(defun sparse-tensor-mode-labels (tensor mode)
  "Return the labels vector for MODE in TENSOR, or NIL if not available."
  (when (sparse-tensor-domains tensor)
    (let ((domain (svref (sparse-tensor-domains tensor) mode)))
      (when domain
        (mode-spec-labels domain)))))

(defun sparse-tensor-mode-name (tensor mode)
  "Return the name string for MODE in TENSOR, or NIL if not available."
  (when (sparse-tensor-domains tensor)
    (let ((domain (svref (sparse-tensor-domains tensor) mode)))
      (when domain
        (mode-spec-name domain)))))

(defun sparse-tensor-total-count (tensor)
  "Return the sum of all values in TENSOR."
  (loop for i from 0 below (length (sparse-tensor-values tensor))
        sum (aref (sparse-tensor-values tensor) i) double-float))
