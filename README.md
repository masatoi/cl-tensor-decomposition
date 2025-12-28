[![CI](https://github.com/masatoi/cl-tensor-decomposition/actions/workflows/test.yml/badge.svg)](https://github.com/masatoi/cl-tensor-decomposition/actions/workflows/test.yml)

# cl-tensor-decomposition

- Non-negative tensor decomposition implementation for Common Lisp
- Update algorithm: Multiplicative Update (MU) for minimization of KL divergence
- Support sparse tensor only

## Installation
```lisp
ros install masatoi/cl-tensor-decomposition
```

## Usage
```lisp
(in-package :cltd)

;; Prepare sparse tensor data

(defparameter *shape* '(2 3 4))
(defparameter *nnz* 3)  ; number of non-zero elements

(defparameter *indices*
  (make-array (list *nnz* (length *shape*))
              :element-type 'fixnum
              :initial-contents '((0 1 0)   ; each row is a coordinate
                                  (1 2 3)
                                  (0 0 1))))

(defparameter *values*
  (make-array *nnz*
              :element-type 'double-float
              :initial-contents '(1d0 2d0 3d0)))

;; Create sparse tensor (validates input)
(defparameter *tensor*
  (make-sparse-tensor *shape* *indices* *values*))

;; Run decomposition
(decomposition *tensor* :n-cycle 10 :r 2 :verbose t)

#|
cycle: 1, kl-divergence: 9.594987
cycle: 2, kl-divergence: 8.59509
...
cycle: 10, kl-divergence: 0.7493448
#(#2A((0.0 1.289) (0.734 0.0))
  #2A((0.0 1.333) (0.0 0.444) (1.719 0.0))
  #2A((0.0 0.436) (0.0 1.309) (0.0 0.0) (1.585 0.0)))
|#
```

## Testing
```lisp
(asdf:test-system :cl-tensor-decomposition)
```

## Reporting

After running `decomposition`, you can summarise the factors with the reporting helpers:

```lisp
;; Create tensor with domain metadata
(defparameter *tensor*
  (make-sparse-tensor *shape* *indices* *values*
    :domains (list
              (make-mode-metadata "purchase" '("yes" "no")
                                  :role :purchase
                                  :positive-label "yes"
                                  :negative-label "no"
                                  :discretization "binary outcome")
              (make-mode-metadata "genre" *genre-names*
                                  :role :product
                                  :discretization "top categories"))))

;; Run decomposition with early stopping
(multiple-value-bind (factors iterations)
    (decomposition *tensor*
                   :r 3
                   :n-cycle 200
                   :convergence-threshold 1d-4
                   :convergence-window 5)
  (format t "Converged after ~a iterations~%" iterations)

  ;; Generate factor cards
  (let* ((indices (sparse-tensor-indices *tensor*))
         (values (sparse-tensor-values *tensor*))
         (domains (sparse-tensor-domains *tensor*))
         (cards (generate-factor-cards factors indices values domains)))

    ;; Write JSON and markdown report
    (generate-report-artifacts factors indices values domains
                               :json-serializer #'cl-json:encode-json
                               :factor-json-path #P"factor_cards.json"
                               :report-path #P"report.md")))
```

### Mode Metadata

`make-mode-metadata` creates descriptive metadata for each tensor mode:

| Parameter | Description |
|-----------|-------------|
| `name` | Mode name (string or symbol) |
| `labels` | List of category labels |
| `:discretization` | How the mode was discretized (e.g., "quartiles", "binary") |
| `:role` | Semantic role (e.g., `:purchase`, `:demographic`, `:temporal`) |
| `:positive-label` | Label for positive outcome in binary modes |
| `:negative-label` | Label for negative outcome in binary modes |

These fields are reflected in factor cards as `:mode_roles`, `:purchase_bias`, and `:notes`.

### Output

`generate-factor-cards` returns nested alists; pass them to your JSON library of choice. `generate-report-artifacts` optionally accepts `:json-serializer` (a function of `(data stream)`) so you can control JSON encoding, and always emits a human-oriented `report.md`.

Set `:convergence-threshold` to enable early stopping based on a moving-average KL divergence check; the primary return value remains the factor matrices, and the secondary return value reports how many iterations actually ran.

## Rank Selection

Use `select-rank` for k-fold cross-validation over candidate ranks:

```lisp
(let ((indices (sparse-tensor-indices *tensor*))
      (values (sparse-tensor-values *tensor*)))
  (multiple-value-bind (best all-results)
      (select-rank indices values
                   '(5 10 15)
                   :k 3
                   :n-cycle 50
                   :convergence-threshold 1d-4
                   :convergence-window 5
                   :random-state (make-random-state t))
    (format t "Best rank: ~a with mean KL ~,5f~%"
            (cdr (assoc :rank best))
            (cdr (assoc :mean best)))

    ;; Run final decomposition with best rank
    (decomposition *tensor*
                   :r (cdr (assoc :rank best))
                   :convergence-threshold 1d-4
                   :convergence-window 5)))
```

`cross-validate-rank` returns detailed fold statistics if you need manual inspection; pass your own evaluation function to use RMSE/MAE.

### Model of a sparse tensor
A sparse tensor consists of pairs of non-zero values and indices.
![Tensor Data Image](./docs/images/tensor-data-image.png)

## Reference

- [Multiple Data Analysis and Non-negative Matrix/Tensor Factorization [II. Finish] : Tensor Data Analysis and Applications](http://www.kecl.ntt.co.jp/icl/ls/members/tatsushi/PDF/IEICE_vol99_no7_691-698.pdf)
