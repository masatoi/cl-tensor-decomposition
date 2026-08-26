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
cycle: 1, kl-divergence: 13.444468
cycle: 2, kl-divergence: 12.223802
...
cycle: 10, kl-divergence: 2.2586484
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

Use `select-rank` for k-fold cross-validation over candidate ranks. It takes the
`sparse-tensor` itself, so the declared shape and the mode metadata survive into
every fold:

```lisp
(multiple-value-bind (best all-results)
    (select-rank *tensor*
                 '(5 10 15)
                 :k 5
                 :n-cycle 50
                 :convergence-threshold 1d-4
                 :convergence-window 5
                 :random-state (make-random-state t))
  (format t "Best rank: ~a with mean score ~,5f~%"
          (cdr (assoc :rank best))
          (cdr (assoc :mean best)))

  ;; Run final decomposition with best rank
  (decomposition *tensor*
                 :r (cdr (assoc :rank best))
                 :convergence-threshold 1d-4
                 :convergence-window 5))
```

`cross-validate-rank` returns the same per-rank statistics if you want to inspect
them yourself. Each entry carries `:rank`, `:mean`, `:std`, `:standard-error`,
`:scores` and `:validation-counts`, in the order the ranks were given. Neither
function sorts that list in place. `select-rank-1se` picks the smallest rank
whose mean is within one standard error (`std / sqrt(k)`) of the best.

### How the folds are built

Counts are **not** split by coordinate. Holding out whole coordinates would be
invalid here: an unstored coordinate is an observed zero, so the training tensor
would present each held-out cell as a zero and the model would then be scored on
recovering a positive count it was fitted to suppress.

Instead every *event* is assigned to a fold uniformly at random — Poisson (
multinomial) thinning:

```
(V_i^1, ..., V_i^k) | X_i  ~  Multinomial(X_i; 1/k, ..., 1/k)
```

Fold `f` trains on `T_i^f = X_i - V_i^f` and validates on `V_i^f`, so the same
cell appears in both halves. Under Poisson thinning the two halves are
independent Poisson samples. Training exposure is `(k-1)/k`, validation exposure
is `1/k`.

Because the fit sees the training exposure, validation predictions are scaled by
the exposure ratio `s = 1/(k-1)` — applied to the stored-coordinate predictions
*and* to the model's total predicted mass, via
`(sparse-kl-divergence ... :prediction-scale s)`.

The default fold score is the generalized KL per validation event:

```
score_f = D(V^f || s * T^^f) / sum_i V_i^f
```

Lower is better. This is not the Poisson deviance — the deviance is twice this.

This requires **non-negative integer counts**; fractional values are rejected
rather than truncated. `k` must be at least 2 and at most the total count, but it
is *not* bounded by the number of stored non-zeros: a single cell holding 100
events splits into 5 folds perfectly well.

`make-poisson-folds` exposes the split directly if you need it:

```lisp
(let ((folds (make-poisson-folds *tensor* 5 :random-state (make-random-state t))))
  (poisson-folds-prediction-scale folds)      ; => 0.25d0
  (multiple-value-bind (train valid train-total valid-total)
      (poisson-fold-tensors folds 0)
    ...))
```

### Custom fold metrics

`:evaluation-function` receives everything a metric needs to apply the exposure
correction itself:

```lisp
(lambda (validation-tensor approximation factor-matrix-vector
         prediction-scale validation-count)
  ...)
```

`approximation` holds the reconstruction at the validation coordinates *at the
training exposure*, so a metric that compares against `validation-count` must
scale it by `prediction-scale`. The default, `normalized-generalized-kl`, does
exactly that.

### Reproducibility

A single `:random-state` drives both the thinning and each fold's factor
initialization, and it is copied rather than advanced, so passing the same state
twice gives identical results. The folds are drawn once and shared by every
candidate rank, and each fold's initialization is fixed in advance, so reordering
`ranks` cannot change any rank's scores.

Nothing is printed unless `:verbose t`.

### Model of a sparse tensor
A sparse tensor consists of pairs of non-zero values and indices.
![Tensor Data Image](./docs/images/tensor-data-image.png)

A coordinate that is absent from the index matrix is an **observed zero**, not a
missing value. There is no observation mask: the tensor is dense in meaning and
sparse only in storage.

### Objective function

`decomposition` minimises the generalized (Poisson) KL divergence between the
observed tensor and its CP reconstruction:

```
D(X||X^) = sum_i [ x_i * log(x_i / x^_i) - x_i + x^_i ]
```

The sum runs over *every* coordinate, so the implicit zeros contribute their
full reconstruction mass. `sparse-kl-divergence` evaluates it in two parts:

```
D(X||X^) = sum_{i in nnz} [ x_i * log(x_i / x^_i) - x_i ]  +  sum_i x^_i
```

The second term is the total predicted mass, aggregated straight from the CP
structure rather than by expanding the tensor:

```
sum_i x^_i = sum_r prod_m ( sum_j A^(m)_{j,r} )
```

which costs `O(R * sum_m I_m)` and allocates no dense array. Because that term
depends on the factor matrices, `sparse-kl-divergence` takes them as a fourth
argument:

```lisp
(let ((x-hat (make-array (length values) :element-type 'double-float
                         :initial-element 0d0)))
  (sdot factors indices x-hat)
  (sparse-kl-divergence indices values x-hat factors))
```

`x-hat` and `factors` must describe the same model state, so call `sdot` after
every factor update. The optional `:prediction-scale` multiplies every
prediction — both the stored-coordinate values and the total mass — which is how
cross-validation corrects for the exposure difference between a fold's training
and validation halves.

The logarithm divides by `x^ + *epsilon*` so an underflowed reconstruction
cannot yield `-infinity`; `*epsilon*` is not added to the total predicted mass.

## Reference

- [Multiple Data Analysis and Non-negative Matrix/Tensor Factorization [II. Finish] : Tensor Data Analysis and Applications](http://www.kecl.ntt.co.jp/icl/ls/members/tatsushi/PDF/IEICE_vol99_no7_691-698.pdf)
