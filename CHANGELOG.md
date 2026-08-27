# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Breaking Changes

- **`:n-cycle` counts outer iterations**: one cycle is now a full sweep over all
  modes rather than a single mode update, so the same number does as much work as
  there are modes. A `:n-cycle 100` on a 3-mode tensor is 300 mode updates.

  Counting a single mode as an iteration made `:n-cycle` mean different amounts
  of work for tensors of different order, and made the convergence window compare
  points that were only partly updated.

- **`decomposition` returns seven values**: the existing five, plus the component
  weight vector and the final KKT residual.

- **The returned factors are normalized**: every mode past the first has unit-sum
  columns and mode 0 carries the component weights. The model is unchanged and
  `sdot` still reads the factors directly; only the split of scale across modes
  differs.

### Added

- **KKT-based convergence**: the run stops when `max |min(A, gradient)|` falls
  below `:kkt-tolerance` (default `1d-4`), which is zero exactly at a stationary
  point of the non-negativity constrained problem. Pass `0` to disable. A sweep
  observes the residual for free, but each mode reports what it saw on entry, so
  that value is used only as a screen; the residual convergence is decided on,
  and that comes back as the seventh return value, is recomputed against the
  completed model. Numeric arguments are coerced to double-float at the API
  boundary, so `:kappa 0` and single-float tolerances work. The older
  `:convergence-threshold` moving-average test still works, but is weak on its
  own: averaging over a window dilutes the step-to-step change, so a larger
  `:convergence-window` reports convergence sooner, including on runs that have
  not converged.

- **Inadmissible-zero handling**: a multiplicative step cannot revive an entry
  that has reached zero, so the fit can stall at a non-KKT point. An entry below
  `:kappa-tolerance` (default `1d-10`) whose gradient is negative is nudged to
  `:kappa` (default `1d-2`) before the step, following Chi & Kolda
  (arXiv:1112.2414, Algorithm 3). The first sweep runs without it, matching their
  `k > 1` condition. This fixes a single pinned entry; a coordinated collapse
  across modes is a genuine boundary stationary point, which `:n-starts`
  addresses instead.

- **Explicit component weights**: columns are normalized after each sweep and the
  scale collected into a weight per component, returned as the sixth value. The
  weights sum to the total predicted mass.

- **`:n-starts`** (default 1): run that many random initializations and keep the
  one with the lowest final KL. Multiplicative updates only find a local optimum.

- **Health checks that actually signal**: a NaN or infinite factor entry signals
  `numerical-instability-error`, naming the mode, row and column. The factors are
  scanned before the iteration starts and again after each sweep's updates, so a
  bad value is reported before normalization divides by it and before `sdot` and
  the loss carry it further; on SBCL the IEEE traps are masked across the
  iteration so the library reports the offending entry instead of an
  implementation floating-point condition.
  `:on-dead-component` handles a component whose weight collapses — `:warn`
  (default), `:error`, or `:ignore`. It defaults to warning rather than erroring
  because a dead component usually means the rank exceeds what the data supports,
  which is exactly what a rank sweep is looking for; erroring would break
  `select-rank`.

- **Cross-validation now splits counts, not coordinates**: `cross-validate-rank`,
  `select-rank` and `select-rank-1se` take a `sparse-tensor` instead of separate
  `indices` and `counts` arguments, and folds are built by Poisson (multinomial)
  count thinning.

  Holding out whole coordinates was invalid for this data model: an unstored
  coordinate is an observed zero, so the training tensor presented each held-out
  cell as a zero and the model was then scored on recovering a positive count it
  had been fitted to suppress. That systematically favoured the smallest rank.

  **Before:**
  ```lisp
  (select-rank indices counts '(5 10 15) :k 3)
  ```

  **After:**
  ```lisp
  (select-rank tensor '(5 10 15) :k 5)
  ```

  Passing the tensor also keeps the declared shape and the mode metadata: folds
  no longer re-infer the shape from the maximum observed coordinate, which could
  silently drop categories that happened not to appear in a fold.

  Requires non-negative **integer** counts; fractional values now signal
  `invalid-input-error` rather than being truncated. `k` must be an integer >= 2,
  and the tensor must hold at least `ln(k/1e-6)/ln(k/(k-1))` events — 21 for
  `k=2`, 70 for `k=5`, 153 for `k=10`. That is the point below which an empty
  fold stops being negligible: folds are redrawn until every one has training and
  validation events, and redrawing conditions the multinomial, so it is only
  harmless where it essentially never fires. The bound is on the event count, not
  on the number of stored non-zeros, so a single cell holding many events can
  fill many folds.

- **`:evaluation-function` protocol changed**: fold metrics are now called as
  `(fn validation-tensor approximation factor-matrix-vector prediction-scale
  validation-count)`. `approximation` is the reconstruction at the validation
  coordinates *at the training exposure*, so a metric must apply
  `prediction-scale` itself.

- **`make-fold-splits` removed**, together with the internal `%build-shape`,
  `%tensor-dimensions`, `%subset-tensor` and `%complement-subset` helpers.
  `make-poisson-folds` replaces it with count thinning.

### Added

- **`make-poisson-folds`** and `poisson-fold-tensors` / `poisson-folds-k` /
  `poisson-folds-count` / `poisson-folds-tensor` /
  `poisson-folds-prediction-scale`, exposing the thinned folds directly.

- **`normalized-generalized-kl`**: the default fold score, the generalized KL per
  validation event. Not the Poisson deviance — the deviance is twice this.

- **`:prediction-scale` on `sparse-kl-divergence`**: multiplies every prediction,
  the per-entry values *and* the total predicted mass, without mutating the
  factor matrices. `*epsilon*` is added after scaling and is never scaled itself.
  This is how the exposure ratio `1/(k-1)` between a fold's training and
  validation halves is applied.

- **`:standard-error` and `:validation-counts`** in each cross-validation result.

- **`select-rank-elbow`**: walks the candidate ranks upward and stops once a rank
  no longer improves the score by more than `:tolerance` standard errors, for
  `:patience` consecutive ranks. A sequential form of the 1-SE rule, so it tends
  to agree with `select-rank-1se` while fitting fewer models — on an 8-rank grid
  it picked the same rank as both other rules while evaluating 25-62% fewer
  ranks. Its second return value covers only the ranks actually evaluated, not
  the whole grid. The rule is greedy and assumes the curve falls to an elbow and
  then flattens; `:patience` softens that, and `cross-validate-rank` remains the
  way to see the whole curve.

### Fixed

- **Cross-validation is reproducible and order-independent**: a single
  `:random-state` now drives both the thinning and each fold's factor
  initialization, and is copied rather than advanced. Folds are drawn once and
  shared by all candidate ranks, with each fold's initialization fixed in
  advance, so reordering `ranks` cannot change any rank's scores.

- **`select-rank` no longer sorts `cv-results` in place**; the returned list keeps
  its length and the input rank order. Ties break toward the smaller rank.

- **Cross-validation is quiet unless `:verbose t`**; the unconditional progress
  `format t` is gone.

- **`sparse-kl-divergence` now requires the factor matrices**: the loss over the
  implicit-zero coordinates depends on the model, so the function takes a fourth
  argument. Unregistered coordinates are observed zeros, not missing values, and
  the objective is the generalized (Poisson) KL divergence:

  ```
  D(X||X^) = sum_{i in nnz} [ x_i log(x_i / x^_i) - x_i ]  +  sum_i x^_i
  ```

  The old three-argument form summed only the stored non-zeros and therefore
  understated the loss by the predicted mass of every implicit zero.

  **Before:**
  ```lisp
  (sparse-kl-divergence indices values x-hat)
  ```

  **After:**
  ```lisp
  (sdot factors indices x-hat)
  (sparse-kl-divergence indices values x-hat factors)
  ```

  The total predicted mass is aggregated from the CP structure as
  `sum_r prod_m (sum_j A^(m)_{j,r})` in `O(R * sum_m I_m)` time; no dense tensor
  is materialized.

  Custom `:evaluation-function` arguments to `cross-validate-rank`,
  `select-rank` and `select-rank-1se` changed to match. They were superseded
  again by the cross-validation rework above, which is the protocol they use
  now; write new metrics against that one.

- **`decomposition` now accepts only `sparse-tensor`**: The legacy API
  `(decomposition shape indices values ...)` has been removed. You must now
  create a `sparse-tensor` first using `make-sparse-tensor`, then pass it to
  `decomposition`. This provides better input validation and cleaner API.

  **Before (no longer works):**
  ```lisp
  (decomposition x-shape x-indices x-values :r 5)
  ```

  **After:**
  ```lisp
  (let ((tensor (make-sparse-tensor x-shape x-indices x-values)))
    (decomposition tensor :r 5))
  ```

### Fixed

- **`final-kl` now describes the returned factor matrices**: `decomposition-inner`
  used to score the reconstruction from *before* the iteration's update, so the
  reported `final-kl` (and the last `kl-history` entry) belonged to an earlier
  model state. Each cycle now updates a mode, re-runs `sdot`, and only then
  computes the KL divergence.

### Added

- **`sparse-tensor` structure**: New consolidated data structure holding shape,
  indices, values, domains, and auxiliary data. Created via `make-sparse-tensor`
  which validates all inputs.

- **`mode-spec` metadata**: Rich mode descriptions with:
  - `:name` - Mode name (string)
  - `:labels` - Category labels vector
  - `:discretization` - How values were discretized
  - `:role` - Semantic role (`:purchase`, `:demographic`, `:temporal`, etc.)
  - `:positive-label` / `:negative-label` - For binary outcome modes

- **Mode metadata in factor cards**: `generate-factor-cards` now reflects
  mode-spec fields in output:
  - `:mode_roles` - Maps mode names to roles
  - `:purchase_bias` - Purchase probabilities when `:role :purchase` is set
  - `:notes` → `:discretization` - Discretization info per mode
  - `:mode_summaries` - Full mode-spec info per mode

- **Input validation**: `validate-input-data` performs comprehensive checks:
  - Shape is non-empty list of positive integers
  - Indices are within bounds
  - Values are non-negative, finite (no NaN/Inf)
  - Domains match shape dimensions

- **Custom condition types**:
  - `tensor-decomposition-error` - Base condition
  - `invalid-input-error` - Validation failures with `:reason` and `:details`
  - `numerical-instability-error` - NaN/Inf detection

- **Diagnostic metrics** via `:include-diagnostics t` in `generate-factor-cards`:
  - Factor similarity matrix and redundancy score
  - KL contribution per factor
  - Observation responsibilities and ambiguity detection
  - Factor exclusivity/overlap metrics
  - Per-observation residuals

- **Sparse tensor utilities**:
  - `sparse-tensor-nnz` - Number of non-zero entries
  - `sparse-tensor-n-modes` - Number of modes
  - `sparse-tensor-mode-labels` - Labels for a mode
  - `sparse-tensor-mode-name` - Name of a mode
  - `sparse-tensor-total-count` - Sum of all values

- **CI improvements**:
  - Examples execution in CI (simple-example, palmer-penguins-analysis)
  - Integration tests with full pipeline validation

### Changed

- `select-rank` and `cross-validate-rank` now work with indices/values
  extracted from `sparse-tensor`
- All examples updated to use new `sparse-tensor` API
- README updated with new API examples and mode metadata documentation

### Fixed

- Domains length validation now properly checks against shape mode count
- `simple-example.lisp` now loads the system before use

## [0.1.0] - Previous

- Initial release with multiplicative update algorithm for KL divergence
- Sparse tensor support
- Basic reporting with factor cards and markdown output
- Cross-validation for rank selection
