# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Breaking Changes

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
  `select-rank` and `select-rank-1se` are now called with four arguments
  `(indices counts approx factor-matrices)` to match.

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
