# irtsim (development version)

# irtsim 0.2.0

## New features

* The `irt_params_*` helper family is now complete across all registered
  IRT models: `irt_params_1pl()`, `irt_params_2pl()`, `irt_params_3pl()`,
  `irt_params_grm()`, `irt_params_pcm()`, and `irt_params_gpcm()`. Each
  helper shares the same distribution-aware signature pattern
  (`<param>_dist`, `<param>_mean`, `<param>_sd`, `seed`) plus model-specific
  extras (`c_mean`/`c_sd` for 3PL guessing; `n_categories` for the
  polytomous family). All six delegate to the model registry's
  `generate_default_params()` method, so generation defaults live next to
  the model definitions and stay in sync automatically. `irt_params_2pl()`
  and `irt_params_grm()` were refactored to use this shared method with no
  change to their signatures, defaults, or return shapes.
* `irt_design()` and `irt_simulate()` now support three-parameter logistic
  (`model = "3PL"`), partial credit (`model = "PCM"`), and generalized
  partial credit (`model = "GPCM"`) models, in addition to the existing
  1PL, 2PL, and GRM. `irt_study(estimation_model = ...)` accepts the full
  set for generation/estimation cross-fits within a response format
  (binary: 1PL/2PL/3PL; polytomous: GRM/PCM/GPCM).

## Bug fixes

* `irt_simulate()` no longer crashes with `"missing value where TRUE/FALSE
  needed"` when `mirt`'s convergence flag returns `NA` on a hard or large
  fit. The flag is coerced to a strict logical via `isTRUE()` at the source
  and consumer, so an unconfirmable fit routes to the existing
  non-converged branch (records `NA` estimates for that iteration) instead
  of aborting the whole simulation. Most often seen on GRM studies at ~60+
  items.
* `irt_simulate()` no longer crashes when `mirt::fscores()` throws during
  theta scoring on GRM fits with sparsely-observed response categories
  (likelier at large item counts). The call is now wrapped in `tryCatch()`;
  scoring failure degrades to `NA` theta recovery for that iteration while
  item-parameter estimates — the primary sample-size-planning output — are
  preserved. This unblocks GRM studies at realistic operational test
  lengths (60–200 items).

## Internal / infrastructure

* New CI check fails if any precomputed vignette HTML in `vignettes/` is
  older (by last-commit timestamp) than its source in `vignettes-raw/`,
  catching the precompute-then-commit drift that previously relied on
  developer memory.
* Roxygen2 docstrings for `irt_design()`, `irt_study()`, and
  `irt_simulate()` updated to list the full registered model set (1PL, 2PL,
  3PL, GRM, PCM, GPCM).
* The precomputed simulation-result objects backing the paper-example
  vignettes are no longer shipped in the installed package (the static
  vignette HTML is self-contained), restoring a small installed size.
* Added a scalability benchmark harness (`benchmarks/`, excluded from the
  installed package). Findings (`benchmarks/README.md`): wall-clock scales
  roughly as `n_items^1.7`, `mirt::mirt()` accounts for ~96% of run time,
  and peak resident heap stays ~335–395 MB across an `n_items`
  {30,60,100,150} x N {200,500,1000} grid — use `parallel = TRUE` for large
  designs.

# irtsim 0.1.2

* New `choosing-item-parameters` vignette
  (`vignette("choosing-item-parameters")`) — deeper reference for the
  three item-parameter specification workflows introduced in the
  getting-started vignette: import from a prior `mirt` fit (with a
  slope-intercept-to-IRT conversion worked example) or a CSV / Excel
  parameter table; domain-typical preset values for cognitive ability,
  personality, clinical, and achievement assessments with cited
  reference ranges; and hypothesized / content-based specification
  with explicit translation from item-review judgements to
  distribution arguments. eval=TRUE knitr engine; no new exported
  helpers in this release.
* New `irtsim` getting-started vignette (`vignette("irtsim")`)
  walking a stranger from "I need to plan an IRT study" through to
  `recommended_n()`. The vignette builds live (eval=TRUE) so it
  cannot drift from the package API. Three item-parameter
  specification paths are demonstrated: by hand, via
  `irt_params_2pl()`, and from a prior `mirt` calibration.
* `irt_design()` now aborts with an informative error if
  `n_factors != 1`. Multidimensional IRT support is planned for
  v0.4.0; until then, the parameter is retained on the design for
  forward compatibility but silently accepting `n_factors > 1`
  produced a cryptic mirt internal error downstream. The new abort
  fires up front and points users at the planned support.
* `recommended_n()` gains an `aggregate` parameter
  (`"max"` / `"mean"` / `"median"` / `"none"`, default `"max"`). The
  default return is now an integer scalar — the smallest sample size
  that powers every item/param at the requested threshold — with a
  `details` attribute carrying the per-item data frame plus
  `aggregate`, `criterion`, and `threshold` attributes. `"mean"` and
  `"median"` round up via `ceiling()` so the recommendation never
  falls below the central tendency. Pass `aggregate = "none"` to
  recover the previous per-item data frame return. **Behavior
  change:** the default return shape changed from a per-item data
  frame to a scalar; closes the footgun where users could under-power
  by forgetting to take `max()` across items.
* Removed the `paper-reproduction-gaps` vignette. Its content was a
  scorecard of paper Examples 2 and 3 reproduction gaps that pointed
  at deferred objectives (Obj 30/31). Those objectives are now
  superseded by a planned pluggable `fit_fn` / `extract_fn` hook
  (Obj 39, targeted for v0.3.0); the standalone gaps vignette no
  longer reflects the roadmap. Cross-references to it from
  `paper-example-2-mcar` and `paper-example-3-grm` have also been
  removed.

# irtsim 0.1.1

CRAN resubmission. Documentation-only changes; no user-facing API or
behavior changes.

* `DESCRIPTION`: expanded all acronyms on first use (API, IRT, 1PL,
  2PL, MCAR, MAR, MSE, RMSE, SE) per CRAN reviewer request.
* `man/`: replaced `\dontrun{}` with `\donttest{}` in `irt_simulate`,
  `summary.irt_results`, `plot.irt_results`, `plot.summary_irt_results`,
  `recommended_n`, `print.irt_results`, and `print.summary_irt_results`
  examples per CRAN reviewer request. Examples remain wrapped (not
  unwrapped) because each depends on a ~300-fit `irt_simulate()` call
  that exceeds the 5-second CRAN example-execution budget.

# irtsim 0.1.0

Initial CRAN release.

## Core pipeline

* `irt_design()` specifies the data-generating IRT model (items, parameters, theta distribution).
* `irt_study()` adds study conditions (sample sizes, missing-data mechanism, optional separate estimation model).
* `irt_simulate()` runs the Monte Carlo simulation loop with deterministic seeding and optional parallelism.
* `summary()`, `plot()`, and `recommended_n()` methods extract simulation-based sample-size recommendations from `irt_results` objects.

## Supported IRT models

* 1PL (Rasch)
* 2PL
* Graded response model (GRM)

## Supported missing-data mechanisms

* `"none"` — complete data
* `"mcar"` — missing completely at random
* `"mar"` — missing at random (monotone, trait-dependent)
* `"booklet"` — structured booklet assignment with common-item overlap
* `"linking"` — two-form linked design with user-supplied linking matrix

## Performance criteria

* Mean squared error (`mse`), root mean squared error (`rmse`), bias, absolute bias, standard error (`se`), empirical coverage, Monte Carlo SE of MSE (`mcse_mse`).
* Criterion metadata (direction of improvement, display label) centralized in `R/criterion_registry.R`.
* Custom per-iteration criteria via the `criterion_fn` argument to `summary.irt_results()` — callbacks receive `estimates`, `true_value`, `ci_lower`, `ci_upper`, and `converged` and return named numeric vectors appended to `item_summary`.

## Model misspecification

* `irt_study(estimation_model = ...)` allows fitting a different IRT model than the one used to generate data (e.g., generate 2PL, fit 1PL). Compatible cross-pairs: `(1PL, 2PL)`, `(2PL, 1PL)`, same-model. GRM is not cross-compatible with dichotomous models.

## Parallelization

* `irt_simulate(parallel = TRUE)` dispatches iterations across workers via `future.apply::future_lapply()`.
* Reproducibility contract: within-mode (identical results on re-run for a given `parallel` setting) guaranteed. Cross-mode results differ because serial uses Mersenne-Twister and parallel uses L'Ecuyer-CMRG substreams — both statistically valid.
* Users control backend via `future::plan()`.

## User experience

* `cli::cli_progress_bar()` replaces `cat()`-based progress reporting (suppressible with `progress = FALSE`).
* Structured `cli::cli_abort()` error messages with valid-option enumerations for invalid model, criterion, missing mechanism, and `estimation_model` arguments.

## Documentation

* Five vignettes reproduce or extend the three examples from Schroeders and Gnambs (2025):
  * **Paper Example 1** — faithful reproduction of the linked-test design with 1PL estimation.
  * **Paper Example 1b** — extension showing bias-variance tradeoff when a 2PL-generated dataset is fit with a 1PL model.
  * **Paper Example 2** — MCAR-only partial reproduction with custom-criterion-callback feature demonstration.
  * **Paper Example 3** — GRM item parameter recovery partial reproduction.
  * **Paper reproduction status** — scorecard documenting what the current API can and cannot reproduce end-to-end.
* Vignettes are shipped as static HTML via `R.rsp::asis` because re-running the Monte Carlo simulations during package checks would exceed CRAN's build-time budget. The source `.Rmd` files and `data-raw/precompute_vignettes.R` are available in the GitHub repository for users who wish to reproduce results locally.

## Dependencies

* Imports: `cli`, `future.apply`, `ggplot2`, `mirt`, `rlang`
* Suggests: `future`, `knitr`, `R.rsp`, `rmarkdown`, `scales`, `testthat`

## Reference

Schroeders, U., and Gnambs, T. (2025). Sample size planning in item response theory: A 10-decision framework. *Advances in Methods and Practices in Psychological Science*. <https://doi.org/10.1177/25152459251314798>

