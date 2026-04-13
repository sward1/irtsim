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
