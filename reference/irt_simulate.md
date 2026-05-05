# Run an IRT Monte Carlo Simulation

Execute a Monte Carlo simulation study based on an
[irt_study](https://sward1.github.io/irtsim/reference/irt_study.md)
specification. For each iteration and sample size, data are generated,
missing values applied, the IRT model is fitted, and parameter estimates
are extracted and stored.

## Usage

``` r
irt_simulate(
  study,
  iterations,
  seed,
  progress = TRUE,
  parallel = FALSE,
  se = TRUE,
  compute_theta = TRUE
)
```

## Arguments

- study:

  An [irt_study](https://sward1.github.io/irtsim/reference/irt_study.md)
  object specifying the design and study conditions.

- iterations:

  Positive integer. Number of Monte Carlo replications.

- seed:

  Integer. Base random seed for reproducibility. Each iteration uses
  `seed + iteration - 1`.

- progress:

  Logical. Print progress messages? Default `TRUE`.

- parallel:

  Logical. Run iterations in parallel using
  [`future.apply::future_lapply()`](https://future.apply.futureverse.org/reference/future_lapply.html)?
  Default `FALSE`. Requires users to set up a future plan (e.g.,
  `future::plan(multisession)`) before calling. See Details.

- se:

  Logical. Compute standard errors and confidence intervals for item
  parameter estimates? Default `TRUE`. Set to `FALSE` for significant
  speed improvement when only point estimates are needed (e.g., MSE,
  bias, RMSE criteria). When `FALSE`, `se`/`ci_lower`/`ci_upper` columns
  in `item_results` are `NA`.

- compute_theta:

  Logical. Compute EAP theta estimates and recovery metrics
  (correlation, RMSE)? Default `TRUE`. Set to `FALSE` to skip the
  [`mirt::fscores()`](https://philchalmers.github.io/mirt/reference/fscores.html)
  call when theta recovery is not needed. When `FALSE`, `theta_cor` and
  `theta_rmse` in `theta_results` are `NA` (but `converged` is still
  tracked).

## Value

An S3 object of class `irt_results` containing:

- item_results:

  Data frame with per-iteration item parameter estimates (columns:
  iteration, sample_size, item, param, true_value, estimate, se,
  ci_lower, ci_upper, converged).

- theta_results:

  Data frame with per-iteration theta recovery summaries (columns:
  iteration, sample_size, theta_cor, theta_rmse, converged).

- study:

  The original
  [irt_study](https://sward1.github.io/irtsim/reference/irt_study.md)
  object.

- iterations:

  Number of replications run.

- seed:

  Base seed used.

- elapsed:

  Elapsed wall-clock time in seconds.

- se:

  Logical flag indicating whether SEs and CIs were computed.

- compute_theta:

  Logical flag indicating whether theta recovery metrics were computed.

## Details

The returned `irt_results` object stores raw per-iteration estimates.
Use
[`summary.irt_results()`](https://sward1.github.io/irtsim/reference/summary.irt_results.md)
to compute performance criteria (bias, MSE, RMSE, coverage, etc.) and
[`plot.irt_results()`](https://sward1.github.io/irtsim/reference/plot.irt_results.md)
to visualize results.

### Parallelization

When `parallel = TRUE`, the Monte Carlo loop over iterations is
parallelized via
[`future.apply::future_lapply()`](https://future.apply.futureverse.org/reference/future_lapply.html).
Each parallel task processes one iteration across all sample sizes
sequentially.

**Important:** This function does NOT configure a future plan. Users
must set their own plan before calling with `parallel = TRUE`:

    library(future)
    plan(multisession, workers = 4)  # or your preferred backend
    results <- irt_simulate(study, iterations = 100, seed = 42, parallel = TRUE)

Without an explicit plan, future defaults to sequential execution (no
parallelism).

### Reproducibility contract

Reproducibility is guaranteed **within a given dispatch mode**, not
across modes:

- **Serial mode** (`parallel = FALSE`) uses deterministic per-cell seeds
  under the session's default RNG kind (Mersenne-Twister). Re-running
  with the same base `seed` reproduces identical results bit-for-bit.

- **Parallel mode** (`parallel = TRUE`) delegates RNG management to
  `future.apply::future_lapply(..., future.seed = TRUE)`, which assigns
  each iteration a formally independent L'Ecuyer-CMRG substream.
  Re-running with the same base `seed` reproduces identical results
  bit-for-bit across parallel runs, including across different worker
  counts.

- **Across modes**, numerical results will differ because the two paths
  use different RNG algorithms and different seeding strategies. Both
  are statistically valid; the parallel path has the stronger formal
  guarantee of independent substreams, which is the standard for Monte
  Carlo work.

Progress messages are suppressed in parallel mode (workers cannot stream
to stdout safely). Set `progress = FALSE` in serial mode to suppress
messages (they appear every 10% of iterations).

## See also

[`irt_study()`](https://sward1.github.io/irtsim/reference/irt_study.md)
for specifying study conditions,
[`summary.irt_results()`](https://sward1.github.io/irtsim/reference/summary.irt_results.md)
and
[`plot.irt_results()`](https://sward1.github.io/irtsim/reference/plot.irt_results.md)
for analyzing output,
[`irt_iterations()`](https://sward1.github.io/irtsim/reference/irt_iterations.md)
for determining the number of replications.

## Examples

``` r
# \donttest{
# Minimal example (iterations and sample sizes reduced for speed;
# use iterations >= 100 and 3+ sample sizes in practice)
design <- irt_design(
  model = "1PL", n_items = 5,
  item_params = list(b = seq(-2, 2, length.out = 5))
)
study <- irt_study(design, sample_sizes = c(200, 500))
results <- irt_simulate(study, iterations = 10, seed = 42)
#> Iteration 5/10
#> Iteration 9/10
#> Iteration 10/10
#> 
summary(results)
#> IRT Simulation Summary
#>   Model:       1PL 
#>   Iterations:  10 
#>   Seed:        42 
#> 
#> Item Parameter Criteria:
#>  sample_size item param true_value        bias empirical_se        mse
#>          200    1     b         -2  0.01826936    0.2813439 0.07157270
#>          200    2     b         -1 -0.05712048    0.2314278 0.05146568
#>          200    3     b          0 -0.07594365    0.2225141 0.05032873
#>          200    4     b          1 -0.03849063    0.1497202 0.02165607
#>          200    5     b          2  0.05270597    0.3076836 0.08798018
#>          500    1     b         -2  0.02029278    0.1618503 0.02398776
#>          500    2     b         -1  0.04266708    0.1450726 0.02076192
#>          500    3     b          0 -0.04696731    0.1306269 0.01756299
#>          500    4     b          1  0.08968595    0.1061969 0.01819357
#>          500    5     b          2 -0.03168185    0.1287219 0.01591614
#>       rmse coverage  mcse_bias    mcse_mse n_converged
#>  0.2675307      0.9 0.08896874 0.037426556          10
#>  0.2268605      0.8 0.07318389 0.019402165          10
#>  0.2243407      0.8 0.07036515 0.025615834          10
#>  0.1471600      1.0 0.04734570 0.005954864          10
#>  0.2966145      0.9 0.09729809 0.074831545          10
#>  0.1548798      0.8 0.05118155 0.010034577          10
#>  0.1440900      0.9 0.04587597 0.006047372          10
#>  0.1325254      1.0 0.04130787 0.004460940          10
#>  0.1348835      1.0 0.03358240 0.005734851          10
#>  0.1261592      1.0 0.04070544 0.005735466          10
#> 
#> Theta Recovery:
#>  sample_size  mean_cor     sd_cor mean_rmse    sd_rmse n_converged
#>          200 0.6710695 0.05118505 0.7312415 0.05366362          10
#>          500 0.6533689 0.02817120 0.7587360 0.02548581          10
plot(results)
# }
```
