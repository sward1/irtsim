# Summarize IRT Simulation Results

Compute performance criteria for each sample size, item, and parameter
combination from an
[`irt_simulate()`](https://sward1.github.io/irtsim/reference/irt_simulate.md)
result. Criteria follow Morris et al. (2019) definitions. Optionally,
users can provide a custom callback function to compute additional
item-level performance criteria (e.g., conditional reliability, external
criterion SE).

## Usage

``` r
# S3 method for class 'irt_results'
summary(object, criterion = NULL, param = NULL, criterion_fn = NULL, ...)
```

## Arguments

- object:

  An `irt_results` object from
  [`irt_simulate()`](https://sward1.github.io/irtsim/reference/irt_simulate.md).

- criterion:

  Optional character vector. Which criteria to include in the output.
  Valid values: `"bias"`, `"empirical_se"`, `"mse"`, `"rmse"`,
  `"coverage"`, `"mcse_bias"`, `"mcse_mse"`. If `NULL` (default), all
  criteria are returned.

- param:

  Optional character vector. Which parameter types to include (e.g.,
  `"a"`, `"b"`, `"b1"`). If `NULL` (default), all parameters are
  summarized.

- criterion_fn:

  Optional function. A user-defined callback to compute custom
  performance criteria. Must accept named arguments `estimates` (numeric
  vector), `true_value` (scalar), `ci_lower` (numeric), `ci_upper`
  (numeric), `converged` (logical), and `...` (for future use). Must
  return a named numeric vector of length \>= 1. The names become new
  columns in `item_summary`, appended after `n_converged`. If `NULL`
  (default), no custom criteria are computed.

- ...:

  Additional arguments (ignored).

## Value

An S3 object of class `summary_irt_results` containing:

- item_summary:

  Data frame with one row per sample_size × item × param combination,
  containing the requested criteria plus `n_converged` and any custom
  columns from `criterion_fn`.

- theta_summary:

  Data frame with one row per sample_size, containing `mean_cor`,
  `sd_cor`, `mean_rmse`, `sd_rmse`, and `n_converged`.

- iterations:

  Number of replications.

- seed:

  Base seed used.

- model:

  IRT model type.

## References

Morris, T. P., White, I. R., & Crowther, M. J. (2019). Using simulation
studies to evaluate statistical methods. *Statistics in Medicine*,
38(11), 2074–2102.
[doi:10.1002/sim.8086](https://doi.org/10.1002/sim.8086)

## See also

[`irt_simulate()`](https://sward1.github.io/irtsim/reference/irt_simulate.md)
for running simulations,
[`plot.irt_results()`](https://sward1.github.io/irtsim/reference/plot.irt_results.md)
for visualization,
[`recommended_n()`](https://sward1.github.io/irtsim/reference/recommended_n.md)
for sample-size recommendations.

## Examples

``` r
# \donttest{
# Minimal example (iterations reduced for speed; use 100+ in practice)
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

s <- summary(results)
s$item_summary
#>    sample_size item param true_value        bias empirical_se        mse
#> 1          200    1     b         -2  0.01826936    0.2813439 0.07157270
#> 2          200    2     b         -1 -0.05712048    0.2314278 0.05146568
#> 3          200    3     b          0 -0.07594365    0.2225141 0.05032873
#> 4          200    4     b          1 -0.03849063    0.1497202 0.02165607
#> 5          200    5     b          2  0.05270597    0.3076836 0.08798018
#> 6          500    1     b         -2  0.02029278    0.1618503 0.02398776
#> 7          500    2     b         -1  0.04266708    0.1450726 0.02076192
#> 8          500    3     b          0 -0.04696731    0.1306269 0.01756299
#> 9          500    4     b          1  0.08968595    0.1061969 0.01819357
#> 10         500    5     b          2 -0.03168185    0.1287219 0.01591614
#>         rmse coverage  mcse_bias    mcse_mse n_converged
#> 1  0.2675307      0.9 0.08896874 0.037426556          10
#> 2  0.2268605      0.8 0.07318389 0.019402165          10
#> 3  0.2243407      0.8 0.07036515 0.025615834          10
#> 4  0.1471600      1.0 0.04734570 0.005954864          10
#> 5  0.2966145      0.9 0.09729809 0.074831545          10
#> 6  0.1548798      0.8 0.05118155 0.010034577          10
#> 7  0.1440900      0.9 0.04587597 0.006047372          10
#> 8  0.1325254      1.0 0.04130787 0.004460940          10
#> 9  0.1348835      1.0 0.03358240 0.005734851          10
#> 10 0.1261592      1.0 0.04070544 0.005735466          10
s$theta_summary
#>   sample_size  mean_cor     sd_cor mean_rmse    sd_rmse n_converged
#> 1         200 0.6710695 0.05118505 0.7312415 0.05366362          10
#> 2         500 0.6533689 0.02817120 0.7587360 0.02548581          10

# Only bias and RMSE for difficulty parameters
summary(results, criterion = c("bias", "rmse"), param = "b")
#> IRT Simulation Summary
#>   Model:       1PL 
#>   Iterations:  10 
#>   Seed:        42 
#> 
#> Item Parameter Criteria:
#>  sample_size item param true_value        bias      rmse n_converged
#>          200    1     b         -2  0.01826936 0.2675307          10
#>          200    2     b         -1 -0.05712048 0.2268605          10
#>          200    3     b          0 -0.07594365 0.2243407          10
#>          200    4     b          1 -0.03849063 0.1471600          10
#>          200    5     b          2  0.05270597 0.2966145          10
#>          500    1     b         -2  0.02029278 0.1548798          10
#>          500    2     b         -1  0.04266708 0.1440900          10
#>          500    3     b          0 -0.04696731 0.1325254          10
#>          500    4     b          1  0.08968595 0.1348835          10
#>          500    5     b          2 -0.03168185 0.1261592          10
#> 
#> Theta Recovery:
#>  sample_size  mean_cor     sd_cor mean_rmse    sd_rmse n_converged
#>          200 0.6710695 0.05118505 0.7312415 0.05366362          10
#>          500 0.6533689 0.02817120 0.7587360 0.02548581          10

# Compute custom criterion: relative bias
custom_fn <- function(estimates, true_value, ci_lower, ci_upper, converged, ...) {
  valid_est <- estimates[!is.na(estimates)]
  rel_bias <- (mean(valid_est) - true_value) / true_value
  c(relative_bias = rel_bias)
}
summary(results, criterion_fn = custom_fn)
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
#>       rmse coverage  mcse_bias    mcse_mse n_converged relative_bias
#>  0.2675307      0.9 0.08896874 0.037426556          10   -0.00913468
#>  0.2268605      0.8 0.07318389 0.019402165          10    0.05712048
#>  0.2243407      0.8 0.07036515 0.025615834          10          -Inf
#>  0.1471600      1.0 0.04734570 0.005954864          10   -0.03849063
#>  0.2966145      0.9 0.09729809 0.074831545          10    0.02635298
#>  0.1548798      0.8 0.05118155 0.010034577          10   -0.01014639
#>  0.1440900      0.9 0.04587597 0.006047372          10   -0.04266708
#>  0.1325254      1.0 0.04130787 0.004460940          10          -Inf
#>  0.1348835      1.0 0.03358240 0.005734851          10    0.08968595
#>  0.1261592      1.0 0.04070544 0.005735466          10   -0.01584093
#> 
#> Theta Recovery:
#>  sample_size  mean_cor     sd_cor mean_rmse    sd_rmse n_converged
#>          200 0.6710695 0.05118505 0.7312415 0.05366362          10
#>          500 0.6533689 0.02817120 0.7587360 0.02548581          10

# Multiple custom criteria
multi_fn <- function(estimates, true_value, ci_lower, ci_upper, converged, ...) {
  valid_est <- estimates[!is.na(estimates)]
  c(mean_est = mean(valid_est), sd_est = sd(valid_est))
}
summary(results, criterion_fn = multi_fn)
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
#>       rmse coverage  mcse_bias    mcse_mse n_converged    mean_est    sd_est
#>  0.2675307      0.9 0.08896874 0.037426556          10 -1.98173064 0.2813439
#>  0.2268605      0.8 0.07318389 0.019402165          10 -1.05712048 0.2314278
#>  0.2243407      0.8 0.07036515 0.025615834          10 -0.07594365 0.2225141
#>  0.1471600      1.0 0.04734570 0.005954864          10  0.96150937 0.1497202
#>  0.2966145      0.9 0.09729809 0.074831545          10  2.05270597 0.3076836
#>  0.1548798      0.8 0.05118155 0.010034577          10 -1.97970722 0.1618503
#>  0.1440900      0.9 0.04587597 0.006047372          10 -0.95733292 0.1450726
#>  0.1325254      1.0 0.04130787 0.004460940          10 -0.04696731 0.1306269
#>  0.1348835      1.0 0.03358240 0.005734851          10  1.08968595 0.1061969
#>  0.1261592      1.0 0.04070544 0.005735466          10  1.96831815 0.1287219
#> 
#> Theta Recovery:
#>  sample_size  mean_cor     sd_cor mean_rmse    sd_rmse n_converged
#>          200 0.6710695 0.05118505 0.7312415 0.05366362          10
#>          500 0.6533689 0.02817120 0.7587360 0.02548581          10
# }
```
