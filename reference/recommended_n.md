# Find the Minimum Sample Size Meeting a Criterion Threshold

Given a
[`summary.irt_results()`](https://sward1.github.io/irtsim/reference/summary.irt_results.md)
object, find the smallest sample size at which a performance criterion
meets the specified threshold for each item and parameter combination.

## Usage

``` r
recommended_n(object, ...)

# S3 method for class 'summary_irt_results'
recommended_n(
  object,
  criterion,
  threshold,
  param = NULL,
  item = NULL,
  aggregate = c("max", "mean", "median", "none"),
  ...
)
```

## Arguments

- object:

  A `summary_irt_results` object from
  [`summary.irt_results()`](https://sward1.github.io/irtsim/reference/summary.irt_results.md).

- ...:

  Additional arguments (ignored).

- criterion:

  Character string. Which criterion to evaluate. One of: `"bias"`,
  `"empirical_se"`, `"mse"`, `"rmse"`, `"coverage"`, `"mcse_bias"`,
  `"mcse_mse"`.

- threshold:

  Positive numeric. The threshold value the criterion must meet.

- param:

  Optional character vector. Filter to specific parameter types (e.g.,
  `"a"`, `"b"`, `"b1"`).

- item:

  Optional integer vector. Filter to specific item numbers.

- aggregate:

  Character. How to roll the per-item recommended sample sizes up into a
  single recommendation. One of `"max"` (default — the smallest N that
  powers every item/param), `"mean"`, `"median"`, or `"none"` (return
  the per-item data frame unchanged). `"mean"` and `"median"` round up
  via [`ceiling()`](https://rdrr.io/r/base/Round.html) so the
  recommendation is never under the computed central tendency.

## Value

When `aggregate = "none"`, a data frame with columns:

- item:

  Item number.

- param:

  Parameter name.

- recommended_n:

  Minimum sample size meeting the threshold, or `NA` if no tested sample
  size meets it.

- criterion:

  The criterion used (echoed back for reference).

- threshold:

  The threshold used (echoed back for reference).

When `aggregate` is `"max"`, `"mean"`, or `"median"` (the typical case),
an integer scalar carrying the recommended sample size with attributes
`details` (the per-item data frame above), `aggregate`, `criterion`, and
`threshold`. If any item/param combination fails to meet the threshold
at every tested sample size, the aggregate is `NA_integer_` and a
warning lists the affected combinations.

## Details

For criteria where smaller is better (bias, empirical_se, mse, rmse,
mcse_bias, mcse_mse), the threshold is met when the criterion value is
at or below the threshold. For bias, the absolute value is used. For
coverage (where higher is better), the threshold is met when coverage is
at or above the threshold.

## See also

[`summary.irt_results()`](https://sward1.github.io/irtsim/reference/summary.irt_results.md)
for computing criteria,
[`plot.irt_results()`](https://sward1.github.io/irtsim/reference/plot.irt_results.md)
for visualization.

## Examples

``` r
# \donttest{
design <- irt_design(
  model = "1PL", n_items = 5,
  item_params = list(b = seq(-2, 2, length.out = 5))
)
study <- irt_study(design, sample_sizes = c(200, 500))
results <- irt_simulate(study, iterations = 10, seed = 42)
#> Iteration 5/10
#> Iteration 8/10
#> Iteration 9/10
#> Iteration 10/10
#> 
s <- summary(results)

# Default — single recommended N (max across items) for RMSE <= 0.20
n_rec <- recommended_n(s, criterion = "rmse", threshold = 0.20)
n_rec
#> [1] 500
#> attr(,"details")
#>   item param recommended_n criterion threshold
#> 1    1     b           500      rmse       0.2
#> 2    2     b           500      rmse       0.2
#> 3    3     b           500      rmse       0.2
#> 4    4     b           200      rmse       0.2
#> 5    5     b           500      rmse       0.2
#> attr(,"aggregate")
#> [1] "max"
#> attr(,"criterion")
#> [1] "rmse"
#> attr(,"threshold")
#> [1] 0.2
attr(n_rec, "details")  # per-item breakdown
#>   item param recommended_n criterion threshold
#> 1    1     b           500      rmse       0.2
#> 2    2     b           500      rmse       0.2
#> 3    3     b           500      rmse       0.2
#> 4    4     b           200      rmse       0.2
#> 5    5     b           500      rmse       0.2

# Mean / median aggregates (rounded up via ceiling)
recommended_n(s, criterion = "rmse", threshold = 0.20, aggregate = "mean")
#> [1] 440
#> attr(,"details")
#>   item param recommended_n criterion threshold
#> 1    1     b           500      rmse       0.2
#> 2    2     b           500      rmse       0.2
#> 3    3     b           500      rmse       0.2
#> 4    4     b           200      rmse       0.2
#> 5    5     b           500      rmse       0.2
#> attr(,"aggregate")
#> [1] "mean"
#> attr(,"criterion")
#> [1] "rmse"
#> attr(,"threshold")
#> [1] 0.2

# Legacy behavior — full per-item data frame
recommended_n(s, criterion = "rmse", threshold = 0.20, aggregate = "none")
#>   item param recommended_n criterion threshold
#> 1    1     b           500      rmse       0.2
#> 2    2     b           500      rmse       0.2
#> 3    3     b           500      rmse       0.2
#> 4    4     b           200      rmse       0.2
#> 5    5     b           500      rmse       0.2

# Minimum N for 95% coverage on difficulty parameters only
recommended_n(s, criterion = "coverage", threshold = 0.95, param = "b")
#> Warning: No tested sample size meets coverage >= 0.95 for some item/param combinations.
#> ℹ Affected: (item 1, param b) and (item 2, param b)
#> ℹ Aggregate returned as NA. Inspect `attr(result, "details")` for per-item
#>   values.
#> [1] NA
#> attr(,"details")
#>   item param recommended_n criterion threshold
#> 1    1     b            NA  coverage      0.95
#> 2    2     b            NA  coverage      0.95
#> 3    3     b           500  coverage      0.95
#> 4    4     b           200  coverage      0.95
#> 5    5     b           500  coverage      0.95
#> attr(,"aggregate")
#> [1] "max"
#> attr(,"criterion")
#> [1] "coverage"
#> attr(,"threshold")
#> [1] 0.95
# }
```
