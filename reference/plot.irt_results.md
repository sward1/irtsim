# Plot IRT Simulation Results

Visualize performance criteria across sample sizes from an
[`irt_simulate()`](https://sward1.github.io/irtsim/reference/irt_simulate.md)
result. Calls
[`summary.irt_results()`](https://sward1.github.io/irtsim/reference/summary.irt_results.md)
internally, then plots the requested criterion by sample size.

## Usage

``` r
# S3 method for class 'irt_results'
plot(x, criterion = "rmse", param = NULL, item = NULL, threshold = NULL, ...)
```

## Arguments

- x:

  An `irt_results` object from
  [`irt_simulate()`](https://sward1.github.io/irtsim/reference/irt_simulate.md).

- criterion:

  Character string. Which criterion to plot. Default `"rmse"`. Valid
  values: `"bias"`, `"empirical_se"`, `"mse"`, `"rmse"`, `"coverage"`,
  `"mcse_bias"`, `"mcse_mse"`.

- param:

  Optional character vector. Filter to specific parameter types (e.g.,
  `"a"`, `"b"`, `"b1"`).

- item:

  Optional integer vector. Filter to specific item numbers.

- threshold:

  Optional numeric. If provided, draws a horizontal reference line at
  this value.

- ...:

  Additional arguments passed to
  [`summary.irt_results()`](https://sward1.github.io/irtsim/reference/summary.irt_results.md).

## Value

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object, returned invisibly.

## See also

[`summary.irt_results()`](https://sward1.github.io/irtsim/reference/summary.irt_results.md)
for the underlying criteria,
[`recommended_n()`](https://sward1.github.io/irtsim/reference/recommended_n.md)
for sample-size recommendations.

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
#> Iteration 9/10
#> Iteration 10/10
#> 
plot(results)
plot(results, criterion = "bias", threshold = 0.05, param = "b")
# }
```
