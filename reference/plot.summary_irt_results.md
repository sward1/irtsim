# Plot Summary of IRT Simulation Results

Visualize performance criteria from a
[`summary.irt_results()`](https://sward1.github.io/irtsim/reference/summary.irt_results.md)
object. This is a convenience method for users who already have a
summary;
[`plot.irt_results()`](https://sward1.github.io/irtsim/reference/plot.irt_results.md)
is the primary interface.

## Usage

``` r
# S3 method for class 'summary_irt_results'
plot(x, criterion = "rmse", param = NULL, item = NULL, threshold = NULL, ...)
```

## Arguments

- x:

  A `summary_irt_results` object from
  [`summary.irt_results()`](https://sward1.github.io/irtsim/reference/summary.irt_results.md).

- criterion:

  Character string. Which criterion to plot. Default `"rmse"`.

- param:

  Optional character vector. Filter to specific parameter types.

- item:

  Optional integer vector. Filter to specific item numbers.

- threshold:

  Optional numeric. If provided, draws a horizontal reference line at
  this value.

- ...:

  Additional arguments (ignored).

## Value

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object, returned invisibly.

## See also

[`plot.irt_results()`](https://sward1.github.io/irtsim/reference/plot.irt_results.md),
[`summary.irt_results()`](https://sward1.github.io/irtsim/reference/summary.irt_results.md)

## Examples

``` r
# \donttest{
design <- irt_design(
  model = "1PL", n_items = 5,
  item_params = list(b = seq(-2, 2, length.out = 5))
)
study <- irt_study(design, sample_sizes = c(200, 500))
results <- irt_simulate(study, iterations = 10, seed = 42)
#> Iteration 4/10
#> Iteration 5/10
#> Iteration 8/10
#> Iteration 9/10
#> Iteration 10/10
#> 
s <- summary(results)
plot(s, criterion = "rmse", threshold = 0.15)
# }
```
