# Build Criterion Plot (Internal)

Shared plotting engine used by both
[`plot.irt_results()`](https://sward1.github.io/irtsim/reference/plot.irt_results.md)
and
[`plot.summary_irt_results()`](https://sward1.github.io/irtsim/reference/plot.summary_irt_results.md).

## Usage

``` r
build_criterion_plot(
  summary_obj,
  criterion = "rmse",
  param = NULL,
  item = NULL,
  threshold = NULL
)
```

## Arguments

- summary_obj:

  A `summary_irt_results` object.

- criterion:

  Character string. Criterion to plot.

- param:

  Optional character vector. Parameter filter.

- item:

  Optional integer vector. Item filter.

- threshold:

  Optional numeric. Horizontal reference line.

## Value

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object, returned invisibly.
