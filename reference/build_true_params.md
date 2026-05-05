# Build True Parameter Data Frame (Internal)

Creates a data frame of true parameter values from the design, used for
populating the true_value column and for building NA rows on
non-convergence.

## Usage

``` r
build_true_params(design)
```

## Arguments

- design:

  An `irt_design` object.

## Value

Data frame with columns: item, param, true_value.
