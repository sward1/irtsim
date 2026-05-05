# Build NA Item Results for Non-Converged Iterations (Internal)

Build NA Item Results for Non-Converged Iterations (Internal)

## Usage

``` r
build_na_item_results(true_params, iteration, sample_size)
```

## Arguments

- true_params:

  Data frame from
  [`build_true_params()`](https://sward1.github.io/irtsim/reference/build_true_params.md).

- iteration:

  Integer iteration number.

- sample_size:

  Integer sample size.

## Value

Data frame matching item_results schema with NA estimates.
