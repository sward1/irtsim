# Generate Discrimination Parameters

Internal helper to generate discrimination (a) parameters under a
specified distribution. Currently supports log-normal.

## Usage

``` r
generate_discrimination(n_items, a_dist, a_mean, a_sd)
```

## Arguments

- n_items:

  Positive integer: the number of items (and thus the number of
  discrimination values to generate).

- a_dist:

  Character string: the distribution name. Currently only `"lnorm"` is
  supported.

- a_mean:

  Numeric: the mean parameter for the log-normal distribution
  (interpreted as `meanlog`).

- a_sd:

  Numeric: the standard deviation parameter for the log-normal
  distribution (interpreted as `sdlog`).

## Value

A numeric vector of length `n_items` containing discrimination values.
