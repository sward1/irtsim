# Generate GRM Item Parameters

Creates a list of discrimination (`a`) and threshold (`b`) parameters
suitable for passing to
[`irt_design()`](https://sward1.github.io/irtsim/reference/irt_design.md)
with `model = "GRM"`.

## Usage

``` r
irt_params_grm(
  n_items,
  n_categories,
  a_dist = "lnorm",
  a_mean = 0,
  a_sd = 0.25,
  b_mean = 0,
  b_sd = 1,
  seed = NULL
)
```

## Arguments

- n_items:

  Positive integer. Number of items.

- n_categories:

  Positive integer \>= 2. Number of response categories per item.
  Produces `n_categories - 1` threshold columns in `b`.

- a_dist:

  Character string for the discrimination distribution. Currently only
  `"lnorm"` is supported. Default: `"lnorm"`.

- a_mean:

  Numeric. `meanlog` for the log-normal distribution. Default: `0`.

- a_sd:

  Numeric. `sdlog` for the log-normal distribution. Default: `0.25`.

- b_mean:

  Numeric. Mean around which thresholds are centered. Default: `0`.

- b_sd:

  Numeric. SD of the base threshold distribution. Default: `1`.

- seed:

  Optional integer seed for reproducibility.

## Value

A named list with elements:

- a:

  Numeric vector of length `n_items`.

- b:

  Numeric matrix with `n_items` rows and `n_categories - 1` columns.
  Thresholds are ordered within each row.

## See also

[`irt_params_2pl()`](https://sward1.github.io/irtsim/reference/irt_params_2pl.md)
for 2PL parameters,
[`irt_design()`](https://sward1.github.io/irtsim/reference/irt_design.md)
to use the generated parameters.

## Examples

``` r
# GRM parameters: 15 items, 5 response categories
params <- irt_params_grm(n_items = 15, n_categories = 5, seed = 42)
```
