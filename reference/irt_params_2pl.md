# Generate 2PL Item Parameters

Creates a list of discrimination (`a`) and difficulty (`b`) parameters
suitable for passing to
[`irt_design()`](https://sward1.github.io/irtsim/reference/irt_design.md).

## Usage

``` r
irt_params_2pl(
  n_items,
  a_dist = "lnorm",
  a_mean = 0,
  a_sd = 0.25,
  b_dist = "normal",
  b_mean = 0,
  b_sd = 1,
  b_range = c(-2, 2),
  seed = NULL
)
```

## Arguments

- n_items:

  Positive integer. Number of items.

- a_dist:

  Character string for the discrimination distribution. Currently only
  `"lnorm"` (log-normal) is supported. Default: `"lnorm"`.

- a_mean:

  Numeric. Mean of the log-normal distribution for `a` (i.e.,
  `meanlog`). Default: `0`.

- a_sd:

  Numeric. SD of the log-normal distribution for `a` (i.e., `sdlog`).
  Default: `0.25`.

- b_dist:

  Character string for the difficulty distribution. One of `"normal"` or
  `"even"`. Default: `"normal"`.

- b_mean:

  Numeric. Mean of the normal distribution for `b`. Only used when
  `b_dist = "normal"`. Default: `0`.

- b_sd:

  Numeric. SD of the normal distribution for `b`. Only used when
  `b_dist = "normal"`. Default: `1`.

- b_range:

  Numeric vector of length 2. Range for evenly-spaced `b` values. Only
  used when `b_dist = "even"`. Default: `c(-2, 2)`.

- seed:

  Optional integer seed for reproducibility. If `NULL` (default), the
  current RNG state is used.

## Value

A named list with elements `a` (numeric vector) and `b` (numeric
vector), each of length `n_items`.

## See also

[`irt_params_grm()`](https://sward1.github.io/irtsim/reference/irt_params_grm.md)
for GRM parameters,
[`irt_design()`](https://sward1.github.io/irtsim/reference/irt_design.md)
to use the generated parameters.

## Examples

``` r
# Default 2PL parameters for 30 items
params <- irt_params_2pl(n_items = 30, seed = 42)

# Evenly-spaced difficulty
params <- irt_params_2pl(n_items = 20, b_dist = "even", b_range = c(-3, 3))
```
