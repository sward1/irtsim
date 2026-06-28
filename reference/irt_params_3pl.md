# Generate 3PL Item Parameters

Creates a list of discrimination (`a`), difficulty (`b`), and guessing
(`c`) parameters suitable for passing to
[`irt_design()`](https://sward1.github.io/irtsim/reference/irt_design.md)
with `model = "3PL"`.

## Usage

``` r
irt_params_3pl(
  n_items,
  a_dist = "lnorm",
  a_mean = 0,
  a_sd = 0.25,
  b_dist = "normal",
  b_mean = 0,
  b_sd = 1,
  b_range = c(-2, 2),
  c_shape1 = 5,
  c_shape2 = 17,
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

  Numeric. `meanlog` for the log-normal distribution. Default: `0`.

- a_sd:

  Numeric. `sdlog` for the log-normal distribution. Default: `0.25`.

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

- c_shape1:

  Positive numeric. First shape parameter of the Beta distribution used
  to generate `c`. Default: `5`.

- c_shape2:

  Positive numeric. Second shape parameter. Default: `17`. The default
  `Beta(5, 17)` has `E[c] ~= 0.227, SD ~= 0.087`, consistent with
  typical four-option multiple-choice items.

- seed:

  Optional integer seed for reproducibility. If `NULL` (default), the
  current RNG state is used.

## Value

A named list with elements `a`, `b`, `c`, each a numeric vector of
length `n_items`.

## See also

[`irt_params_2pl()`](https://sward1.github.io/irtsim/reference/irt_params_2pl.md),
[`irt_params_grm()`](https://sward1.github.io/irtsim/reference/irt_params_grm.md),
[`irt_design()`](https://sward1.github.io/irtsim/reference/irt_design.md).

## Examples

``` r
# Default 3PL parameters for 30 items
params <- irt_params_3pl(n_items = 30, seed = 42)

# Custom guessing distribution (e.g., 5-option items, lower chance level)
params <- irt_params_3pl(
  n_items = 30, c_shape1 = 4, c_shape2 = 16, seed = 42
)
```
