# Generate GPCM Item Parameters

Creates a list of discrimination (`a`) and step (`b`) parameters
suitable for passing to
[`irt_design()`](https://sward1.github.io/irtsim/reference/irt_design.md)
with `model = "GPCM"`.

## Usage

``` r
irt_params_gpcm(
  n_items,
  n_categories,
  a_dist = "lnorm",
  a_mean = 0,
  a_sd = 0.25,
  b_dist = "normal",
  b_mean = 0,
  b_sd = 1,
  b_range = c(-2, 2),
  step_dispersion = 1,
  seed = NULL
)
```

## Arguments

- n_items:

  Positive integer. Number of items.

- n_categories:

  Positive integer \>= 2. Number of response categories per item.
  Produces `n_categories - 1` step columns in `b`.

- a_dist:

  Character string for the discrimination distribution. Currently only
  `"lnorm"` (log-normal) is supported. Default: `"lnorm"`.

- a_mean:

  Numeric. `meanlog` for the log-normal distribution. Default: `0`.

- a_sd:

  Numeric. `sdlog` for the log-normal distribution. Default: `0.25`.

- b_dist:

  Character string for the item-center distribution: either `"normal"`
  (default) or `"even"`.

- b_mean:

  Numeric. Mean of item centers when `b_dist = "normal"`. Default: `0`.

- b_sd:

  Numeric. SD of item centers when `b_dist = "normal"`. Default: `1`.

- b_range:

  Length-2 numeric vector giving the minimum and maximum item-center
  values. Only used when `b_dist = "even"`. Default: `c(-2, 2)`.

- step_dispersion:

  Non-negative numeric. SD of the within-item step offsets drawn from
  `rnorm(0, step_dispersion)` and added to each item's center. Default:
  `1.0`. `0` is allowed (all steps within an item equal the item center
  — degenerate but useful for design exploration).

- seed:

  Optional integer seed for reproducibility.

## Value

A named list with elements:

- a:

  Positive numeric vector of length `n_items`.

- b:

  Numeric matrix with `n_items` rows and `n_categories - 1` columns.
  Steps are NOT sorted within row.

## Details

The Generalized Partial Credit Model (Muraki, 1992) is partial-credit
family — like the Partial Credit Model, step parameters within each item
are NOT required to be ordered (the defining contrast with the Graded
Response Model). Unlike PCM, GPCM allows per-item discrimination: `a` is
a free positive vector rather than fixed at 1. See
[`irt_params_pcm()`](https://sward1.github.io/irtsim/reference/irt_params_pcm.md)
for the Rasch-family alternative.

## See also

[`irt_params_pcm()`](https://sward1.github.io/irtsim/reference/irt_params_pcm.md)
for the Rasch-family (a fixed at 1) alternative,
[`irt_params_grm()`](https://sward1.github.io/irtsim/reference/irt_params_grm.md)
for the ordered-threshold polytomous model,
[`irt_design()`](https://sward1.github.io/irtsim/reference/irt_design.md)
to use the generated parameters.

## Examples

``` r
# GPCM parameters: 15 items, 4 response categories
params <- irt_params_gpcm(n_items = 15, n_categories = 4, seed = 42)

# Tighter within-item step spread and a wider discrimination distribution
params <- irt_params_gpcm(
  n_items = 15, n_categories = 4,
  a_sd = 0.50, step_dispersion = 0.5, seed = 42
)
```
