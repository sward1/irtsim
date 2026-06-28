# Generate PCM Item Parameters

Creates a list of discrimination (`a`, fixed at 1) and step (`b`)
parameters suitable for passing to
[`irt_design()`](https://sward1.github.io/irtsim/reference/irt_design.md)
with `model = "PCM"`.

## Usage

``` r
irt_params_pcm(
  n_items,
  n_categories,
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
  `1.0`, consistent with mirt::simdata's polytomous conventions and the
  PCM examples in Embretson & Reise (2000). `0` is allowed (all steps
  within an item equal the item center — degenerate but useful for
  design exploration).

- seed:

  Optional integer seed for reproducibility.

## Value

A named list with elements:

- a:

  Numeric vector of length `n_items`, all 1 (Rasch family).

- b:

  Numeric matrix with `n_items` rows and `n_categories - 1` columns.
  Steps are NOT sorted within row.

## Details

The Partial Credit Model (Masters, 1982) is a Rasch-family polytomous
model: every item shares the same discrimination (fixed at 1), and the
step parameters within each item are NOT required to be ordered. This is
the defining contrast with the Graded Response Model — see
[`irt_params_grm()`](https://sward1.github.io/irtsim/reference/irt_params_grm.md)
for the ordered-threshold alternative.

## See also

[`irt_params_grm()`](https://sward1.github.io/irtsim/reference/irt_params_grm.md)
for the ordered-threshold polytomous model,
[`irt_design()`](https://sward1.github.io/irtsim/reference/irt_design.md)
to use the generated parameters.

## Examples

``` r
# PCM parameters: 15 items, 4 response categories
params <- irt_params_pcm(n_items = 15, n_categories = 4, seed = 42)

# Tighter within-item step spread (steps closer to the item center)
params <- irt_params_pcm(
  n_items = 15, n_categories = 4, step_dispersion = 0.5, seed = 42
)
```
