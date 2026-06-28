# Generate 1PL Item Parameters

Creates a list of difficulty (`b`) parameters suitable for passing to
[`irt_design()`](https://sward1.github.io/irtsim/reference/irt_design.md)
with `model = "1PL"`. The 1PL model is Rasch-family: every item shares
the same discrimination (fixed at `1`), so only `b` is generated here —
the `a = 1` contract is applied downstream in the design's
`validate_params` step.

## Usage

``` r
irt_params_1pl(
  n_items,
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

A named list with a single element `b` (numeric vector of length
`n_items`). Note: no `a` is returned — 1PL fixes discrimination at 1
downstream rather than at generation time.

## See also

[`irt_params_2pl()`](https://sward1.github.io/irtsim/reference/irt_params_2pl.md)
for the free-discrimination binary alternative,
[`irt_design()`](https://sward1.github.io/irtsim/reference/irt_design.md)
to use the generated parameters.

## Examples

``` r
# Default 1PL parameters for 30 items
params <- irt_params_1pl(n_items = 30, seed = 42)

# Evenly-spaced difficulty across a wider range
params <- irt_params_1pl(n_items = 20, b_dist = "even", b_range = c(-3, 3))
```
