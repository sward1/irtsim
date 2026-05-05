# Create an IRT Design Specification

Define the data-generating model for an IRT simulation study. This
captures decisions 1–3 from the Schroeders & Gnambs (2025) framework:
dimensionality, item parameters, and item type.

## Usage

``` r
irt_design(model, n_items, item_params, theta_dist = "normal", n_factors = 1L)
```

## Arguments

- model:

  Character string specifying the IRT model. One of `"1PL"`, `"2PL"`, or
  `"GRM"`.

- n_items:

  Positive integer. Number of items in the instrument.

- item_params:

  A named list of item parameters. Contents depend on `model`:

  1PL

  :   `b` (numeric vector of length `n_items`). Discrimination is fixed
      at 1 for all items and added automatically.

  2PL

  :   `a` (discrimination, positive numeric vector or matrix) and `b`
      (difficulty, numeric vector), each of length `n_items`.

  GRM

  :   `a` (discrimination, positive numeric vector) of length `n_items`
      and `b` (threshold matrix, `n_items` rows by `n_categories - 1`
      columns).

- theta_dist:

  Either a character string (`"normal"` or `"uniform"`) or a function
  that takes a single argument `n` and returns a numeric vector of
  length `n`. Defaults to `"normal"`.

- n_factors:

  Positive integer specifying the number of latent factors. Defaults to
  `1L`. Currently only `n_factors = 1` is supported; multidimensional
  IRT (`n_factors > 1`) is planned for v0.4.0. Passing any value other
  than `1` raises an error rather than silently propagating an
  unsupported design to the estimator.

## Value

An S3 object of class `irt_design` (a named list) with elements `model`,
`n_items`, `item_params`, `theta_dist`, and `n_factors`.

## See also

[`irt_study()`](https://sward1.github.io/irtsim/reference/irt_study.md)
to add study conditions,
[`irt_params_2pl()`](https://sward1.github.io/irtsim/reference/irt_params_2pl.md)
and
[`irt_params_grm()`](https://sward1.github.io/irtsim/reference/irt_params_grm.md)
to generate item parameters.

## Examples

``` r
# 1PL (Rasch) design with 20 items
design_1pl <- irt_design(
  model = "1PL",
  n_items = 20,
  item_params = list(b = seq(-2, 2, length.out = 20))
)

# 2PL design
design_2pl <- irt_design(
  model = "2PL",
  n_items = 30,
  item_params = list(
    a = rlnorm(30, 0, 0.25),
    b = seq(-2, 2, length.out = 30)
  )
)
```
