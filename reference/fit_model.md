# Fit an IRT Model (Internal)

Wraps
[`mirt::mirt()`](https://philchalmers.github.io/mirt/reference/mirt.html)
with error and convergence handling.

## Usage

``` r
fit_model(data, model, se = TRUE)
```

## Arguments

- data:

  Numeric matrix of response data (may contain NAs).

- model:

  Character string: one of "1PL", "2PL", "3PL", "GRM", "PCM", or "GPCM"
  (canonical list registered in
  [`get_model_config()`](https://sward1.github.io/irtsim/reference/get_model_config.md)).

- se:

  Logical. Compute standard errors? Default `TRUE`.

## Value

A list with elements `model` (fitted mirt object or NULL) and
`converged` (logical).
