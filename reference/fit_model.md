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

  Character string: "1PL", "2PL", or "GRM".

- se:

  Logical. Compute standard errors? Default `TRUE`.

## Value

A list with elements `model` (fitted mirt object or NULL) and
`converged` (logical).
