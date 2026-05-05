# Generate IRT Response Data (Internal)

Wraps
[`mirt::simdata()`](https://philchalmers.github.io/mirt/reference/simdata.html)
to produce a response matrix from an
[irt_design](https://sward1.github.io/irtsim/reference/irt_design.md)
specification. Handles the b-to-d parameterization translation and theta
generation.

## Usage

``` r
generate_data(design, n, seed = NULL, theta = NULL)
```

## Arguments

- design:

  An `irt_design` object.

- n:

  Integer. Number of respondents.

- seed:

  Optional integer. Random seed for reproducibility. If `NULL`, the
  current RNG state drives draws (used by the parallel dispatch path in
  [`irt_simulate()`](https://sward1.github.io/irtsim/reference/irt_simulate.md)
  so future.apply's L'Ecuyer-CMRG substreams are not clobbered by an
  explicit [`set.seed()`](https://rdrr.io/r/base/Random.html) call).

- theta:

  Optional numeric vector of length `n`. Pre-generated theta values. If
  `NULL`, theta is drawn from `design$theta_dist`.

## Value

A numeric matrix with `n` rows and `design$n_items` columns.
