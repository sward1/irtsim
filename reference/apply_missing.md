# Apply Missing Data Mechanism (Internal)

Takes a complete response matrix and introduces missingness according to
the study specification.

## Usage

``` r
apply_missing(data, study, seed = NULL, theta = NULL)
```

## Arguments

- data:

  Numeric matrix (N x n_items). Complete response data.

- study:

  An `irt_study` object specifying the missing data mechanism.

- seed:

  Integer. Random seed for reproducibility.

- theta:

  Optional numeric vector of length `nrow(data)`. Required when
  `study$missing == "mar"`.

## Value

Numeric matrix of same dimensions as `data`, with `NA` values introduced
according to the missingness mechanism.
