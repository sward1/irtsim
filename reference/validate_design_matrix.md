# Validate a Binary Design Matrix

Internal helper to validate that a matrix meets standard requirements
for booklet and linking design matrices: is a matrix, has correct column
count, and contains only binary values.

## Usage

``` r
validate_design_matrix(mat, n_items, matrix_name)
```

## Arguments

- mat:

  An object to validate (should be a matrix).

- n_items:

  Integer: the expected number of columns.

- matrix_name:

  Character string: the name of the matrix type for error messages
  (e.g., "booklet_matrix", "linking_matrix").

## Value

Invisibly returns `NULL` if all checks pass. Throws an error (via
[`stop()`](https://rdrr.io/r/base/stop.html)) if any check fails.
