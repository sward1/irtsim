# Extract a Single Parameter Estimate with SE and CI

Internal helper to extract estimate, standard error, and confidence
interval bounds from a single column of a mirt coefficient matrix.

## Usage

``` r
extract_one_param(mat, col_name)
```

## Arguments

- mat:

  A coefficient matrix from `mirt::coef()` with rows "par", "CI_2.5",
  "CI_97.5" and column names matching mirt's parameter naming.

- col_name:

  Character string: the column name to extract (e.g., "a1", "d", "d2").

## Value

A list with elements:

- est:

  The parameter estimate (from "par" row).

- se:

  The standard error derived from CI width, or NA if CI rows absent.

- ci_lower:

  Lower CI bound (from "CI_2.5" row), or NA if absent.

- ci_upper:

  Upper CI bound (from "CI_97.5" row), or NA if absent.
