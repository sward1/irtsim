# Get a Criterion Configuration from the Registry (Internal)

Retrieves the metadata for a specified criterion and validates that it
exists.

## Usage

``` r
get_criterion_config(criterion)
```

## Arguments

- criterion:

  Character string: one of "bias", "empirical_se", "mse", "rmse",
  "coverage", "mcse_bias", "mcse_mse".

## Value

A named list with criterion-specific metadata: `direction` (char:
"lower_is_better" or "higher_is_better"), `use_abs` (logical), and
`display_label` (char).
