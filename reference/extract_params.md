# Extract Item Parameter Estimates from a Fitted mirt Model (Internal)

Pulls point estimates, SEs, and CIs from a fitted mirt object and
returns them in long format matching the item_results schema.

## Usage

``` r
extract_params(
  mod,
  design,
  estimation_model,
  iteration,
  sample_size,
  true_params,
  true_params_lookup,
  se = TRUE
)
```

## Arguments

- mod:

  A fitted mirt object.

- design:

  An `irt_design` object (for true values and model type).

- estimation_model:

  Character string: "1PL", "2PL", or "GRM" (the model that was fitted,
  which may differ from design\$model).

- iteration:

  Integer iteration number.

- sample_size:

  Integer sample size.

- true_params:

  Data frame (used for schema).

- true_params_lookup:

  Named character vector mapping keys of the form `"item_param"` (e.g.,
  `"Item_1_a"`) to `true_value` (for O(1) lookup instead of repeated
  vector scans).

- se:

  Logical. Extract standard errors and CIs? Default `TRUE`.

## Value

Data frame with item_results columns.

## Details

Uses the list-based `coef(mod)` output (one matrix per item, keyed by
item name). Default [`coef()`](https://rdrr.io/r/stats/coef.html)
returns rows `par, CI_2.5, CI_97.5`; SE is derived from CI width:
`SE = (upper - lower) / (2 * z_0.975)`.
