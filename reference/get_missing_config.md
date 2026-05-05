# Get a Missing Mechanism Configuration from the Registry (Internal)

Retrieves the metadata for a specified missing data mechanism and
validates that it exists.

## Usage

``` r
get_missing_config(mechanism)
```

## Arguments

- mechanism:

  Character string: one of "none", "mcar", "mar", "booklet", or
  "linking".

## Value

A named list with mechanism-specific metadata: `requires_rate`
(logical), `requires_test_design` (logical), `test_design_key`
(character or NA), `print_label` (character), `print_style` (character:
"plain", "rate", or "design"), and `design_unit` (character or NA).
