# Build True Parameter Data Frame for Estimation Model (Internal)

Creates a data frame of true parameter values adjusted for the
estimation model, accounting for misspecification. When generation and
estimation models differ, some parameters may need to be filled with
defaults (e.g., discrimination = 1 for Rasch when estimating 1PL from
2PL data) or dropped entirely (e.g., discrimination when estimating 1PL
from 2PL).

## Usage

``` r
build_true_params_for_estimation(design, estimation_model)
```

## Arguments

- design:

  An `irt_design` object specifying the generation model.

- estimation_model:

  Character string: one of "1PL", "2PL", "3PL", "GRM", "PCM", or "GPCM"
  (canonical list registered in
  [`get_model_config()`](https://sward1.github.io/irtsim/reference/get_model_config.md)).

## Value

Data frame with columns: item, param, true_value, matching the schema of
the estimation model.
