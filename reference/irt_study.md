# Define Study Conditions for an IRT Simulation

Add study-level conditions to an IRT design specification. This captures
decisions 4–5 from the Schroeders & Gnambs (2025) framework: sample
sizes and missing data mechanism.

## Usage

``` r
irt_study(
  design,
  sample_sizes,
  missing = "none",
  missing_rate = NULL,
  test_design = NULL,
  estimation_model = NULL
)
```

## Arguments

- design:

  An
  [`irt_design`](https://sward1.github.io/irtsim/reference/irt_design.md)
  object specifying the data-generating model.

- sample_sizes:

  Integer vector of sample sizes to evaluate. Values are coerced to
  integer, sorted in ascending order, and deduplicated.

- missing:

  Character string specifying the missing data mechanism. One of
  `"none"` (default), `"mcar"`, `"mar"`, `"booklet"`, or `"linking"`.

- missing_rate:

  Numeric value in \\\[0, 1)\\ specifying the proportion of missing
  data. Required when `missing` is `"mcar"` or `"mar"`; ignored when
  `missing` is `"none"`.

- test_design:

  A list specifying the test design for structured missingness. Required
  when `missing` is `"booklet"` or `"linking"`.

  booklet

  :   Must contain `booklet_matrix`: a binary matrix (n_booklets x
      n_items) where 1 indicates the item is administered.

  linking

  :   Must contain `linking_matrix`: a binary matrix (n_forms x n_items)
      where 1 indicates the item appears on the form.

- estimation_model:

  Character string specifying the IRT model to fit. One of `"1PL"`,
  `"2PL"`, or `"GRM"`. If `NULL` (default), defaults to `design$model`
  (i.e., the generation model is also the estimation model). Set to a
  different model to perform misspecification studies (e.g., generate
  2PL, estimate 1PL). Cross-fits are only allowed within the same
  response format (binary: 1PL, 2PL; polytomous: GRM).

## Value

An S3 object of class `irt_study` (a named list) with elements `design`,
`missing`, `missing_rate`, `sample_sizes`, `test_design`, and
`estimation_model`.

## See also

[`irt_design()`](https://sward1.github.io/irtsim/reference/irt_design.md)
for the design specification,
[`irt_simulate()`](https://sward1.github.io/irtsim/reference/irt_simulate.md)
to run the simulation.

## Examples

``` r
# Simple study with no missing data
d <- irt_design(
  model = "1PL", n_items = 20,
  item_params = list(b = seq(-2, 2, length.out = 20))
)
study <- irt_study(d, sample_sizes = c(100, 250, 500))

# Study with MCAR missingness
study_mcar <- irt_study(d, sample_sizes = c(200, 400),
                        missing = "mcar", missing_rate = 0.2)

# Model misspecification: generate 2PL, fit 1PL
d_2pl <- irt_design(
  model = "2PL", n_items = 15,
  item_params = list(a = rlnorm(15, 0, 0.25), b = rnorm(15))
)
study_misspec <- irt_study(d_2pl, sample_sizes = c(100, 300),
                           estimation_model = "1PL")
```
