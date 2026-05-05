# Extract Theta Recovery Summary from a Fitted mirt Model (Internal)

Computes EAP theta estimates and summarizes recovery via correlation and
RMSE against true theta.

## Usage

``` r
extract_theta_summary(mod, theta_true, iteration, sample_size)
```

## Arguments

- mod:

  A fitted mirt object.

- theta_true:

  Numeric vector of true theta values.

- iteration:

  Integer iteration number.

- sample_size:

  Integer sample size.

## Value

Single-row data frame with theta_results columns.
