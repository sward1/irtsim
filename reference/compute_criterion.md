# Compute Performance Criteria for a Single Parameter (Internal)

Given a vector of per-iteration estimates and the true parameter value,
computes bias, empirical SE, MSE, RMSE, coverage, and Monte Carlo SEs
following Morris et al. (2019).

## Usage

``` r
compute_criterion(estimates, true_value, ci_lower = NULL, ci_upper = NULL)
```

## Arguments

- estimates:

  Numeric vector of per-iteration parameter estimates. May contain NAs
  (non-converged iterations), which are excluded.

- true_value:

  Single numeric value. The data-generating (true) parameter value.

- ci_lower:

  Optional numeric vector (same length as `estimates`). Lower bounds of
  confidence intervals. If `NULL`, coverage is not computed.

- ci_upper:

  Optional numeric vector (same length as `estimates`). Upper bounds of
  confidence intervals. If `NULL`, coverage is not computed.

## Value

A named list with elements:

- bias:

  Mean estimate minus true value.

- empirical_se:

  Sample standard deviation of estimates (n-1 denominator).

- mse:

  Mean squared error: `mean((estimate - true_value)^2)`.

- rmse:

  Root mean squared error: `sqrt(mse)`.

- coverage:

  Proportion of CIs containing the true value, or `NULL` if CIs not
  provided. NAs in CIs are excluded from the denominator.

- mcse_bias:

  Monte Carlo SE of bias: `empirical_se / sqrt(K)`.

- mcse_mse:

  Monte Carlo SE of MSE: `sd((est - true)^2) / sqrt(K)`.

## References

Morris, T. P., White, I. R., & Crowther, M. J. (2019). Using simulation
studies to evaluate statistical methods. *Statistics in Medicine*,
38(11), 2074–2102.
[doi:10.1002/sim.8086](https://doi.org/10.1002/sim.8086)
