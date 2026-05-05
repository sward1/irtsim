# Compute Required Monte Carlo Replications

Uses the Burton (2003) formula to determine the minimum number of
simulation replications needed to achieve a desired level of Monte Carlo
precision.

## Usage

``` r
irt_iterations(sigma, delta, alpha = 0.05)
```

## Arguments

- sigma:

  Positive numeric. The empirical standard error of the estimand across
  replications (or a pilot estimate thereof).

- delta:

  Positive numeric. The acceptable Monte Carlo error (half-width of the
  MC confidence interval for the estimand).

- alpha:

  Numeric in (0, 1). Two-sided significance level. Default `0.05` (i.e.,
  95 percent MC confidence).

## Value

An integer: the minimum number of replications.

## Details

The formula is: \$\$R = \lceil (z\_{\alpha/2} \cdot \sigma / \delta)^2
\rceil\$\$

where \\\sigma\\ is the empirical standard error of the estimand,
\\\delta\\ is the acceptable Monte Carlo error, and \\z\_{\alpha/2}\\ is
the critical value for the desired confidence level.

## References

Burton, A., Altman, D. G., Royston, P., & Holder, R. L. (2006). The
design of simulation studies in medical statistics. *Statistics in
Medicine*, 25(24), 4279–4292.
[doi:10.1002/sim.2673](https://doi.org/10.1002/sim.2673)

## See also

[`irt_simulate()`](https://sward1.github.io/irtsim/reference/irt_simulate.md)
for running the simulation with the computed number of replications.

## Examples

``` r
# How many replications for MC SE of bias < 0.1
# when empirical SE of the estimand is 0.5?
irt_iterations(sigma = 0.5, delta = 0.1)
#> [1] 97

# Tighter tolerance with 99% MC confidence
irt_iterations(sigma = 0.5, delta = 0.05, alpha = 0.01)
#> [1] 664
```
