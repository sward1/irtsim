# Convert Mirt's d Parameter to IRT Difficulty b

Applies the delta method to convert d (intercept) to b (difficulty)
under the 2PL/GRM parameterization: b = -d / a. Propagates standard
errors and confidence intervals correctly.

## Usage

``` r
convert_d_to_b(a_info, d_info)
```

## Arguments

- a_info:

  A list with elements `est`, `se`, `ci_lower`, `ci_upper` for the
  discrimination parameter a. Typically from
  [`extract_one_param()`](https://sward1.github.io/irtsim/reference/extract_one_param.md).

- d_info:

  A list with elements `est`, `se`, `ci_lower`, `ci_upper` for the
  intercept parameter d. Typically from
  [`extract_one_param()`](https://sward1.github.io/irtsim/reference/extract_one_param.md).

## Value

A list with elements `est`, `se`, `ci_lower`, `ci_upper` for the
converted difficulty parameter b, using delta method variance
propagation.

## Details

The delta method variance for b = -d/a is: \$\$\text{Var}(b) =
\frac{\text{Var}(d)}{a^2} + \frac{d^2 \text{Var}(a)}{a^4}\$\$

CI bounds are transformed directly: b_ci = -d_ci / a.
