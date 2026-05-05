# Print Summary of IRT Simulation Results

Display item parameter criteria and theta recovery statistics from a
[`summary.irt_results()`](https://sward1.github.io/irtsim/reference/summary.irt_results.md)
object.

## Usage

``` r
# S3 method for class 'summary_irt_results'
print(x, ...)
```

## Arguments

- x:

  A `summary_irt_results` object.

- ...:

  Additional arguments (ignored).

## Value

`x`, invisibly.

## See also

[`summary.irt_results()`](https://sward1.github.io/irtsim/reference/summary.irt_results.md)

## Examples

``` r
# \donttest{
design <- irt_design(
  model = "1PL", n_items = 5,
  item_params = list(b = seq(-2, 2, length.out = 5))
)
study <- irt_study(design, sample_sizes = c(200, 500))
results <- irt_simulate(study, iterations = 10, seed = 42)
#> Iteration 4/10
#> Iteration 5/10
#> Iteration 7/10
#> Iteration 8/10
#> Iteration 9/10
#> Iteration 10/10
#> 
s <- summary(results)
print(s)
#> IRT Simulation Summary
#>   Model:       1PL 
#>   Iterations:  10 
#>   Seed:        42 
#> 
#> Item Parameter Criteria:
#>  sample_size item param true_value        bias empirical_se        mse
#>          200    1     b         -2  0.01826936    0.2813439 0.07157270
#>          200    2     b         -1 -0.05712048    0.2314278 0.05146568
#>          200    3     b          0 -0.07594365    0.2225141 0.05032873
#>          200    4     b          1 -0.03849063    0.1497202 0.02165607
#>          200    5     b          2  0.05270597    0.3076836 0.08798018
#>          500    1     b         -2  0.02029278    0.1618503 0.02398776
#>          500    2     b         -1  0.04266708    0.1450726 0.02076192
#>          500    3     b          0 -0.04696731    0.1306269 0.01756299
#>          500    4     b          1  0.08968595    0.1061969 0.01819357
#>          500    5     b          2 -0.03168185    0.1287219 0.01591614
#>       rmse coverage  mcse_bias    mcse_mse n_converged
#>  0.2675307      0.9 0.08896874 0.037426556          10
#>  0.2268605      0.8 0.07318389 0.019402165          10
#>  0.2243407      0.8 0.07036515 0.025615834          10
#>  0.1471600      1.0 0.04734570 0.005954864          10
#>  0.2966145      0.9 0.09729809 0.074831545          10
#>  0.1548798      0.8 0.05118155 0.010034577          10
#>  0.1440900      0.9 0.04587597 0.006047372          10
#>  0.1325254      1.0 0.04130787 0.004460940          10
#>  0.1348835      1.0 0.03358240 0.005734851          10
#>  0.1261592      1.0 0.04070544 0.005735466          10
#> 
#> Theta Recovery:
#>  sample_size  mean_cor     sd_cor mean_rmse    sd_rmse n_converged
#>          200 0.6710695 0.05118505 0.7312415 0.05366362          10
#>          500 0.6533689 0.02817120 0.7587360 0.02548581          10
# }
```
