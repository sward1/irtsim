# Print an IRT Simulation Result

Display a compact summary of an
[`irt_simulate()`](https://sward1.github.io/irtsim/reference/irt_simulate.md)
result, including model, items, sample sizes, iterations, convergence
rate, and elapsed time.

## Usage

``` r
# S3 method for class 'irt_results'
print(x, ...)
```

## Arguments

- x:

  An `irt_results` object.

- ...:

  Additional arguments (ignored).

## Value

`x`, invisibly.

## See also

[`irt_simulate()`](https://sward1.github.io/irtsim/reference/irt_simulate.md)

## Examples

``` r
# \donttest{
design <- irt_design(
  model = "1PL", n_items = 5,
  item_params = list(b = seq(-2, 2, length.out = 5))
)
study <- irt_study(design, sample_sizes = c(200, 500))
results <- irt_simulate(study, iterations = 10, seed = 42)
#> Iteration 5/10
#> Iteration 9/10
#> Iteration 10/10
#> 
print(results)
#> IRT Simulation Results
#>   Model:         1PL 
#>   Items:         5 
#>   Sample sizes:  200, 500 
#>   Iterations:    10 
#>   Convergence:  20/20 (100%)
#>   Elapsed:       2.4 s
#>   Seed:          42 
# }
```
