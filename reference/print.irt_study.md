# Print an IRT Study

Display a compact summary of an
[irt_study](https://sward1.github.io/irtsim/reference/irt_study.md)
object, including model, items, sample sizes, and missing data
mechanism.

## Usage

``` r
# S3 method for class 'irt_study'
print(x, ...)
```

## Arguments

- x:

  An `irt_study` object.

- ...:

  Additional arguments (ignored).

## Value

`x`, invisibly.

## See also

[`irt_study()`](https://sward1.github.io/irtsim/reference/irt_study.md)

## Examples

``` r
d <- irt_design("1PL", 10, list(b = seq(-2, 2, length.out = 10)))
s <- irt_study(d, sample_sizes = c(100, 500))
print(s)
#> IRT Study
#>   Model:          1PL 
#>   Items:          10 
#>   Sample sizes:   100, 500 
#>   Missing data:   none (complete data)
```
