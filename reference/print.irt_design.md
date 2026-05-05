# Print an IRT Design

Display a compact summary of an
[irt_design](https://sward1.github.io/irtsim/reference/irt_design.md)
object, including model type, number of items, theta distribution, and
parameter ranges.

## Usage

``` r
# S3 method for class 'irt_design'
print(x, ...)
```

## Arguments

- x:

  An `irt_design` object.

- ...:

  Additional arguments (ignored).

## Value

`x`, invisibly.

## See also

[`irt_design()`](https://sward1.github.io/irtsim/reference/irt_design.md)

## Examples

``` r
d <- irt_design("1PL", 10, list(b = seq(-2, 2, length.out = 10)))
print(d)
#> IRT Design
#>   Model:        1PL 
#>   Items:        10 items
#>   Theta dist:   normal 
#>   Factors:      1 
#>   a range:      [1, 1]
#>   b range:      [-2, 2]
```
