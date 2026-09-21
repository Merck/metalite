# Round half away from zero

Round numeric values to a given number of decimal places, with decimal
ties (for example, 1.25 at `digits = 1`) rounded half away from zero.
This differs from [`base::round()`](https://rdrr.io/r/base/Round.html),
which uses round-to-even for ties. Values that round to zero, including
small negative values, return positive zero so formatted output does not
display negative zero.

## Usage

``` r
round_half_away_from_zero(x, digits = 0)
```

## Arguments

- x:

  A numeric vector, matrix, array, or data frame with only numeric
  columns.

- digits:

  A finite, integer-valued scalar giving the number of decimal places.
  Negative values round to positions left of the decimal point.

## Value

A numeric object with the same dimensions, dimension names, and names as
`x`. A data frame input returns a data frame.

## Details

To account for floating-point representation, values within
`sqrt(.Machine$double.eps)` below a tie at the requested precision are
treated as ties. The implementation is adapted from `roundSAS()` in
[pharmaverse/tidytlg](https://github.com/pharmaverse/tidytlg/blob/5f169c76428976f53d2af9e7fe52460348ef6cb7/R/roundSAS.R).

## Examples

``` r
round_half_away_from_zero(c(1.25, -1.25), digits = 1)
#> [1]  1.3 -1.3
round_half_away_from_zero(c(-0.04, NA), digits = 1)
#> [1]  0 NA
```
