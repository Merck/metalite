# Format numbers with fixed decimal places

Round with
[`round_half_away_from_zero()`](https://merck.github.io/metalite/reference/round_half_away_from_zero.md)
and format with a fixed number of decimal places. Decimal ties round
half away from zero, and values that round to zero display as positive
zero (for example, `"0.0"` rather than `"-0.0"`).

## Usage

``` r
format_number(x, digits = 1, width = NULL)
```

## Arguments

- x:

  A numeric vector.

- digits:

  A non-negative, integer-valued scalar giving the number of decimal
  places.

- width:

  `NULL`, or a non-negative, integer-valued scalar giving the minimum
  field width passed to
  [`base::formatC()`](https://rdrr.io/r/base/formatc.html). The default,
  `NULL`, does not set a minimum width.

## Value

A character vector containing the formatted values.

## Examples

``` r
format_number(c(1.25, -1.25), digits = 1)
#> [1] "1.3"  "-1.3"
format_number(c(6.25, -0.04), digits = 1, width = 5)
#> [1] "  6.3" "  0.0"
```
