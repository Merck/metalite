# Construct `outdata` class

The `outdata` class defines a standard output format for analysis and
reporting.

## Usage

``` r
outdata(
  meta,
  population,
  observation,
  parameter,
  n,
  order,
  group,
  reference_group,
  ...
)
```

## Arguments

- meta:

  A metadata object created by metalite.

- population:

  A character value of population term name. The term name is used as
  key to link information.

- observation:

  A character value of observation term name. The term name is used as
  key to link information.

- parameter:

  A character value of parameter term name. The term name is used as key
  to link information.

- n:

  A data frame for number of subjects in each criteria.

- order:

  A numeric vector of row display order.

- group:

  A character vector of group variable names in an ADaM dataset.

- reference_group:

  A numeric value to indicate reference group in levels of group.

- ...:

  Additional variables to save to `outdata`.

## Value

A list with class `outdata`. Components of the list are either quosures
or constants.

## Details

The design is inspired by
[`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).

## Examples

``` r
outdata(
  meta = meta_example(),
  population = "apat",
  observation = "wk12",
  parameter = "rel",
  n = data.frame(
    TRTA = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
    n = c(86, 84, 84)
  ),
  group = "TRTA",
  reference_group = 1,
  order = 1:3
)
#> List of 8
#>  $ meta           :List of 7
#>  $ population     : chr "apat"
#>  $ observation    : chr "wk12"
#>  $ parameter      : chr "rel"
#>  $ n              :'data.frame': 3 obs. of  2 variables:
#>  $ order          : int [1:3] 1 2 3
#>  $ group          : chr "TRTA"
#>  $ reference_group: num 1
```
