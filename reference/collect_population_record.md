# Collect population record from population dataset

The key variables used in `id`, `group`, and `subset` are displayed by
default.

## Usage

``` r
collect_population_record(meta, population, var = NULL)
```

## Arguments

- meta:

  A `meta_adam` object.

- population:

  A character value of population term name. The term name is used as
  key to link information.

- var:

  A character vector of additional variables to be displayed in the
  output.

## Value

A data frame containing the variables in the population dataset.

## Examples

``` r
meta <- meta_example()
head(collect_population_record(meta, "apat"))
#>       USUBJID                 TRTA SAFFL
#> 1 01-701-1015              Placebo     Y
#> 2 01-701-1023              Placebo     Y
#> 3 01-701-1028 Xanomeline High Dose     Y
#> 4 01-701-1033  Xanomeline Low Dose     Y
#> 5 01-701-1034 Xanomeline High Dose     Y
#> 6 01-701-1047              Placebo     Y
head(collect_population_record(meta, "apat", var = "AGE"))
#>       USUBJID                 TRTA SAFFL AGE
#> 1 01-701-1015              Placebo     Y  63
#> 2 01-701-1023              Placebo     Y  64
#> 3 01-701-1028 Xanomeline High Dose     Y  71
#> 4 01-701-1033  Xanomeline Low Dose     Y  74
#> 5 01-701-1034 Xanomeline High Dose     Y  77
#> 6 01-701-1047              Placebo     Y  85
```
