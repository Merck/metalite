# Collect specification for population definition

Collect specification for population definition

## Usage

``` r
collect_population(meta, population, observation = NULL, parameter = NULL)
```

## Arguments

- meta:

  A `meta_adam` object.

- population:

  A character value of population term name. The term name is used as
  key to link information.

- observation:

  A character value of observation term name. The term name is used as
  key to link information.

- parameter:

  A character value of parameter term name. The term name is used as key
  to link information.

## Value

A list covering the filter of population, observation (if given) and
parameter (if given).

## Examples

``` r
meta <- meta_example()
collect_population(meta, "apat")
#> $population
#> [1] "SAFFL == 'Y'"
#> 
collect_population(meta, "apat", "wk12")
#> $population
#> [1] "SAFFL == 'Y'"
#> 
#> $observation
#> [1] "SAFFL == 'Y'"
#> 
collect_population(meta, "apat", "wk12", "ser")
#> $population
#> [1] "SAFFL == 'Y'"
#> 
#> $observation
#> [1] "SAFFL == 'Y'"
#> 
#> $parameter
#> [1] "AESER == 'Y'"
#> 
```
