# Create a analysis plan from all combination of variables

This function is a wrapper of
[`base::expand.grid()`](https://rdrr.io/r/base/expand.grid.html).

## Usage

``` r
plan(analysis, population, observation, parameter, mock = 1, ...)
```

## Arguments

- analysis:

  A character value of analysis term name. The term name is used as key
  to link information.

- population:

  A character value of population term name. The term name is used as
  key to link information.

- observation:

  A character value of observation term name. The term name is used as
  key to link information.

- parameter:

  A character value of parameter term name. The term name is used as key
  to link information.

- mock:

  A numeric value of mock table number.

- ...:

  Additional arguments.

## Value

A data frame containing the analysis plan.

## Examples

``` r
# Example 1
# Create an analysis plan of AE summary
# with any AE, drug-related AE, and serious AE
plan(
  analysis = "ae_summary",
  population = "apat",
  observation = c("wk12", "wk24"),
  parameter = "any;rel;ser"
)
#>   mock   analysis population observation   parameter
#> 1    1 ae_summary       apat        wk12 any;rel;ser
#> 2    1 ae_summary       apat        wk24 any;rel;ser

# Example 2
# Create an analysis plan of AE specific
# with any AE, drug-related AE, and serious AE
plan(
  analysis = "ae_specific",
  population = "apat",
  observation = c("wk12", "wk24"),
  parameter = c("any", "rel", "ser")
)
#>   mock    analysis population observation parameter
#> 1    1 ae_specific       apat        wk12       any
#> 2    1 ae_specific       apat        wk24       any
#> 3    1 ae_specific       apat        wk12       rel
#> 4    1 ae_specific       apat        wk24       rel
#> 5    1 ae_specific       apat        wk12       ser
#> 6    1 ae_specific       apat        wk24       ser
```
