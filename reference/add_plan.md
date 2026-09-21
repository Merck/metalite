# Add additional analysis plan

Add additional analysis plan

## Usage

``` r
add_plan(plan, analysis, population, observation, parameter, ...)
```

## Arguments

- plan:

  A `meta_plan` object.

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

- ...:

  Additional arguments.

## Value

A data frame containing analysis plans with new plans added.

## Examples

``` r
plan("ae_summary",
  population = "apat",
  observation = c("wk12", "wk24"), parameter = "any;rel"
) |>
  add_plan("ae_specific",
    population = "apat",
    observation = c("wk12", "wk24"), parameter = c("any", "rel")
  )
#>   mock    analysis population observation parameter
#> 1    1  ae_summary       apat        wk12   any;rel
#> 2    1  ae_summary       apat        wk24   any;rel
#> 3    2 ae_specific       apat        wk12       any
#> 4    2 ae_specific       apat        wk24       any
#> 5    2 ae_specific       apat        wk12       rel
#> 6    2 ae_specific       apat        wk24       rel
```
