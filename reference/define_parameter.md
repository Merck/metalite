# Define analysis parameter meta information for ADaM dataset

Define analysis parameter meta information for ADaM dataset

## Usage

``` r
define_parameter(meta, name, subset = NULL, ...)
```

## Arguments

- meta:

  A `meta_adam` object.

- name:

  A character value of term name. The term name is used as key to link
  information.

- subset:

  An expression to identify analysis records. See
  [`base::subset()`](https://rdrr.io/r/base/subset.html).

- ...:

  Additional variables.

## Value

A metadata object with parameters defined.

## Examples

``` r
plan <- plan(
  analysis = "ae_summary", population = "apat",
  observation = c("wk12", "wk24"), parameter = "any;rel;ser"
)

meta_adam(
  population = r2rtf::r2rtf_adsl,
  observation = r2rtf::r2rtf_adae
) |>
  define_plan(plan = plan) |>
  define_parameter(
    name = "rel",
    subset = AEREL %in% c("POSSIBLE", "PROBABLE")
  )
#> ADaM metadata: 
#>    .$data_population     Population data with 254 subjects 
#>    .$data_observation    Observation data with 1191 records 
#>    .$plan    Analysis plan with 2 plans 
#> 
#> 
#>   Analysis parameter type:
#>    name                         label                               subset
#> 1 'rel' 'drug-related adverse events' AEREL %in% c('POSSIBLE', 'PROBABLE')
#> 
#> 
```
