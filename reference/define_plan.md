# Define analysis plan meta information for ADaM dataset

Define analysis plan meta information for ADaM dataset

## Usage

``` r
define_plan(meta, plan)
```

## Arguments

- meta:

  A `meta_adam` object.

- plan:

  A data frame for analysis plan.

## Value

A metadata object with plans defined.

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
  define_plan(plan)
#> ADaM metadata: 
#>    .$data_population     Population data with 254 subjects 
#>    .$data_observation    Observation data with 1191 records 
#>    .$plan    Analysis plan with 2 plans 
#> 
#> 
```
