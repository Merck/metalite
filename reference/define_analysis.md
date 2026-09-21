# Define analysis function meta information for ADaM dataset

Define analysis function meta information for ADaM dataset

## Usage

``` r
define_analysis(meta, name, ...)
```

## Arguments

- meta:

  A `meta_adam` object.

- name:

  A character value of term name. The term name is used as key to link
  information.

- ...:

  Additional variables.

## Value

A metadata object with analysis details defined.

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
  define_analysis(
    name = "ae_summary",
    title = "Summary of Adverse Events"
  )
#> ADaM metadata: 
#>    .$data_population     Population data with 254 subjects 
#>    .$data_observation    Observation data with 1191 records 
#>    .$plan    Analysis plan with 2 plans 
#> 
#> 
#>   Analysis function:
#>           name                          label
#> 1 'ae_summary' 'Table: adverse event summary'
#> 
```
