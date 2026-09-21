# Define analysis population meta information for ADaM dataset

Define analysis population meta information for ADaM dataset

## Usage

``` r
define_population(
  meta,
  name,
  id = "USUBJID",
  group = NULL,
  var = NULL,
  subset = NULL,
  label = NULL,
  ...
)
```

## Arguments

- meta:

  A `meta_adam` object.

- name:

  A character value of term name. The term name is used as key to link
  information.

- id:

  A character value of subject identifier variable name in an ADaM
  dataset.

- group:

  A character vector of group variable names in an ADaM dataset.

- var:

  A character vector of useful variable names in an ADaM dataset.

- subset:

  An expression to identify analysis records. See
  [`base::subset()`](https://rdrr.io/r/base/subset.html).

- label:

  A character value of analysis label.

- ...:

  Additional variables.

## Value

A metadata object with population defined.

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
  define_plan(plan) |>
  define_population(name = "apat")
#> ADaM metadata: 
#>    .$data_population     Population data with 254 subjects 
#>    .$data_observation    Observation data with 1191 records 
#>    .$plan    Analysis plan with 2 plans 
#> 
#> 
#>   Analysis population type:
#>     name        id group var subset                         label
#> 1 'apat' 'USUBJID'                  'All Participants as Treated'
#> 
#> 
```
