# Build complete meta information

Build complete meta information

## Usage

``` r
meta_build(meta)
```

## Arguments

- meta:

  A `meta_adam` object.

## Value

A composed metadata object.

## Examples

``` r
meta_adam(
  observation = r2rtf::r2rtf_adae,
  population = r2rtf::r2rtf_adsl
) |>
  # define analysis plan
  define_plan(
    plan(
      analysis = "ae_summary",
      population = "apat",
      observation = c("wk12"),
      parameter = "any;rel"
    )
  ) |>
  # define population
  define_population(
    name = "apat",
    group = "TRT01A",
    subset = SAFFL == "Y"
  ) |>
  # define observation
  define_observation(
    name = "wk12",
    group = "TRTA",
    subset = SAFFL == "Y",
    label = "Weeks 0 to 12"
  ) |>
  # define parameter - rel
  define_parameter(
    name = "rel",
    subset = AEREL %in% c("POSSIBLE", "PROBABLE")
  ) |>
  # define analysis
  define_parameter(
    name = "rel",
    subset = AEREL %in% c("POSSIBLE", "PROBABLE")
  ) |>
  meta_build()
#> ADaM metadata: 
#>    .$data_population     Population data with 254 subjects 
#>    .$data_observation    Observation data with 1191 records 
#>    .$plan    Analysis plan with 1 plans 
#> 
#> 
#>   Analysis population type:
#>     name        id    group var       subset                         label
#> 1 'apat' 'USUBJID' 'TRT01A'     SAFFL == 'Y' 'All Participants as Treated'
#> 
#> 
#>   Analysis observation type:
#>     name        id  group var       subset           label
#> 1 'wk12' 'USUBJID' 'TRTA'     SAFFL == 'Y' 'Weeks 0 to 12'
#> 
#> 
#>   Analysis parameter type:
#>    name                         label                               subset
#> 1 'rel' 'drug-related adverse events' AEREL %in% c('POSSIBLE', 'PROBABLE')
#> 2 'any'          'any adverse events'                                     
#> 
#> 
#>   Analysis function:
#>           name                          label
#> 1 'ae_summary' 'Table: adverse event summary'
#> 
```
