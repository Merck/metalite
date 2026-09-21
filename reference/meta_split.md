# Split metadata into groups

Split metadata into groups

## Usage

``` r
meta_split(meta, by)
```

## Arguments

- meta:

  A `meta_adam` object.

- by:

  A character variable name both in population level and observation
  level data of a metadata object.

## Value

A metadata object split by the input variable.

## Examples

``` r
meta_example() |> meta_split("RACE")
#> $WHITE
#> ADaM metadata: 
#>    .$data_population     Population data with 230 subjects 
#>    .$data_observation    Observation data with 1086 records 
#>    .$plan    Analysis plan with 10 plans 
#> 
#> 
#>   Analysis population type:
#>     name        id  group var       subset                         label
#> 1 'apat' 'USUBJID' 'TRTA'     SAFFL == 'Y' 'All Participants as Treated'
#> 
#> 
#>   Analysis observation type:
#>     name        id  group var          subset           label
#> 1 'wk12' 'USUBJID' 'TRTA'        SAFFL == 'Y' 'Weeks 0 to 12'
#> 2 'wk24' 'USUBJID' 'TRTA'     AOCC01FL == 'Y' 'Weeks 0 to 24'
#> 
#> 
#>   Analysis parameter type:
#>      name                                label
#> 1   'rel'        'drug-related adverse events'
#> 2 'aeosi' 'adverse events of special interest'
#> 3   'any'                 'any adverse events'
#> 4   'ser'             'serious adverse events'
#>                                 subset
#> 1 AEREL %in% c('POSSIBLE', 'PROBABLE')
#> 2                         AEOSI == 'Y'
#> 3                                     
#> 4                         AESER == 'Y'
#> 
#> 
#>   Analysis function:
#>            name                           label
#> 1  'ae_summary'  'Table: adverse event summary'
#> 2 'ae_specific' 'Table: specific adverse event'
#> 
#> 
#> $`BLACK OR AFRICAN AMERICAN`
#> ADaM metadata: 
#>    .$data_population     Population data with 23 subjects 
#>    .$data_observation    Observation data with 90 records 
#>    .$plan    Analysis plan with 10 plans 
#> 
#> 
#>   Analysis population type:
#>     name        id  group var       subset                         label
#> 1 'apat' 'USUBJID' 'TRTA'     SAFFL == 'Y' 'All Participants as Treated'
#> 
#> 
#>   Analysis observation type:
#>     name        id  group var          subset           label
#> 1 'wk12' 'USUBJID' 'TRTA'        SAFFL == 'Y' 'Weeks 0 to 12'
#> 2 'wk24' 'USUBJID' 'TRTA'     AOCC01FL == 'Y' 'Weeks 0 to 24'
#> 
#> 
#>   Analysis parameter type:
#>      name                                label
#> 1   'rel'        'drug-related adverse events'
#> 2 'aeosi' 'adverse events of special interest'
#> 3   'any'                 'any adverse events'
#> 4   'ser'             'serious adverse events'
#>                                 subset
#> 1 AEREL %in% c('POSSIBLE', 'PROBABLE')
#> 2                         AEOSI == 'Y'
#> 3                                     
#> 4                         AESER == 'Y'
#> 
#> 
#>   Analysis function:
#>            name                           label
#> 1  'ae_summary'  'Table: adverse event summary'
#> 2 'ae_specific' 'Table: specific adverse event'
#> 
#> 
#> $`AMERICAN INDIAN OR ALASKA NATIVE`
#> ADaM metadata: 
#>    .$data_population     Population data with 1 subjects 
#>    .$data_observation    Observation data with 15 records 
#>    .$plan    Analysis plan with 10 plans 
#> 
#> 
#>   Analysis population type:
#>     name        id  group var       subset                         label
#> 1 'apat' 'USUBJID' 'TRTA'     SAFFL == 'Y' 'All Participants as Treated'
#> 
#> 
#>   Analysis observation type:
#>     name        id  group var          subset           label
#> 1 'wk12' 'USUBJID' 'TRTA'        SAFFL == 'Y' 'Weeks 0 to 12'
#> 2 'wk24' 'USUBJID' 'TRTA'     AOCC01FL == 'Y' 'Weeks 0 to 24'
#> 
#> 
#>   Analysis parameter type:
#>      name                                label
#> 1   'rel'        'drug-related adverse events'
#> 2 'aeosi' 'adverse events of special interest'
#> 3   'any'                 'any adverse events'
#> 4   'ser'             'serious adverse events'
#>                                 subset
#> 1 AEREL %in% c('POSSIBLE', 'PROBABLE')
#> 2                         AEOSI == 'Y'
#> 3                                     
#> 4                         AESER == 'Y'
#> 
#> 
#>   Analysis function:
#>            name                           label
#> 1  'ae_summary'  'Table: adverse event summary'
#> 2 'ae_specific' 'Table: specific adverse event'
#> 
#> 
```
