# Create an example `meta_adam` object

This function is only for illustration purposes. The r2rtf package is
required.

## Usage

``` r
meta_example()
```

## Value

A metadata object.

## Examples

``` r
meta_example()
#> ADaM metadata: 
#>    .$data_population     Population data with 254 subjects 
#>    .$data_observation    Observation data with 1191 records 
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
```
