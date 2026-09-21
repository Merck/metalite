# Specification for population definition

Specification for population definition

## Usage

``` r
spec_analysis_population(meta)
```

## Arguments

- meta:

  A `meta_adam` object.

## Value

A vector of character strings containing the populations used in the
order of the analysis plans.

## Examples

``` r
meta <- meta_example()
spec_analysis_population(meta)
#>  [1] "Population: SAFFL == 'Y'\nObservation: SAFFL == 'Y'"                                        
#>  [2] "Population: SAFFL == 'Y'\nObservation: AOCC01FL == 'Y'"                                     
#>  [3] "Population: SAFFL == 'Y'\nObservation: SAFFL == 'Y'"                                        
#>  [4] "Population: SAFFL == 'Y'\nObservation: AOCC01FL == 'Y'"                                     
#>  [5] "Population: SAFFL == 'Y'\nObservation: SAFFL == 'Y' AEOSI == 'Y'"                           
#>  [6] "Population: SAFFL == 'Y'\nObservation: AOCC01FL == 'Y' AEOSI == 'Y'"                        
#>  [7] "Population: SAFFL == 'Y'\nObservation: SAFFL == 'Y' AEREL %in% c('POSSIBLE', 'PROBABLE')"   
#>  [8] "Population: SAFFL == 'Y'\nObservation: AOCC01FL == 'Y' AEREL %in% c('POSSIBLE', 'PROBABLE')"
#>  [9] "Population: SAFFL == 'Y'\nObservation: SAFFL == 'Y' AESER == 'Y'"                           
#> [10] "Population: SAFFL == 'Y'\nObservation: AOCC01FL == 'Y' AESER == 'Y'"                        
```
