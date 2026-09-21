# Specification for analysis call program

Specification for analysis call program

## Usage

``` r
spec_call_program(meta, ...)
```

## Arguments

- meta:

  A `meta_adam` object.

- ...:

  Additional arguments used in all call programs.

## Value

A vector of character strings containing the call program in the order
of the analysis plans.

## Examples

``` r
meta <- meta_example()
spec_call_program(meta)
#>  [1] "ae_summary(meta = meta, population = 'apat', observation = 'wk12', parameter = 'any;rel;ser')"
#>  [2] "ae_summary(meta = meta, population = 'apat', observation = 'wk24', parameter = 'any;rel;ser')"
#>  [3] "ae_specific(meta = meta, population = 'apat', observation = 'wk12', parameter = 'any')"       
#>  [4] "ae_specific(meta = meta, population = 'apat', observation = 'wk24', parameter = 'any')"       
#>  [5] "ae_specific(meta = meta, population = 'apat', observation = 'wk12', parameter = 'aeosi')"     
#>  [6] "ae_specific(meta = meta, population = 'apat', observation = 'wk24', parameter = 'aeosi')"     
#>  [7] "ae_specific(meta = meta, population = 'apat', observation = 'wk12', parameter = 'rel')"       
#>  [8] "ae_specific(meta = meta, population = 'apat', observation = 'wk24', parameter = 'rel')"       
#>  [9] "ae_specific(meta = meta, population = 'apat', observation = 'wk12', parameter = 'ser')"       
#> [10] "ae_specific(meta = meta, population = 'apat', observation = 'wk24', parameter = 'ser')"       
spec_call_program(meta, data_source = "[Study CDISCpilot: adam-adsl; adae]")
#>  [1] "ae_summary(meta = meta, population = 'apat', observation = 'wk12', parameter = 'any;rel;ser', data_source = '[Study CDISCpilot: adam-adsl; adae]')"
#>  [2] "ae_summary(meta = meta, population = 'apat', observation = 'wk24', parameter = 'any;rel;ser', data_source = '[Study CDISCpilot: adam-adsl; adae]')"
#>  [3] "ae_specific(meta = meta, population = 'apat', observation = 'wk12', parameter = 'any', data_source = '[Study CDISCpilot: adam-adsl; adae]')"       
#>  [4] "ae_specific(meta = meta, population = 'apat', observation = 'wk24', parameter = 'any', data_source = '[Study CDISCpilot: adam-adsl; adae]')"       
#>  [5] "ae_specific(meta = meta, population = 'apat', observation = 'wk12', parameter = 'aeosi', data_source = '[Study CDISCpilot: adam-adsl; adae]')"     
#>  [6] "ae_specific(meta = meta, population = 'apat', observation = 'wk24', parameter = 'aeosi', data_source = '[Study CDISCpilot: adam-adsl; adae]')"     
#>  [7] "ae_specific(meta = meta, population = 'apat', observation = 'wk12', parameter = 'rel', data_source = '[Study CDISCpilot: adam-adsl; adae]')"       
#>  [8] "ae_specific(meta = meta, population = 'apat', observation = 'wk24', parameter = 'rel', data_source = '[Study CDISCpilot: adam-adsl; adae]')"       
#>  [9] "ae_specific(meta = meta, population = 'apat', observation = 'wk12', parameter = 'ser', data_source = '[Study CDISCpilot: adam-adsl; adae]')"       
#> [10] "ae_specific(meta = meta, population = 'apat', observation = 'wk24', parameter = 'ser', data_source = '[Study CDISCpilot: adam-adsl; adae]')"       
```
