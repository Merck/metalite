# Create a metadata representation for ADaM data analysis

Create a metadata representation for ADaM data analysis

## Usage

``` r
meta_adam(observation, population = observation)
```

## Arguments

- observation:

  A data frame for observation level data.

- population:

  A data frame for population level data. Default is the same as
  `observation`.

## Value

An initialized metadata object with observation and population defined.

## Examples

``` r
meta_adam(observation = r2rtf::r2rtf_adae, population = r2rtf::r2rtf_adae)
#> ADaM metadata: 
#>    .$data_population     Population data with 1191 subjects 
#>    .$data_observation    Observation data with 1191 records 
#> 
#> 
```
