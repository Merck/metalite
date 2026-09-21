# Print a metadata object with its population, observation, and analysis plans

Print a metadata object with its population, observation, and analysis
plans

## Usage

``` r
# S3 method for class 'meta_adam'
print(x, ...)
```

## Arguments

- x:

  An object returned by
  [`meta_adam()`](https://merck.github.io/metalite/reference/meta_adam.md).

- ...:

  Additional parameters for
  [`print()`](https://rdrr.io/r/base/print.html) (not used).

## Value

A printed summary of the metadata.

## Examples

``` r
meta_adam(observation = r2rtf::r2rtf_adae, population = r2rtf::r2rtf_adae) |> print()
#> ADaM metadata: 
#>    .$data_population     Population data with 1191 subjects 
#>    .$data_observation    Observation data with 1191 records 
#> 
#> 
```
