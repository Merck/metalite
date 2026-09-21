# Collect specification for dataset name

Collect specification for dataset name

## Usage

``` r
collect_dataname(meta)
```

## Arguments

- meta:

  A `meta_adam` object.

## Value

A vector of character strings containing the name of the
population/observation.

## Examples

``` r
meta <- meta_example()
collect_dataname(meta)
#>  population observation 
#>      "adsl"      "adae" 
```
