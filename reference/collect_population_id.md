# Collect subject identifier information from population dataset

Collect subject identifier information from population dataset

## Usage

``` r
collect_population_id(meta, population)
```

## Arguments

- meta:

  A `meta_adam` object.

- population:

  A character value of population term name. The term name is used as
  key to link information.

## Value

A vector of patient ID within the population group.

## Examples

``` r
meta <- meta_example()
head(collect_population_id(meta, "apat"))
#> [1] "01-701-1015" "01-701-1023" "01-701-1028" "01-701-1033" "01-701-1034"
#> [6] "01-701-1047"
```
