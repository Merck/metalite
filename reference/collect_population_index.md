# Collect population record index from population dataset

Collect population record index from population dataset

## Usage

``` r
collect_population_index(meta, population)
```

## Arguments

- meta:

  A `meta_adam` object.

- population:

  A character value of population term name. The term name is used as
  key to link information.

## Value

A vector of patient index within the population group.

## Examples

``` r
meta <- meta_example()
head(collect_population_index(meta, "apat"))
#> [1] 1 2 3 4 5 6
```
