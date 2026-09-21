# Collect observation record index from observation dataset

Collect observation record index from observation dataset

## Usage

``` r
collect_observation_index(meta, population, observation, parameter)
```

## Arguments

- meta:

  A `meta_adam` object.

- population:

  A character value of population term name. The term name is used as
  key to link information.

- observation:

  A character value of observation term name. The term name is used as
  key to link information.

- parameter:

  A character value of parameter term name. The term name is used as key
  to link information.

## Value

A vector of patient index within the observation group.

## Examples

``` r
meta <- meta_example()
collect_observation_index(meta, "apat", "wk12", "ser")
#> [1]  689 1131 1173
```
