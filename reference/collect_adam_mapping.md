# Collect `adam_mapping` from `meta_adam` by `name`

Collect `adam_mapping` from `meta_adam` by `name`

## Usage

``` r
collect_adam_mapping(meta, name)
```

## Arguments

- meta:

  A `meta_adam` object.

- name:

  A keyword value.

## Value

An `adam_mapping` class object containing the definition of the search
variable in `name`.

## Examples

``` r
meta <- meta_example()
collect_adam_mapping(meta, "apat")
#> ADaM mapping: 
#> * `name`      -> "apat"
#> * `id`        -> "USUBJID"
#> * `group`     -> "TRTA"
#> * `var`       -> NULL
#> * `subset`    -> SAFFL == "Y"
#> * `label`     -> "All Participants as Treated"
#> * `.location` -> "population"
```
