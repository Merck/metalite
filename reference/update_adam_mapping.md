# Update mapping rule in `adam_mapping`

Update mapping rule in `adam_mapping`

## Usage

``` r
update_adam_mapping(meta, name, ...)
```

## Arguments

- meta:

  A `meta_adam` object.

- name:

  A vector of keywords.

- ...:

  Additional variables to be added in the mapping rule among those
  keywords.

## Value

A metadata object with the input updated.

## Examples

``` r
meta <- meta_example()
meta <- update_adam_mapping(meta, names(meta$parameter), start_date = "ASTDT")
collect_adam_mapping(meta, "ser")
#> ADaM mapping: 
#> * `name`       -> "ser"
#> * `id`         -> NULL
#> * `group`      -> NULL
#> * `var`        -> "AEDECOD"
#> * `subset`     -> AESER == "Y"
#> * `label`      -> "serious adverse events"
#> * `soc`        -> "AEBODSYS"
#> * `seq`        -> 401
#> * `term1`      -> "Serious"
#> * `term2`      -> ""
#> * `summ_row`   -> "with serious adverse events"
#> * `.location`  -> "parameter"
#> * `start_date` -> "ASTDT"
```
