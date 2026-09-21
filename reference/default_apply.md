# Apply default values to ADaM mappings

Apply default values to ADaM mappings

## Usage

``` r
default_apply(x)
```

## Arguments

- x:

  An `adam_mapping` object.

## Value

Similar to the input, but with the missing values updated to the default
values.

## Examples

``` r
default_apply(adam_mapping(name = "apat"))
#> ADaM mapping: 
#> * `name`   -> "apat"
#> * `id`     -> "USUBJID"
#> * `group`  -> NULL
#> * `var`    -> NULL
#> * `subset` -> NULL
#> * `label`  -> "All Participants as Treated"
```
