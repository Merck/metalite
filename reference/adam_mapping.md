# Construct ADaM mappings

ADaM mappings describe how variables and meta information in the ADaM
data are mapped to standardized term.

## Usage

``` r
adam_mapping(
  name,
  id = NULL,
  group = NULL,
  var = NULL,
  subset = NULL,
  label = NULL,
  ...
)
```

## Arguments

- name:

  A character value of term name. The term name is used as key to link
  information.

- id:

  A character value of subject identifier variable name in an ADaM
  dataset.

- group:

  A character vector of group variable names in an ADaM dataset.

- var:

  A character vector of useful variable names in an ADaM dataset.

- subset:

  An expression to identify analysis records. See
  [`base::subset()`](https://rdrr.io/r/base/subset.html).

- label:

  A character value of analysis label.

- ...:

  Additional variables.

## Value

A list with class `adam_mapping`. Components of the list are either
quosures or constants.

## Details

The design is inspired by
[`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).

## Examples

``` r
adam_mapping(
  name = "apat",
  id = "USUBJID",
  group = "TRT01A",
  subset = TRTFL == "Y",
  label = "All Participants as Treated"
)
#> ADaM mapping: 
#> * `name`   -> "apat"
#> * `id`     -> "USUBJID"
#> * `group`  -> "TRT01A"
#> * `var`    -> NULL
#> * `subset` -> TRTFL == "Y"
#> * `label`  -> "All Participants as Treated"
```
