# Inherit meta information by keywords

Inherit meta information by keywords

## Usage

``` r
meta_inherit(meta, inherit, name, overwrite = FALSE)
```

## Arguments

- meta:

  A `meta_adam` object.

- inherit:

  A `meta_adam` object to be inherit.

- name:

  A vector of keywords from `meta_inherit` to `meta_adam`.

- overwrite:

  A logical value to force mapping update.

## Value

A metadata object with population defined.

## Examples

``` r
meta_adam(
  population = r2rtf::r2rtf_adsl,
  observation = r2rtf::r2rtf_adae
) |>
  meta_inherit(meta_example(), c("apat", "wk12", "ae_summary"))
#> ADaM metadata: 
#>    .$data_population     Population data with 254 subjects 
#>    .$data_observation    Observation data with 1191 records 
#> 
#> 
#>   Analysis population type:
#>     name        id  group var       subset                         label
#> 1 'apat' 'USUBJID' 'TRTA'     SAFFL == 'Y' 'All Participants as Treated'
#>      .location
#> 1 'population'
#> 
#> 
#>   Analysis observation type:
#>     name        id  group var       subset           label     .location
#> 1 'wk12' 'USUBJID' 'TRTA'     SAFFL == 'Y' 'Weeks 0 to 12' 'observation'
#> 
#> 
#>   Analysis function:
#>           name                          label
#> 1 'ae_summary' 'Table: adverse event summary'
#> 
```
