# Collect observation record from observation dataset

The key variables used in `id`, `group`, and `subset` are displayed by
default.

## Usage

``` r
collect_observation_record(
  meta,
  population,
  observation,
  parameter,
  var = NULL
)
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

- var:

  A character vector of additional variables to be displayed in the
  output.

## Value

A data frame of the observation dataset.

## Examples

``` r
meta <- meta_example()
collect_observation_record(meta, "apat", "wk12", "ser")
#>          USUBJID                 TRTA
#> 689  01-709-1424 Xanomeline High Dose
#> 1131 01-718-1170  Xanomeline Low Dose
#> 1173 01-718-1371 Xanomeline High Dose
#>                                             AEDECOD SAFFL AESER
#> 689                                         SYNCOPE     Y     Y
#> 1131                                        SYNCOPE     Y     Y
#> 1173 PARTIAL SEIZURES WITH SECONDARY GENERALISATION     Y     Y
collect_observation_record(meta, "apat", "wk12", "ser", var = "AEDECOD")
#>          USUBJID                 TRTA
#> 689  01-709-1424 Xanomeline High Dose
#> 1131 01-718-1170  Xanomeline Low Dose
#> 1173 01-718-1371 Xanomeline High Dose
#>                                             AEDECOD SAFFL AESER
#> 689                                         SYNCOPE     Y     Y
#> 1131                                        SYNCOPE     Y     Y
#> 1173 PARTIAL SEIZURES WITH SECONDARY GENERALISATION     Y     Y
```
