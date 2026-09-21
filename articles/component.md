# Collect Components

``` r

library(metalite)
```

In this document, let’s explore how to collect components using
metalite. The document is for developer who plan to create new tools.

Let’s first define a meta object by using the
[`meta_example()`](https://merck.github.io/metalite/reference/meta_example.md)
function. It is an ad-hoc function to create a meta object same as in
the `get started` page.

``` r

meta <- meta_example()
meta
#> ADaM metadata: 
#>    .$data_population     Population data with 254 subjects 
#>    .$data_observation    Observation data with 1191 records 
#>    .$plan    Analysis plan with 10 plans 
#> 
#> 
#>   Analysis population type:
#>     name        id  group var       subset                         label
#> 1 'apat' 'USUBJID' 'TRTA'     SAFFL == 'Y' 'All Participants as Treated'
#> 
#> 
#>   Analysis observation type:
#>     name        id  group var          subset           label
#> 1 'wk12' 'USUBJID' 'TRTA'        SAFFL == 'Y' 'Weeks 0 to 12'
#> 2 'wk24' 'USUBJID' 'TRTA'     AOCC01FL == 'Y' 'Weeks 0 to 24'
#> 
#> 
#>   Analysis parameter type:
#>      name                                label
#> 1   'rel'        'drug-related adverse events'
#> 2 'aeosi' 'adverse events of special interest'
#> 3   'any'                 'any adverse events'
#> 4   'ser'             'serious adverse events'
#>                                 subset
#> 1 AEREL %in% c('POSSIBLE', 'PROBABLE')
#> 2                         AEOSI == 'Y'
#> 3                                     
#> 4                         AESER == 'Y'
#> 
#> 
#>   Analysis function:
#>            name                           label
#> 1  'ae_summary'  'Table: adverse event summary'
#> 2 'ae_specific' 'Table: specific adverse event'
```

## Collect ADaM mapping

The object in `meta` is organized as a list of ADaM mapping objects. For
example, you can directly access an ADaM mapping object as a list.

``` r

meta$population$apat
#> ADaM mapping: 
#> * `name`   -> "apat"
#> * `id`     -> "USUBJID"
#> * `group`  -> "TRTA"
#> * `var`    -> NULL
#> * `subset` -> SAFFL == "Y"
#> * `label`  -> "All Participants as Treated"
```

We created helper function to help you access components by component
name. For example, we can collect the same `apat` component from `meta`
by `name` using
[`collect_adam_mapping()`](https://merck.github.io/metalite/reference/collect_adam_mapping.md).

Collect ADaM mapping for all participants as treated.

``` r

collect_adam_mapping(meta, name = "apat")
#> ADaM mapping: 
#> * `name`      -> "apat"
#> * `id`        -> "USUBJID"
#> * `group`     -> "TRTA"
#> * `var`       -> NULL
#> * `subset`    -> SAFFL == "Y"
#> * `label`     -> "All Participants as Treated"
#> * `.location` -> "population"
```

We can collect ADaM mapping for serious adverse events parameter
`meta$parameter$ser`.

``` r

collect_adam_mapping(meta, name = "ser")
#> ADaM mapping: 
#> * `name`      -> "ser"
#> * `id`        -> NULL
#> * `group`     -> NULL
#> * `var`       -> "AEDECOD"
#> * `subset`    -> AESER == "Y"
#> * `label`     -> "serious adverse events"
#> * `soc`       -> "AEBODSYS"
#> * `seq`       -> 401
#> * `term1`     -> "Serious"
#> * `term2`     -> ""
#> * `summ_row`  -> "with serious adverse events"
#> * `.location` -> "parameter"
```

We can collect ADaM mapping for AE summary analysis method
`meta$analysis$ae_summary`.

``` r

collect_adam_mapping(meta, name = "ae_summary")
#> ADaM mapping: 
#> * `name`      -> "ae_summary"
#> * `id`        -> NULL
#> * `group`     -> NULL
#> * `var`       -> NULL
#> * `subset`    -> NULL
#> * `label`     -> "Table: adverse event summary"
#> * `title`     -> "Summary of Adverse Events"
#> * `.location` -> "analysis"
```

## Collect population

While developing tools, developer also need to access the subset
condition of a population. We can access the information from the list
as

``` r

meta$population$apat$subset
#> SAFFL == "Y"
```

Equivalently, we can also use
[`collect_population()`](https://merck.github.io/metalite/reference/collect_population.md)
to collect the definition of a population. For example, we can collect
subset condition for `apat` as

``` r

collect_population(meta, population = "apat")
#> $population
#> [1] "SAFFL == 'Y'"
```

By using
[`collect_population()`](https://merck.github.io/metalite/reference/collect_population.md),
we can find subset condition from multiple levels. For example, we can
collect analysis population definition for `apat` population, `wk12`
observation and `ser` parameters.

``` r

collect_population(meta,
  population = "apat",
  observation = "wk12",
  parameter = "ser"
)
#> $population
#> [1] "SAFFL == 'Y'"
#> 
#> $observation
#> [1] "SAFFL == 'Y'"
#> 
#> $parameter
#> [1] "AESER == 'Y'"
```

## Collect population records

User may want to identify records belong to an analysis population. We
can use
[`collect_population_index()`](https://merck.github.io/metalite/reference/collect_population_index.md)
to show all population record index. In this example, user would get
index from all population data set for `apat` or all participants as
treated.

``` r

population_index <- collect_population_index(meta, "apat")
```

``` r

head(collect_population_index(meta, "apat"))
#> [1] 1 2 3 4 5 6
```

Alternatively, people may want to know the ID of those subjects in a
population. In this case,
[`collect_population_id()`](https://merck.github.io/metalite/reference/collect_population_id.md)
can be used to collect `ID - > USUBJID` from the `apat` population.

``` r

population_id <- collect_population_id(meta, "apat")
```

``` r

head(collect_population_id(meta, "apat"))
#> [1] "01-701-1015" "01-701-1023" "01-701-1028" "01-701-1033" "01-701-1034"
#> [6] "01-701-1047"
```

We can further directly collect observations using
[`collect_population_record()`](https://merck.github.io/metalite/reference/collect_population_record.md).
By default, key variables used in `id`, `group`, and `subset` are
displayed.

This example shows how to collect population record from population data
set for all participants as treated and only display default variables.

``` r

head(collect_population_record(meta, "apat"))
#>       USUBJID                 TRTA SAFFL
#> 1 01-701-1015              Placebo     Y
#> 2 01-701-1023              Placebo     Y
#> 3 01-701-1028 Xanomeline High Dose     Y
#> 4 01-701-1033  Xanomeline Low Dose     Y
#> 5 01-701-1034 Xanomeline High Dose     Y
#> 6 01-701-1047              Placebo     Y
```

This example show how to add variables in population data set to
display.

``` r

head(collect_population_record(meta, "apat", var = "AGE"))
#>       USUBJID                 TRTA SAFFL AGE
#> 1 01-701-1015              Placebo     Y  63
#> 2 01-701-1023              Placebo     Y  64
#> 3 01-701-1028 Xanomeline High Dose     Y  71
#> 4 01-701-1033  Xanomeline Low Dose     Y  74
#> 5 01-701-1034 Xanomeline High Dose     Y  77
#> 6 01-701-1047              Placebo     Y  85
```

We can also add add multiple additional variable to display

``` r

head(collect_population_record(meta, "apat", var = c("AGE", "TRT01P")))
#>       USUBJID                 TRTA SAFFL AGE               TRT01P
#> 1 01-701-1015              Placebo     Y  63              Placebo
#> 2 01-701-1023              Placebo     Y  64              Placebo
#> 3 01-701-1028 Xanomeline High Dose     Y  71 Xanomeline High Dose
#> 4 01-701-1033  Xanomeline Low Dose     Y  74  Xanomeline Low Dose
#> 5 01-701-1034 Xanomeline High Dose     Y  77 Xanomeline High Dose
#> 6 01-701-1047              Placebo     Y  85              Placebo
```

## Collect observation record

Similarly we can collect observation records with examples below. This
example shows how to collect observation record index from observation
data set for serious AE from weeks 0 to 12 using
[`collect_observation_index()`](https://merck.github.io/metalite/reference/collect_observation_index.md)

``` r

collect_observation_index(meta, "apat", "wk12", "ser")
#> [1]  689 1131 1173
```

We can also directly collect records using
[`collect_observation_record()`](https://merck.github.io/metalite/reference/collect_observation_record.md).
By default, key variables used in `id`, `group`, and `subset` are
displayed.

This example shows how to collect observation record from observation
data set for all participants as treated from 0 to 12 week with serious
AE and only display default variables.

``` r

collect_observation_record(meta, "apat", "wk12", "ser")
#>          USUBJID                 TRTA
#> 689  01-709-1424 Xanomeline High Dose
#> 1131 01-718-1170  Xanomeline Low Dose
#> 1173 01-718-1371 Xanomeline High Dose
#>                                             AEDECOD SAFFL AESER
#> 689                                         SYNCOPE     Y     Y
#> 1131                                        SYNCOPE     Y     Y
#> 1173 PARTIAL SEIZURES WITH SECONDARY GENERALISATION     Y     Y
```

This example show how to add variables in observation data set to
display.

``` r

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

## Collect specifications

We also provided helper functions to collect commonly used items

Developer can collect table title for analysis function meta information
using
[`collect_title()`](https://merck.github.io/metalite/reference/collect_title.md).

``` r

collect_title(meta, "apat", "wk12", "ser", "ae_summary")
#> [1] "Summary of Adverse Events"   "Weeks 0 to 12"              
#> [3] "All Participants as Treated"
```

Developer can collect specification for data set name using
[`collect_dataname()`](https://merck.github.io/metalite/reference/collect_dataname.md).
It will show the data set name for population and observation.

``` r

collect_dataname(meta)
#>  population observation 
#>      "adsl"      "adae"
```
