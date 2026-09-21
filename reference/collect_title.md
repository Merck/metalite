# Collect specification for title

Collect specification for title

## Usage

``` r
collect_title(
  meta,
  population,
  observation,
  parameter,
  analysis,
  title_order = c("analysis", "observation", "population")
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

- analysis:

  A character value of analysis term name. The term name is used as key
  to link information.

- title_order:

  A character vector to define the order of title from each component.

## Value

A vector of strings to compose the table captions.

## Examples

``` r
meta <- meta_example()
collect_title(meta, "apat", "wk12", "ser", "ae_summary")
#> [1] "Summary of Adverse Events"   "Weeks 0 to 12"              
#> [3] "All Participants as Treated"
collect_title(meta, "apat", "wk12", "ser", "ae_specific")
#> [1] "Participants With Serious Adverse Events"
#> [2] "Weeks 0 to 12"                           
#> [3] "All Participants as Treated"             
```
