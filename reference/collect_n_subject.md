# Collect number of subjects and its subset condition

Collect number of subjects and its subset condition

## Usage

``` r
collect_n_subject(
  meta,
  population,
  parameter,
  listing = FALSE,
  histogram = FALSE,
  var_listing = NULL,
  remove_blank_group = FALSE,
  type = "Subjects",
  use_na = c("ifany", "no", "always"),
  display_total = TRUE,
  quantile_method = 2,
  decimal_places_summary = 1,
  decimal_places_percent = 1
)
```

## Arguments

- meta:

  A `meta_adam` object.

- population:

  A character value of population term name. The term name is used as
  key to link information.

- parameter:

  A character value of parameter term name. The term name is used as key
  to link information.

- listing:

  A logical value to display drill down listing per row.

- histogram:

  A logical value to display histogram by group.

- var_listing:

  A character vector of additional variables included in the listing.

- remove_blank_group:

  A logical value to remove a group with all missing value of a
  parameter.

- type:

  A character value to control title name, e.g., Subjects or Records.

- use_na:

  A character value for whether to include `NA` values in the table. See
  the `useNA` argument in
  [`base::table()`](https://rdrr.io/r/base/table.html) for more details.

- display_total:

  A logical value to display total column.

- quantile_method:

  An integer between 1 and 9 selecting one of the nine quantile
  algorithms to be used for `type` argument in
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html). Default
  is 2 as the same percentile definition used by the UNIVARIATE
  procedure in SAS.

- decimal_places_summary:

  Number of decimal places to be displayed in statistical summary values
  (Mean, SD, Median, Min, Max, Q1 and Q3). Default is 1.

- decimal_places_percent:

  Number of decimal places to be displayed in percentage values. Default
  is 1.

## Value

A list containing number of subjects and its subset condition.

## Details

Summary statistics and percentages are calculated without rounding, then
rounded once at the display boundary with
[`round_half_away_from_zero()`](https://merck.github.io/metalite/reference/round_half_away_from_zero.md).
Decimal ties are rounded away from zero and trailing zeros are retained.

## Examples

``` r
suppressWarnings(
  meta <- meta_example() |>
    define_parameter(name = "sex", var = "SEX", label = "Sex")
)
collect_n_subject(meta, "apat", "sex")
#> $table
#>                 name     Placebo Xanomeline Low Dose Xanomeline High Dose
#> 1 Number of Subjects          86                  84                   84
#> 2                Sex        <NA>                <NA>                 <NA>
#> 3 Subjects with Data          86                  84                   84
#> 4                  F 53 ( 61.6%)         50 ( 59.5%)          40 ( 47.6%)
#> 5                  M 33 ( 38.4%)         34 ( 40.5%)          44 ( 52.4%)
#>          Total
#> 1          254
#> 2         <NA>
#> 3          254
#> 4 143 ( 56.3%)
#> 5 111 ( 43.7%)
#> 
#> $n
#>                 name Placebo Xanomeline Low Dose Xanomeline High Dose Total
#> 1 Number of Subjects      86                  84                   84   254
#> 
#> $subset
#>                 name                                           Placebo
#> 1 Number of Subjects           TRUE & TRTA == 'Placebo' & SAFFL == 'Y'
#> 2 Subjects with Data (! is.na(SEX)) & TRTA == 'Placebo' & SAFFL == 'Y'
#> 3               <NA>     is.na(SEX) & TRTA == 'Placebo' & SAFFL == 'Y'
#> 4                  F     SEX == 'F' & TRTA == 'Placebo' & SAFFL == 'Y'
#> 5                  M     SEX == 'M' & TRTA == 'Placebo' & SAFFL == 'Y'
#>                                             Xanomeline Low Dose
#> 1           TRUE & TRTA == 'Xanomeline Low Dose' & SAFFL == 'Y'
#> 2 (! is.na(SEX)) & TRTA == 'Xanomeline Low Dose' & SAFFL == 'Y'
#> 3     is.na(SEX) & TRTA == 'Xanomeline Low Dose' & SAFFL == 'Y'
#> 4     SEX == 'F' & TRTA == 'Xanomeline Low Dose' & SAFFL == 'Y'
#> 5     SEX == 'M' & TRTA == 'Xanomeline Low Dose' & SAFFL == 'Y'
#>                                             Xanomeline High Dose
#> 1           TRUE & TRTA == 'Xanomeline High Dose' & SAFFL == 'Y'
#> 2 (! is.na(SEX)) & TRTA == 'Xanomeline High Dose' & SAFFL == 'Y'
#> 3     is.na(SEX) & TRTA == 'Xanomeline High Dose' & SAFFL == 'Y'
#> 4     SEX == 'F' & TRTA == 'Xanomeline High Dose' & SAFFL == 'Y'
#> 5     SEX == 'M' & TRTA == 'Xanomeline High Dose' & SAFFL == 'Y'
#> 
#> $listing
#> NULL
#> 
#> $histogram
#> NULL
#> 
```
