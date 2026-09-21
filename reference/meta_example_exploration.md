# Create a data exploration `meta_adam` object

Create a data exploration `meta_adam` object

## Usage

``` r
meta_example_exploration(
  data,
  group,
  name = "ase",
  subset = NULL,
  label = "All Subjects Enrolled"
)
```

## Arguments

- data:

  A data frame.

- group:

  A character vector of group variable names in an ADaM dataset.

- name:

  A character value of term name. The term name is used as key to link
  information.

- subset:

  An expression to identify analysis records. See
  [`base::subset()`](https://rdrr.io/r/base/subset.html).

- label:

  A character value of analysis label.

## Value

A metadata object.

## Examples

``` r
meta <- meta_example_exploration(r2rtf::r2rtf_adsl, group = "TRT01A")
collect_n_subject(meta, "ase", "AGE")
#> $table
#>                 name           Placebo             Total Xanomeline High Dose
#> 1 Number of Subjects                86               254                   84
#> 2                Age              <NA>              <NA>                 <NA>
#> 3 Subjects with Data                86               254                   84
#> 4          Mean (SD)        75.2 (8.6)        75.1 (8.2)           74.4 (7.9)
#> 5  Median [Min, Max] 76.0 [52.0, 89.0] 77.0 [51.0, 89.0]    76.0 [56.0, 88.0]
#> 6           Q1 to Q3      69.0 to 82.0      70.0 to 81.0         70.5 to 80.0
#>   Xanomeline Low Dose
#> 1                  84
#> 2                <NA>
#> 3                  84
#> 4          75.7 (8.3)
#> 5   77.5 [51.0, 88.0]
#> 6        71.0 to 82.0
#> 
#> $n
#>                 name Placebo Total Xanomeline High Dose Xanomeline Low Dose
#> 1 Number of Subjects      86   254                   84                  84
#> 
#> $subset
#>                 name                                     Placebo
#> 1 Number of Subjects           TRUE & TRT01A == 'Placebo' & NULL
#> 2 Subjects with Data (! is.na(AGE)) & TRT01A == 'Placebo' & NULL
#> 3               <NA>     is.na(AGE) & TRT01A == 'Placebo' & NULL
#>                                       Xanomeline High Dose
#> 1           TRUE & TRT01A == 'Xanomeline High Dose' & NULL
#> 2 (! is.na(AGE)) & TRT01A == 'Xanomeline High Dose' & NULL
#> 3     is.na(AGE) & TRT01A == 'Xanomeline High Dose' & NULL
#>                                       Xanomeline Low Dose
#> 1           TRUE & TRT01A == 'Xanomeline Low Dose' & NULL
#> 2 (! is.na(AGE)) & TRT01A == 'Xanomeline Low Dose' & NULL
#> 3     is.na(AGE) & TRT01A == 'Xanomeline Low Dose' & NULL
#> 
#> $listing
#> NULL
#> 
#> $histogram
#> NULL
#> 
collect_n_subject(meta, "ase", "SEX")
#> $table
#>                 name     Placebo        Total Xanomeline High Dose
#> 1 Number of Subjects          86          254                   84
#> 2                Sex        <NA>         <NA>                 <NA>
#> 3 Subjects with Data          86          254                   84
#> 4                  F 53 ( 61.6%) 143 ( 56.3%)          40 ( 47.6%)
#> 5                  M 33 ( 38.4%) 111 ( 43.7%)          44 ( 52.4%)
#>   Xanomeline Low Dose
#> 1                  84
#> 2                <NA>
#> 3                  84
#> 4         50 ( 59.5%)
#> 5         34 ( 40.5%)
#> 
#> $n
#>                 name Placebo Total Xanomeline High Dose Xanomeline Low Dose
#> 1 Number of Subjects      86   254                   84                  84
#> 
#> $subset
#>                 name                                     Placebo
#> 1 Number of Subjects           TRUE & TRT01A == 'Placebo' & NULL
#> 2 Subjects with Data (! is.na(SEX)) & TRT01A == 'Placebo' & NULL
#> 3               <NA>     is.na(SEX) & TRT01A == 'Placebo' & NULL
#> 4                  F     SEX == 'F' & TRT01A == 'Placebo' & NULL
#> 5                  M     SEX == 'M' & TRT01A == 'Placebo' & NULL
#>                                       Xanomeline High Dose
#> 1           TRUE & TRT01A == 'Xanomeline High Dose' & NULL
#> 2 (! is.na(SEX)) & TRT01A == 'Xanomeline High Dose' & NULL
#> 3     is.na(SEX) & TRT01A == 'Xanomeline High Dose' & NULL
#> 4     SEX == 'F' & TRT01A == 'Xanomeline High Dose' & NULL
#> 5     SEX == 'M' & TRT01A == 'Xanomeline High Dose' & NULL
#>                                       Xanomeline Low Dose
#> 1           TRUE & TRT01A == 'Xanomeline Low Dose' & NULL
#> 2 (! is.na(SEX)) & TRT01A == 'Xanomeline Low Dose' & NULL
#> 3     is.na(SEX) & TRT01A == 'Xanomeline Low Dose' & NULL
#> 4     SEX == 'F' & TRT01A == 'Xanomeline Low Dose' & NULL
#> 5     SEX == 'M' & TRT01A == 'Xanomeline Low Dose' & NULL
#> 
#> $listing
#> NULL
#> 
#> $histogram
#> NULL
#> 
```
