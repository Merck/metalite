# Count number of unique subjects

Count number of unique subjects

## Usage

``` r
n_subject(
  id,
  group,
  par = NULL,
  na = "Missing",
  use_na = c("ifany", "no", "always")
)
```

## Arguments

- id:

  A character vector of subject identifier.

- group:

  A factor vector of group name.

- par:

  A character vector of parameter name.

- na:

  A character string used to label missing values. Defaults to
  `"Missing"`.

- use_na:

  A character value for whether to include `NA` values in the table. See
  the `useNA` argument in
  [`base::table()`](https://rdrr.io/r/base/table.html) for more details.

## Value

A data frame summarizing the number of unique subjects in different
arms.

## Examples

``` r
library(r2rtf)

r2rtf_adae$TRTA <- factor(r2rtf_adae$TRTA)
r2rtf_adae$SEX[1:5] <- NA

n_subject(r2rtf_adae$USUBJID, r2rtf_adae$TRTA)
#>   Placebo Xanomeline High Dose Xanomeline Low Dose
#> 1      69                   79                  77
n_subject(r2rtf_adae$USUBJID, r2rtf_adae$TRTA, r2rtf_adae$SEX)
#>      name Placebo Xanomeline High Dose Xanomeline Low Dose
#> 1       F      39                   37                  44
#> 2       M      29                   42                  33
#> 3 Missing       2                    0                   0
n_subject(r2rtf_adae$USUBJID, r2rtf_adae$TRTA, r2rtf_adae$SEX, use_na = "always")
#>      name Placebo Xanomeline High Dose Xanomeline Low Dose Missing
#> 1       F      39                   37                  44       0
#> 2       M      29                   42                  33       0
#> 3 Missing       2                    0                   0       0
n_subject(r2rtf_adae$USUBJID, r2rtf_adae$TRTA, r2rtf_adae$SEX, na = "Null")
#>   name Placebo Xanomeline High Dose Xanomeline Low Dose
#> 1    F      39                   37                  44
#> 2    M      29                   42                  33
#> 3 Null       2                    0                   0
```
