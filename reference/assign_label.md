# A function to assign labels to a data frame

A function to assign labels to a data frame

## Usage

``` r
assign_label(data, var = names(data), label = names(data))
```

## Arguments

- data:

  A data frame.

- var:

  The variables to assign labels.

- label:

  The labels to be assigned.

## Value

A data frame with labels updated.

## Details

- Case 1: If the variable's label is already define in the original data
  frame but not redefined in `assign_label(...)`, its original labels
  will be kept.

- Case 2: If the variable's label is already define in the original data
  frame but re-defined by `assign_label(...)`, its labels will be
  re-defined.

- Case 3: If the variable's label is not define in the original data
  frame but it is defined by `assign_label(...)`, its labels will added.

- Case 4: If the variable's label is not define in the original data
  frame, neither was it defined by `assign_label(...)`, its labels will
  be the variable name itself.

## Examples

``` r
assign_label(r2rtf::r2rtf_adae) |> head()
#>        STUDYID SITEID     USUBJID    TRTA TRTAN AGE AGEGR1 AGEGR1N  RACE RACEN
#> 1 CDISCPILOT01    701 01-701-1015 Placebo     0  63    <65       1 WHITE     1
#> 2 CDISCPILOT01    701 01-701-1015 Placebo     0  63    <65       1 WHITE     1
#> 3 CDISCPILOT01    701 01-701-1015 Placebo     0  63    <65       1 WHITE     1
#> 4 CDISCPILOT01    701 01-701-1023 Placebo     0  64    <65       1 WHITE     1
#> 5 CDISCPILOT01    701 01-701-1023 Placebo     0  64    <65       1 WHITE     1
#> 6 CDISCPILOT01    701 01-701-1023 Placebo     0  64    <65       1 WHITE     1
#>   SEX SAFFL     TRTSDT     TRTEDT      ASTDT ASTDTF ASTDY      AENDT AENDY
#> 1   F     Y 2014-01-02 2014-07-02 2014-01-03            2       <NA>    NA
#> 2   F     Y 2014-01-02 2014-07-02 2014-01-03            2       <NA>    NA
#> 3   F     Y 2014-01-02 2014-07-02 2014-01-09            8 2014-01-11    10
#> 4   M     Y 2012-08-05 2012-09-01 2012-08-07            3 2012-08-30    26
#> 5   M     Y 2012-08-05 2012-09-01 2012-08-07            3       <NA>    NA
#> 6   M     Y 2012-08-05 2012-09-01 2012-08-26           22       <NA>    NA
#>   ADURN ADURU                               AETERM                    AELLT
#> 1    NA                  APPLICATION SITE ERYTHEMA APPLICATION SITE REDNESS
#> 2    NA                  APPLICATION SITE PRURITUS APPLICATION SITE ITCHING
#> 3     3   DAY                            DIARRHOEA                 DIARRHEA
#> 4    24   DAY                             ERYTHEMA                 ERYTHEMA
#> 5    NA                                   ERYTHEMA       LOCALIZED ERYTHEMA
#> 6    NA       ATRIOVENTRICULAR BLOCK SECOND DEGREE   AV BLOCK SECOND DEGREE
#>   AELLTCD                              AEDECOD AEPTCD    AEHLT AEHLTCD
#> 1      NA            APPLICATION SITE ERYTHEMA     NA HLT_0617      NA
#> 2      NA            APPLICATION SITE PRURITUS     NA HLT_0317      NA
#> 3      NA                            DIARRHOEA     NA HLT_0148      NA
#> 4      NA                             ERYTHEMA     NA HLT_0284      NA
#> 5      NA                             ERYTHEMA     NA HLT_0284      NA
#> 6      NA ATRIOVENTRICULAR BLOCK SECOND DEGREE     NA HLT_0415      NA
#>      AEHLGT AEHLGTCD                                             AEBODSYS
#> 1 HLGT_0152       NA GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS
#> 2 HLGT_0338       NA GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS
#> 3 HLGT_0588       NA                           GASTROINTESTINAL DISORDERS
#> 4 HLGT_0192       NA               SKIN AND SUBCUTANEOUS TISSUE DISORDERS
#> 5 HLGT_0192       NA               SKIN AND SUBCUTANEOUS TISSUE DISORDERS
#> 6 HLGT_0086       NA                                    CARDIAC DISORDERS
#>                                                  AESOC AESOCCD    AESEV AESER
#> 1 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS      NA     MILD     N
#> 2 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS      NA     MILD     N
#> 3                           GASTROINTESTINAL DISORDERS      NA     MILD     N
#> 4               SKIN AND SUBCUTANEOUS TISSUE DISORDERS      NA     MILD     N
#> 5               SKIN AND SUBCUTANEOUS TISSUE DISORDERS      NA MODERATE     N
#> 6                                    CARDIAC DISORDERS      NA     MILD     N
#>   AESCAN AESCONG AESDISAB AESDTH AESHOSP AESLIFE AESOD    AEREL AEACN
#> 1      N       N        N      N       N       N     N PROBABLE      
#> 2      N       N        N      N       N       N     N PROBABLE      
#> 3      N       N        N      N       N       N     N   REMOTE      
#> 4      N       N        N      N       N       N     N POSSIBLE      
#> 5      N       N        N      N       N       N     N PROBABLE      
#> 6      N       N        N      N       N       N     N POSSIBLE      
#>                        AEOUT AESEQ TRTEMFL AOCCFL AOCCSFL AOCCPFL AOCC02FL
#> 1 NOT RECOVERED/NOT RESOLVED     1       Y      Y       Y       Y         
#> 2 NOT RECOVERED/NOT RESOLVED     2       Y                      Y         
#> 3         RECOVERED/RESOLVED     3       Y              Y       Y         
#> 4 NOT RECOVERED/NOT RESOLVED     1       Y      Y       Y       Y         
#> 5 NOT RECOVERED/NOT RESOLVED     2       Y                                
#> 6 NOT RECOVERED/NOT RESOLVED     3       Y              Y       Y         
#>   AOCC03FL AOCC04FL             CQ01NAM AOCC01FL
#> 1                   DERMATOLOGIC EVENTS        Y
#> 2                   DERMATOLOGIC EVENTS         
#> 3                                               
#> 4                   DERMATOLOGIC EVENTS        Y
#> 5                   DERMATOLOGIC EVENTS         
#> 6                                               
assign_label(
  r2rtf::r2rtf_adae,
  var = "USUBJID",
  label = "Unique subject identifier"
) |> head()
#>        STUDYID SITEID     USUBJID    TRTA TRTAN AGE AGEGR1 AGEGR1N  RACE RACEN
#> 1 CDISCPILOT01    701 01-701-1015 Placebo     0  63    <65       1 WHITE     1
#> 2 CDISCPILOT01    701 01-701-1015 Placebo     0  63    <65       1 WHITE     1
#> 3 CDISCPILOT01    701 01-701-1015 Placebo     0  63    <65       1 WHITE     1
#> 4 CDISCPILOT01    701 01-701-1023 Placebo     0  64    <65       1 WHITE     1
#> 5 CDISCPILOT01    701 01-701-1023 Placebo     0  64    <65       1 WHITE     1
#> 6 CDISCPILOT01    701 01-701-1023 Placebo     0  64    <65       1 WHITE     1
#>   SEX SAFFL     TRTSDT     TRTEDT      ASTDT ASTDTF ASTDY      AENDT AENDY
#> 1   F     Y 2014-01-02 2014-07-02 2014-01-03            2       <NA>    NA
#> 2   F     Y 2014-01-02 2014-07-02 2014-01-03            2       <NA>    NA
#> 3   F     Y 2014-01-02 2014-07-02 2014-01-09            8 2014-01-11    10
#> 4   M     Y 2012-08-05 2012-09-01 2012-08-07            3 2012-08-30    26
#> 5   M     Y 2012-08-05 2012-09-01 2012-08-07            3       <NA>    NA
#> 6   M     Y 2012-08-05 2012-09-01 2012-08-26           22       <NA>    NA
#>   ADURN ADURU                               AETERM                    AELLT
#> 1    NA                  APPLICATION SITE ERYTHEMA APPLICATION SITE REDNESS
#> 2    NA                  APPLICATION SITE PRURITUS APPLICATION SITE ITCHING
#> 3     3   DAY                            DIARRHOEA                 DIARRHEA
#> 4    24   DAY                             ERYTHEMA                 ERYTHEMA
#> 5    NA                                   ERYTHEMA       LOCALIZED ERYTHEMA
#> 6    NA       ATRIOVENTRICULAR BLOCK SECOND DEGREE   AV BLOCK SECOND DEGREE
#>   AELLTCD                              AEDECOD AEPTCD    AEHLT AEHLTCD
#> 1      NA            APPLICATION SITE ERYTHEMA     NA HLT_0617      NA
#> 2      NA            APPLICATION SITE PRURITUS     NA HLT_0317      NA
#> 3      NA                            DIARRHOEA     NA HLT_0148      NA
#> 4      NA                             ERYTHEMA     NA HLT_0284      NA
#> 5      NA                             ERYTHEMA     NA HLT_0284      NA
#> 6      NA ATRIOVENTRICULAR BLOCK SECOND DEGREE     NA HLT_0415      NA
#>      AEHLGT AEHLGTCD                                             AEBODSYS
#> 1 HLGT_0152       NA GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS
#> 2 HLGT_0338       NA GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS
#> 3 HLGT_0588       NA                           GASTROINTESTINAL DISORDERS
#> 4 HLGT_0192       NA               SKIN AND SUBCUTANEOUS TISSUE DISORDERS
#> 5 HLGT_0192       NA               SKIN AND SUBCUTANEOUS TISSUE DISORDERS
#> 6 HLGT_0086       NA                                    CARDIAC DISORDERS
#>                                                  AESOC AESOCCD    AESEV AESER
#> 1 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS      NA     MILD     N
#> 2 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS      NA     MILD     N
#> 3                           GASTROINTESTINAL DISORDERS      NA     MILD     N
#> 4               SKIN AND SUBCUTANEOUS TISSUE DISORDERS      NA     MILD     N
#> 5               SKIN AND SUBCUTANEOUS TISSUE DISORDERS      NA MODERATE     N
#> 6                                    CARDIAC DISORDERS      NA     MILD     N
#>   AESCAN AESCONG AESDISAB AESDTH AESHOSP AESLIFE AESOD    AEREL AEACN
#> 1      N       N        N      N       N       N     N PROBABLE      
#> 2      N       N        N      N       N       N     N PROBABLE      
#> 3      N       N        N      N       N       N     N   REMOTE      
#> 4      N       N        N      N       N       N     N POSSIBLE      
#> 5      N       N        N      N       N       N     N PROBABLE      
#> 6      N       N        N      N       N       N     N POSSIBLE      
#>                        AEOUT AESEQ TRTEMFL AOCCFL AOCCSFL AOCCPFL AOCC02FL
#> 1 NOT RECOVERED/NOT RESOLVED     1       Y      Y       Y       Y         
#> 2 NOT RECOVERED/NOT RESOLVED     2       Y                      Y         
#> 3         RECOVERED/RESOLVED     3       Y              Y       Y         
#> 4 NOT RECOVERED/NOT RESOLVED     1       Y      Y       Y       Y         
#> 5 NOT RECOVERED/NOT RESOLVED     2       Y                                
#> 6 NOT RECOVERED/NOT RESOLVED     3       Y              Y       Y         
#>   AOCC03FL AOCC04FL             CQ01NAM AOCC01FL
#> 1                   DERMATOLOGIC EVENTS        Y
#> 2                   DERMATOLOGIC EVENTS         
#> 3                                               
#> 4                   DERMATOLOGIC EVENTS        Y
#> 5                   DERMATOLOGIC EVENTS         
#> 6                                               
```
