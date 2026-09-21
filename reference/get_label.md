# A function to get the labels of data frame columns

A function to get the labels of data frame columns

## Usage

``` r
get_label(data)
```

## Arguments

- data:

  A data frame.

## Value

Labels of the input data frame.

## Examples

``` r
get_label(r2rtf::r2rtf_adae)
#>                                   STUDYID 
#>                        "Study Identifier" 
#>                                    SITEID 
#>                   "Study Site Identifier" 
#>                                   USUBJID 
#>               "Unique Subject Identifier" 
#>                                      TRTA 
#>                        "Actual Treatment" 
#>                                     TRTAN 
#>                    "Actual Treatment (N)" 
#>                                       AGE 
#>                                     "Age" 
#>                                    AGEGR1 
#>                      "Pooled Age Group 1" 
#>                                   AGEGR1N 
#>                  "Pooled Age Group 1 (N)" 
#>                                      RACE 
#>                                    "Race" 
#>                                     RACEN 
#>                                "Race (N)" 
#>                                       SEX 
#>                                     "Sex" 
#>                                     SAFFL 
#>                  "Safety Population Flag" 
#>                                    TRTSDT 
#>     "Date of First Exposure to Treatment" 
#>                                    TRTEDT 
#>      "Date of Last Exposure to Treatment" 
#>                                     ASTDT 
#>                     "Analysis Start Date" 
#>                                    ASTDTF 
#>     "Analysis Start Date Imputation Flag" 
#>                                     ASTDY 
#>             "Analysis Start Relative Day" 
#>                                     AENDT 
#>                       "Analysis End Date" 
#>                                     AENDY 
#>               "Analysis End Relative Day" 
#>                                     ADURN 
#>                         "AE Duration (N)" 
#>                                     ADURU 
#>                       "AE Duration Units" 
#>                                    AETERM 
#>     "Reported Term for the Adverse Event" 
#>                                     AELLT 
#>                       "Lowest Level Term" 
#>                                   AELLTCD 
#>                  "Lowest Level Term Code" 
#>                                   AEDECOD 
#>                 "Dictionary-Derived Term" 
#>                                    AEPTCD 
#>                     "Preferred Term Code" 
#>                                     AEHLT 
#>                         "High Level Term" 
#>                                   AEHLTCD 
#>                    "High Level Term Code" 
#>                                    AEHLGT 
#>                   "High Level Group Term" 
#>                                  AEHLGTCD 
#>              "High Level Group Term Code" 
#>                                  AEBODSYS 
#>              "Body System or Organ Class" 
#>                                     AESOC 
#>              "Primary System Organ Class" 
#>                                   AESOCCD 
#>         "Primary System Organ Class Code" 
#>                                     AESEV 
#>                      "Severity/Intensity" 
#>                                     AESER 
#>                           "Serious Event" 
#>                                    AESCAN 
#>                         "Involves Cancer" 
#>                                   AESCONG 
#>      "Congenital Anomaly or Birth Defect" 
#>                                  AESDISAB 
#> "Persist or Signif Disability/Incapacity" 
#>                                    AESDTH 
#>                        "Results in Death" 
#>                                   AESHOSP 
#>    "Requires or Prolongs Hospitalization" 
#>                                   AESLIFE 
#>                     "Is Life Threatening" 
#>                                     AESOD 
#>                  "Occurred with Overdose" 
#>                                     AEREL 
#>                               "Causality" 
#>                                     AEACN 
#>       "Action Taken with Study Treatment" 
#>                                     AEOUT 
#>                "Outcome of Adverse Event" 
#>                                     AESEQ 
#>                         "Sequence Number" 
#>                                   TRTEMFL 
#>        "Treatment Emergent Analysis Flag" 
#>                                    AOCCFL 
#>           "1st Occurrence of Any AE Flag" 
#>                                   AOCCSFL 
#>              "1st Occurrence of SOC Flag" 
#>                                   AOCCPFL 
#>   "1st Occurrence of Preferred Term Flag" 
#>                                  AOCC02FL 
#>      "1st Occurrence 02 Flag for Serious" 
#>                                  AOCC03FL 
#>  "1st Occurrence 03 Flag for Serious SOC" 
#>                                  AOCC04FL 
#>   "1st Occurrence 04 Flag for Serious PT" 
#>                                   CQ01NAM 
#>                "Customized Query 01 Name" 
#>                                  AOCC01FL 
#>         "1st Occurrence 01 Flag for CQ01" 
```
