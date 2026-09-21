# Specification for analysis title

Specification for analysis title

## Usage

``` r
spec_title(meta)
```

## Arguments

- meta:

  A `meta_adam` object.

## Value

A vector of character strings containing the table captions in the order
of the analysis plans.

## Examples

``` r
meta <- meta_example()
spec_title(meta)
#>  [1] "Summary of Adverse Events\nWeeks 0 to 12\nAll Participants as Treated"                           
#>  [2] "Summary of Adverse Events\nWeeks 0 to 24\nAll Participants as Treated"                           
#>  [3] "Participants With Adverse Events\nWeeks 0 to 12\nAll Participants as Treated"                    
#>  [4] "Participants With Adverse Events\nWeeks 0 to 24\nAll Participants as Treated"                    
#>  [5] "Participants With Adverse Events of special interest\nWeeks 0 to 12\nAll Participants as Treated"
#>  [6] "Participants With Adverse Events of special interest\nWeeks 0 to 24\nAll Participants as Treated"
#>  [7] "Participants With Drug-Related Adverse Events\nWeeks 0 to 12\nAll Participants as Treated"       
#>  [8] "Participants With Drug-Related Adverse Events\nWeeks 0 to 24\nAll Participants as Treated"       
#>  [9] "Participants With Serious Adverse Events\nWeeks 0 to 12\nAll Participants as Treated"            
#> [10] "Participants With Serious Adverse Events\nWeeks 0 to 24\nAll Participants as Treated"            
```
