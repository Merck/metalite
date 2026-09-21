# Specification for analysis output filename

Specification for analysis output filename

## Usage

``` r
spec_filename(meta)
```

## Arguments

- meta:

  A `meta_adam` object.

## Value

A vector of character strings containing the RTF file names.

## Examples

``` r
meta <- meta_example()
spec_filename(meta)
#>  [1] "ae0summary0wk12.rtf"        "ae0summary0wk24.rtf"       
#>  [3] "ae0specific0wk120any.rtf"   "ae0specific0wk240any.rtf"  
#>  [5] "ae0specific0wk120aeosi.rtf" "ae0specific0wk240aeosi.rtf"
#>  [7] "ae0specific0wk120rel.rtf"   "ae0specific0wk240rel.rtf"  
#>  [9] "ae0specific0wk120ser.rtf"   "ae0specific0wk240ser.rtf"  
```
