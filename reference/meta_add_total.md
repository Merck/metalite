# Add duplicate data to enable a total group

Add duplicate data to enable a total group

## Usage

``` r
meta_add_total(meta, total = "Total")
```

## Arguments

- meta:

  A metalite object.

- total:

  A character value of total group name.

## Value

A metadata object with a total group added.

## Examples

``` r
x <- meta_add_total(meta_example())
# A `Total` group is added
table(x$data_population$TRTA)
#> 
#>              Placebo  Xanomeline Low Dose Xanomeline High Dose 
#>                   86                   84                   84 
#>                Total 
#>                  254 
```
