# Execute analysis based on the analysis plan

Execute analysis based on the analysis plan

## Usage

``` r
meta_run(meta, i = NULL, ...)
```

## Arguments

- meta:

  A `meta_adam` object.

- i:

  A vector of integers to indicate `i`-th analysis in `meta$plan`.

- ...:

  Additional arguments passed to `[spec_call_program()]`.

## Value

Executed analysis based on the analysis plan.

## Examples

``` r
if (interactive()) {
  meta <- meta_example()
  ae_summary <- function(...) {
    "results of ae_summary"
  }
  ae_specific <- function(...) {
    "results of ae_specific"
  }
  meta_run(meta)
  meta_run(meta, i = 2)
}
```
