library(testthat)
library(metalite)

ae_summary <- function(...) {
  paste("results of", deparse(match.call(), nlines = 1))
}
ae_specific <- function(...) {
  paste("results of", deparse(match.call(), nlines = 1))
}


test_check("metalite")
