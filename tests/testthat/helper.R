# Create 2 dummy functions
ae_summary <- function(...) {
  paste("results of", deparse(match.call(), nlines = 1))
}
ae_specific <- function(...) {
  paste("results of", deparse(match.call(), nlines = 1))
}