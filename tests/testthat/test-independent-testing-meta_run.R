ae_summary <- function(...) {
  paste("results of", deparse(match.call(), nlines = 1))
}
ae_specific <- function(...) {
  paste("results of", deparse(match.call(), nlines = 1))
}

# Create dummy meta_adam object
meta <- meta_dummy()

# Snapshot testing
test_that("all analysis based on the analysis plan would be executed", {
  expect_snapshot(meta_run(meta))
})

test_that("only the first analysis based on the analysis plan would be executed", {
  expect_snapshot(meta_run(meta, i = 1))
})

test_that("selected analysis based on the analysis plan would be executed", {
  expect_snapshot(meta_run(meta, i = c(1, 3, 5)))
})
