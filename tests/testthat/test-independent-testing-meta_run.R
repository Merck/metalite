# Create example `meta_adam` object
meta <- meta_example()

ae_summary <- function(meta, population, observation, parameter) {
  message("run ae_summary")
  return(NULL)
}

ae_specific <- function(meta, population, observation, parameter) {
  message("run ae_specific")
  return(NULL)
}

test_that("all analysis based on the analysis plan would be executed", {
  expect_equal(class(meta_run(meta)), "list")
})

test_that("only the first analysis based on the analysis plan would be executed", {
  expect_equal(class(meta_run(meta, i = 1)), "list")
})

test_that("selected analysis based on the analysis plan would be executed", {
  expect_equal(class(meta_run(meta, i = c(1, 3, 5))), "list")
})
