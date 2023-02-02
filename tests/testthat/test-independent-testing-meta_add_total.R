library(testthat)
library(dplyr)

test_that("Multple names for total column has to throw error", {
  expect_error(meta_add_total(meta = meta_example(), total = c("Total", "Sum")))
})


test_that("number of distinct treatment arms after adding", {
  x <- meta_add_total(meta = meta_example(), total = "Sum")
  y <- meta_example()
  expect_equal(n_distinct(x$data_population$TRTA), n_distinct(y$data_population$TRTA) + 1)
  expect_equal(n_distinct(x$data_observation$TRTA), n_distinct(y$data_observation$TRTA) + 1)
  expect_true("Sum" %in% x$data_observation$TRTA)
  expect_false("Sum" %in% y$data_observation$TRTA)
  expect_true("Sum" %in% x$data_population$TRTA)
  expect_false("Sum" %in% y$data_population$TRTA)
})
