x <- meta_example()

test_that("meta_example() structure", {
  expect_equal(class(x), "meta_adam")
  expect_equal(class(x$data_population), "data.frame")
  expect_equal(class(x$data_observation), "data.frame")
  expect_equal(class(x$population), "list")
  expect_equal(class(x$observation), "list")
  expect_equal(class(x$parameter), "list")
  expect_equal(class(x$analysis), "list")
})
