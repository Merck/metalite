test_that("Name of Variable when using is_named", {
  expect_false(is_named(matrix(1:2, 2)))

  expect_false(is_named(""))

  expect_false(is_named(NA))

  expect_true(is_named(data.frame(x = 1)))
})
