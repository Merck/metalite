library(r2rtf)


test_that("meta_adam testing", {
  test <- meta_adam(
    population = r2rtf::r2rtf_adsl,
    observation = r2rtf::r2rtf_adae
  )
  expect_equal(class(test), "meta_adam")
  expect_equal(test$plan, list())
  expect_equal(test$population, list())
  expect_equal(test$observation, list())
  expect_equal(test$parameter, list())
  expect_equal(test$analysis, list())
  
  meta <- meta_dummy()
  expect_true(length(meta$plan) > 0)
  expect_true(length(meta$observation) > 0)
  expect_true(length(meta$population) > 0)
  expect_true(length(meta$parameter) > 0)
  expect_true(length(meta$analysis) > 0)
  

})

