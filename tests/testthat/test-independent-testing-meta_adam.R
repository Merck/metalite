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
})
