test_that("throw an error if the input meta doesn't define the observation", {
  meta <- meta_example()
  meta$data_observation <- NULL
  expect_error(meta |> meta_build())
})

test_that("throw an error if the input meta doesn't define the population", {
  meta <- meta_example()
  meta$data_population <- NULL
  expect_error(meta |> meta_build())
})

test_that("throw an error if the input meta doesn't have any analysis plan", {
  meta <- meta_example()
  meta$plan <- NULL
  expect_error(meta |> meta_build())
})

test_that("check if the keywords are available as their default value", {
  meta <- meta_example()
  test_meta <- meta |> meta_build()
  expect_equal(
    names(test_meta),
    c(
      "data_population", "data_observation", "plan",
      "observation", "population", "parameter", "analysis"
    )
  )
  expect_true(unique(c("id", "group") %in% names(test_meta$observation$wk12)))
})
