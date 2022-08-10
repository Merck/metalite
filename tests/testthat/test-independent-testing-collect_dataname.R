test_that("extract source dataset name", {
  meta <- meta_dummy()
  x  <- c(
    population = attr(meta$data_population, "data_name"),
    observation = attr(meta$data_observation, "data_name")
  )
  expect_equal(collect_dataname(meta_dummy()), x)
})
