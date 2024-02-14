test_that("Its class is 'outdata'", {
  meta <- meta_example()
  output <- outdata(meta_example(), "apat", "wk12", "rel", n = meta$data_population %>% dplyr::group_by(TRTA) %>% dplyr::summarize(n = dplyr::n()), group = "TRTA", reference_group = 1, order = 1:3)

  expect_equal(class(output), "outdata")
  expect_equal(length(output), 8)
  expect_equal(names(output), c("meta", "population", "observation", "parameter", "n", "order", "group", "reference_group"))
})


test_that("Expecting an error", {
  expect_error(metalite:::new_outdata(1))
})

test_that("Its class is 'outdata'", {
  meta <- meta_example()
  x <- list(meta = meta_example(), population = "apat", observation = "wk12", parameter = "rel", n = meta$data_population %>% dplyr::group_by(TRTA) %>% dplyr::summarize(n = dplyr::n()), group = "TRTA", reference_group = 1, order = 1:3)
  output <- metalite:::new_outdata(x)
  expect_equal(class(output), "outdata")
})

test_that("Expecting an error", {
  expect_error(metalite:::validate_outdata())

  meta <- meta_example()

  expect_error(metalite:::validate_outdata(outdata(meta = meta_example(), population = "apat", observation = "wk12", parameter = "rel", n = meta$data_population %>% dplyr::group_by(TRTA) %>% dplyr::summarize(n = dplyr::n()), group = "TRTA", reference_group = "Placebo", order = 1:3)))
  expect_error(metalite:::validate_outdata(outdata(meta = meta_example(), population = "apat", observation = "wk12", parameter = "rel", n = meta$data_population %>% dplyr::group_by(TRTA) %>% dplyr::summarize(n = dplyr::n()), group = "TRTA", reference_group = 1, order = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))))
  expect_error(metalite:::validate_outdata(outdata(meta = meta_example(), population = 1, observation = "wk12", parameter = "rel", n = meta$data_population %>% dplyr::group_by(TRTA) %>% dplyr::summarize(n = dplyr::n()), group = "TRTA", reference_group = 1, order = 1:3)))
  expect_error(metalite:::validate_outdata(outdata(meta = meta_example(), population = "apat", observation = "wk12", parameter = "rel", n = meta$data_population %>% dplyr::group_by(TRTA) %>% dplyr::summarize(n = dplyr::n()), group = "TRTA", reference_group = 1:2, order = 1:3)))
  expect_error(metalite:::validate_outdata(outdata(meta = meta_example(), population = c("apat", "ITT"), observation = "wk12", parameter = "rel", n = meta$data_population %>% dplyr::group_by(TRTA) %>% dplyr::summarize(n = dplyr::n()), group = "TRTA", reference_group = 1, order = 1:3)))
})
