library(dplyr)
library(metalite)

test_that("Its class is 'outdata'", {
meta <- meta_dummy()
output<-outdata(meta_dummy(), "apat", "wk12", "rel", n = meta$data_population %>% group_by(TRTA) %>% summarize(n = n()), group = "TRTA", reference_group = 1, order = 1:3)

  expect_equal(class(output), "outdata")
})


test_that("Expecting an error", {
expect_error(metalite:::new_outdata(1))
})

test_that("Its class is 'outdata'", {
  meta <- meta_dummy()
  x <- list(meta = meta_dummy(), population = "apat", observation = "wk12", parameter = "rel", n = meta$data_population %>% group_by(TRTA) %>% summarize(n = n()), group = "TRTA", reference_group = 1, order = 1:3)
output<-metalite:::new_outdata(x)
  expect_equal(class(output), "outdata")
})

test_that("Expecting an error", {
  expect_error(metalite:::validate_outdata())
  
  meta <- meta_dummy()
  
  expect_error(metalite:::validate_outdata(outdata(meta = meta_dummy(), population = "apat", observation = "wk12", parameter = "rel", n = meta$data_population %>% group_by(TRTA) %>% summarize(n = n()), group = "TRTA", reference_group = "Placebo", order = 1:3)))
  

  expect_error(metalite:::validate_outdata(outdata(meta = meta_dummy(), population = "apat", observation = "wk12", parameter = "rel", n = meta$data_population %>% group_by(TRTA) %>% summarize(n = n()), group = "TRTA", reference_group = 1, order = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))))
  
  expect_error(metalite:::validate_outdata(outdata(meta = meta_dummy(), population = 1, observation = "wk12", parameter = "rel", n = meta$data_population %>% group_by(TRTA) %>% summarize(n = n()), group = "TRTA", reference_group = 1, order = 1:3)))
  
  expect_error(metalite:::validate_outdata(outdata(meta = meta_dummy(), population = "apat", observation = "wk12", parameter = "rel", n = meta$data_population %>% group_by(TRTA) %>% summarize(n = n()), group = "TRTA", reference_group = 1:2, order = 1:3)))
  
  expect_error(metalite:::validate_outdata(outdata(meta = meta_dummy(), population = c("apat", "ITT"), observation = "wk12", parameter = "rel", n = meta$data_population %>% group_by(TRTA) %>% summarize(n = n()), group = "TRTA", reference_group = 1, order = 1:3)))
})









