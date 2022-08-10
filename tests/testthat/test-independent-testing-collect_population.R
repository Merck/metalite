test_that("The output of collect_population()", {
  output1 <- collect_population(meta_dummy(), "apat", "wk12", "ser")
  expect_equal(names(output1), c("population", "observation", "parameter"))
  expect_equal(output1$population, fmt_quote(deparse(collect_adam_mapping(meta_dummy(), "apat")$subset)))
  expect_equal(output1$observation, fmt_quote(deparse(collect_adam_mapping(meta_dummy(), "wk12")$subset)))
  expect_equal(output1$parameter, fmt_quote(deparse(collect_adam_mapping(meta_dummy(), "ser")$subset)))
  
  output2 <- collect_population(meta_dummy(), "apat", "wk24", "ser")
  expect_equal(names(output2), c("population", "observation", "parameter"))
  expect_equal(output2$population, fmt_quote(deparse(collect_adam_mapping(meta_dummy(), "apat")$subset)))
  expect_equal(output2$observation, fmt_quote(deparse(collect_adam_mapping(meta_dummy(), "wk24")$subset)))
  expect_equal(output2$parameter, fmt_quote(deparse(collect_adam_mapping(meta_dummy(), "ser")$subset)))
  
  output3 <- collect_population(meta_dummy(), "apat", "wk24", "rel")
  expect_equal(names(output3), c("population", "observation", "parameter"))
  expect_equal(output3$population, fmt_quote(deparse(collect_adam_mapping(meta_dummy(), "apat")$subset)))
  expect_equal(output3$observation, fmt_quote(deparse(collect_adam_mapping(meta_dummy(), "wk24")$subset)))
  expect_equal(output3$parameter, fmt_quote(deparse(collect_adam_mapping(meta_dummy(), "rel")$subset)))
})
