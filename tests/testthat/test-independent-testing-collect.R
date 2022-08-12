test_that("bulletproof", {
  # throw an error when collect_adam_mapping(meta_dummy())
  expect_error(collect_adam_mapping(meta_dummy()))
  # throw an error when collect_adam_mapping(meta = meta_dummy(), name = 1)
  expect_error(collect_adam_mapping(meta_dummy(), name = 1))
  # output NULL when collect_adam_mapping(meta = meta_dummy(), name = "itt")
  expect_null(collect_adam_mapping(meta = meta_dummy(), name = "itt"))
})

test_that(".location works well", {
  # the .location of collect_adam_mapping(meta = meta_dummy(), name = "apat") is "population"
  expect_equal(collect_adam_mapping(meta = meta_dummy(), name = "apat")$.location, "population")
  # the .location of collect_adam_mapping(meta = meta_dummy(), name = "wk12") is "observation"
  expect_equal(collect_adam_mapping(meta = meta_dummy(), name = "wk12")$.location, "observation")
  # the .location of collect_adam_mapping(meta = meta_dummy(), name = "any") is "parameter"
  expect_equal(collect_adam_mapping(meta = meta_dummy(), name = "any")$.location, "parameter")
  # the .location of collect_adam_mapping(meta = meta_dummy(), name = "ae_summary") is "analysis"
  expect_equal(collect_adam_mapping(meta = meta_dummy(), name = "ae_summary")$.location, "analysis")
})

test_that("extract source dataset name", {
  meta <- meta_dummy()
  x  <- c(
    population = attr(meta$data_population, "data_name"),
    observation = attr(meta$data_observation, "data_name")
  )
  expect_equal(collect_dataname(meta_dummy()), x)
})

test_that("collect_observation_index() output", {
  meta <- meta_dummy()
  obs <- which((meta$data_observation$SAFFL =="Y" & meta$data_observation$AESER == 'Y'))
  
  expect_equal(collect_observation_index(meta_dummy(), "apat", "wk12", "ser"), obs)
})

test_that("collect_observation_record() output", {
  meta <- meta_dummy()
  
  expect_equal(head(collect_observation_record(meta, "apat", "wk12", "ser")), 
               head(meta$data_observation[collect_observation_index(meta, "apat", "wk12", "ser"), ][c("USUBJID", "TRTA", "AEDECOD", "SAFFL", "AESER")]))
  
  expect_equal(head(collect_observation_record(meta, "apat", var = "AGE", "wk12", "ser")), 
               head(meta$data_observation[collect_observation_index(meta, "apat", "wk12", "ser"), ][c("USUBJID", "TRTA", "AEDECOD", "SAFFL", "AESER", "AGE")]))
  
})

test_that("collect_population_id() output", {
  meta <- meta_dummy()
  pop <- meta$data_population$SAFFL =="Y"
  
  expect_error(collect_population_id(meta_dummy(), "itt"))
  expect_equal(collect_population_id(meta_dummy(), "apat"), meta$data_population[collect_population_index(meta, "apat"), ]$USUBJID)
})

test_that("collect_population_index() output", {
  meta <- meta_dummy()
  pop <- meta$data_population$SAFFL =="Y"
  
  expect_equal(collect_population_index(meta_dummy(), "itt"), 1:nrow(meta$data_population))
  expect_equal(collect_population_index(meta_dummy(), "apat"), which(pop))
})

test_that("collect_population_record() output", {
  meta <- meta_dummy()
  
  expect_equal(head(collect_population_record(meta, "apat")), 
               head(meta$data_population[collect_population_index(meta, "apat"), ][c("USUBJID", "TRTA", "SAFFL")]))
  
  expect_equal(head(collect_population_record(meta, "apat", var = "AGE")), 
               head(meta$data_population[collect_population_index(meta, "apat"), ][c("USUBJID", "TRTA", "SAFFL", "AGE")]))
  
})

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

test_that("output is is a string vector", {
  expect_equal(collect_title(meta_dummy(), "apat", "wk12", "ser", "ae_summary"), 
               c("Summary of Adverse Events", "Weeks 0 to 12", "All Participants as Treated"))
  expect_equal(collect_title(meta_dummy(), "apat", "wk12", "ser", "ae_forest"), 
               c("Weeks 0 to 12", "All Participants as Treated"))
  
})



