test_that("collect_observation_record() output", {
  meta <- meta_dummy()
  
  expect_equal(head(collect_observation_record(meta, "apat", "wk12", "ser")), 
               head(meta$data_observation[collect_observation_index(meta, "apat", "wk12", "ser"), ][c("USUBJID", "TRTA", "AEDECOD", "SAFFL", "AESER")]))
  
  expect_equal(head(collect_observation_record(meta, "apat", var = "AGE", "wk12", "ser")), 
               head(meta$data_observation[collect_observation_index(meta, "apat", "wk12", "ser"), ][c("USUBJID", "TRTA", "AEDECOD", "SAFFL", "AESER", "AGE")]))
  
})
