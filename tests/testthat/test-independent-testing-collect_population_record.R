test_that("collect_population_record() output", {
  meta <- meta_dummy()
  
  expect_equal(head(collect_population_record(meta, "apat")), 
               head(meta$data_population[collect_population_index(meta, "apat"), ][c("USUBJID", "TRTA", "SAFFL")]))
  
  expect_equal(head(collect_population_record(meta, "apat", var = "AGE")), 
               head(meta$data_population[collect_population_index(meta, "apat"), ][c("USUBJID", "TRTA", "SAFFL", "AGE")]))
  
})
