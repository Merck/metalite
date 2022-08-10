test_that("collect_population_id() output", {
  meta <- meta_dummy()
  pop <- meta$data_population$SAFFL =="Y"
  
  expect_error(collect_population_id(meta_dummy(), "itt"))
  expect_equal(collect_population_id(meta_dummy(), "apat"), meta$data_population[collect_population_index(meta, "apat"), ]$USUBJID)
})
