test_that("collect_population_index() output", {
  meta <- meta_dummy()
  pop <- meta$data_population$SAFFL =="Y"
    
  expect_equal(collect_population_index(meta_dummy(), "itt"), 1:nrow(meta$data_population))
  expect_equal(collect_population_index(meta_dummy(), "apat"), which(pop))
})
