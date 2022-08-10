test_that("collect_observation_index() output", {
  meta <- meta_dummy()
  obs <- which((meta$data_observation$SAFFL =="Y" & meta$data_observation$AESER == 'Y'))
   
  expect_equal(collect_observation_index(meta_dummy(), "apat", "wk12", "ser"), obs)
})
