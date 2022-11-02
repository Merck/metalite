x <- meta_dummy_exploration(data = r2rtf::r2rtf_adsl, group = "TRT01A", name = "apat")

test_that("meta_dummy_exploration() structure", {
  expect_equal(x$analysis$exploration$name, "exploration")
  expect_equal(x$population$apat$name, "apat")
  expect_equal(x$observation$inf$name, "inf")
  expect_equal(ncol(x$data_population), 48)
  expect_equal(ncol(x$data_observation), 48)
})