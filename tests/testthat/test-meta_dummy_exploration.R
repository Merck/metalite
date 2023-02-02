x <- meta_example_exploration(data = r2rtf::r2rtf_adsl, group = "TRT01A", name = "apat")

test_that("meta_example_exploration() structure", {
  expect_equal(x$analysis$exploration$name, "exploration")
  expect_equal(x$population$apat$name, "apat")
  expect_equal(x$observation$inf$name, "inf")
  expect_equal(length(x$parameter), 48)
  expect_equal(names(x$parameter), names(r2rtf::r2rtf_adsl))
})
