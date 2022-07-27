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

