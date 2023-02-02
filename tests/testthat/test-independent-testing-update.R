test_that("update adam mapping with additional variable AEREL in existing adam mapping", {
  meta1 <- update_adam_mapping(meta_example(), "rel", start_date = "ASTDT", stop_date = "ASTDT")
  expect_equal(names(collect_adam_mapping(meta1, "rel")), c("name", "id", "group", "var", "subset", "label", "soc", "seq", "term1", "term2", "summ_row", "summ_foot", ".location", "start_date", "stop_date"))
})


test_that("update adam mapping with additional variable ser in existing adam mapping", {
  meta2 <- update_adam_mapping(meta_example(), "ser", clinint = "Y")
  expect_equal(names(collect_adam_mapping(meta2, "ser")), c("name", "id", "group", "var", "subset", "label", "soc", "seq", "term1", "term2", "summ_row", ".location", "clinint"))
})
