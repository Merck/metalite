test_that("Inherit keywords", {
  x <- meta_adam(
    population = r2rtf::r2rtf_adsl,
    observation = r2rtf::r2rtf_adae
  ) |>
    meta_inherit(meta_example(), c("apat", "wk12", "ae_summary"))

  expect_equal(names(x$population), "apat")
  expect_equal(names(x$observation), "wk12")
  expect_equal(names(x$analysis), "ae_summary")
})

test_that("Wrong key words", {
  expect_error(
    meta_adam(
      population = r2rtf::r2rtf_adsl,
      observation = r2rtf::r2rtf_adae
    ) |>
      meta_inherit(meta_example(), c("apat", "wk12", "ae_ser"))
  )
})
