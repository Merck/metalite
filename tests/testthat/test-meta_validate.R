test_that("Check data_type", {
  meta <- meta_example()
  meta$data_population <- NULL
  expect_error(meta_validate(meta))

  meta <- meta_example()
  meta$data_observation <- NULL
  expect_error(meta_validate(meta))

  meta <- meta_example()
  meta$plan <- NULL
  expect_error(meta_validate(meta))
})

test_that("Check group factor level", {
  meta <- meta_example()
  meta$data_population$TRTA <- factor(meta$data_population$TRTA, c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"))
  expect_error(meta_validate(meta))
})

test_that("Check plan variable name", {
  meta <- meta_example()
  names(meta$plan)[5] <- "param"
  expect_error(meta_validate(meta))
})

test_that("Check id variable", {
  meta <- meta_example()
  meta$population$apat$id <- "id"
  expect_error(meta_validate(meta))
})

test_that("Check label variable", {
  meta <- meta_example()
  meta$population$apat$label <- NULL
  expect_warning(meta_validate(meta))
})

test_that("Check observation variables in the datasets", {
  meta <- meta_example()
  meta$observation$wk12$id <- "ID"
  expect_error(meta_validate(meta))

  meta <- meta_example()
  meta$observation$wk12$group <- "group"
  expect_error(meta_validate(meta))

  meta <- meta_example()
  meta$observation$wk12$var <- "var"
  expect_error(meta_validate(meta))
})
