meta <- meta_example()

test_that("Check data_type", {
  meta$data_population <- NULL
  expect_error(meta_validate(meta))

  meta <- meta_example()
  meta$data_observation <- NULL
  expect_error(meta_validate(meta))

  meta <- meta_example()
  meta$plan <- NULL
  expect_error(meta_validate(meta))
})


test_that("Check plan variable name", {
  names(meta$plan)[5] <- "param"
  expect_error(meta_validate(meta))
})



test_that("Check id variable", {
  meta$population$apat$id <- "id"
  expect_error(meta_validate(meta))
})

test_that("Check label variable", {
  meta$population$apat$label <- NULL
  expect_warning(meta_validate(meta))
})

test_that("Check observation variables in the datasets", {
  meta$observation$wk12$id <- "ID"
  expect_error(meta_validate(meta))

  meta <- meta_example()
  meta$observation$wk12$group <- "group"
  expect_error(meta_validate(meta))

  meta <- meta_example()
  meta$observation$wk12$var <- "var"
  expect_error(meta_validate(meta))
})
