test_that("expect error when name is NULL", {
  expect_error(validate_adam_mapping(new_adam_mapping(list(name = NULL))))
})

test_that("expect mapping to generate", {
  a <- validate_adam_mapping(new_adam_mapping(list(name = "apat")))

  expect_equal(a$name, "apat")
})

test_that("if required variable is not character then expect error:", {
  fun <- function(name, id) {
    exprs <- rlang::enquos(
      name = name,
      id = id
    )
    exprs
  }

  expect_error(validate_adam_mapping(new_adam_mapping(fun("apat", USUBJID), env = parent.frame())))
})

test_that("name, id, label should be of length 1, if not then expect error:", {
  expect_error(validate_adam_mapping(new_adam_mapping(list(name = "apat", id = c("USUBJID", "SUBJID")))))
})
