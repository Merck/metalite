library(dplyr)
library(metalite)


test_that("error is thrown", {
  expect_error(metalite::plan(c("ae_summary", "ae_specific"), population = "apat", observation = c("wk12", "wk24"), parameter = "any;rel"))
})

test_that("Testing no of rows", {
  rows <- nrow(metalite:::new_plan(
    analysis = "ae_specific",
    population = "apart",
    observation = c("wk12", "wk24"),
    parameter = c("any", "rel", "ser")
  ))

  expect_equal(6, rows)
})


test_that("Expecting an Error", {
  expect_error(metalite:::validate_plan(1))

  expect_error(metalite:::validate_plan(plan(analysis = "ae_summary", population = "apat", observation = "wk12")))

  expect_error(metalite:::validate_plan(plan(analysis = "ae_summary", observation = "apat", parameter = "any")))

  expect_error(metalite:::validate_plan(plan(analysis = "ae_summary", population = "apat", parameter = "any")))

  expect_error(metalite:::validate_plan(plan(population = "ae_summary", observation = "wk12", parameter = "any")))
})


test_that("Outputs are same", {
  output1 <- plan("ae_summary", population = "apat", observation = c("wk12", "wk24"), parameter = "any;rel") |>
    add_plan("ae_specific", population = "apat", observation = c("wk12", "wk24"), parameter = c("any", "rel"))

  output2 <- dplyr::bind_rows(
    plan("ae_summary", population = "apat", observation = c("wk12", "wk24"), parameter = "any;rel"),
    plan("ae_specific", population = "apat", observation = c("wk12", "wk24"), parameter = c("any", "rel"))
  )

  expect_equal(output1, output2)
})
