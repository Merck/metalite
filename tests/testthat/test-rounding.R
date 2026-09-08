test_that("round_half_away_from_zero handles ties and signed zero", {
  expect_equal(
    round_half_away_from_zero(c(1.25, -1.25), digits = 1),
    c(1.3, -1.3)
  )
  expect_equal(round_half_away_from_zero(1 / 16 * 100, digits = 1), 6.3)
  expect_identical(round_half_away_from_zero(NA_real_, digits = 1), NA_real_)

  rounded_zero <- round_half_away_from_zero(-0.04, digits = 1)
  expect_equal(rounded_zero, 0)
  expect_equal(1 / rounded_zero, Inf)
})

test_that("round_half_away_from_zero preserves supported input shapes", {
  x <- matrix(c(1.25, -1.25), nrow = 1, dimnames = list("row", c("a", "b")))
  expect_equal(
    round_half_away_from_zero(x, digits = 1),
    matrix(c(1.3, -1.3), nrow = 1, dimnames = list("row", c("a", "b")))
  )

  x_df <- data.frame(a = 1.25, b = -1.25)
  expect_equal(
    round_half_away_from_zero(x_df, digits = 1),
    data.frame(a = 1.3, b = -1.3)
  )
})

test_that("rounding helpers validate formatting arguments", {
  expect_error(round_half_away_from_zero("1.25", digits = 1), "must be numeric")
  expect_error(round_half_away_from_zero(1.25, digits = 1.5), "finite integer")
  expect_error(round_half_away_from_zero(data.frame(a = 1, b = "2")), "must be numeric")
  expect_error(format_number(1.25, digits = -1), "non-negative integer")
  expect_error(format_number(1.25, width = 1.5), "non-negative integer")
})

test_that("format_number uses fixed decimals without negative zero", {
  expect_equal(
    format_number(c(1.25, -1.25, 1 / 16 * 100, -0.04), digits = 1),
    c("1.3", "-1.3", "6.3", "0.0")
  )
  expect_equal(format_number(c(6.25, -0.04), digits = 1, width = 5), c("  6.3", "  0.0"))
})

test_that("collect_n_subject rounds summaries and percentages at display", {
  population <- data.frame(
    ID = seq_len(32),
    TRT = factor(rep(c("Positive", "Negative"), each = 16)),
    NUM = c(rep(1.25, 15), NA, rep(-1.25, 15), NA),
    CAT = factor(rep(c("Yes", rep("No", 15)), 2), levels = c("Yes", "No"))
  )

  meta <- meta_adam(population)
  meta$population$pop <- adam_mapping(
    name = "pop", id = "ID", group = "TRT"
  )
  meta$parameter$num <- adam_mapping(
    name = "num", var = "NUM", label = "Numeric"
  )
  meta$parameter$cat <- adam_mapping(
    name = "cat", var = "CAT", label = "Categorical"
  )

  numeric_table <- collect_n_subject(
    meta, "pop", "num", display_total = FALSE
  )$table
  categorical_table <- collect_n_subject(
    meta, "pop", "cat", display_total = FALSE
  )$table

  expect_equal(
    unname(unlist(
      numeric_table[numeric_table$name == "Mean (SD)", c("Positive", "Negative")],
      use.names = FALSE
    )),
    c("1.3 (0.0)", "-1.3 (0.0)")
  )
  expect_equal(
    unname(unlist(
      numeric_table[numeric_table$name == "Missing", c("Positive", "Negative")],
      use.names = FALSE
    )),
    c("1 (  6.3%)", "1 (  6.3%)")
  )
  expect_equal(
    unname(unlist(
      categorical_table[categorical_table$name == "Yes", c("Positive", "Negative")],
      use.names = FALSE
    )),
    c("1 (  6.3%)", "1 (  6.3%)")
  )
})
