test_that("Get label for only tbl with out label", {
  tbl <- data.frame(a = c(1, 2, 3), b = c(-1, -2, -3))
  expect_equal(get_label(tbl)[[1]], c("a"))
  expect_equal(get_label(tbl)[[2]], c("b"))
})


test_that("Get label for tbl with label", {
  tbl <- data.frame(a = c(1, 2, 3), b = c(-1, -2, -3))
  attr(tbl[[1]], "label") <- "variable 1"
  attr(tbl[[2]], "label") <- "variable 2"

  expect_equal(get_label(tbl)[[1]], c("variable 1"))
  expect_equal(get_label(tbl)[[2]], c("variable 2"))
})


test_that("Assign multiple labels for single variable", {
  tbl <- data.frame(a = c(1, 2, 3), b = c(-1, -2, -3))
  expect_error(tbl <- assign_label(tbl, var = "a", label = c("variable 1", "variable 2")))
})

test_that("Assign labels for duplicate variables", {
  tbl <- data.frame(a = c(1, 2, 3), b = c(-1, -2, -3))
  expect_error(tbl <- assign_label(tbl, var = c("a", "a"), label = c("variable 1", "variable 2")))
})


# test_that("Assign labels for single variable", {
#   tbl <- data.frame(a = c(1, 2, 3), b = c(-1, -2, -3))
#   expect_message(tbl <- assign_label(tbl, var = "a", label = "variable 1"))
# })


test_that("Assign labels for all variable", {
  tbl <- data.frame(a = c(1, 2, 3), b = c(-1, -2, -3))
  tbl <- assign_label(tbl, var = c("a", "b"), label = c("variable 1", "variable 2"))
  expect_equal(get_label(tbl)[[1]], c("variable 1"))
  expect_equal(get_label(tbl)[[2]], c("variable 2"))
})
