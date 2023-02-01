test_that("check duplicate names", {
  expect_warning(check_duplicate_name(list(a = 1, b = 2, b = 3)))
  expect_warning(check_duplicate_name(list(a = 1, b = 2, b = 2)))
})

test_that("check omit null", {
  expect_true(omit_null(list(a = 1, b = NULL))$a == 1)
  expect_equal(omit_null(list(a = 10, b = 10, c = NULL)), list(a = 10, b = 10))
})

test_that("check format quote", {
  expect_equal(fmt_quote('"xy"'), "'xy'")
  expect_equal(fmt_quote("x y"), "x y")
  expect_equal(fmt_quote("z z"), "z z")
})

test_that("check format sentence", {
  expect_equal(fmt_sentence("  a b c   "), "a b c")
  expect_equal(fmt_sentence(" John Marshal's "), "John Marshal's")
})
