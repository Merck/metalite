# Testing Quote Function
test_that("Testing of Quote", {
  expect_equal(fmt_quote('"b"'), "'b'")
  expect_equal(fmt_quote("b"), "b")
  expect_equal(fmt_quote("b"), "b")

  expect_equal(fmt_quote('"a" and "b"'), "'a' and 'b'")
})

# Testing Format_sentense
test_that("Testing of Quote", {
  expect_equal(fmt_sentence(" a "), "a")
  expect_equal(fmt_sentence(" a and b"), "a and b")

  expect_equal(fmt_sentence(" a and b "), "a and b")
  expect_equal(fmt_sentence("a and b "), "a and b")
})
