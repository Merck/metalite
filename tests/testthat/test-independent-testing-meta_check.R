test_that("variable 'RACE' checking", {
  p <- meta_check_var(meta_example(), var = "RACE", type = c("observation"))
  expect_true("RACE" %in% names(p$data_observation))
})



test_that("variable 'AEDECOD' checking in population", {
  expect_error(meta_check_var(meta_example(), var = "AEDECOD", type = c("population")))
})

test_that("variable 'AEDECOD' checking in observation", {
  q <- meta_check_var(meta_example(), var = "AEDECOD", type = c("observation"))
  expect_true("AEDECOD" %in% names(q$data_observation))
})



test_that("variable 'BMIBL' checking in population or observation", {
  expect_error(meta_check_var(meta_example(), var = "BMIBL", type = c("population", "observation")))
})


test_that("variable 'BMIBL' checking in population", {
  l <- meta_check_var(meta_example(), var = "BMIBL", type = c("population"))
  expect_true("BMIBL" %in% names(l$data_population))
})

test_that("variable 'BMIBL' checking in obervation", {
  expect_error(meta_check_var(meta_example(), var = "BMIBL", type = c("observation")))
})
