
test_that("variable 'RACE' checking", {
  p <- meta_check_var(meta_dummy(), var = "RACE", type = c("observation"))
  expect_equal(names(p$data_observation)[9],"RACE")
}) 



test_that("variable 'AEDECOD' checking in population", {
  expect_error(meta_check_var(meta_dummy(), var = "AEDECOD", type = c("population")))
  }) 

test_that("variable 'AEDECOD' checking in observation", {
  q <- meta_check_var(meta_dummy(), var = "AEDECOD", type = c("observation"))
  expect_equal(names(q$data_observation)[25],"AEDECOD")
}) 



test_that("variable 'BMIBL' checking in population or observation", {
  expect_error(meta_check_var(meta_dummy(), var = "BMIBL", type = c("population", "observation")))
  
}) 


test_that("variable 'BMIBL' checking in population", {
  l <- meta_check_var(meta_dummy(), var = "BMIBL", type = c("population"))
  expect_equal(names(l$data_population)[33],"BMIBL")
}) 

test_that("variable 'BMIBL' checking in obervation", {
  expect_error(meta_check_var(meta_dummy(), var = "BMIBL", type = c("observation")))
}) 