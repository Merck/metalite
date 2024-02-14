test_that("variable 'RACE' checking", {
  expect_equal(names(meta_example() |> meta_split("RACE")), sort(unique(meta_example()$data_population$RACE)))
})


test_that("variable 'RACE = WHITE' checking in data_population", {
  x <- meta_example() |> meta_split("RACE")
  z <- meta_example()$data_population %>% dplyr::filter(RACE == "WHITE")
  expect_equal(x$WHITE$data_population$USUBJID, unlist(as.list(z$USUBJID)))
})


test_that("variable 'RACE = BLACK OR AFRICAN AMERICAN' checking in data_population", {
  x <- meta_example() |> meta_split("RACE")
  z <- meta_example()$data_population %>% dplyr::filter(RACE == "BLACK OR AFRICAN AMERICAN")
  expect_equal(x$`BLACK OR AFRICAN AMERICAN`$data_population$USUBJID, unlist(as.list(z$USUBJID)))
})


test_that("variable 'RACE = AMERICAN INDIAN OR ALASKA NATIVE' checking in data_population", {
  x <- meta_example() |> meta_split("RACE")
  z <- meta_example()$data_population %>% dplyr::filter(RACE == "AMERICAN INDIAN OR ALASKA NATIVE")
  expect_equal(x$`AMERICAN INDIAN OR ALASKA NATIVE`$data_population$USUBJID, unlist(as.list(z$USUBJID)))
})


test_that("variable 'RACE = WHITE' checking in data_observation", {
  x <- meta_example() |> meta_split("RACE")
  z <- meta_example()$data_observation %>% dplyr::filter(RACE == "WHITE")
  expect_equal(x$WHITE$data_observation$USUBJID, unlist(as.list(z$USUBJID)))
})


test_that("variable 'RACE = BLACK OR AFRICAN AMERICAN' checking in data_observation", {
  x <- meta_example() |> meta_split("RACE")
  z <- meta_example()$data_observation %>% dplyr::filter(RACE == "BLACK OR AFRICAN AMERICAN")
  expect_equal(x$`BLACK OR AFRICAN AMERICAN`$data_observation$USUBJID, unlist(as.list(z$USUBJID)))
})


test_that("variable 'RACE = AMERICAN INDIAN OR ALASKA NATIVE' checking in data_observation", {
  x <- meta_example() |> meta_split("RACE")
  z <- meta_example()$data_observation %>% dplyr::filter(RACE == "AMERICAN INDIAN OR ALASKA NATIVE")
  expect_equal(x$`AMERICAN INDIAN OR ALASKA NATIVE`$data_observation$USUBJID, unlist(as.list(z$USUBJID)))
})
