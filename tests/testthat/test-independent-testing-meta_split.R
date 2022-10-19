library(dplyr)

test_that("variable 'RACE' checking", {
 expect_equal(names(meta_dummy() |> meta_split("RACE")), sort(unique(meta_dummy()$data_population$RACE)))
}) 


test_that("variable 'RACE = WHITE' checking in data_population", {
  x <- meta_dummy() |> meta_split("RACE")
  z <- meta_dummy()$data_population %>% filter(RACE=="WHITE")
  expect_equal(x$WHITE$data_population$USUBJID, unlist(as.list(z$USUBJID)))
}) 


test_that("variable 'RACE = BLACK OR AFRICAN AMERICAN' checking in data_population", {
  x <- meta_dummy() |> meta_split("RACE")
  z <- meta_dummy()$data_population %>% filter(RACE=="BLACK OR AFRICAN AMERICAN")
  expect_equal(x$`BLACK OR AFRICAN AMERICAN`$data_population$USUBJID, unlist(as.list(z$USUBJID)))
}) 


test_that("variable 'RACE = AMERICAN INDIAN OR ALASKA NATIVE' checking in data_population", {
  x <- meta_dummy() |> meta_split("RACE")
  z <- meta_dummy()$data_population %>% filter(RACE=="AMERICAN INDIAN OR ALASKA NATIVE")
    expect_equal(x$`AMERICAN INDIAN OR ALASKA NATIVE`$data_population$USUBJID, unlist(as.list(z$USUBJID)))
}) 


test_that("variable 'RACE = WHITE' checking in data_observation", {
  x <- meta_dummy() |> meta_split("RACE")
  z <- meta_dummy()$data_observation %>% filter(RACE=="WHITE")
  expect_equal(x$WHITE$data_observation$USUBJID, unlist(as.list(z$USUBJID)))
}) 


test_that("variable 'RACE = BLACK OR AFRICAN AMERICAN' checking in data_observation", {
  x <- meta_dummy() |> meta_split("RACE")
  z <- meta_dummy()$data_observation %>% filter(RACE=="BLACK OR AFRICAN AMERICAN")
  expect_equal(x$`BLACK OR AFRICAN AMERICAN`$data_observation$USUBJID, unlist(as.list(z$USUBJID)))
}) 


test_that("variable 'RACE = AMERICAN INDIAN OR ALASKA NATIVE' checking in data_observation", {
  x <- meta_dummy() |> meta_split("RACE")
  z <- meta_dummy()$data_observation %>% filter(RACE=="AMERICAN INDIAN OR ALASKA NATIVE")
  expect_equal(x$`AMERICAN INDIAN OR ALASKA NATIVE`$data_observation$USUBJID, unlist(as.list(z$USUBJID)))
}) 