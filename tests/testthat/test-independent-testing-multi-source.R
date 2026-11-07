# Tests for multiple source datasets in a single `meta_adam` object (GH #174).

# A small lab-like dataset that shares subject IDs with the population data but
# carries variables that do NOT exist in the observation (AE) data.
make_meta_multi_source <- function() {
  adsl <- r2rtf::r2rtf_adsl
  adae <- r2rtf::r2rtf_adae
  adlb <- data.frame(
    USUBJID = adsl$USUBJID,
    TRTA = adsl$TRT01A,
    PARAMCD = "ALT",
    AVAL = seq_len(nrow(adsl)),
    SAFFL = "Y",
    stringsAsFactors = FALSE
  )
  adsl$TRTA <- adsl$TRT01A
  adae$TRTA <- adae$TRTA

  plan <- plan(
    analysis = "ae_specific", population = "apat",
    observation = "apat", parameter = "rel"
  ) |>
    add_plan(
      analysis = "lab_box", population = "apat",
      observation = "lb", parameter = "alt"
    )

  # `observation` names two datasets separated by ";"; they are get()'d by name.
  meta_adam(population = "adsl", observation = "adae;adlb") |>
    define_plan(plan) |>
    define_analysis(name = "ae_specific", label = "AE") |>
    define_analysis(name = "lab_box", label = "Lab") |>
    define_population(
      name = "apat", group = "TRTA", id = "USUBJID",
      subset = SAFFL == "Y", label = "APaT"
    ) |>
    define_observation(
      name = "apat", group = "TRTA",
      subset = SAFFL == "Y", label = "AE", var = "AEDECOD"
    ) |>
    define_observation(
      name = "lb", group = "TRTA", from = "adlb",
      subset = SAFFL == "Y", label = "Lab", var = "AVAL"
    ) |>
    define_parameter(
      name = "rel", subset = AEREL %in% c("POSSIBLE", "PROBABLE"),
      var = "AEDECOD", label = "Rel"
    ) |>
    define_parameter(
      name = "alt", subset = PARAMCD == "ALT",
      var = "AVAL", label = "ALT"
    ) |>
    meta_build()
}

test_that("meta_adam registers extra datasets from a ';'-separated string", {
  meta <- make_meta_multi_source()
  expect_equal(meta$data_observation_name, "adae")
  expect_equal(meta$data_population_name, "adsl")
  expect_true("adlb" %in% names(meta$data_source))
  expect_setequal(
    metalite:::meta_source_names(meta),
    c("population", "observation", "adsl", "adae", "adlb")
  )
})

test_that("define_observation records the `from` source", {
  meta <- make_meta_multi_source()
  expect_equal(meta$observation$apat$from, "observation")
  expect_equal(meta$observation$lb$from, "adlb")
})

test_that("collect_observation_record routes to the declared source dataset", {
  meta <- make_meta_multi_source()

  ae <- collect_observation_record(meta, "apat", "apat", "rel")
  lb <- collect_observation_record(meta, "apat", "lb", "alt")

  # AE records come from adae (has AEDECOD), lab records from adlb (has AVAL)
  expect_true("AEDECOD" %in% names(ae))
  expect_true("AVAL" %in% names(lb))
  expect_false(identical(nrow(ae), nrow(lb)))
})

test_that("meta_adam accepts a named list of datasets", {
  adsl <- r2rtf::r2rtf_adsl
  adae <- r2rtf::r2rtf_adae
  meta <- meta_adam(
    population = adsl,
    observation = list(adae = adae, adsl2 = adsl)
  )
  expect_equal(meta$data_observation_name, "adae")
  expect_true("adsl2" %in% names(meta$data_source))
  expect_equal(nrow(meta$data_observation), nrow(adae))
})

test_that("the primary dataset name resolves through `from`", {
  meta <- make_meta_multi_source()
  # `from = "adae"` (the primary observation name) resolves to the live slot
  expect_identical(
    collect_data_source(meta, "adae", default = "observation"),
    meta$data_observation
  )
  expect_identical(
    collect_data_source(meta, "observation", default = "observation"),
    meta$data_observation
  )
})

test_that("define_observation errors on an unregistered `from`", {
  adsl <- r2rtf::r2rtf_adsl
  adae <- r2rtf::r2rtf_adae
  plan <- plan(
    analysis = "ae", population = "apat",
    observation = "lb", parameter = "any"
  )
  expect_error(
    meta_adam(population = adsl, observation = adae) |>
      define_plan(plan) |>
      define_observation(name = "lb", from = "does_not_exist"),
    "not a registered source dataset"
  )
})

test_that("meta_adam errors on a non-data-frame dataset name", {
  adae <- r2rtf::r2rtf_adae
  not_a_df <- 1:10
  expect_error(
    meta_adam(observation = "not_a_df"),
    "not a data frame"
  )
})

test_that("meta_adam errors on an unnamed list of datasets", {
  adsl <- r2rtf::r2rtf_adsl
  adae <- r2rtf::r2rtf_adae
  expect_error(
    meta_adam(population = adsl, observation = list(adae, adsl)),
    "must be named"
  )
})

test_that("backward compatibility: single-source metadata is unaffected", {
  meta <- meta_example()
  expect_equal(meta$data_source, list())
  # default `from` resolves to the primary observation/population slots
  expect_identical(
    collect_data_source(meta, meta$observation$wk12$from, default = "observation"),
    meta$data_observation
  )
})
