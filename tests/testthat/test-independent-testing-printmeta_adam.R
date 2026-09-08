adsl <- r2rtf::r2rtf_adsl
adsl$TRTA <- adsl$TRT01A
adsl$TRTA <- factor(adsl$TRTA,
  levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
)

adae <- r2rtf::r2rtf_adae
adae$TRTA <- factor(adae$TRTA,
  levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
)

plan <- plan(
  analysis = "ae_summary", population = "apat",
  observation = c("wk12"), parameter = "rel"
)

meta <- meta_adam(
  population = adsl,
  observation = adae
) |>
  define_plan(plan = plan) |>
  define_population(
    name = "apat",
    group = "TRTA",
    subset = quote(SAFFL == "Y")
  ) |>
  define_observation(
    name = "wk12",
    group = "TRTA",
    subset = quote(SAFFL == "Y"),
    label = "Weeks 0 to 12"
  ) |>
  define_analysis(
    name = "ae_summary",
    title = "Summary of Adverse Events"
  )


test_that("meta print", {
  out <- capture.output(print(meta))
  expect_true(any(grepl("ADaM metadata", out, fixed = TRUE)))
  expect_true(any(grepl("Population data with 254 subjects", out, fixed = TRUE)))
  expect_true(any(grepl("Observation data with 1191 records", out, fixed = TRUE)))
  expect_true(any(grepl("Analysis plan with 1 plans", out, fixed = TRUE)))
  expect_true(any(grepl("'apat'", out, fixed = TRUE)))
  expect_true(any(grepl("'wk12'", out, fixed = TRUE)))
  expect_true(any(grepl("'ae_summary'", out, fixed = TRUE)))
})
