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
  testthat::expect_snapshot(meta |> print())
})
