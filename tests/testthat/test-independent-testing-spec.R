# test spec_filename()
test_that("spec_filename: verify correct filenames in meta_example", {
  filenames <- c(
    "ae0summary0wk12.rtf", "ae0summary0wk24.rtf",
    "ae0specific0wk120any.rtf", "ae0specific0wk240any.rtf",
    "ae0specific0wk120aeosi.rtf", "ae0specific0wk240aeosi.rtf",
    "ae0specific0wk120rel.rtf", "ae0specific0wk240rel.rtf",
    "ae0specific0wk120ser.rtf", "ae0specific0wk240ser.rtf"
  )
  expect_equal(spec_filename(meta_example()), filenames)
})

# test spec_analysis_population()
test_that("spec_analysis_population: verify correct analysis specs in meta_example", {
  analysis_spec <- c(
    "Population: SAFFL == 'Y'\nObservation: SAFFL == 'Y'",
    "Population: SAFFL == 'Y'\nObservation: AOCC01FL == 'Y'",
    "Population: SAFFL == 'Y'\nObservation: SAFFL == 'Y'",
    "Population: SAFFL == 'Y'\nObservation: AOCC01FL == 'Y'",
    "Population: SAFFL == 'Y'\nObservation: SAFFL == 'Y' AEOSI == 'Y'",
    "Population: SAFFL == 'Y'\nObservation: AOCC01FL == 'Y' AEOSI == 'Y'",
    "Population: SAFFL == 'Y'\nObservation: SAFFL == 'Y' AEREL %in% c('POSSIBLE', 'PROBABLE')",
    "Population: SAFFL == 'Y'\nObservation: AOCC01FL == 'Y' AEREL %in% c('POSSIBLE', 'PROBABLE')",
    "Population: SAFFL == 'Y'\nObservation: SAFFL == 'Y' AESER == 'Y'",
    "Population: SAFFL == 'Y'\nObservation: AOCC01FL == 'Y' AESER == 'Y'"
  )
  expect_equal(spec_analysis_population(meta_example()), analysis_spec)
})

# test spec_call_program()
test_that("spec_call_program: verify correct call programs in meta_example", {
  program_spec <- c(
    "ae_summary(meta = meta_example(), population = 'apat', observation = 'wk12', parameter = 'any;rel;ser')",
    "ae_summary(meta = meta_example(), population = 'apat', observation = 'wk24', parameter = 'any;rel;ser')",
    "ae_specific(meta = meta_example(), population = 'apat', observation = 'wk12', parameter = 'any')",
    "ae_specific(meta = meta_example(), population = 'apat', observation = 'wk24', parameter = 'any')",
    "ae_specific(meta = meta_example(), population = 'apat', observation = 'wk12', parameter = 'aeosi')",
    "ae_specific(meta = meta_example(), population = 'apat', observation = 'wk24', parameter = 'aeosi')",
    "ae_specific(meta = meta_example(), population = 'apat', observation = 'wk12', parameter = 'rel')",
    "ae_specific(meta = meta_example(), population = 'apat', observation = 'wk24', parameter = 'rel')",
    "ae_specific(meta = meta_example(), population = 'apat', observation = 'wk12', parameter = 'ser')",
    "ae_specific(meta = meta_example(), population = 'apat', observation = 'wk24', parameter = 'ser')"
  )
  expect_equal(spec_call_program(meta_example()), program_spec)
})

# test spec_title()
test_that("spec_title: verify correct titles in meta_example", {
  title_spec <- c(
    "Summary of Adverse Events\nWeeks 0 to 12\nAll Participants as Treated",
    "Summary of Adverse Events\nWeeks 0 to 24\nAll Participants as Treated",
    "Participants With Adverse Events\nWeeks 0 to 12\nAll Participants as Treated",
    "Participants With Adverse Events\nWeeks 0 to 24\nAll Participants as Treated",
    "Participants With Adverse Events of special interest\nWeeks 0 to 12\nAll Participants as Treated",
    "Participants With Adverse Events of special interest\nWeeks 0 to 24\nAll Participants as Treated",
    "Participants With Drug-Related Adverse Events\nWeeks 0 to 12\nAll Participants as Treated",
    "Participants With Drug-Related Adverse Events\nWeeks 0 to 24\nAll Participants as Treated",
    "Participants With Serious Adverse Events\nWeeks 0 to 12\nAll Participants as Treated",
    "Participants With Serious Adverse Events\nWeeks 0 to 24\nAll Participants as Treated"
  )
  expect_equal(spec_title(meta_example()), title_spec)
})
