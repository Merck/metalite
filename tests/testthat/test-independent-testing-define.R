plan1 <- plan(
  analysis = "ae_summary", population = "apat",
  observation = c("wk12", "wk24"), parameter = "any;rel;ser"
)

x <- meta_adam(
  population = r2rtf::r2rtf_adsl,
  observation = r2rtf::r2rtf_adae
) |>
  define_plan(plan1)

test_that("data frame plan is contained in the output list", {
  expect_equal(names(x)[3], "plan")
  expect_equal(class(x$plan)[2], "data.frame")
  expect_true("list" %in% typeof(x$plan))
})


y <- meta_adam(
  population = r2rtf::r2rtf_adsl,
  observation = r2rtf::r2rtf_adae
) |>
  define_plan(plan1) |>
  define_population(name = "apat")

# Test cases for define_population

test_that("Warning if one of name is not in the plan data frame of meta define_population", {
  expect_warning(y %>% define_population(name = "apat1"))
})


test_that("meta_adam class object with list population contains in the object at define_population", {
  expect_equal(names(y)[5], "population")
})


test_that("population contains a list of default argument at define_population", {
  expect_equal(names(y$population$apat), c("name", "id", "group", "var", "subset", "label"))
})


test_that("name,id,group,var,subset,label would be mapped to their input or default value accordingly in define_population", {
  expect_equal(y$population$apat$name, "apat")
  expect_equal(y$population$apat$id, "USUBJID")
  expect_equal(y$population$apat$group, NULL)
  expect_equal(y$population$apat$var, NULL)
  expect_equal(y$population$apat$subset, NULL)
  expect_equal(y$population$apat$label, "All Participants as Treated")
})


# Test cases for define_observation

z <- meta_adam(
  population = r2rtf::r2rtf_adsl,
  observation = r2rtf::r2rtf_adae
) |>
  define_plan(plan1) |>
  define_observation(
    name = "wk12",
    group = "TRTA",
    subset = ,
    label = "Weeks 0 to 12"
  )

test_that("warning if one of name is not in the plan data frame of meta define_observation", {
  expect_warning(z %>% define_observation(name = "wk36"))
})



test_that("meta_adam class object with list population contains in the object at define_observation", {
  expect_equal(names(z)[4], "observation")
})


test_that("population contains a list of default argument at define_observation", {
  expect_equal(names(z$observation$wk12), c("name", "id", "group", "var", "subset", "label"))
})


test_that("name,id,group,var,subset,label would be mapped to their input or default value accordingly in define_observation", {
  expect_equal(z$observation$wk12$name, "wk12")
  expect_equal(z$observation$wk12$id, "USUBJID")
  expect_equal(z$observation$wk12$group, "TRTA")
  expect_equal(z$observation$wk12$var, NULL)
  expect_equal(z$observation$wk12$subset, NULL)
  expect_equal(z$observation$wk12$label, "Weeks 0 to 12")
})

# Test cases for define_parameter

zz <- meta_adam(
  population = r2rtf::r2rtf_adsl,
  observation = r2rtf::r2rtf_adae
) |>
  define_plan(plan1) |>
  define_parameter(
    name = "rel",
    subset = AEREL %in% c("POSSIBLE", "PROBABLE")
  )

test_that("warning if one of name is not in the plan data frame of meta define_parameter", {
  expect_warning(zz %>% define_parameter(name = "disc"))
})

test_that("meta_adam class object with list population contains in the object at define_parameter", {
  expect_equal(names(zz)[6], "parameter")
})

test_that("population contains a list of default argument at define_parameter", {
  expect_equal(names(zz$parameter$rel), c("name", "id", "group", "var", "subset", "label", "soc", "seq", "term1", "term2", "summ_row", "summ_foot"))
})

test_that("name,subset,label would be mapped to their input or default value accordingly in define_parameter", {
  expect_equal(
    c(zz$parameter$rel$name, zz$parameter$rel$subset, zz$parameter$rel$label),
    c("rel", quote(AEREL %in% c("POSSIBLE", "PROBABLE")), "drug-related adverse events")
  )
})


# Test cases for define_analysis

pp <- meta_adam(
  population = r2rtf::r2rtf_adsl,
  observation = r2rtf::r2rtf_adae
) |>
  define_plan(plan1) |>
  define_analysis(
    name = "ae_summary",
    title = "Summary of Adverse Events"
  )

test_that("warning if one of name is not in the plan data frame of meta define_analysis", {
  expect_warning(pp %>% define_analysis(name = "ae_specific"))
})

test_that("meta_adam class object with list population contains in the object at define_analysis", {
  expect_equal(names(pp)[7], "analysis")
})


test_that("population contains a list of default argument at define_analysis", {
  expect_equal(names(pp$analysis$ae_summary), c("name", "id", "group", "var", "subset", "label", "title"))
})

test_that("name,label,title would be mapped to their input or default value accordingly in define_analysis", {
  expect_equal(
    c(pp$analysis$ae_summary$name, pp$analysis$ae_summary$label, pp$analysis$ae_summary$title),
    c("ae_summary", "Table: adverse event summary", "Summary of Adverse Events")
  )
})
