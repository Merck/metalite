default_population <- list(
  apat = adam_mapping(name = "apat", id = "USUBJID", label = "All Participants as Treated"),
  apr = adam_mapping(name = "apr", id = "USUBJID", label = "All Participants Randomized"),
  apt = adam_mapping(name = "apt", id = "USUBJID", label = "All Participants Treated"),
  asat = adam_mapping(name = "asat", id = "USUBJID", label = "All Subjects as Treated"),
  asr = adam_mapping(name = "asr", id = "USUBJID", label = "All Subjects Randomized"),
  ast = adam_mapping(name = "ast", id = "USUBJID", label = "All Subjects Treated")
)

default_observation <- list()

default_analysis <- list(
  ae_summary = adam_mapping(
    name = "ae_summary",
    label = "Table: adverse event summary",
    title = "adverse event summary"
  ),
  ae_specific = adam_mapping(
    name = "ae_specific",
    label = "Table: specific adverse event",
    title = "Participants With {term1} Adverse Events {term2}"
  ),
  ae_listing = adam_mapping(
    name = "ae_listing",
    label = "Listing: adverse event",
    title = "Listing of Participants With {term1} Adverse Events {term2}"
  ),
  ae_forest = adam_mapping(
    name = "ae_forest",
    label = "Figure: forest plot for adverse event",
    title = "Forest plot for Participants With {term1} Adverse Events {term2}"
  ),
  ae_forestly = adam_mapping(
    name = "ae_forest",
    label = "Interactive: forest plot for adverse event",
    title = "Forest plot for Participants With {term1} Adverse Events {term2}"
  )
)
