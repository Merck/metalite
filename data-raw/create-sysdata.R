source("data-raw/default.R")
source("data-raw/default_parameter_ae.R")

usethis::use_data(
  default_population,
  default_observation,
  default_parameter_ae,
  default_analysis,
  internal = TRUE,
  overwrite = TRUE
)
