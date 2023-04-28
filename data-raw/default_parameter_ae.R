# Default parameter values for adverse events analysis
default_parameter_ae <- list(
  # Define all item in first item
  any = adam_mapping(
    name = "any",
    label = "any adverse events",
    subset = NULL,
    var = "AEDECOD",
    soc = "AEBODSYS",
    seq = 100,
    term1 = "",
    term2 = "",
    summ_row = "with one or more adverse events",
    summ_foot = NULL
  ),
  rel = adam_mapping(
    name = "rel",
    label = "drug-related adverse events",
    subset = quote(toupper(AREL) == "RELATED"),
    var = "AEDECOD",
    soc = "AEBODSYS",
    seq = 200,
    term1 = "Drug-Related",
    term2 = "",
    summ_row = "with drug-related{^a} adverse events",
    summ_foot = "{^a}Determined by the investigator to be related to the drug."
  ),
  g34 = adam_mapping(
    name = "g34",
    label = "grade 3-4 adverse events",
    subset = quote(ATOXGRN %in% c(3, 4)),
    var = "AEDECOD",
    soc = "AEBODSYS",
    seq = 250,
    term1 = "Grade 3-4",
    term2 = "",
    summ_row = "with grade 3-4 adverse events"
  ),
  g340rel = adam_mapping(
    name = "g340rel",
    label = "drug-related grade 3-4 adverse events",
    subset = quote(ATOXGRN %in% c(3, 4) & AREL == "Y"),
    var = "AEDECOD",
    soc = "AEBODSYS",
    seq = 251,
    term1 = "Drug-Related Grade 3-4",
    term2 = "",
    summ_row = "with grade 3-4 adverse events"
  ),
  g35 = adam_mapping(
    name = "g35",
    label = "grade 3-5 adverse events",
    subset = quote(ATOXGRN %in% c(3, 4, 5)),
    var = "AEDECOD",
    soc = "AEBODSYS",
    seq = 300,
    term1 = "Grade 3-5",
    term2 = "",
    summ_row = "with grade 3-5 adverse events"
  ),
  g350rel = adam_mapping(
    name = "g350rel",
    label = "drug-related grade 3-5 adverse events",
    subset = quote(ATOXGRN %in% c(3, 4, 5) & AREL == "Y"),
    var = "AEDECOD",
    soc = "AEBODSYS",
    seq = 301,
    term1 = "Drug-Related Grade 3-5",
    term2 = "",
    summ_row = "with grade 3-5 adverse events"
  ),
  nonser = adam_mapping(
    name = "nonser",
    label = "non-serious adverse events",
    subset = quote(AESER != "Y" | is.na(AESER)),
    var = "AEDECOD",
    soc = "AEBODSYS",
    seq = 400,
    term1 = "Non-Serious",
    term2 = "",
    summ_row = "with non-serious adverse events"
  ),
  ser = adam_mapping(
    name = "ser",
    label = "serious adverse events",
    subset = quote(AESER == "Y"),
    var = "AEDECOD",
    soc = "AEBODSYS",
    seq = 401,
    term1 = "Serious",
    term2 = "",
    summ_row = "with serious adverse events"
  ),
  ser0rel = adam_mapping(
    name = "ser0rel",
    label = "serious drug-related adverse events",
    subset = quote(AESER == "Y" & AREL == "Y"),
    var = "AEDECOD",
    soc = "AEBODSYS",
    seq = 402,
    term1 = "Serious Drug-Related",
    term2 = "",
    summ_row = "with serious drug-related adverse events"
  ),
  mod = adam_mapping(
    name = "mod",
    label = "adverse events result in dose modification",
    subset = quote(toupper(AEACN) %in% c("DOSE REDUCED", "DRUG INTERRUPTED", "DRUG WITHDRAWN")),
    var = "AEDECOD",
    soc = "AEBODSYS",
    seq = 500,
    term1 = "",
    term2 = "Resulting Dose Modification",
    summ_row = "with any does modification {^b} due to an adverse event",
    summ_foot = "{^b}Defined as an action taken of dose reduced, drug interrupted or drug withdrawn."
  ),
  dth = adam_mapping(
    name = "dth",
    label = "adverse events result in death",
    subset = quote(AESDTH == "Y"),
    var = "AEDECOD",
    soc = "AEBODSYS",
    seq = 600,
    term1 = "",
    term2 = "Resulting in Death",
    summ_row = "who died"
  ),
  dtc0rel = adam_mapping(
    name = "dtc0rel",
    label = "drug-related adverse events result in death",
    subset = quote(AESDTH == "Y" & AREL == "Y"),
    var = "AEDECOD",
    soc = "AEBODSYS",
    seq = 601,
    term1 = "Drug-Related",
    term2 = "Resulting in Death",
    summ_row = "who died due to a drug-related adverse event"
  ),
  disc = adam_mapping(
    name = "disc",
    label = "adverse events resulting in discontinuation",
    subset = quote(toupper(AEACN) == "DRUG WITHDRAWN"),
    var = "AEDECOD",
    soc = "AEBODSYS",
    seq = 700,
    term1 = "",
    term2 = "Resulting in Discontinuation",
    summ_row = "discontinued any drug due to an adverse events"
  ),
  disc0drel = adam_mapping(
    name = "disc0drel",
    label = "drug-related adverse events resulting in discontinuation",
    subset = quote(toupper(AEACN) == "DRUG WITHDRAWN" & AREL == "Y"),
    var = "AEDECOD",
    soc = "AEBODSYS",
    seq = 750,
    term1 = "Drug-Related",
    term2 = "Resulting in Discontinuation",
    summ_row = "discontinued any drug due to a drug-related adverse events"
  ),
  disc0ser = adam_mapping(
    name = "disc0ser",
    label = "serious adverse events resulting in discontinuation",
    subset = quote(toupper(AEACN) == "DRUG WITHDRAWN" & AESER == "Y"),
    var = "AEDECOD",
    soc = "AEBODSYS",
    seq = 800,
    term1 = "Serious",
    term2 = "Resulting in Discontinuation",
    summ_row = "discontinued any drug due to a serious adverse event"
  ),
  disc0ser0rel = adam_mapping(
    name = "disc0ser0rel",
    label = "serious drug-related adverse events resulting in discontinuation",
    subset = quote(toupper(AEACN) == "DRUG WITHDRAWN" & AESER == "Y" & AREL == "Y"),
    var = "AEDECOD",
    soc = "AEBODSYS",
    seq = 850,
    term1 = "Serious Drug-Related",
    term2 = "Resulting in Discontinuation",
    summ_row = "discontinued any drug due to a serious drug-related adverse event"
  ),
  inj = adam_mapping(
    name = "inj",
    label = "injection-site",
    subset = quote(toupper(AECAT) == "I"),
    var = "AEDECOD",
    soc = "AEBODSYS",
    seq = 150,
    term1 = "",
    term2 = "",
    summ_row = "injection-site"
  ),
  noninj = adam_mapping(
    name = "noninj",
    label = "non-injection-site",
    subset = quote(toupper(AECAT) != "I" | is.na(AECAT)),
    var = "AEDECOD",
    soc = "AEBODSYS",
    seq = 151,
    term1 = "",
    term2 = "",
    summ_row = "non-injection-site"
  ),
  inj0rel = adam_mapping(
    name = "inj0rel",
    label = "drug-related injection-site",
    subset = quote(toupper(AECAT) == "I" & AREL == "Y"),
    var = "AEDECOD",
    soc = "AEBODSYS",
    seq = 250,
    term1 = "",
    term2 = "",
    summ_row = "injection-site"
  ),
  noninj0rel = adam_mapping(
    name = "noninj0rel",
    label = "drug-related non-injection-site",
    subset = quote((toupper(AECAT) == "I" | is.na(AECAT)) & AREL == "Y"),
    var = "AEDECOD",
    soc = "AEBODSYS",
    seq = 251,
    term1 = "",
    term2 = "",
    summ_row = "non-injection-site"
  )
)
