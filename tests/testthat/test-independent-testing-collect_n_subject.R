test_that("test n_subject can calculate number of records by group in diffrent `use_na` situation", {
  df1 <- data.frame(
    id = c(1, 2, 3, 4, 5, 6, 7),
    trta = c("A", "A", "B", "B", "B", "A", "B"),
    sex = c("M", "F", "M", "M", NA, NA, "F"),
    agegrp = c(1, 2, 3, 2, 1, NA, NA)
  )
  # Throw error message if group variable is not factor
  expect_error(n_subject(df1$id, df$trta))
  # use_na = "ifany"
  df1$trta <- factor(df1$trta)
  act1 <- n_subject(df1$id, df1$trta, df1$sex, use_na = "ifany")
  exp1 <- data.frame(
    name = c("F", "M", "Missing"),
    A = c(1, 1, 1),
    B = c(1, 2, 1)
  )
  expect_equal(exp1, act1)
  # use_na = "no"
  act2 <- n_subject(df1$id, df1$trta, df1$sex, use_na = "no")
  exp2 <- data.frame(
    name = c("F", "M"),
    A = c(1, 1),
    B = c(1, 2)
  )
  expect_equal(exp2, act2)
  # use_na = "always"
  act3 <- n_subject(df1$id, df1$trta, df1$sex, use_na = "always")
  exp3 <- data.frame(
    name = c("F", "M", "Missing"),
    A = c(1, 1, 1),
    B = c(1, 2, 1),
    Missing = c(0, 0, 0)
  )
  expect_equal(exp3, act3)

  df1$agegrp <- factor(df1$agegrp)
  df1$sex <- factor(df1$sex)
  act4 <- n_subject(df1$id, df1$sex, df1$agegrp, use_na = "always")
  exp4 <- data.frame(
    "name" = c(1, 2, 3, "Missing"),
    "F" = c(0, 1, 0, 1),
    "M" = c(1, 1, 1, 0),
    "Missing" = c(1, 0, 0, 1)
  )
  expect_equal(exp4, act4)
})


test_that("test meta_remove_blank_group", {
  # parameter needs to be a factor with a level that did not contain any records.
  # e.g. x <- factor(c("a", "b"), c("a", "b", "c"))

  # then the function can remove group "c" in the analysis.

  adsl <- r2rtf::r2rtf_adsl |> subset(TRT01A %in% c("Placebo", "Xanomeline Low Dose"))
  adsl$TRT01A <- factor(adsl$TRT01A,
    levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
  )

  adae <- r2rtf::r2rtf_adae |> subset(TRTA %in% c("Placebo", "Xanomeline Low Dose"))
  adae$TRT01A <- factor(adae$TRTA,
    levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
  )

  plan <- plan(
    analysis = "ae_summary", population = "apat",
    observation = c("wk12"), parameter = "group"
  )

  meta <- meta_adam(
    population = adsl,
    observation = adae
  ) |>
    define_plan(plan = plan) |>
    define_population(
      name = "apat",
      group = "TRT01A",
      subset = quote(SAFFL == "Y")
    ) |>
    define_observation(
      name = "wk12",
      group = "TRT01A",
      subset = quote(SAFFL == "Y"),
      label = "Weeks 0 to 12"
    ) |>
    define_parameter(
      name = "group",
      var = "TRT01A",
      label = "group"
    ) |>
    meta_build()
  # levels(meta$data_population$TRTA)
  meta <- meta_remove_blank_group(meta, "apat", "group")
  expect_equal(levels(meta$data_population$TRT01A), c("Placebo", "Xanomeline Low Dose"))
  expect_equal(levels(meta$data_observation$TRT01A), c("Placebo", "Xanomeline Low Dose"))


  adsl <- r2rtf::r2rtf_adsl
  adsl$TRT01A <- factor(adsl$TRT01A,
    levels = c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo")
  )
  adsl$TRT01A[100] <- NA

  adae <- r2rtf::r2rtf_adae
  adae$TRT01A <- factor(adae$TRTA,
    levels = c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo")
  )
  adae$TRT01A[100] <- NA

  plan <- plan(
    analysis = "ae_summary", population = "apat",
    observation = c("wk12"), parameter = "group"
  )

  meta <- meta_adam(
    population = adsl,
    observation = adae
  ) |>
    define_plan(plan = plan) |>
    define_population(
      name = "apat",
      group = "TRT01A",
      subset = quote(SAFFL == "Y")
    ) |>
    define_observation(
      name = "wk12",
      group = "TRT01A",
      subset = quote(SAFFL == "Y"),
      label = "Weeks 0 to 12"
    ) |>
    define_parameter(
      name = "group",
      var = "TRT01A",
      label = "group"
    ) |>
    meta_build()
  meta <- meta_remove_blank_group(meta, "apat", "group")

  meta <- meta_remove_blank_group(meta, "apat", "group")
  expect_equal(levels(meta$data_population$TRT01A), c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo"))
  expect_equal(levels(meta$data_observation$TRT01A), c("Xanomeline Low Dose", "Xanomeline High Dose", "Placebo"))
})


test_that("test collect_n_subject with continous parameter", {
  # test different combination of parameters
  suppressWarnings(
    meta <- meta_example() |>
      define_parameter(name = "age", var = "AGE", label = "Age")
  )
  test1 <- collect_n_subject(meta, "apat", "age")
  ## test case1
  ## test test1$n
  meta_add <- meta_add_total(meta)
  pop <- collect_population_record(meta_add, "apat", "AGE")
  Total <- length(unique(pop$USUBJID))
  pop_all <- n_subject(pop$USUBJID, pop$TRTA)
  pop_all <- data.frame(
    name = "Number of Subjects",
    pop_all
  )
  names(pop_all) <- c("name", "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose", "Total")
  expect_equal(test1$n, pop_all, ignore_attr = TRUE)
  ## test test1$table
  pop_all[2, ] <- c("Age", NA, NA, NA, NA)

  pop_all_with_data <- n_subject(pop$USUBJID, pop$TRTA, par = factor(
    is.na(pop[["AGE"]]),
    c(FALSE, TRUE), c("Subjects with Data", "Missing")
  ))
  names(pop_all_with_data) <- c("name", "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose", "Total")

  pop_age <- tapply(pop$AGE, pop$TRTA, function(x) {
    value <- c(
      mean = mean(x, na.rm = TRUE),
      sd = stats::sd(x, na.rm = TRUE),
      median = stats::median(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE)
    )
    value <- formatC(value, format = "f", digits = 1)
    c(glue::glue("{value[['mean']]} ({value[['sd']]})"), glue::glue("{value[['median']]} [{value[['min']]}, {value[['max']]}]"))
  })
  pop_age <- data.frame(
    name = c("Mean (SD)", "Median [Min, Max]"),
    do.call(cbind, pop_age)
  )

  names(pop_age) <- c("name", "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose", "Total")

  pop_pct <- pop_all_with_data
  for (i in 2:ncol(pop_all_with_data)) {
    pct <- formatC(pop_all_with_data[, i] / as.numeric(pop_all[1, i]) * 100, format = "f", digits = 1, width = 5)
    pop_pct[, i] <- glue::glue("{pop_all_with_data[,i]} ({pct}%)")
  }

  pop_n <- rbind(pop_all, pop_all_with_data[1, ], pop_age, pop_pct[2, ])
  expect_equal(test1$table, pop_n[-nrow(pop_n), ], ignore_attr = TRUE)
  ## test test1$subset
  pop_subset <- collect_adam_mapping(meta, "apat")$subset
  pop_subset <- fmt_quote(deparse(pop_subset))
  grp_var <- collect_adam_mapping(meta, "apat")$group
  grp_name <- pop[[grp_var]]

  subset <- data.frame(name = c("Number of Subjects", "Subjects with Data", NA))
  subset_condition <- c("TRUE", "(! is.na(AGE))", "is.na(AGE)")
  for (i in c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")) {
    group_subset <- paste0("TRTA == ", "'", i, "'")
    subset[[i]] <- sapply(FUN = paste, subset_condition, group_subset, pop_subset, sep = " & ")
  }
  expect_equal(test1$subset, subset)
  expect_equal(test1$listing, NULL)
  expect_equal(test1$histogram, NULL)

  suppressWarnings(
    meta <- meta_example() |>
      define_parameter(name = "age", var = "AGE", label = "Age")
  )

  test1_no <- collect_n_subject(meta, "apat", "age", use_na = "no")
  expect_equal(test1$table, test1_no$table)

  test1_always <- collect_n_subject(meta, "apat", "age", use_na = "always")
  expect_equal(test1_always$table, pop_n, ignore_attr = TRUE)

  ## test case2: test histogram and listing
  test2 <- collect_n_subject(meta, "apat", "age",
    listing = TRUE, var_listing = "RACE",
    histogram = TRUE
  )

  pop1 <- collect_population_record(meta, "apat", var = c("RACE", "AGE"))
  expect_equal(test2$listing$`Number of Subjects`, subset(pop1, SAFFL == "Y" & (!pop$TRTA == "Total")), ignore_attr = TRUE)
  expect_equal(test2$listing$`Subjects with Data`, subset(pop1, (!is.na(AGE)) & SAFFL == "Y" & (!pop$TRTA == "Total")), ignore_attr = TRUE)
  expect_equal(test2$listing$`NA`, subset(pop1, is.na(AGE) & SAFFL == "Y" & (!pop$TRTA == "Total")), ignore_attr = TRUE)
  expect_true(!is.null(test2$histogram))
  # test missing values in parameters
  # test missing values in group
  ## test test3$n
  meta$data_population$AGE[2:5] <- NA
  test3 <- collect_n_subject(meta, "apat", "age")

  meta_add <- meta_add_total(meta)
  pop <- collect_population_record(meta_add, "apat", "AGE")
  Total <- length(unique(pop$USUBJID))
  pop_all <- n_subject(pop$USUBJID, pop$TRTA)
  pop_all <- data.frame(
    name = "Number of Subjects",
    pop_all
  )
  names(pop_all) <- c("name", "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose", "Total")
  expect_equal(test1$n, pop_all, ignore_attr = TRUE)
  ## test test1$table
  pop_all[2, ] <- c("Age", NA, NA, NA, NA)

  pop_all_with_data <- n_subject(pop$USUBJID, pop$TRTA, par = factor(
    is.na(pop[["AGE"]]),
    c(FALSE, TRUE), c("Subjects with Data", "Missing")
  ))
  names(pop_all_with_data) <- c("name", "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose", "Total")

  pop_age <- tapply(pop$AGE, pop$TRTA, function(x) {
    value <- c(
      mean = mean(x, na.rm = TRUE),
      sd = stats::sd(x, na.rm = TRUE),
      median = stats::median(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE)
    )
    value <- formatC(value, format = "f", digits = 1)
    c(glue::glue("{value[['mean']]} ({value[['sd']]})"), glue::glue("{value[['median']]} [{value[['min']]}, {value[['max']]}]"))
  })
  pop_age <- data.frame(
    name = c("Mean (SD)", "Median [Min, Max]"),
    do.call(cbind, pop_age)
  )

  names(pop_age) <- c("name", "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose", "Total")

  pop_pct <- pop_all_with_data
  for (i in 2:ncol(pop_all_with_data)) {
    pct <- formatC(pop_all_with_data[, i] / as.numeric(pop_all[1, i]) * 100, format = "f", digits = 1, width = 5)
    pop_pct[, i] <- glue::glue("{pop_all_with_data[,i]} ({pct}%)")
  }

  pop_n <- rbind(pop_all, pop_all_with_data[1, ], pop_age, pop_pct[2, ])
  expect_equal(test3$table, pop_n, ignore_attr = TRUE)

  test3_no <- collect_n_subject(meta, "apat", "age", use_na = "no")
  expect_equal(test3_no$table, test3$table[-nrow(test3$table), ])

  test3_always <- collect_n_subject(meta, "apat", "age", use_na = "always")
  expect_equal(test3_always$table, test3$table)
})

test_that("test collect_n_subject with logical parameter", {
  # test different combination of parameters
  suppressWarnings(
    meta <- meta_example() |>
      define_parameter(name = "saffl", var = "SAFFL", label = "Safety Flag")
  )
  test1 <- collect_n_subject(meta, "apat", "saffl")
  ## test case1
  ## test test1$n
  pop <- collect_population_record(meta, "apat", "SAFFL")
  Total <- length(unique(pop$USUBJID))
  pop_all <- n_subject(pop$USUBJID, pop$TRTA)
  pop_all <- data.frame(
    name = "Number of Subjects",
    pop_all,
    Total = sum(pop_all[1, ])
  )
  names(pop_all) <- c("name", "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose", "Total")
  expect_equal(test1$n, pop_all)
  ## test test1$table
  pop_all[2, ] <- c("Safety Flag", NA, NA, NA, NA)

  pop_all_with_data <- n_subject(pop$USUBJID, pop$TRTA, par = factor(
    is.na(pop[["SAFFL"]]),
    FALSE, "Subjects with Data"
  ))
  pop_all_with_data <- data.frame(pop_all_with_data,
    Total = sum(pop_all_with_data[1, 2:ncol(pop_all_with_data)])
  )
  names(pop_all_with_data) <- c("name", "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose", "Total")

  pop_saffl <- n_subject(pop$USUBJID, pop$TRTA, pop$SAFFL, use_na = "ifany")
  pop_saffl$Total <- rowSums(pop_saffl[1:nrow(pop_saffl), 2:4])


  for (i in 2:ncol(pop_saffl)) {
    pct <- formatC(pop_saffl[, i] / as.numeric(pop_all[1, i]) * 100, format = "f", digits = 1, width = 5)
    pop_saffl[, i] <- glue::glue("{pop_saffl[,i]} ({pct}%)")
  }

  pop_n <- rbind(pop_all, pop_all_with_data, pop_saffl)
  expect_equal(test1$table, pop_n)

  test1_no <- collect_n_subject(meta, "apat", "saffl", use_na = "no")
  expect_equal(test1$table, test1_no$table)
  test1_always <- collect_n_subject(meta, "apat", "saffl", use_na = "always")
  expect_equal(rbind(
    test1$table,
    c("Missing", "0 (  0.0%)", "0 (  0.0%)", "0 (  0.0%)", "0 (  0.0%)")
  ), test1_always$table)
})

test_that("test collect_n_subject with factor parameter", {
  # test different combination of parameters
  suppressWarnings(
    meta <- meta_example() |>
      define_parameter(name = "trt01a", var = "TRT01A", label = "Actual Group")
  )

  meta$data_population$TRT01A <- as.factor(meta$data_population$TRT01A)
  meta$data_population$TRT01A[4:6] <- NA

  test1 <- collect_n_subject(meta, "apat", "trt01a")
  ## test case1
  ## test test1$n
  pop <- collect_population_record(meta, "apat", "TRT01A")
  Total <- length(unique(pop$USUBJID))
  pop_all <- n_subject(pop$USUBJID, pop$TRTA)
  pop_all <- data.frame(
    name = "Number of Subjects",
    pop_all,
    Total = sum(pop_all[1, ])
  )
  names(pop_all) <- c("name", "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose", "Total")
  expect_equal(test1$n, pop_all)
  ## test test1$table
  pop_all[2, ] <- c("Actual Group", NA, NA, NA, NA)

  pop_all_with_data <- n_subject(pop$USUBJID, pop$TRTA, par = factor(
    is.na(pop[["TRT01A"]]),
    FALSE, "Subjects with Data"
  ))
  pop_all_with_data <- data.frame(pop_all_with_data,
    Total = sum(pop_all_with_data[1, 2:ncol(pop_all_with_data)])
  )
  names(pop_all_with_data) <- c("name", "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose", "Total")

  pop_trt01a <- n_subject(pop$USUBJID, pop$TRTA, pop$TRT01A, use_na = "ifany")
  pop_trt01a$Total <- rowSums(pop_trt01a[1:nrow(pop_trt01a), 2:4])


  for (i in 2:ncol(pop_trt01a)) {
    pct <- formatC(pop_trt01a[, i] / as.numeric(pop_all[1, i]) * 100, format = "f", digits = 1, width = 5)
    pop_trt01a[, i] <- glue::glue("{pop_trt01a[,i]} ({pct}%)")
  }

  pop_n <- rbind(pop_all, pop_all_with_data[1, ], pop_trt01a)
  expect_equal(test1$table, pop_n)

  test1_no <- collect_n_subject(meta, "apat", "trt01a", use_na = "no")
  expect_equal(test1$table[-nrow(test1$table), ], test1_no$table)
  test1_always <- collect_n_subject(meta, "apat", "trt01a", use_na = "always")
  expect_equal(test1$table, test1_always$table)
})

test_that("test collect_n_subject with character parameter", {
  # test different combination of parameters
  suppressWarnings(
    meta <- meta_example() |>
      define_parameter(name = "sex", var = "SEX", label = "Sex")
  )
  test1 <- collect_n_subject(meta, "apat", "sex")
  ## test case1
  ## test test1$n
  pop <- collect_population_record(meta, "apat", "SEX")
  Total <- length(unique(pop$USUBJID))
  pop_all <- n_subject(pop$USUBJID, pop$TRTA)
  pop_all <- data.frame(
    name = "Number of Subjects",
    pop_all,
    Total = sum(pop_all[1, ])
  )
  names(pop_all) <- c("name", "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose", "Total")
  expect_equal(test1$n, pop_all)
  ## test test1$table
  pop_all[2, ] <- c("Sex", NA, NA, NA, NA)

  pop_all_with_data <- n_subject(pop$USUBJID, pop$TRTA, par = factor(
    is.na(pop[["SEX"]]),
    FALSE, "Subjects with Data"
  ))
  pop_all_with_data <- data.frame(pop_all_with_data,
    Total = sum(pop_all_with_data[1, 2:ncol(pop_all_with_data)])
  )
  names(pop_all_with_data) <- c("name", "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose", "Total")

  pop_sex <- n_subject(pop$USUBJID, pop$TRTA, pop$SEX, use_na = "ifany")
  pop_sex$Total <- rowSums(pop_sex[1:nrow(pop_sex), 2:4])


  for (i in 2:ncol(pop_sex)) {
    pct <- formatC(pop_sex[, i] / as.numeric(pop_all[1, i]) * 100, format = "f", digits = 1, width = 5)
    pop_sex[, i] <- glue::glue("{pop_sex[,i]} ({pct}%)")
  }

  pop_n <- rbind(pop_all, pop_all_with_data, pop_sex)
  expect_equal(test1$table, pop_n)
  ## test test1$subset
  pop_subset <- collect_adam_mapping(meta, "apat")$subset
  pop_subset <- fmt_quote(deparse(pop_subset))
  grp_var <- collect_adam_mapping(meta, "apat")$group
  grp_name <- pop[[grp_var]]

  subset <- data.frame(name = c("Number of Subjects", "Subjects with Data", NA, "F", "M"))
  subset_condition <- c("TRUE", "(! is.na(SEX))", "is.na(SEX)", "SEX == 'F'", "SEX == 'M'")
  for (i in c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")) {
    group_subset <- paste0("TRTA == ", "'", i, "'")
    subset[[i]] <- sapply(FUN = paste, subset_condition, group_subset, pop_subset, sep = " & ")
  }
  expect_equal(test1$subset, subset)
  expect_equal(test1$listing, NULL)
  expect_equal(test1$histogram, NULL)
  ## test case2: test histogram and listing
  test2 <- collect_n_subject(meta, "apat", "sex",
    listing = TRUE,
    histogram = TRUE
  )

  expect_equal(test2$listing$`Number of Subjects`, subset(pop, SAFFL == "Y" & (!pop$TRTA == "Total")), ignore_attr = TRUE)
  expect_equal(test2$listing$`Subjects with Data`, subset(pop, (!is.na(SEX)) & SAFFL == "Y" & (!pop$TRTA == "Total")), ignore_attr = TRUE)
  expect_equal(test2$listing$`NA`, subset(pop, is.na(SEX) & SAFFL == "Y" & (!pop$TRTA == "Total")), ignore_attr = TRUE)
  expect_equal(test2$listing$`F`, subset(pop, SEX == "F" & SAFFL == "Y" & (!pop$TRTA == "Total")), ignore_attr = TRUE)
  expect_equal(test2$listing$`M`, subset(pop, SEX == "M" & SAFFL == "Y" & (!pop$TRTA == "Total")), ignore_attr = TRUE)
  expect_true(!is.null(test2$histogram))

  ## test var_listing contain variables names that is not avaiable in population dataset.
  expect_error(collect_n_subject(meta, "apat", "sex", var_listing = "param"))

  test1_always <- collect_n_subject(meta, "apat", "sex", use_na = "always")
  expect_equal(
    test1_always$table,
    rbind(
      test1$table,
      c("Missing", "0 (  0.0%)", "0 (  0.0%)", "0 (  0.0%)", "0 (  0.0%)")
    )
  )
  test1_no <- collect_n_subject(meta, "apat", "sex", use_na = "no")
  expect_equal(
    test1$table,
    test1_no$table
  )

  # test missing values in parameters
  meta$data_population$SEX[1] <- NA
  test3 <- collect_n_subject(meta, "apat", "sex")

  ## test test3$n
  pop <- collect_population_record(meta, "apat", "SEX")
  Total <- length(unique(pop$USUBJID))
  pop_all <- n_subject(pop$USUBJID, pop$TRTA)
  pop_all <- data.frame(
    name = "Number of Subjects",
    pop_all,
    Total = sum(pop_all[1, ])
  )
  names(pop_all) <- c("name", "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose", "Total")
  expect_equal(test3$n, pop_all)

  ## test test3$table
  pop_all[2, ] <- c("Sex", NA, NA, NA, NA)

  pop_all_with_data <- n_subject(pop$USUBJID, pop$TRTA, par = factor(
    is.na(pop[["SEX"]]),
    c(FALSE, TRUE), c("Subjects with Data", "Missing")
  ))
  pop_all_with_data <- data.frame(pop_all_with_data,
    Total = rowSums(pop_all_with_data[, 2:4])
  )
  names(pop_all_with_data) <- c("name", "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose", "Total")

  pop_sex <- n_subject(pop$USUBJID, pop$TRTA, pop$SEX, use_na = "ifany")
  pop_sex$Total <- rowSums(pop_sex[1:nrow(pop_sex), 2:4])

  for (i in 2:ncol(pop_sex)) {
    pct <- formatC(pop_sex[, i] / as.numeric(pop_all[1, i]) * 100, format = "f", digits = 1, width = 5)
    pop_sex[, i] <- glue::glue("{pop_sex[,i]} ({pct}%)")
  }

  pop_n <- rbind(pop_all, pop_all_with_data[1, ], pop_sex)
  expect_equal(test3$table, pop_n)

  test3_always <- collect_n_subject(meta, "apat", "sex", use_na = "always")
  expect_equal(test3$table, test3_always$table)

  test3_no <- collect_n_subject(meta, "apat", "sex", use_na = "no")
  expect_equal(test3$table[-nrow(test3$table), ], test3_no$table)

  # test missing values in group
  meta$data_population$TRTA[1] <- NA
  meta$data_population$TRTA[2] <- NA
  expect_error(collect_n_subject(meta, "apat", "sex"))

  # test duplicated subject id
  suppressWarnings(
    meta <- meta_example() |>
      define_parameter(name = "sex", var = "SEX", label = "Sex")
  )

  meta$data_population$USUBJID[1] <- meta$data_population$USUBJID[2]
  expect_warning(collect_n_subject(meta, "apat", "sex"))
})
