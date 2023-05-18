# Copyright (c) 2023 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
# All rights reserved.
#
# This file is part of the metalite program.
#
# metalite is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Count number of unique subjects
#'
#' @param id A character vector of subject identifier.
#' @param group A factor vector of group name.
#' @param par A character vector of parameter name.
#' @param use_na A character value for whether to include `NA` values
#'   in the table. See the `useNA` argument in [base::table()] for more details.
#'
#' @return A data frame summarizing the number of unique subjects
#'   in different arms.
#'
#' @export
#'
#' @examples
#' library(r2rtf)
#'
#' r2rtf_adae$TRTA <- factor(r2rtf_adae$TRTA)
#' r2rtf_adae$SEX[1:5] <- NA
#'
#' n_subject(r2rtf_adae$USUBJID, r2rtf_adae$TRTA)
#' n_subject(r2rtf_adae$USUBJID, r2rtf_adae$TRTA, r2rtf_adae$SEX)
#' n_subject(r2rtf_adae$USUBJID, r2rtf_adae$TRTA, r2rtf_adae$SEX, use_na = "always")
n_subject <- function(id,
                      group,
                      par = NULL,
                      use_na = c("ifany", "no", "always")) {
  use_na <- match.arg(use_na)

  if ("factor" %in% class(group)) {
    u_group <- c(as.character(levels(group)), "Missing")
  } else {
    stop("n_subject: group variable must be a factor")
  }

  if (is.null(par)) {
    db <- data.frame(id = id, group = group)
    res <- table(unique(db)[["group"]], useNA = use_na)

    n_row <- nrow(res)
    res <- data.frame(t(as.vector(res)))
    names(res) <- c(u_group[1:n_row])
  } else {
    db <- data.frame(id = id, group = group, par = par)
    res <- table(unique(db)[, c("group", "par")], useNA = use_na)
    name <- colnames(res)
    name[is.na(name)] <- "Missing"

    n_row <- nrow(res)
    n_col <- ncol(res)
    res <- data.frame(name = name[1:n_col], matrix(res, ncol = n_row, byrow = TRUE))
    names(res) <- c("name", u_group[1:n_row])
  }

  res
}

#' Remove blank group based on analysis parameter
#'
#' @inheritParams collect_n_subject
#'
#' @noRd
meta_remove_blank_group <- function(meta,
                                    population,
                                    parameter) {
  pop <- meta$data_population
  obs <- meta$data_observation

  pop_grp <- collect_adam_mapping(meta, population)$group
  obs_grp <- collect_adam_mapping(meta, population)$group

  pop_var <- collect_adam_mapping(meta, parameter)$var

  if (is.null(pop[[pop_var]])) {
    stop(glue::glue("meta_remove_blank_group: parameter {pop_var} is not available in meta$population"))
  }

  loc <- which(table(is.na(pop[[pop_var]]), pop[[pop_grp]])["FALSE", ] == 0)

  if (length(loc) > 0) {
    pop_ind <- !pop[[pop_grp]] %in% levels(pop[[pop_grp]])[loc]
    pop <- pop[pop_ind, ]
    pop[[pop_grp]] <- factor(pop[[pop_grp]], levels(pop[[pop_grp]])[-loc])

    obs_ind <- !obs[[obs_grp]] %in% levels(obs[[obs_grp]])[loc]
    obs <- obs[obs_ind, ]
    obs[[obs_grp]] <- factor(obs[[obs_grp]], levels(obs[[obs_grp]])[-loc])
  }

  meta$data_population <- pop
  meta$data_observation <- obs

  meta
}

#' Collect number of subjects and its subset condition
#'
#' @inheritParams plan
#' @inheritParams define_population
#' @param listing A logical value to display drill down listing per row.
#' @param histogram A logical value to display histogram by group.
#' @param var_listing A character vector of additional variables included
#'   in the listing.
#' @param remove_blank_group A logical value to remove a group with all
#'   missing value of a parameter.
#' @param type A character value to control title name,
#'   e.g., Subjects or Records.
#' @param use_na A character value for whether to include `NA` values
#' in the table. See the `useNA` argument in [base::table()] for more details.
#' @param display_total A logical value to display total column.
#'
#' @return A list containing number of subjects and its subset condition.
#'
#' @export
#'
#' @examples
#' suppressWarnings(
#'   meta <- meta_example() |>
#'     define_parameter(name = "sex", var = "SEX", label = "Sex")
#' )
#' collect_n_subject(meta, "apat", "sex")
collect_n_subject <- function(meta,
                              population,
                              parameter,
                              listing = FALSE,
                              histogram = FALSE,
                              var_listing = NULL,
                              remove_blank_group = FALSE,
                              type = "Subjects",
                              use_na = c("ifany", "no", "always"),
                              display_total = TRUE) {
  use_na <- match.arg(use_na)

  title <- c(
    all = glue::glue("Number of {type}"),
    with_data = glue::glue("{type} with Data"),
    missing = NA
  )

  if (remove_blank_group) {
    meta <- meta_remove_blank_group(meta, population, parameter)
  }

  if (display_total) {
    meta <- meta_add_total(meta)
  }

  # Obtain variables
  par_var <- collect_adam_mapping(meta, parameter)$var

  # Obtain Data
  pop <- collect_population_record(meta, population, var = c(var_listing, par_var))

  # Obtain ID
  pop_id <- collect_adam_mapping(meta, population)$id

  # Obtain Group
  pop_group <- collect_adam_mapping(meta, population)$group

  # Define analysis dataset
  uid <- pop[[pop_id]]
  id <- seq(uid)
  group <- pop[[pop_group]]
  var <- pop[[par_var]]

  class_var <- class(var)

  # Check ID duplication
  if (any(duplicated(uid[!group %in% "Total"]))) {
    warning(pop_id, " is not a unique ID")
  }

  # Obtain variable label
  label <- collect_adam_mapping(meta, parameter)$label
  if (is.null(label)) {
    label <- collect_adam_mapping(meta, parameter)$var
  }

  # standardize group variable
  stopifnot(inherits(group, c("factor", "character")))
  if (any(is.na(group))) {
    stop("Missing value in population `group` variable is not allowed")
  }

  group <- factor(group)

  # standardize continuous variables
  stopifnot(inherits(var, c("numeric", "integer", "factor", "character", "logical")))

  # summary of population
  all <- rep(title["all"], length(var))
  pop_all <- n_subject(id, group = group, par = all, use_na = "no")

  var_n <- factor(is.na(var), c(FALSE, TRUE), title[c("with_data", "missing")])

  # Obtain Number of Subjects
  pop_n <- n_subject(id, group = group, par = var_n, use_na = use_na)

  # Remove Missing Column
  if (all(pop_n[["Missing"]] == 0)) {
    pop_n <- pop_n[, !names(pop_n) %in% "Missing"]
  }

  # Transfer logical value
  if ("logical" %in% class_var) {
    class_var <- "character"
    var <- factor(var, c(TRUE, FALSE), c("Yes", "No"))
  }

  if (any(c("numeric", "integer") %in% class_var)) {
    # calculate summary statistics
    pop_num <- tapply(var, group, function(x) {
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
    pop_num <- data.frame(
      name = c("Mean (SD)", "Median [Min, Max]"),
      do.call(cbind, pop_num)
    )

    # combine results
    names(pop_num) <- names(pop_n)

    # add percentage
    pop_tmp <- pop_n
    for (i in seq(names(pop_n))) {
      if ("integer" %in% class(pop_n[[i]])) {
        pct <- formatC(pop_n[[i]] / pop_all[[i]] * 100, format = "f", digits = 1, width = 5)
        pop_tmp[[i]] <- glue::glue("{pop_n[[i]]} ({pct}%)")
      }
    }

    # prepare summary table
    pop_table <- rbind(pop_all, pop_n[1, ], pop_num)

    if ((use_na == "ifany" & sum(pop_n[2, -1], na.rm = TRUE) != 0) |
      use_na == "always") {
      pop_table <- rbind(pop_table, pop_tmp[2, ])
    }


    var_level <- title
    names(var_level) <- NULL
  }

  # standardize categorical variables
  if (any(c("factor", "character") %in% class_var)) {
    var <- factor(var, exclude = NULL)

    if (all(is.na(var))) {
      levels(var) <- c(levels(var), title["missing"])
    } else {
      levels(var)[is.na(levels(var))] <- title["missing"]
    }

    # Obtain Number of Subjects
    pop_num <- n_subject(id, group = group, par = var, use_na = use_na)

    if (all(pop_n[["Missing"]] == 0)) {
      pop_tmp <- pop_num[, !names(pop_num) %in% "Missing"]
    } else {
      pop_tmp <- pop_num
    }

    for (i in seq(names(pop_tmp))) {
      if ("integer" %in% class(pop_tmp[[i]])) {
        pct <- formatC(pop_tmp[[i]] / pop_all[[i]] * 100, format = "f", digits = 1, width = 5)
        pop_tmp[[i]] <- glue::glue("{pop_tmp[[i]]} ({pct}%)")
      }
    }

    # prepare summary table
    pop_table <- rbind(pop_all, pop_n[1, ], pop_tmp)
    var_level <- unique(c(title, levels(var)))
  }

  # add table header using variable label
  header <- data.frame(t(c(label, rep(NA, ncol(pop_table) - 1))))
  names(header) <- names(pop_table)

  pop_table <- rbind(pop_table[1, ], header, pop_table[-1, ])
  rownames(pop_table) <- NULL

  # Prepare subset condition
  subset_condition <- function(x, name) {
    if (is.na(x)) {
      return(glue::glue("is.na({name})"))
    }

    if (x == title["all"]) {
      return("TRUE")
    }

    if (x == title["with_data"]) {
      return(glue::glue("(! is.na({name}))"))
    }

    glue::glue("{name} == '{x}'")
  }

  var_subset <- vapply(var_level, subset_condition, name = par_var, FUN.VALUE = character(1))
  group_subset <- vapply(levels(group), subset_condition, name = pop_group, FUN.VALUE = character(1))
  pop_subset <- collect_adam_mapping(meta, population)$subset
  pop_subset <- fmt_quote(deparse(pop_subset))

  full_subset <- paste(group_subset, pop_subset, sep = " & ")
  full_subset <- outer(var_subset, full_subset, FUN = paste, sep = " & ")

  res <- data.frame(name = var_level, full_subset)
  names(res) <- c("name", levels(group))
  res <- res[, 1:ncol(pop_n)]
  rownames(res) <- NULL

  res <- res[, setdiff(names(res), "Total")]

  # Create row listing
  if (listing) {
    row_subset <- paste(var_subset, pop_subset, sep = " & ")
    listing <- lapply(var_subset, function(x) {
      pop_listing <- subset(pop, rlang::eval_tidy(expr = str2lang(x), data = pop) & (!group %in% "Total"))
      pop_listing <- reset_label(pop_listing, meta$data_population)
    })
  } else {
    listing <- NULL
  }

  # Show distribution graph
  if (histogram) {
    ana <- data.frame(id = id, group = group, var = pop[[par_var]])
    ana <- stats::na.omit(ana)

    if (any(c("numeric", "integer") %in% class_var)) {
      ana$var <- cut(ana$var,
        seq(min(ana$var, na.rm = TRUE),
          max(ana$var, na.rm = TRUE),
          length.out = 6
        ),
        include.lowest = TRUE
      )
    }

    pop_hist <- ggplot2::ggplot(data = ana, ggplot2::aes(x = var, group = group)) +
      ggplot2::facet_wrap(~group) +
      ggplot2::xlab(label) +
      ggplot2::ylab(title["all"]) +
      ggplot2::ggtitle(glue::glue("Histogram of {label}")) +
      ggplot2::theme_bw()

    # Rotate x-axis direction
    if (nchar(paste(unique(ana$var), collapse = "")) > 30) {
      pop_hist <- pop_hist + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, hjust = 0))
    }

    pop_hist <- pop_hist + ggplot2::geom_bar()
  } else {
    pop_hist <- NULL
  }

  list(table = pop_table, n = pop_all, subset = res, listing = listing, histogram = pop_hist)
}
