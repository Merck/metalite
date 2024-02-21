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

#' Collect `adam_mapping` from `meta_adam` by `name`
#'
#' @inheritParams define_population
#' @param name A keyword value.
#'
#' @return An `adam_mapping` class object containing the definition of
#'   the search variable in `name`.
#'
#' @export
#'
#' @examples
#' meta <- meta_example()
#' collect_adam_mapping(meta, "apat")
collect_adam_mapping <- function(meta, name) {
  if (is.null(name)) {
    return(list())
  }

  check_args(arg = name, type = "character", length = 1)

  adam <- list(
    population = meta$population,
    observation = meta$observation,
    parameter = meta$parameter,
    analysis = meta$analysis
  )

  # find where the name is, population, or observation, or parameter, or analysis
  location <- vapply(adam, function(x) name %in% names(x), FUN.VALUE = logical(1))

  # add `.location` to the mapping, either population, or observation, or parameter, or analysis
  if (any(location)) {
    map <- adam[location][[1]][[name]]
    map[[".location"]] <- names(location)[location]
  } else {
    map <- NULL
  }
  map
}

#' Collect specification for population definition
#'
#' @inheritParams define_population
#' @inheritParams plan
#'
#' @return A list covering the filter of population,
#' observation (if given) and parameter (if given).
#'
#' @export
#'
#' @examples
#' meta <- meta_example()
#' collect_population(meta, "apat")
#' collect_population(meta, "apat", "wk12")
#' collect_population(meta, "apat", "wk12", "ser")
collect_population <- function(meta,
                               population,
                               observation = NULL,
                               parameter = NULL) {
  term <- c(
    population = collect_adam_mapping(meta, population)$subset,
    observation = collect_adam_mapping(meta, observation)$subset,
    parameter = collect_adam_mapping(meta, parameter)$subset
  )

  term <- lapply(term, function(x) fmt_quote(deparse(x)))

  term
}

#' Collect population record index from population dataset
#'
#' @inheritParams define_population
#' @inheritParams plan
#'
#' @return A vector of patient index within the population group.
#'
#' @export
#'
#' @examples
#' meta <- meta_example()
#' head(collect_population_index(meta, "apat"))
collect_population_index <- function(meta,
                                     population) {
  # eval_tidy() is a variant of base::eval() that powers the tidy evaluation framework
  pop <- rlang::eval_tidy(
    expr = collect_adam_mapping(meta, population)$subset,
    data = meta$data_population
  )

  n <- nrow(meta$data_population)

  # if the `population = ...` is not defined
  if (is.null(pop)) {
    return(1:n)
  }

  which(pop)
}


#' Collect subject identifier information from population dataset
#'
#' @inheritParams define_population
#' @inheritParams plan
#'
#' @return A vector of patient ID within the population group.
#'
#' @export
#'
#' @examples
#' meta <- meta_example()
#' head(collect_population_id(meta, "apat"))
collect_population_id <- function(meta,
                                  population) {
  # get the USUBJID (usually) from the population                    (extract the variable name "USUBJID")
  meta$data_population[collect_population_index(meta, population), ][[collect_adam_mapping(meta, population)$id]]
}


#' Collect population record from population dataset
#'
#' The key variables used in `id`, `group`, and `subset`
#' are displayed by default.
#'
#' @inheritParams define_population
#' @inheritParams plan
#' @param var A character vector of additional variables to be displayed
#'   in the output.
#'
#' @return A data frame containing the variables in the population dataset.
#'
#' @export
#'
#' @examples
#' meta <- meta_example()
#' head(collect_population_record(meta, "apat"))
#' head(collect_population_record(meta, "apat", var = "AGE"))
collect_population_record <- function(meta,
                                      population,
                                      var = NULL) {
  # collect the subject index (e.g., 1:254) from the population
  id <- collect_population_index(meta, population)

  # format the key var must to be output,
  # including the subject ID (e.g., USUBJID), grouping variable (TRTA, TRT01A)
  key <- c(
    collect_adam_mapping(meta, population)[c("id", "group", "var")],
    all.vars(collect_adam_mapping(meta, population)$subset)
  )

  # incorporate the key var with user input var
  var <- unique(unlist(c(key, var)))

  # output the population dataset with their index (id), and selected `var = ...`
  meta$data_population[id, var]
}

#' Collect observation record index from observation dataset
#'
#' @inheritParams define_population
#' @inheritParams plan
#'
#' @return A vector of patient index within the observation group.
#'
#' @export
#'
#' @examples
#' meta <- meta_example()
#' collect_observation_index(meta, "apat", "wk12", "ser")
collect_observation_index <- function(meta,
                                      population,
                                      observation,
                                      parameter) {
  pop_id <- collect_population_id(meta, population)

  # Records in the population
  pop <- meta$data_observation[[collect_adam_mapping(meta, observation)$id]] %in% pop_id

  # analysis observations
  obs <- rlang::eval_tidy(
    expr = collect_adam_mapping(meta, observation)$subset,
    data = meta$data_observation
  )

  # parameter observations
  par <- rlang::eval_tidy(
    expr = collect_adam_mapping(meta, parameter)$subset,
    data = meta$data_observation
  )

  n <- nrow(meta$data_observation)

  if (is.null(pop)) pop <- rep(TRUE, n)
  if (is.null(obs)) obs <- rep(TRUE, n)
  if (is.null(par)) par <- rep(TRUE, n)

  (1:n)[pop & obs & par]
}

#' Collect observation record from observation dataset
#'
#' The key variables used in `id`, `group`, and `subset`
#' are displayed by default.
#'
#' @inheritParams define_population
#' @inheritParams plan
#' @param var A character vector of additional variables to be displayed
#'   in the output.
#'
#' @return A data frame of the observation dataset.
#'
#' @export
#'
#' @examples
#' meta <- meta_example()
#' collect_observation_record(meta, "apat", "wk12", "ser")
#' collect_observation_record(meta, "apat", "wk12", "ser", var = "AEDECOD")
collect_observation_record <- function(meta,
                                       population,
                                       observation,
                                       parameter,
                                       var = NULL) {
  id <- collect_observation_index(meta, population, observation, parameter)


  key <- c(
    collect_adam_mapping(meta, observation)[c("id", "group", "var")],
    collect_adam_mapping(meta, parameter)[c("id", "group", "var")],
    all.vars(collect_adam_mapping(meta, observation)$subset),
    all.vars(collect_adam_mapping(meta, parameter)$subset)
  )

  var <- unique(unlist(c(key, var)))

  # subset the data to be output
  ans <- meta$data_observation[id, var, drop = FALSE]
  # get all labels from the un-subset data
  ans_label <- get_label(meta$data_observation)
  # assign labels
  assign_label(
    data = ans,
    var = names(ans),
    label = ans_label[match(names(ans), names(ans_label))]
  )
}

#' Collect specification for title
#'
#' @inheritParams define_population
#' @inheritParams plan
#' @param title_order A character vector to define the order of title
#'   from each component.
#'
#' @return A vector of strings to compose the table captions.
#'
#' @export
#'
#' @examples
#' meta <- meta_example()
#' collect_title(meta, "apat", "wk12", "ser", "ae_summary")
#' collect_title(meta, "apat", "wk12", "ser", "ae_specific")
collect_title <- function(meta,
                          population,
                          observation,
                          parameter,
                          analysis,
                          title_order = c("analysis", "observation", "population")) {
  title_component <- c()
  for (i in seq(title_order)) {
    title_component[i] <- get(title_order[i])
  }

  x <- lapply(
    title_component,
    function(x) {
      tmp <- omit_null(collect_adam_mapping(meta, x)[c("title", "label")])
      if (length(tmp) > 0) {
        with(collect_adam_mapping(meta, parameter), fmt_sentence(gluestick(tmp[[1]])))
      } else {
        NULL
      }
    }
  )

  unlist(x)
}

#' Collect specification for dataset name
#'
#' @inheritParams define_population
#' @inheritParams plan
#'
#' @return A vector of character strings containing the name of
#'   the population/observation.
#'
#' @export
#'
#' @examples
#' meta <- meta_example()
#' collect_dataname(meta)
collect_dataname <- function(meta) {
  c(
    population = attr(meta$data_population, "data_name"),
    observation = attr(meta$data_observation, "data_name")
  )
}
