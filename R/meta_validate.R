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

#' Validate meta information
#'
#' @inheritParams define_population
#'
#' @noRd
meta_validate <- function(meta) {
  # Check data type
  if (!is.data.frame(meta$data_population)) {
    stop(".$data_population must be a data frame")
  }

  if (!is.data.frame(meta$data_observation)) {
    stop(".$data_observation must be a data frame")
  }

  if (!is.data.frame(meta$plan)) {
    stop(".$plan must be a data frame")
  }

  # Check plan variable name
  if (!all(c("mock", "analysis", "population", "observation", "parameter") %in% names(meta$plan))) {
    stop(".$plan must contain all required variable names: 'mock', 'analysis', 'population', 'observation', 'parameter'")
  }

  meta$data_population
  meta$population
  # Check id variable
  lapply(c(meta$population, meta$observation), function(x) {
    if (is.null(x$id)) {
      stop(x$name, "missing 'id' variable")
    }
  })

  # Check label variable
  adam_obs <- c(meta$observation, meta$parameter, meta$analysis)

  lapply(c(meta$population, adam_obs), function(x) {
    if (is.null(x$label)) {
      warning(x$name, ": has missing label")
    }
  })

  # Check population variables in the datasets
  lapply(meta$population, function(x) {
    data_pop <- collect_data_source(meta, x$from, default = "population")

    if (!all(x$id %in% names(data_pop))) {
      stop(x$name, ": variable name in '.$id' is not defined in the source dataset")
    }

    if (!all(x$group %in% names(data_pop))) {
      stop(x$name, ": variable name in '.$group' is not defined in the source dataset")
    }

    if (!all(x$var %in% names(data_pop))) {
      stop(x$name, ": variable name in '.$var' is not defined in the source dataset")
    }
  })

  # Check observation variables against each observation's own source dataset.
  lapply(meta$observation, function(x) {
    data_obs <- collect_data_source(meta, x$from, default = "observation")

    if (!all(x$id %in% names(data_obs))) {
      stop(x$name, ": variable name in '.$id' is not defined in the source dataset")
    }

    if (!all(x$group %in% names(data_obs))) {
      stop(x$name, ": variable name in '.$group' is not defined in the source dataset")
    }

    if (!all(x$var %in% names(data_obs))) {
      stop(x$name, ": variable name in '.$var' is not defined in the source dataset")
    }
  })

  # Parameter (and analysis) terms do not carry their own `from`; they are
  # evaluated against the observation dataset they are paired with in the plan.
  # Validate each parameter's variables against the source of every observation
  # it is paired with, so a parameter reading from an alternate source dataset
  # (e.g. a lab dataset) is checked against that dataset.
  for (i in seq_len(nrow(meta$plan))) {
    obs_terms <- trimws(unlist(strsplit(meta$plan[i, "observation"], split = ";")))
    par_terms <- trimws(unlist(strsplit(meta$plan[i, "parameter"], split = ";")))

    for (obs_name in obs_terms) {
      data_obs <- collect_data_source(meta, meta$observation[[obs_name]]$from, default = "observation")

      for (par_name in par_terms) {
        x <- meta$parameter[[par_name]]
        if (is.null(x)) next

        if (!all(x$var %in% names(data_obs))) {
          stop(
            x$name, ": variable name in '.$var' is not defined in the source ",
            "dataset for observation '", obs_name, "'"
          )
        }
      }
    }
  }

  # check group factor level are the same
  u_plan <- unique(meta$plan[, c("population", "observation")])
  for (i in 1:nrow(u_plan)) {
    key_pop <- u_plan[i, "population"]
    key_obs <- u_plan[i, "observation"]
    data_pop <- collect_data_source(meta, meta$population[[key_pop]]$from, default = "population")
    data_obs <- collect_data_source(meta, meta$observation[[key_obs]]$from, default = "observation")
    level_pop <- levels(data_pop[[metalite::collect_adam_mapping(meta, key_pop)$group]])
    level_obs <- levels(data_obs[[metalite::collect_adam_mapping(meta, key_obs)$group]])
    if (!all(level_pop == level_obs)) {
      stop("Inconsistent group level: the levels of group variable from population and observation datasets are not the same")
    }
  }

  meta
}
