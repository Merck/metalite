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
    if (!all(x$id %in% names(meta$data_population))) {
      stop(x$name, ": variable name in '.$id' is not defined in .$data_population")
    }

    if (!all(x$group %in% names(meta$data_population))) {
      stop(x$name, ": variable name in '.$group' is not defined in .$data_population")
    }

    if (!all(x$var %in% names(meta$data_population))) {
      stop(x$name, ": variable name in '.$var' is not defined in .$data_population")
    }
  })

  # Check observation variables in the datasets
  lapply(adam_obs, function(x) {
    if (!all(x$id %in% names(meta$data_observation))) {
      stop(x$name, ": variable name in '.$id' is not defined in .$data_observation")
    }

    if (!all(x$group %in% names(meta$data_observation))) {
      stop(x$name, ": variable name in '.$group' is not defined in .$data_observation")
    }

    if (!all(x$var %in% names(meta$data_observation))) {
      stop(x$name, ": variable name in '.$var' is not defined in .$data_observation")
    }
  })

  # check group factor level are the same
  u_plan <- unique(meta$plan[, c("population", "observation")])
  for (i in 1:nrow(u_plan)) {
    key_pop <- u_plan[i, "population"]
    key_obs <- u_plan[i, "observation"]
    level_pop <- levels(meta$data_population[[metalite::collect_adam_mapping(meta, key_pop)$group]])
    level_obs <- levels(meta$data_observation[[metalite::collect_adam_mapping(meta, key_obs)$group]])
    if (!all(level_pop == level_obs)) {
      stop("Inconsistent group level: the levels of group variable from population and observation datasets are not the same")
    }
  }

  meta
}
