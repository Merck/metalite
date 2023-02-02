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

#' Define analysis plan meta information for ADaM dataset
#'
#' @param meta A `meta_adam` object.
#' @param plan A data frame for analysis plan.
#'
#' @return A metadata object with plans defined.
#'
#' @export
#'
#' @examples
#' plan <- plan(
#'   analysis = "ae_summary", population = "apat",
#'   observation = c("wk12", "wk24"), parameter = "any;rel;ser"
#' )
#'
#' meta_adam(
#'   population = r2rtf::r2rtf_adsl,
#'   observation = r2rtf::r2rtf_adae
#' ) |>
#'   define_plan(plan)
define_plan <- function(meta, plan) {
  meta$plan <- plan
  meta
}

#' Define analysis population meta information for ADaM dataset
#'
#' @param meta A `meta_adam` object.
#' @inheritParams adam_mapping
#'
#' @return A metadata object with population defined.
#'
#' @export
#'
#' @examples
#' plan <- plan(
#'   analysis = "ae_summary", population = "apat",
#'   observation = c("wk12", "wk24"), parameter = "any;rel;ser"
#' )
#'
#' meta_adam(
#'   population = r2rtf::r2rtf_adsl,
#'   observation = r2rtf::r2rtf_adae
#' ) |>
#'   define_plan(plan) |>
#'   define_population(name = "apat")
define_population <- function(meta,
                              name,
                              id = "USUBJID",
                              group = NULL,
                              var = NULL,
                              subset = NULL,
                              label = NULL,
                              ...) {
  if (!any(grepl(name, meta$plan[["population"]]))) {
    warning(name, " is not in .$plan")
  }

  try(subset, silent = TRUE)
  list(...)

  x <- adam_mapping(
    name = !!name,
    id = !!id,
    group = !!group,
    var = !!var,
    subset = !!rlang::enquo(subset),
    label = !!label,
    ...
  )

  meta$population[[name]] <- default_apply(x)

  meta
}

#' Define analysis observation meta information for ADaM dataset
#'
#' @inheritParams define_population
#'
#' @return A metadata object with observation defined.
#'
#' @export
#'
#' @examples
#' plan <- plan(
#'   analysis = "ae_summary", population = "apat",
#'   observation = c("wk12", "wk24"), parameter = "any;rel;ser"
#' )
#'
#' meta_adam(
#'   population = r2rtf::r2rtf_adsl,
#'   observation = r2rtf::r2rtf_adae
#' ) |>
#'   define_plan(plan = plan) |>
#'   define_observation(
#'     name = "wk12",
#'     group = "TRTA",
#'     subset = SAFFL == "Y",
#'     label = "Weeks 0 to 12"
#'   )
define_observation <- function(meta,
                               name,
                               id = "USUBJID",
                               group = NULL,
                               var = NULL,
                               subset = NULL,
                               label = NULL,
                               ...) {
  if (!any(grepl(name, meta$plan[["observation"]]))) {
    warning(name, " is not in .$plan")
  }

  try(subset, silent = TRUE)
  list(...)

  x <- adam_mapping(
    name = !!name,
    id = !!id,
    group = !!group,
    var = !!var,
    subset = !!rlang::enquo(subset),
    label = !!label,
    ...
  )

  meta$observation[[name]] <- default_apply(x)

  meta
}

#' Define analysis parameter meta information for ADaM dataset
#'
#' @inheritParams define_population
#'
#' @return A metadata object with parameters defined.
#'
#' @export
#'
#' @examples
#' plan <- plan(
#'   analysis = "ae_summary", population = "apat",
#'   observation = c("wk12", "wk24"), parameter = "any;rel;ser"
#' )
#'
#' meta_adam(
#'   population = r2rtf::r2rtf_adsl,
#'   observation = r2rtf::r2rtf_adae
#' ) |>
#'   define_plan(plan = plan) |>
#'   define_parameter(
#'     name = "rel",
#'     subset = AEREL %in% c("POSSIBLE", "PROBABLE")
#'   )
define_parameter <- function(meta,
                             name,
                             subset = NULL,
                             ...) {
  if (!any(grepl(name, meta$plan[["parameter"]]))) {
    warning(name, " is not in .$plan")
  }

  try(subset, silent = TRUE)
  list(...)

  x <- adam_mapping(
    name = !!name,
    subset = !!rlang::enquo(subset),
    ...
  )

  meta$parameter[[name]] <- default_apply(x)

  meta
}

#' Define analysis function meta information for ADaM dataset
#'
#' @inheritParams define_population
#'
#' @return A metadata object with analysis details defined.
#'
#' @export
#'
#' @examples
#' plan <- plan(
#'   analysis = "ae_summary", population = "apat",
#'   observation = c("wk12", "wk24"), parameter = "any;rel;ser"
#' )
#'
#' meta_adam(
#'   population = r2rtf::r2rtf_adsl,
#'   observation = r2rtf::r2rtf_adae
#' ) |>
#'   define_plan(plan = plan) |>
#'   define_analysis(
#'     name = "ae_summary",
#'     title = "Summary of Adverse Events"
#'   )
define_analysis <- function(meta,
                            name,
                            ...) {
  if (!any(grepl(name, meta$plan[["analysis"]]))) {
    warning(name, " is not in .$plan")
  }

  list(...)

  x <- adam_mapping(name = !!name, ...)

  meta$analysis[[name]] <- default_apply(x)

  meta
}
