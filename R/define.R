#    Copyright (c) 2022 Merck Sharp & Dohme Corp. a subsidiary of Merck & Co., Inc., Kenilworth, NJ, USA.
#
#    This file is part of the metalite program.
#
#    metalite is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Define analysis plan meta information for ADaM dataset
#'
#' @param meta a `meta_adam` object.
#' @param plan a dataframe for analysis plan
#'
#' @export
define_plan <- function(meta,
                        plan) {
  meta$plan <- plan
  meta
}

#' Define analysis population meta information for ADaM dataset
#'
#' @param meta an `meta_adam` object.
#' @inheritParams adam_mapping
#'
#' @export
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
#' @export
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
#' @export
define_parameter <- function(meta,
                             name,
                             subset,
                             ...) {
  if (!any(grepl(name, meta$plan[["parameter"]]))) {
    warning(name, " is not in .$plan")
  }

  try(subset, silent = TRUE)

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
#' @export
define_analysis <- function(meta,
                            name,
                            ...) {
  if (!any(grepl(name, meta$plan[["analysis"]]))) {
    warning(name, " is not in .$plan")
  }

  x <- adam_mapping(name = !!name, ...)

  meta$analysis[[name]] <- default_apply(x)

  meta
}
