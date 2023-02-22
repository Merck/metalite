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

#' Construct `outdata` class
#'
#' The `outdata` class defines a standard output format for
#' analysis and reporting.
#'
#' The design is inspired by `ggplot2::aes()`.
#'
#' @param meta A metadata object created by metalite.
#' @param population A character value of population term name.
#'   The term name is used as key to link information.
#' @param observation A character value of observation term name.
#'   The term name is used as key to link information.
#' @param parameter A character value of parameter term name.
#'   The term name is used as key to link information.
#' @param n A data frame for number of subjects in each criteria.
#' @param order A numeric vector of row display order.
#' @param group A character vector of group variable names in an ADaM dataset.
#' @param reference_group A numeric value to indicate reference group
#'   in levels of group.
#' @param ... Additional variables to save to `outdata`.
#'
#' @return A list with class `outdata`.
#'   Components of the list are either quosures or constants.
#'
#' @export
#'
#' @examples
#' outdata(
#'   meta = meta_example(),
#'   population = "apat",
#'   observation = "wk12",
#'   parameter = "rel",
#'   n = data.frame(
#'     TRTA = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
#'     n = c(86, 84, 84)
#'   ),
#'   group = "TRTA",
#'   reference_group = 1,
#'   order = 1:3
#' )
outdata <- function(meta,
                    population,
                    observation,
                    parameter,
                    n,
                    order,
                    group,
                    reference_group,
                    ...) {
  x <- list(
    meta = meta,
    population = population,
    observation = observation,
    parameter = parameter,
    n = n,
    order = order,
    group = group,
    reference_group = reference_group,
    ...
  )

  x <- new_outdata(x, env = parent.frame())

  # Input checking
  validate_outdata(x)
}

#' Structure outdata class
#'
#' @param x A list, see Details.
#' @param env An environment.
#'
#' @details
#' The list contains these elements:
#'
#' - `meta`
#' - `population`
#' - `observation`
#' - `parameter`
#' - `n`
#' - `order`
#' - `group`
#' - `reference_group`
#'
#' @noRd
#'
#' @examples
#' meta <- meta_example()
#' x <- list(
#'   meta = meta_example(),
#'   population = "apat",
#'   observation = "wk12",
#'   parameter = "rel",
#'   n = data.frame(
#'     TRTA = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
#'     n = c(86, 84, 84)
#'   ),
#'   group = "TRTA",
#'   reference_group = 1,
#'   order = 1:3
#' )
#' metalite:::new_outdata(x)
new_outdata <- function(x, env = globalenv()) {
  if (!is.list(x)) {
    rlang::abort("`x` must be a list")
  }

  structure(x, class = "outdata")
}

#' Validate outdata class
#'
#' @param x An `outdata` object.
#'
#' @noRd
#'
#' @examples
#' meta <- meta_example()
#' metalite:::validate_outdata(
#'   metalite:::outdata(
#'     meta = meta_example(),
#'     population = "apat",
#'     observation = "wk12",
#'     parameter = "rel",
#'     n = data.frame(
#'       TRTA = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
#'       n = c(86, 84, 84)
#'     ),
#'     group = "TRTA",
#'     reference_group = 1,
#'     order = 1:3
#'   )
#' )
validate_outdata <- function(x) {
  # All required variable
  char <- c("population", "observation", "parameter", "group")
  num <- c("reference_group", "order")

  # Length 1 variable
  char_length_1 <- c("population", "observation", "parameter")
  num_length_1 <- c("reference_group")

  # Check duplicate name
  check_duplicate_name(x)

  # Check meta
  stopifnot(class(x$meta) == "meta_adam")

  # Check required numeric variable
  lapply(num, function(term) {
    if (!is.null(x[[term]])) {
      if (!rlang::is_bare_numeric(x[[term]])) {
        rlang::abort(glue::glue("variable '{term}' must be a numeric value"))
      }

      if (term %in% num_length_1 & length(x[[term]]) > 1L) {
        rlang::abort(glue::glue("variable '{term}' must be length 1"))
      }
    }
  })

  # Check required character variable
  lapply(char, function(term) {
    if (!is.null(x[[term]])) {
      if (!rlang::is_character(x[[term]])) {
        rlang::abort(glue::glue("variable '{term}' must be a character value"))
      }

      if (term %in% char_length_1 & length(x[[term]]) > 1L) {
        rlang::abort(glue::glue("variable '{term}' must be length 1"))
      }
    }
  })

  x
}

#' @export
print.outdata <- function(x, ...) {
  utils::str(x, max.level = 1, give.attr = FALSE, ...)
  invisible(x)
}

#' @export
"[.outdata" <- function(x, i, ...) {
  new_outdata(NextMethod())
}

#' @export
"[[<-.outdata" <- function(x, i, value) {
  new_outdata(NextMethod())
}

#' @export
"$<-.outdata" <- function(x, i, value) {
  new_outdata(NextMethod())
}

#' @export
"[<-.outdata" <- function(x, i, value) {
  new_outdata(NextMethod())
}
