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

#' Construct outdata class
#'
#' The outdata class define a standard output format for analysis and reporting.
#'
#' The implementation idea mimic `ggplot2::aes`.
#'
#' @param meta a meta data created by `metalite`.
#' @param population a character value of population term name.
#' The term name is used as key to link information.
#' @param observation a character value of observation term name.
#' The term name is used as key to link information.
#' @param parameter a character value of parameter term name.
#' The term name is used as key to link information.
#' @param n a dataframe for number of subjects in each criteria
#' @param order a numeric vector of row display order
#' @param group a character vector of group variable names in an ADaM dataset.
#' @param reference_group a numeric value to indicate reference group in levels of group.
#' @param ... additional variables save to `outdata`.
#'
#' @return A list with class `outdata`. Components of the list are either quosures or constants.
#'
#' @export
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

  # Input Checking
  validate_outdata(x)
}

#' Validate outdata class
#'
#' @param x an `outdata` object
#' @param env an environment
#'
#'
new_outdata <- function(x, env = globalenv()) {
  if (!is.list(x)) {
    rlang::abort("`x` must be a list")
  }

  structure(x, class = "outdata")
}

#' Validate outdata class
#'
#' @param x an `outdata` object
#'
#'
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
