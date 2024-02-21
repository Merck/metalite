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

#' Construct ADaM mappings
#'
#' ADaM mappings describe how variables and meta information in the ADaM data
#' are mapped to standardized term.
#'
#' The design is inspired by `ggplot2::aes()`.
#'
#' @param name A character value of term name.
#'   The term name is used as key to link information.
#' @param id A character value of subject identifier variable name
#'   in an ADaM dataset.
#' @param group A character vector of group variable names in an ADaM dataset.
#' @param var A character vector of useful variable names in an ADaM dataset.
#' @param subset An expression to identify analysis records.
#'   See [base::subset()].
#' @param label A character value of analysis label.
#' @param ... Additional variables.
#'
#' @return A list with class `adam_mapping`.
#'   Components of the list are either quosures or constants.
#'
#' @export
#'
#' @examples
#' adam_mapping(
#'   name = "apat",
#'   id = "USUBJID",
#'   group = "TRT01A",
#'   subset = TRTFL == "Y",
#'   label = "All Participants as Treated"
#' )
adam_mapping <- function(name,
                         id = NULL,
                         group = NULL,
                         var = NULL,
                         subset = NULL,
                         label = NULL,
                         ...) {
  try(subset, silent = TRUE)
  list(...)

  exprs <- rlang::enquos(
    name = name,
    id = id,
    group = group,
    var = var,
    subset = subset,
    label = label,
    ...,
    .ignore_empty = "all"
  )

  x <- new_adam_mapping(exprs, env = parent.frame())

  # Input Checking
  validate_adam_mapping(x)
}

new_mapping <- function(x, env = globalenv()) {
  if (rlang::is_quosure(x)) {
    x <- rlang::quo_get_expr(x)
    return(x)
  }

  x
}

new_adam_mapping <- function(x, env = globalenv()) {
  if (!is.list(x)) {
    rlang::abort("`x` must be a list")
  }
  x <- lapply(x, new_mapping, env = env)
  structure(x, class = "adam_mapping")
}

#' Check adam_mapping class
#'
#' @param x An `adam_mapping` object.
#'
#' @noRd
#'
#' @examples
#' validate_adam_mapping(new_adam_mapping(list(name = "apat")))
validate_adam_mapping <- function(x) {
  # All required variable
  char <- c("name", "id", "group", "var", "header", "label")

  # Length 1 variable
  char_length_1 <- c("name", "id", "label")

  # Expression variable
  expr <- c("subset")

  # Check duplicate name
  check_duplicate_name(x)

  # Check "name"
  if (is.null(x[["name"]])) {
    rlang::abort("variable 'name' is required")
  }

  # Check required variable
  lapply(char, function(term) {
    if (!is.null(x[[term]])) {
      if (!rlang::is_character(x[[term]])) {
        rlang::abort(gluestick("variable '{term}' must be a character value"))
      }

      if (term %in% char_length_1 & length(x[[term]]) > 1L) {
        rlang::abort(gluestick("variable '{term}' must be length 1"))
      }
    }
  })

  # Check expression variable
  lapply(expr, function(term) {
    if (!(rlang::is_expression(x[[term]]) | rlang::is_null(x[[term]]))) {
      rlang::abort(gluestick("variable '{term}' must be an expression"))
    }
  })

  x
}

#' @export
print.adam_mapping <- function(x, ...) {
  cat("ADaM mapping: \n")

  if (length(x) == 0) {
    cat("<empty>\n")
  } else {
    values <- vapply(x, rlang::as_label, character(1))
    bullets <- paste0("* ", format(paste0("`", names(x), "`")), " -> ", values, "\n")

    cat(bullets, sep = "")
  }

  invisible(x)
}

#' @export
"[.adam_mapping" <- function(x, i, ...) {
  new_adam_mapping(NextMethod())
}

#' @export
"[[<-.adam_mapping" <- function(x, i, value) {
  new_adam_mapping(NextMethod())
}

#' @export
"$<-.adam_mapping" <- function(x, i, value) {
  new_adam_mapping(NextMethod())
}

#' @export
"[<-.adam_mapping" <- function(x, i, value) {
  new_adam_mapping(NextMethod())
}

#' @export
as.data.frame.adam_mapping <- function(x, row.names = NULL, optional = FALSE, ...) {
  foo <- function(term) {
    if (is.null(term)) {
      return("")
    }

    if (rlang::is_expression(term)) {
      fmt_quote(rlang::as_label(term))
    } else {
      paste(term, collapse = ", ")
    }
  }

  do.call(data.frame, lapply(x, foo))
}

#' @export
merge.adam_mapping <- function(x, y, all.x = TRUE, ...) {
  x_name <- names(omit_null(x))
  y_name <- names(omit_null(y))
  d_name <- setdiff(y_name, x_name)
  x[d_name] <- y[d_name]

  x
}
