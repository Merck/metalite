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

#' Omit `NULL` values in a list
#'
#' @param x A list.
#'
#' @noRd
#'
#' @examples
#' omit_null(list(a = 1, b = NULL))
omit_null <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

#' Format quote
#'
#' @param x A vector of character strings.
#'
#' @noRd
#'
#' @examples
#' 'a = "b"'
#' fmt_quote('a = "b"')
fmt_quote <- function(x) {
  # replace `"` into `'` in x
  gsub('"', "'", x)
}

#' Check duplicate names
#'
#' @param x A list.
#'
#' @noRd
#'
#' @examples
#' check_duplicate_name(list(a = 1, b = 2, b = 3))
check_duplicate_name <- function(x) {
  duplicated_names <- names(x)[duplicated(names(x))]
  if (length(duplicated_names) > 0L) {
    duplicated_message <- paste0(unique(duplicated_names), collapse = ", ")
    rlang::warn(glue::glue("Duplicated name: {duplicated_message}"))
  }
  x
}

#' Format sentence
#'
#' @inheritParams fmt_quote
#'
#' @noRd
#'
#' @examples
#' fmt_sentence(" a sentence that  needs to be cleaned  ")
fmt_sentence <- function(x) {
  # Replace internal extra whitespace and leading
  # and/or trailing whitespace from character strings.
  trimws(gsub("\\s+", " ", x))
}

#' Reset dataset label
#'
#' @param data A data frame.
#' @param data_label A data frame with label.
#'
#' @noRd
reset_label <- function(data, data_label) {
  name <- names(data)

  for (i in seq(name)) {
    attr(data[[i]], "label") <- attr(data_label[[name[i]]], "label")
  }

  data
}
