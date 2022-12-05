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

#' Omit NULL values in a list
#'
#' @param x a list.
#'
#' @examples
#' \dontrun{
#' omit_null(list(a = 1, b = NULL))
#' }
#'
omit_null <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

#' Format quote
#'
#' @param x a vector of character string.
#'
#' @examples
#' \dontrun{
#' 'a = "b"'
#' fmt_quote('a = "b"')
#' }
fmt_quote <- function(x) {
  # replace `"` into `'` in x
  gsub('"', "'", x)
}

#' Check duplicate name
#'
#' @param x a list
#'
#' @examples
#' \dontrun{
#' check_duplicate_name(list(a = 1, b = 2, b = 3))
#' }
#'
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
#' @examples
#' \dontrun{
#' fmt_sentence(" a sentence that  needs to be cleaned  ")
#' }
fmt_sentence <- function(x) {
  # replace innternal extra whitespace and leading
  # and/or trailing whitespace from character strings.
  trimws(gsub("\\s+", " ", x))
}


#' Reset Dataset Label
#'
#' @param data a data frame
#' @param data_label a data frame with label
reset_label <- function(data, data_label) {
  name <- names(data)

  for (i in seq(name)) {
    attr(data[[i]], "label") <- attr(data_label[[name[i]]], "label")
  }

  data
}
