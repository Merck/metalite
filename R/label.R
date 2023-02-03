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

#' A function to get the labels of data frame columns
#'
#' @param data A data frame.
#'
#' @return Labels of the input data frame.
#'
#' @export
#'
#' @examples
#' get_label(r2rtf::r2rtf_adae)
get_label <- function(data) {
  label <- vapply(data, function(x) {
    if (is.null(attr(x, "label"))) {
      return(NA_character_)
    } else {
      attr(x, "label")
    }
  }, FUN.VALUE = character(1))

  ifelse(is.na(label), names(data), label)
}

#' A function to assign labels to a data frame
#'
#' @details
#' - Case 1:
#'   If the variable's label is already define in the original data frame
#'   but not redefined in `assign_label(...)`, its original labels will be kept.
#' - Case 2:
#'   If the variable's label is already define in the original data frame
#'   but re-defined by `assign_label(...)`, its labels will be re-defined.
#' - Case 3:
#'   If the variable's label is not define in the original data frame
#'   but it is defined by `assign_label(...)`, its labels will added.
#' - Case 4:
#'   If the variable's label is not define in the original data frame,
#'   neither was it defined by `assign_label(...)`, its labels will be the
#'   variable name itself.
#'
#' @param data A data frame.
#' @param var The variables to assign labels.
#' @param label The labels to be assigned.
#'
#' @return A data frame with labels updated.
#'
#' @export
#'
#' @examples
#' assign_label(r2rtf::r2rtf_adae) |> head()
#' assign_label(
#'   r2rtf::r2rtf_adae,
#'   var = "USUBJID",
#'   label = "Unique subject identifier"
#' ) |> head()
assign_label <- function(data,
                         var = names(data),
                         label = names(data)) {
  # input checking
  stopifnot(length(var) == length(label))
  stopifnot(!any(duplicated(var)))

  # get existing labels and its corresponding variables
  name <- names(data)
  existing_lables <- get_label(data)
  existing_labels_var <- names(existing_lables)

  # assign label
  for (i in seq(name)) {
    if (name[i] %in% existing_labels_var & !(name[i] %in% var)) {
      next
    } else if (name[i] %in% var) {
      attr(data[[i]], "label") <- label[names(data[i]) == var]
    } else {
      attr(data[[i]], "label") <- name[i]
    }
  }

  data
}
