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

# These function are directly copied from
# https://github.com/nathaneastwood/poorman/tree/v0.2.5

#' Check whether any elements of a list are nested
#'
#' @param lst A list.
#'
#' @noRd
#'
#' @examples
#' is_nested(list(a = 1, b = 2, c = 3))
#' is_nested(list(a = 1, b = list(c = 2, d = 3)))
is_nested <- function(lst) vapply(lst, function(x) inherits(x[1L], "list"), FALSE)

is_named <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    return(FALSE)
  }
  if (any(names_are_invalid(nms))) {
    return(FALSE)
  }
  TRUE
}

names_are_invalid <- function(x) {
  x == "" | is.na(x)
}

#' Check whether the input is an atomic vector or a data frame
#' @noRd
is_df_or_vector <- function(x) {
  res <- is.data.frame(x) || is.atomic(x)
  if (isFALSE(res)) stop("You must pass vector(s) and/or data.frame(s).")
  TRUE
}

#' Move entries within a list up one level
#' @noRd
flatten <- function(lst) {
  nested <- is_nested(lst)
  res <- c(lst[!nested], unlist(lst[nested], recursive = FALSE))
  if (sum(nested)) Recall(res) else return(res)
}

bind_rows2 <- function(..., .id = NULL) {
  lsts <- list(...)
  lsts <- flatten(lsts)
  lsts <- Filter(Negate(is.null), lsts)
  lapply(lsts, function(x) is_df_or_vector(x))
  lapply(lsts, function(x) if (is.atomic(x) && !is_named(x)) stop("Vectors must be named."))

  if (!missing(.id)) {
    lsts <- lapply(seq_along(lsts), function(i) {
      nms <- names(lsts)
      id_df <- data.frame(id = if (is.null(nms)) as.character(i) else nms[i], stringsAsFactors = FALSE)
      colnames(id_df) <- .id
      cbind(id_df, lsts[[i]])
    })
  }

  nms <- unique(unlist(lapply(lsts, names)))
  lsts <- lapply(
    lsts,
    function(x) {
      if (!is.data.frame(x)) x <- data.frame(as.list(x), stringsAsFactors = FALSE)
      for (i in nms[!nms %in% names(x)]) x[[i]] <- NA
      x
    }
  )
  names(lsts) <- NULL
  do.call(rbind, lsts)
}
