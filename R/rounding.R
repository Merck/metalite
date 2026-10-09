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

#' Round half away from zero
#'
#' Round numeric values to a given number of decimal places, with decimal
#' ties (for example, 1.25 at `digits = 1`) rounded half away from zero.
#' This differs from [base::round()], which uses round-to-even for ties.
#' Values that round to zero, including small negative values, return positive
#' zero so formatted output does not display negative zero.
#'
#' To account for floating-point representation, values within
#' `sqrt(.Machine$double.eps)` below a tie at the requested precision are
#' treated as ties. The implementation is adapted from `roundSAS()` in
#' [pharmaverse/tidytlg](https://github.com/pharmaverse/tidytlg/blob/5f169c76428976f53d2af9e7fe52460348ef6cb7/R/roundSAS.R).
#'
#' @param x A numeric vector, matrix, array, or data frame with only numeric
#'   columns.
#' @param digits A finite, integer-valued scalar giving the number of decimal
#'   places. Negative values round to positions left of the decimal point.
#'
#' @return A numeric object with the same dimensions, dimension names, and
#'   names as `x`. A data frame input returns a data frame.
#'
#' @export
#'
#' @examples
#' round_half_away_from_zero(c(1.25, -1.25), digits = 1)
#' round_half_away_from_zero(c(-0.04, NA), digits = 1)
round_half_away_from_zero <- function(x, digits = 0) {
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
    !is.finite(digits) || digits %% 1 != 0) {
    stop("`digits` must be one finite integer.", call. = FALSE)
  }

  if (is.data.frame(x)) {
    if (!all(vapply(x, is.numeric, logical(1)))) {
      stop("All columns of `x` must be numeric.", call. = FALSE)
    }

    x[] <- lapply(x, function(col) round_half_away_from_zero(col, digits = digits))
    return(x)
  }

  if (!is.numeric(x)) {
    stop("`x` must be numeric.", call. = FALSE)
  }

  n <- names(x)
  d <- dim(x)
  dn <- dimnames(x)

  posneg <- sign(x)
  z <- abs(x)
  finite <- is.finite(z)
  tolerance <- sqrt(.Machine$double.eps) * 10^-digits
  z[finite] <- z[finite] + pmin(tolerance, .Machine$double.xmax - z[finite])
  z <- round(z, digits = digits)
  z <- ifelse(!is.na(z) & z > 0, z * posneg, z)

  dim(z) <- d
  dimnames(z) <- dn
  names(z) <- n

  z
}

#' Format numbers with fixed decimal places
#'
#' Round with [round_half_away_from_zero()] and format with a fixed number of
#' decimal places. Decimal ties round half away from zero, and values that
#' round to zero display as positive zero (for example, `"0.0"` rather than
#' `"-0.0"`).
#'
#' @param x A numeric vector.
#' @param digits A non-negative, integer-valued scalar giving the number of
#'   decimal places.
#' @param width `NULL`, or a non-negative, integer-valued scalar giving the
#'   minimum field width passed to [base::formatC()]. The default, `NULL`, does
#'   not set a minimum width.
#'
#' @return A character vector containing the formatted values.
#'
#' @export
#'
#' @examples
#' format_number(c(1.25, -1.25), digits = 1)
#' format_number(c(6.25, -0.04), digits = 1, width = 5)
format_number <- function(x, digits = 1, width = NULL) {
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
    !is.finite(digits) || digits %% 1 != 0 || digits < 0) {
    stop("`digits` must be one non-negative integer.", call. = FALSE)
  }
  if (!is.null(width) &&
    (!is.numeric(width) || length(width) != 1L || is.na(width) ||
      !is.finite(width) || width %% 1 != 0 || width < 0)) {
    stop("`width` must be NULL or one non-negative integer.", call. = FALSE)
  }

  x <- round_half_away_from_zero(x, digits = digits)

  out <- if (is.null(width)) {
    formatC(x, digits = digits, format = "f")
  } else {
    formatC(x, digits = digits, format = "f", width = width)
  }

  out
}
