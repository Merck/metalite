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

default_all <- function() {
  default_all <- c(
    default_population,
    default_observation,
    default_parameter_ae,
    default_analysis
  )
  check_duplicate_name(default_all)
}

#' Apply default values to ADaM mappings
#'
#' @param x An `adam_mapping` object.
#'
#' @return Similar to the input, but with the missing values
#'   updated to the default values.
#'
#' @export
#'
#' @examples
#' default_apply(adam_mapping(name = "apat"))
default_apply <- function(x) {
  default <- default_all()

  i <- which(x$name == names(default))

  if (length(i) == 0) {
    return(x)
  }

  default <- default[[i]]

  x <- merge.adam_mapping(x, default)

  x
}
