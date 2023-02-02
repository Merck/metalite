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

#' Check variable name are in metadata
#'
#' @inheritParams define_plan
#' @param var A character vector of variable names.
#' @param type A character vector of the data type to check.
#'
#' @return The input object (invisibly). Nothing is printed when check
#'   is passed, otherwise some error messages.
#'
#' @noRd
#'
#' @examples
#' metalite:::meta_check_var(meta_example(), var = "RACE")
meta_check_var <- function(meta,
                           var,
                           type = c("population", "observation")) {
  pop <- meta$data_population
  obs <- meta$data_observation

  if ((!all(var %in% names(pop))) & "population" %in% type) {
    stop("Can not find all variables in the population level data")
  }

  if (!all(var %in% names(obs)) & "observation" %in% type) {
    stop("Can not find all variables in the observation level data")
  }

  invisible(meta)
}
