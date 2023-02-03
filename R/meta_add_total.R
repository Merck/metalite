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

#' Add duplicate data to enable a total group
#'
#' @param meta A metalite object.
#' @param total A character value of total group name.
#'
#' @return A metadata object with a total group added.
#'
#' @export
#'
#' @examples
#' x <- meta_add_total(meta_example())
#' # A `Total` group is added
#' table(x$data_population$TRTA)
meta_add_total <- function(meta, total = "Total") {
  stopifnot(length(total) == 1)

  pop <- meta$data_population
  obs <- meta$data_observation

  pop_grp <- vapply(meta$population, "[[", FUN.VALUE = character(1), "group")
  obs_grp <- vapply(meta$population, "[[", FUN.VALUE = character(1), "group")
  grp <- unique(c(pop_grp, obs_grp))

  for (i in seq(grp)) {
    pop[[grp[i]]] <- factor(total)
    obs[[grp[i]]] <- factor(total)
  }

  meta$data_population <- rbind(meta$data_population, pop)
  meta$data_observation <- rbind(meta$data_observation, obs)

  meta
}
