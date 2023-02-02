#    Copyright (c) 2022 Merck & Co., Inc., Rahway, NJ, USA and its affiliates. All rights reserved.
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

#' Divide Meta into Groups
#'
#' @inheritParams define_plan
#' @param by a character variable name both in
#' population level and observation level data of a meta object.
#' @return a metadata split by the input variable
#' @export
#'
#' @examples
#' meta_dummy() |>
#'   meta_split("RACE")
meta_split <- function(meta, by) {
  meta_check_var(meta, by)

  pop <- meta$data_population
  obs <- meta$data_observation

  pop_by <- split(pop, pop[[by]])
  obs_by <- split(obs, obs[[by]])

  res <- list()
  for (i in 1:length(pop_by)) {
    res[[i]] <- meta
    res[[i]]$data_population <- pop_by[[i]]
    res[[i]]$data_observation <- obs_by[[i]]
  }
  names(res) <- names(pop_by)

  res
}
