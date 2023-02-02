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

#' Create a data exploration `meta_adam` object
#'
#' @param data A data frame.
#' @inheritParams define_population
#'
#' @return A metadata object.
#'
#' @export
#'
#' @examples
#' meta <- meta_example_exploration(r2rtf::r2rtf_adsl, group = "TRT01A")
#' collect_n_subject(meta, "ase", "AGE")
#' collect_n_subject(meta, "ase", "SEX")
meta_example_exploration <- function(data,
                                   group,
                                   name = "ase",
                                   subset = NULL,
                                   label = "All Subjects Enrolled") {
  meta <- meta_adam(observation = data)

  var <- names(data)
  var_label <- vapply(data, function(x) attr(x, "label"), FUN.VALUE = "character")
  names(var_label) <- NULL

  # add analysis plan of all variables
  meta <- define_plan(meta, plan = plan(
    analysis = "exploration",
    population = name,
    observation = "inf",
    parameter = paste(var, collapse = ";")
  ))

  # define population
  meta <- define_population(meta,
    name = name,
    group = group,
    subset = subset,
    label = label
  )

  # define observation
  meta <- define_observation(meta,
    name = "inf",
    subset = NULL,
    label = "All Observations"
  )

  # define parameter
  for (i in seq(var)) {
    meta <- define_parameter(meta, name = var[i], var = var[i], label = var_label[i], subset = NULL)
  }

  # define analysis
  meta <- define_analysis(meta, name = "exploration", label = "Data Exploration")

  # build metadata
  meta <- meta_build(meta)
}
