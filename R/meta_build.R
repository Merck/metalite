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

#' Build complete meta information
#'
#' @inheritParams define_population
#'
#' @return A composed metadata object.
#'
#' @export
#'
#' @examples
#' meta_adam(
#'   observation = r2rtf::r2rtf_adae,
#'   population = r2rtf::r2rtf_adsl
#' ) |>
#'   # define analysis plan
#'   define_plan(
#'     plan(
#'       analysis = "ae_summary",
#'       population = "apat",
#'       observation = c("wk12"),
#'       parameter = "any;rel"
#'     )
#'   ) |>
#'   # define population
#'   define_population(
#'     name = "apat",
#'     group = "TRT01A",
#'     subset = SAFFL == "Y"
#'   ) |>
#'   # define observation
#'   define_observation(
#'     name = "wk12",
#'     group = "TRTA",
#'     subset = SAFFL == "Y",
#'     label = "Weeks 0 to 12"
#'   ) |>
#'   # define parameter - rel
#'   define_parameter(
#'     name = "rel",
#'     subset = AEREL %in% c("POSSIBLE", "PROBABLE")
#'   ) |>
#'   # define analysis
#'   define_parameter(
#'     name = "rel",
#'     subset = AEREL %in% c("POSSIBLE", "PROBABLE")
#'   ) |>
#'   meta_build()
meta_build <- function(meta) {
  # Attach
  data_pop <- meta$data_population
  data_obs <- meta$data_observation

  plan <- meta$plan

  # Input Checking
  if (is.null(data_pop)) stop("analysis population dataset is not defined")
  if (is.null(data_obs)) stop("analysis observation dataset is not defined")
  if (length(plan) == 0) stop("analysis plan is not defined")

  # Add default value (if users do not define the key words)
  types <- c("population", "observation", "parameter", "analysis")

  for (i in seq_along(types)) {
    type <- types[i]
    u_term <- unique(trimws(unlist(strsplit(plan[[type]], split = ";"))))

    meta[[type]][u_term] <- lapply(u_term, function(x) {
      if (is.null(meta[[type]][[x]])) meta[[type]][[x]] <- adam_mapping(name = !!x)
      default_apply(meta[[type]][[x]])
    })
  }

  # Update observation default values (recall in `meta_adam`, observation = population)
  for (i in seq_along(names(meta$observation))) {
    id <- lapply(meta$population, `[[`, "id")
    id <- unique(unlist(id))

    group <- lapply(meta$population, `[[`, "group")
    group <- unique(unlist(group))

    if (length(id) == 1 & is.null(meta$observation[[i]]$id)) {
      meta$observation[[i]]$id <- id
    }

    if (length(group) == 1 & is.null(meta$observation[[i]]$group)) {
      meta$observation[[i]]$group <- group
    }
  }

  meta_validate(meta)
}
