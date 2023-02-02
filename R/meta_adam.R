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

#' Create a metadata representation for ADaM data analysis
#'
#' @param observation A data frame for observation level data.
#' @param population A data frame for population level data.
#'   Default is the same as `observation`.
#'
#' @return An initialized metadata object with
#'   observation and population defined.
#'
#' @export
#'
#' @examples
#' meta_adam(observation = r2rtf::r2rtf_adae, population = r2rtf::r2rtf_adae)
meta_adam <- function(observation,
                      population = observation) {
  attr(population, "data_name") <- deparse(substitute(population))
  attr(observation, "data_name") <- deparse(substitute(observation))

  # Creating a `meta_adam` by using structure() function
  structure(
    list(
      data_population = population,
      data_observation = observation,
      plan = list(),
      observation = list(),
      population = list(),
      parameter = list(),
      analysis = list()
    ),
    class = "meta_adam"
  )
}

#' Print a metadata object with its population, observation, and analysis plans
#'
#' @param x An object returned by [meta_adam()].
#' @param ... Additional parameters for [print()] (not used).
#'
#' @return A printed summary of the metadata.
#'
#' @export
#'
#' @examples
#' meta_adam(observation = r2rtf::r2rtf_adae, population = r2rtf::r2rtf_adae) |> print()
print.meta_adam <- function(x, ...) {
  e <- c(".$data_population", ".$data_observation", ".$plan")

  # print the number of subjects in population & observation
  cat("ADaM metadata:", "\n")
  cat("  ", e[1], "\tPopulation data", "with", nrow(x$data_population), "subjects", "\n")
  cat("  ", e[2], "\tObservation data", "with", nrow(x$data_observation), "records", "\n")

  # print the number of analysis plans
  if (length(x$plan) > 0) {
    cat("  ", e[3], "\tAnalysis plan", "with", nrow(x$plan), "plans", "\n")
  }

  cat("\n\n")

  # print the details of population
  if (length(x$population) > 0) {
    cat(" ", "Analysis population type:\n")
    print(bind_rows2(lapply(x$population, as.data.frame)))
    cat("\n\n")
  }

  # print the details of observation
  if (length(x$observation) > 0) {
    cat(" ", "Analysis observation type:\n")
    print(bind_rows2(lapply(x$observation, as.data.frame)))
    cat("\n\n")
  }

  # print the details of parameters
  if (length(x$parameter) > 0) {
    cat(" ", "Analysis parameter type:\n")
    print(bind_rows2(lapply(x$parameter, as.data.frame))[, c("name", "label", "subset")])
    cat("\n\n")
  }

  # print the details of analysis
  if (length(x$analysis) > 0) {
    cat(" ", "Analysis function:\n")
    print(bind_rows2(lapply(x$analysis, as.data.frame))[, c("name", "label")])
    cat("\n")
  }
}
