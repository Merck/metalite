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
#' @param observation Observation level data. One of:
#'   * a data frame;
#'   * a character value of one or more dataset names separated by `";"`
#'     (e.g. `"adae;adlb"`); each name is retrieved with [get()] from the
#'     calling environment;
#'   * a named list of data frames.
#'
#'   When more than one dataset is supplied, the first is the primary
#'   observation dataset and the rest are registered as additional source
#'   datasets. Any dataset can then be selected by name through the `from`
#'   argument of [define_observation()], allowing a single metadata object to
#'   drive analyses that read from different source datasets (for example an AE
#'   dataset and a lab dataset).
#' @param population Population level data, specified the same way as
#'   `observation`. Default is the same as `observation`.
#'
#' @return An initialized metadata object with
#'   observation and population defined.
#'
#' @export
#'
#' @examples
#' meta_adam(observation = r2rtf::r2rtf_adae, population = r2rtf::r2rtf_adae)
#'
#' # Multiple observation datasets by name
#' adae <- r2rtf::r2rtf_adae
#' adsl <- r2rtf::r2rtf_adsl
#' meta_adam(population = "adsl", observation = "adae;adsl")
meta_adam <- function(observation,
                      population = observation) {
  if (missing(observation)) stop("`observation` is required")

  env <- parent.frame()
  obs_sub <- substitute(observation)
  obs_list <- resolve_source(observation, obs_sub, env, "observation")

  if (missing(population)) {
    pop_list <- obs_list
  } else {
    pop_sub <- substitute(population)
    pop_list <- resolve_source(population, pop_sub, env, "population")
  }

  # The first dataset of each side is the primary dataset, stored in its own
  # slot so it stays the single source of truth (and stays in sync with any
  # in-place edits, e.g. by `meta_add_total()`). Remaining datasets are
  # registered in `data_source` and can be selected by name via `from`.
  data_population <- pop_list[[1]]
  data_observation <- obs_list[[1]]

  extra <- c(pop_list[-1], obs_list[-1])
  extra <- extra[!duplicated(names(extra))]

  structure(
    list(
      data_population = data_population,
      data_observation = data_observation,
      plan = list(),
      observation = list(),
      population = list(),
      parameter = list(),
      analysis = list(),
      data_source = extra,
      data_population_name = names(pop_list)[1],
      data_observation_name = names(obs_list)[1]
    ),
    class = "meta_adam"
  )
}

#' Normalize a `meta_adam()` dataset argument into a named list of data frames
#'
#' @param value The value of the `observation`/`population` argument.
#' @param sub The captured (unevaluated) expression of that argument, used to
#'   name a bare data frame.
#' @param env The environment in which `";"`-separated dataset names are
#'   resolved with [get()].
#' @param default_name Fallback name for an anonymous data frame.
#'
#' @return A named list of data frames, each carrying a `"data_name"` attribute.
#'
#' @noRd
resolve_source <- function(value, sub, env, default_name) {
  # `";"`-separated dataset names, resolved with get()
  if (is.character(value)) {
    nms <- trimws(unlist(strsplit(value, ";")))
    nms <- nms[nzchar(nms)]
    if (length(nms) == 0) {
      stop("`", default_name, "` does not name any dataset")
    }
    out <- lapply(nms, function(n) {
      d <- get(n, envir = env)
      if (!is.data.frame(d)) stop("object '", n, "' is not a data frame")
      attr(d, "data_name") <- n
      d
    })
    names(out) <- nms
    return(out)
  }

  # named list of data frames
  if (is.list(value) && !is.data.frame(value)) {
    if (is.null(names(value)) || any(!nzchar(names(value)))) {
      stop("a list of datasets passed to `meta_adam()` must be named")
    }
    out <- Map(function(d, n) {
      if (!is.data.frame(d)) stop("element '", n, "' is not a data frame")
      attr(d, "data_name") <- n
      d
    }, value, names(value))
    return(out)
  }

  # a single data frame (backward compatible)
  if (is.data.frame(value)) {
    nm <- if (is.symbol(sub)) deparse(sub) else default_name
    attr(value, "data_name") <- nm
    out <- list(value)
    names(out) <- nm
    return(out)
  }

  stop(
    "`", default_name, "` must be a data frame, a ';'-separated string of ",
    "dataset names, or a named list of data frames"
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

  # print extra registered source datasets, if any
  if (length(x$data_source) > 0) {
    for (nm in names(x$data_source)) {
      cat(
        "  ", paste0(".$data_source$", nm), "\tSource data", "with",
        nrow(x$data_source[[nm]]), "records", "\n"
      )
    }
  }

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

  invisible(x)
}
