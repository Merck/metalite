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

#' Specification for analysis output filename
#'
#' @inheritParams define_population
#' @inheritParams plan
#'
#' @return A vector of character strings containing the RTF file names.
#'
#' @export
#'
#' @examples
#' meta <- meta_example()
#' spec_filename(meta)
spec_filename <- function(meta) {
  x <- split(meta$plan, meta$plan$analysis)

  x <- lapply(x, function(xx) {
    n_term <- apply(xx, 2, function(term) length(unique(term)))
    n_term <- c("analysis", setdiff(
      names(xx)[n_term > 1],
      "mock"
    ))

    if (length(xx[, n_term]) == 1) {
      xx$filename <- xx[, n_term]
    } else {
      xx$filename <- do.call(paste, xx[, n_term])
    }

    xx$filename <- paste0(gsub("[-_ ;]", "0", xx$filename), ".rtf")

    xx$filename <- gsub("[<>=]", "", xx$filename)
    xx
  })


  x <- unsplit(x, meta$plan$analysis)

  x$filename
}

#' Specification for population definition
#'
#' @inheritParams define_population
#'
#' @return A vector of character strings containing the populations used
#'   in the order of the analysis plans.
#'
#' @export
#'
#' @examples
#' meta <- meta_example()
#' spec_analysis_population(meta)
spec_analysis_population <- function(meta) {
  plan <- meta$plan

  pop <- mapply(collect_population,
    population = plan$population,
    observation = plan$observation,
    parameter = plan$parameter,
    MoreArgs = list(meta = meta),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  vapply(pop, function(x) {
    x1 <- paste("Population:", x$population)
    if (is.null(x$parameter)) {
      x2 <- paste("Observation:", x$observation)
    } else {
      x2 <- paste("Observation:", paste(x$observation, x$parameter, collapse = " & "))
    }

    paste(x1, x2, sep = "\n")
  }, character(1))
}

#' Specification for analysis call program
#'
#' @inheritParams define_population
#' @param ... Additional arguments used in all call programs.
#'
#' @return A vector of character strings containing the call program
#'   in the order of the analysis plans.
#'
#' @export
#'
#' @examples
#' meta <- meta_example()
#' spec_call_program(meta)
#' spec_call_program(meta, data_source = "[Study CDISCpilot: adam-adsl; adae]")
spec_call_program <- function(meta, ...) {
  plan <- meta$plan

  fun <- c()
  for (i in 1:nrow(meta$plan)) {
    x <- meta$plan[i, -1]
    x$analysis <- as.symbol(x$analysis)
    x$meta <- substitute(meta)

    # re-order call information
    x <- x[, c("analysis", "meta", setdiff(names(x), c("analysis", "meta")))]

    x <- c(as.list(x), list(...))

    fun[i] <- gsub("`", "", fmt_quote(deparse1(as.call(x))))
  }

  fun
}

#' Specification for analysis title
#'
#' @inheritParams define_population
#'
#' @return A vector of character strings containing the table captions
#'   in the order of the analysis plans.
#'
#' @export
#'
#' @examples
#' meta <- meta_example()
#' spec_title(meta)
spec_title <- function(meta) {
  plan <- meta$plan

  title <- mapply(collect_title,
    population = plan$population,
    observation = plan$observation,
    parameter = plan$parameter,
    analysis = plan$analysis,
    MoreArgs = list(meta = meta),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  vapply(title, paste, character(1), collapse = "\n")
}
