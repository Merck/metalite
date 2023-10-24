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

#' Execute analysis based on the analysis plan
#'
#' @inherit define_plan
#' @param i A vector of integers to indicate `i`-th analysis in `meta$plan`.
#' @param ... Additional arguments passed to `[spec_call_program()]`.
#'
#' @return Executed analysis based on the analysis plan.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   meta <- meta_example()
#'   ae_summary <- function(...) {
#'     "results of ae_summary"
#'   }
#'   ae_specific <- function(...) {
#'     "results of ae_specific"
#'   }
#'   meta_run(meta)
#'   meta_run(meta, i = 2)
#' }
meta_run <- function(meta, i = NULL, ...) {
  if (is.null(i)) i <- 1:nrow(meta$plan)
  call <- spec_call_program(meta, ...)
  call <- call[i]
  res <- lapply(call, function(x) eval(parse(text = x)))
  names(res) <- call
  res
}
