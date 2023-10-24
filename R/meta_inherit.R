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

#' Inherit meta information by keywords
#'
#' @param meta A `meta_adam` object.
#' @param inherit A `meta_adam` object to be inherit.
#' @param name A vector of keywords from `meta_inherit` to `meta_adam`.
#' @param overwrite A logical value to force mapping update.
#'
#' @return A metadata object with population defined.
#'
#' @examples
#' meta_adam(
#'   population = r2rtf::r2rtf_adsl,
#'   observation = r2rtf::r2rtf_adae
#' ) |>
#'   meta_inherit(meta_example(), c("apat", "wk12", "ae_summary"))
#'
#' @export
meta_inherit <- function(
    meta,
    inherit,
    name,
    overwrite = FALSE) {
  mapping <- list()
  for (i in seq_along(name)) {
    x <- collect_adam_mapping(inherit, name[i])
    if (is.null(x)) {
      stop(name[i], ": keyword is not defined in the `inherit` meta information")
    } else {
      mapping[[i]] <- x
    }
  }

  for (i in seq_along(mapping)) {
    if (is.null(collect_adam_mapping(meta, name[[i]])) | overwrite) {
      meta[[mapping[[i]][[".location"]]]][[name[[i]]]] <- mapping[[i]]
    }
  }

  meta
}
