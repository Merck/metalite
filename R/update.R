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

#' Update mapping rule in `adam_mapping`
#'
#' @inheritParams define_plan
#' @param name A vector of keywords.
#' @param ... Additional variables to be added in the mapping rule
#'   among those keywords.
#'
#' @return A metadata object with the input updated.
#'
#' @export
#'
#' @examples
#' meta <- meta_example()
#' meta <- update_adam_mapping(meta, names(meta$parameter), start_date = "ASTDT")
#' collect_adam_mapping(meta, "ser")
update_adam_mapping <- function(meta, name, ...) {
  map <- lapply(name, collect_adam_mapping, meta = meta)

  for (i in 1:length(map)) {
    x <- new_adam_mapping(c(map[[i]], list(...)))
    meta[[x$.location]][[x$name]] <- x
  }

  meta
}
