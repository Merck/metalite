#    Copyright (c) 2022 Merck Sharp & Dohme Corp. a subsidiary of Merck & Co., Inc., Kenilworth, NJ, USA.
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

#' Update mapping rule in `adam_mapping`
#'
#' @inheritParams define_plan
#' @param name a vector of keywords
#' @param ... additional variables to be added in the mapping rule among those keywords.
#' @return a metadata with the input updated
#' @examples
#' meta <- meta_dummy()
#' meta <- update_adam_mapping(meta, names(meta$parameter), start_date = "ASTDT")
#' collect_adam_mapping(meta, "ser")
#' @export
update_adam_mapping <- function(meta, name, ...) {
  map <- lapply(name, collect_adam_mapping, meta = meta)

  for (i in 1:length(map)) {
    x <- new_adam_mapping(c(map[[i]], list(...)))
    meta[[x$.location]][[x$name]] <- x
  }

  meta
}
