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

#' Assemble RTF files into one single RTF file
#'
#' @param path a vector of RTF file path
#' 
#' @examples 
#' library(r2rtf)
#' 
#' head(iris) %>%
#' rtf_body() %>%
#' rtf_encode() %>%
#' write_rtf(file = paste0(tempdir(), "/ex-1.rtf"))
#' 
#' tail(iris) %>%
#' rtf_body() %>%
#' rtf_encode() %>%
#' write_rtf(file = paste0(tempdir(), "/ex-2.rtf"))
#' 
#' rtf_assemble(c(paste0(tempdir(), "/ex-1.rtf"), paste0(tempdir(), "/ex-2.rtf")))
#'
#' @export
rtf_assemble <- function(path) {
  rtf <- lapply(path, readLines)

  n <- length(rtf)

  start <- c(1, rep(2, n - 1))
  end <- vapply(rtf, length, numeric(1))
  end[-n] <- end[-n] - 1

  for (i in 1:n) {
    rtf[[i]] <- rtf[[i]][start[i]:end[i]]
    if (i < n) rtf[[i]] <- c(rtf[[i]], r2rtf:::as_rtf_new_page())
  }
  rtf <- do.call(c, rtf)

  rtf
}

#' Enable color for mockup table
#'
#' The function will automatically replace text between `<` and `>` into blue.
#'
#' @param text a string.
#' @examples 
#' library(r2rtf)
#' 
#' head(iris)  %>%
#' r2rtf::rtf_title(rtf_mock_color("{week}")) %>%
#' rtf_body() %>%
#' rtf_encode() %>%
#' write_rtf(file = paste0("~/ex-1.rtf"))
#' 
#' @export
rtf_mock_color <- function(text) {

  # Match text in glue expression { }
  match_text <- stringr::str_match_all(text, "\\{\\s*(.*?)\\s*\\}")
  match_text <- stats::na.omit(do.call(rbind, match_text))

  # Convert to blue color
  .x <- vapply(match_text[, 2], function(x) {

    # borrow from r2rtf:::rtf_text logic.
    x <- paste0("{\\cf26 <", x, ">}")
  }, FUN.VALUE = "character")
  .x <- .x[unique(names(.x))]

  # Apply the change
  res <- vapply(text, function(x) glue::glue_data(.x, x), FUN.VALUE = "character")
  names(res) <- names(text)
  res
}
