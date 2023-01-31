#' A function to get the labels of data frame columns
#'
#' @param data a data frame
#'
#' @return labels of the input data frame
#' 
#' @examples
#' get_label(r2rtf::r2rtf_adae)
#'
#' @export
#'
get_label <- function(data) {
  label <- vapply(data, function(x) {
    if (is.null(attr(x, "label"))) {
      return(NA_character_)
    } else {
      attr(x, "label")
    }
  }, FUN.VALUE = character(1))

  ifelse(is.na(label), names(data), label)
}

#' A function to assign labels to a data frame
#' Case 1:
#' If the variable's label is already define in the original data frame,
#' but not re-defined in assign_label(...), its original labels will be kept.
#' Case 2:
#' If the variable's label is already define in the original data frame,
#' but re-defined by assign_label(...), its labels will be re-defined.
#' Case 3:
#' If the variable's label is not define in the original data frame,
#' but it is defined by assign_label(...), its labels will added.
#' Case 4:
#' If the variable's label is not define in the original data frame,
#' neither does it defined by assign_label(...), its labels will be the variable name itself.
#'
#' @param data a data frame
#' @param var the variables to assign labels
#' @param label the labels to be assigned
#'
#' @return a data frame with labels updated
#'
#' @examples
#' assign_label(r2rtf::r2rtf_adae)
#' assign_label(r2rtf::r2rtf_adae, var = "USUBJID", label = "Unique subject id")
#'
#' @export
#'
assign_label <- function(data,
                         var = names(data),
                         label = names(data)) {
  # input checking
  stopifnot(length(var) == length(label))
  stopifnot(!any(duplicated(var)))

  # get existing labels and its corresponding variables
  name <- names(data)
  existing_lables <- get_label(data)
  existing_labels_var <- names(existing_lables)

  # assign label
  for (i in seq(name)) {
    if (name[i] %in% existing_labels_var & !(name[i] %in% var)) {
      next
    } else if (name[i] %in% var) {
      attr(data[[i]], "label") <- label[names(data[i]) == var]
    } else {
      attr(data[[i]], "label") <- name[i]
    }
  }

  data
}
