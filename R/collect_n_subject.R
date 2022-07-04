#' Count number of unique subjects
#'
#' @param id a character vector of subject id
#' @param group a factor vector of group name
#' @param par a character vector of parameter name
#' @param use_na a character value for whether to include NA values in the table. Refer `useNA` argument in `table` function for more details.
#'
#' @examples
#' library(r2rtf)
#' r2rtf_adae$TRTA <- factor(r2rtf_adae$TRTA)
#' r2rtf_adae$SEX[1:5] <- NA
#' metalite:::n_subject(r2rtf_adae$USUBJID, r2rtf_adae$TRTA)
#' metalite:::n_subject(r2rtf_adae$USUBJID, r2rtf_adae$TRTA, r2rtf_adae$SEX)
#' metalite:::n_subject(r2rtf_adae$USUBJID, r2rtf_adae$TRTA, r2rtf_adae$SEX, use_na = "always")
n_subject <- function(id, group, par = NULL, use_na = c("ifany", "no", "always")) {
  
  use_na <- match.arg(use_na)
  
  if ("factor" %in% class(group)) {
    u_group <- c(as.character(levels(group)), "Missing")
  } else {
    stop("n_subject: group variable must be a factor")
  }
  
  if (is.null(par)) {
    db <- data.frame(id = id, group = group)
    res <- table(unique(db)[["group"]], useNA = use_na)
    
    n_row <- nrow(res)
    res <- data.frame(t(as.vector(res)))
    names(res) <- c(u_group[1:n_row])
  } else {
    db <- data.frame(id = id, group = group, par = par)
    res <- table(unique(db)[, c("group", "par")], useNA = use_na)
    name <- colnames(res)
    name[is.na(name)] <- "Missing"
    
    n_row <- nrow(res)
    n_col <- ncol(res)
    res <- data.frame(name = name[1:n_col], matrix(res, ncol = n_row, byrow = TRUE))
    names(res) <- c("name", u_group[1:n_row])
  }
  
  res
}

#' Collect number of subjects and its subset condition
#'
#' @inheritParams plan
#' @inheritParams define_population
#' @param use_na a character value for whether to include NA values in the table. Refer `useNA` argument in `table` function for more details.
#'
#' @examples
#' suppressWarnings(
#' meta <- meta_dummy() |> 
#'   define_parameter(name="sex", var = "SEX", label = "Sex") 
#'  )
#' collect_n_subject(meta, "apat", "sex")
#' 
#' @export
collect_n_subject <- function(meta, population, parameter, use_na = c("ifany", "no", "always")){
  
  use_na <- match.arg(use_na)
  
  # Obtain variables
  par_var <- collect_adam_mapping(meta, parameter)$var
  
  # Obtain Data
  pop <- collect_population_record(meta, population, var = par_var)

  # Obtain ID
  pop_id <- collect_adam_mapping(meta, population)$id
  
  # Obtain Group 
  pop_group <- collect_adam_mapping(meta, population)$group
  
  # Proper Handle Missing Value 
  pop[[par_var]] <- factor(pop[[par_var]], exclude = NULL)
  pop[[pop_group]] <- factor(pop[[pop_group]], exclude = NULL)
  levels(pop[[par_var]])[is.na(levels(pop[[par_var]]))] <- "Missing"
  levels(pop[[pop_group]])[is.na(levels(pop[[pop_group]]))] <- "Missing"
  
  # Obtain Number of Subjects
  pop_n <- n_subject(pop[[pop_id]], pop[[pop_group]], par = pop[[par_var]])
  pop_n
  
  # Prepare subset considtion dataset
  level_par_var <- paste0(par_var," == '", levels(pop[[par_var]]), "'")
  level_pop_var <- paste0(pop_group, " == '", levels(pop[[pop_group]]), "'")
  
  pop_subset <- collect_adam_mapping(meta, population)$subset
  level_pop_var <- paste(level_pop_var, fmt_quote(deparse(pop_subset)), sep = " & ")
  
  res <- outer(level_par_var, level_pop_var, FUN = paste, sep = " & ")
  
  
  res <- data.frame(name = levels(pop[[par_var]]), res)
  names(res) <- c("name", levels(pop[[pop_group]]))
  res <- res[1:nrow(pop_n), 1:ncol(pop_n)]  
  
  list(n = pop_n, subset = res)  
}


