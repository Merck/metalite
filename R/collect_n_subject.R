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
#' @param listing a logical value to display drill down listing per row.
#' @param histogram a logical value to display histogram by group. 
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
collect_n_subject <- function(meta, 
                              population, 
                              parameter, 
                              listing = FALSE, 
                              histogram = FALSE,
                              use_na = c("ifany", "no", "always")){
  
  use_na <- match.arg(use_na)
  
  # Obtain variables
  par_var <- collect_adam_mapping(meta, parameter)$var
  
  # Obtain Data
  pop <- collect_population_record(meta, population, var = par_var)
  
  # Obtain ID
  pop_id <- collect_adam_mapping(meta, population)$id
  
  # Obtain Group 
  pop_group <- collect_adam_mapping(meta, population)$group
  
  # define analysis dataset
  id <- pop[[pop_id]]
  group <- pop[[pop_group]]
  var <- pop[[par_var]]
  
  # standardize group variable 
  stopifnot(any(c("factor", "character") %in% class(group)))
  group <- factor(group, exclude = NULL)
  levels(group)[is.na(levels(group))] <- "Missing"
  
  # standardize continuous variables 
  stopifnot(any(c("numeric", "integer", "Date", "factor", "character") %in% class(var)))
  if(any(c("numeric", "integer", "Date") %in% class(var))){
    var <- ifelse(is.na(var), "Missing", "Subjects in Population")
    var <- factor(var, levels = c("Subjects in Population", "Missing"))
  }
  
  # standardize categorical variables
  if(any(c("factor", "character") %in% class(var))){
    var <- factor(var, exclude = NULL)
    levels(var)[is.na(levels(var))] <- "Missing"
  }
  
  # Obtain Number of Subjects
  pop_n <- n_subject(id, group, par = var)
  
  # Prepare subset condition
  subset_condition <- function(x, name){
    switch(x, 
           "Subjects in Population" = glue::glue("! is.na({name})"), 
           "Missing" = glue::glue("is.na({name})"), 
           glue::glue("{name} == '{x}'")
    )
  }
  
  var_subset <- vapply(levels(var), subset_condition, name = par_var, FUN.VALUE = character(1))
  group_subset <-vapply(levels(group), subset_condition, name = pop_group, FUN.VALUE = character(1))
  pop_subset <- collect_adam_mapping(meta, population)$subset
  pop_subset<- fmt_quote(deparse(pop_subset))
  
  full_subset <- paste(group_subset, pop_subset, sep = " & ")
  full_subset <- outer(var_subset, full_subset, FUN = paste, sep = " & ")
  
  res <- data.frame(name = levels(var), full_subset)
  names(res) <- c("name", levels(group))
  res <- res[1:nrow(pop_n), 1:ncol(pop_n)]
  rownames(res) <- NULL
  
  # Create row listing 
  if(listing){
    
    row_subset <- paste(var_subset, pop_subset, sep = " & ")
    listing <- lapply(var_subset, function(x){
      pop_listing <- subset(pop, rlang::eval_tidy(expr = str2lang(x), data = pop))
      pop_listing <- reset_label(pop_listing, meta$data_population)
    })
    
  }else{
    listing <- NULL
  }
  
  # Show distribution graph
  if(histogram){

    ana <- data.frame(id = id, group = group, var = pop[[par_var]])
    ana <- stats::na.omit(ana)
    
    label <- attr(meta$data_population[[par_var]], "label")
    
    pop_hist <- ggplot2::ggplot(data = ana, ggplot2::aes(x = var, group = group)) + 
      ggplot2::facet_wrap(~ group) + 
      ggplot2::xlab(label) + 
      ggplot2::ylab("Number of Subjects") + 
      ggplot2::ggtitle(glue::glue("Histogram of {label}")) + 
      ggplot2::theme_bw() 
    
    if(any(c("factor", "character") %in% class(ana$var))){
      pop_hist <- pop_hist + ggplot2::geom_bar() 
    }
    
    if(any(c("numeric", "integer", "Date") %in% class(ana$var))){
      pop_hist <- pop_hist + ggplot2::geom_histogram(bins = 5) 
    }  
    
  }else{
    pop_hist <- NULL
  }
  
  list(n = pop_n, subset = res, listing = listing, histogram = pop_hist)  
}