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
#' \dontrun{
#'    n_subject(r2rtf_adae$USUBJID, r2rtf_adae$TRTA)
#'    n_subject(r2rtf_adae$USUBJID, r2rtf_adae$TRTA, r2rtf_adae$SEX)
#'    n_subject(r2rtf_adae$USUBJID, r2rtf_adae$TRTA, r2rtf_adae$SEX, use_na = "always")
#' }
n_subject <- function(id, 
                      group, 
                      par = NULL, 
                      use_na = c("ifany", "no", "always")) {
  
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

#' Remove blank group based on analysis parameter. 
#' 
#' @inheritParams collect_n_subject
meta_remove_blank_group <- function(meta, 
                                    population,
                                    parameter){
  
  pop <- meta$data_population
  obs <- meta$data_observation
  
  pop_grp <- collect_adam_mapping(meta, population)$group
  obs_grp <- collect_adam_mapping(meta, population)$group
  
  pop_var <- collect_adam_mapping(meta, parameter)$var
  
  if(is.null(pop_var)){
    stop("meta_remove_blank_group: parameter is not available in meta$population")
  }
  
  loc <- which(table(is.na(pop[[pop_var]]), pop[[pop_grp]])["FALSE", ] == 0 )
  
  if(length(loc) > 0){
    pop_ind <- ! pop[[pop_grp]] %in% levels(pop[[pop_grp]])[loc]
    pop <- pop[pop_ind, ]
    pop[[pop_grp]] <- factor(pop[[pop_grp]], levels(pop[[pop_grp]])[- loc])
    
    obs_ind <- ! obs[[obs_grp]] %in% levels(obs[[obs_grp]])[loc]
    obs <- obs[obs_ind, ]
    obs[[obs_grp]] <- factor(obs[[obs_grp]], levels(obs[[obs_grp]])[- loc])
  }
  
  meta$data_population  <- pop 
  meta$data_observation <- obs 
  
  meta
  
}

#' Collect number of subjects and its subset condition
#'
#' @inheritParams plan
#' @inheritParams define_population
#' @param listing a logical value to display drill down listing per row.
#' @param histogram a logical value to display histogram by group. 
#' @param var_listing a character vector of additional variables included in the listing.  
#' @param remove_blank_group a logical value to remove a group with all missing value of a parameter. 
#' @param use_na a character value for whether to include NA values in the table. Refer `useNA` argument in `table` function for more details.
#' @param display_total a logical value to display total column. 
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
                              var_listing = NULL,
                              remove_blank_group = FALSE,
                              use_na = c("ifany", "no", "always"), 
                              display_total = TRUE){
  
  use_na <- match.arg(use_na)
  
  if(remove_blank_group){
    meta <- meta_remove_blank_group(meta, population, parameter)
  }
  
  if(display_total){
    
    meta <- meta_add_total(meta)
    
  }
  
  # Obtain variables
  par_var <- collect_adam_mapping(meta, parameter)$var
  
  # Obtain Data
  pop <- collect_population_record(meta, population, var = c(var_listing, par_var) )
  
  # Obtain ID
  pop_id <- collect_adam_mapping(meta, population)$id
  
  # Obtain Group 
  pop_group <- collect_adam_mapping(meta, population)$group
  
  # Define analysis dataset
  id <- pop[[pop_id]]
  group <- pop[[pop_group]]
  var <- pop[[par_var]]
  
  class_var <- class(var)
  
  # Obtain variable label 
  label <- collect_adam_mapping(meta, parameter)$label
  if(is.null(label)){
    label <- collect_adam_mapping(meta, parameter)$var
  }
  
  # standardize group variable 
  stopifnot(any(c("factor", "character") %in% class(group)))
  group <- factor(group, exclude = NULL)
  levels(group)[is.na(levels(group))] <- "Missing"
  
  # standardize continuous variables 
  stopifnot(any(c("numeric", "integer", "factor", "character", "logical") %in% class(var)))
  
  # Transfer logical value
  if("logical" %in% class_var){
    class_var <- "character"
    var <- factor(var, c(TRUE, FALSE), c("Yes", "No") )
  }
  
  if(any(c("numeric", "integer") %in% class_var)){
    
    # calculate summary statistics
    pop_num <- tapply(var, group, function(x){
      value <-c( mean = mean(x, na.rm = TRUE), 
                 sd = stats::sd(x, na.rm = TRUE),
                 median = stats::median(x, na.rm = TRUE),
                 min = min(x, na.rm = TRUE), 
                 max = max(x, na.rm = TRUE))
      value <- formatC(value, format = "f", digits = 1)
      c(glue::glue("{value[['mean']]} ({value[['sd']]})"), glue::glue("{value[['median']]} [{value[['min']]}, {value[['max']]}]"))
    }) 
    pop_num <- data.frame(name = c("Mean (SD)", "Median [Min, Max]"), 
                          do.call(cbind, pop_num))
    
    var <- ifelse(is.na(var), "Missing", "Subjects with Data")
    var <- factor(var, levels = c("Subjects with Data", "Missing"))
    
    # Obtain Number of Subjects
    pop_n <- n_subject(id, group, par = var)
    
    # combine results
    names(pop_num) <- names(pop_n)  
  }
  
  # standardize categorical variables
  if(any(c("factor", "character") %in% class_var)){
    var <- factor(var, exclude = NULL)
    levels(var)[is.na(levels(var))] <- "Missing"
    
    # Obtain Number of Subjects
    pop_n <- n_subject(id, group, par = var)
  }
  
  # add percentage 
  pop_tmp <- pop_n
  for(i in seq(names(pop_n))){
    if("integer" %in% class(pop_n[[i]])){
      pct <- formatC(pop_n[[i]] / sum(pop_n[[i]]) * 100, format = "f", digits = 1, width = 5)
      pop_tmp[[i]] <- glue::glue("{pop_n[[i]]} ({pct}%)")
    }
  }
  
  # prepare summary table
  if(any(c("numeric", "integer") %in% class_var)){
    pop_table <- rbind(pop_n[1, ], pop_num, pop_tmp[2, ]) 
  }
  
  if(any(c("factor", "character") %in% class_var)){
    pop_table <- pop_tmp
  }
  
  # add table header using variable label
  header <- data.frame( t(c(label, rep(NA, ncol(pop_table) - 1)) ) )
  names(header) <- names(pop_table)
  
  pop_table <- rbind(header, pop_table)
  rownames(pop_table) <- NULL
  
  # Prepare subset condition
  subset_condition <- function(x, name){
    switch(x, 
           "Subjects with Data" = glue::glue("! is.na({name})"), 
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
  
  res <- res[, setdiff(names(res), "Total")]
  
  # Create row listing 
  if(listing){
    
    row_subset <- paste(var_subset, pop_subset, sep = " & ")
    listing <- lapply(var_subset, function(x){
      pop_listing <- subset(pop, rlang::eval_tidy(expr = str2lang(x), data = pop) & (! group %in% "Total") )
      pop_listing <- reset_label(pop_listing, meta$data_population)
    })
    
  }else{
    listing <- NULL
  }
  
  # Show distribution graph
  if(histogram){
    
    ana <- data.frame(id = id, group = group, var = pop[[par_var]])
    ana <- stats::na.omit(ana)
    # ana <- subset(ana, group != "Total")
    
    pop_hist <- ggplot2::ggplot(data = ana, ggplot2::aes(x = var, group = group)) + 
      ggplot2::facet_wrap(~ group) + 
      ggplot2::xlab(label) + 
      ggplot2::ylab("Number of Subjects") + 
      ggplot2::ggtitle(glue::glue("Histogram of {label}")) + 
      ggplot2::theme_bw() 
    
    # Rotate x-axis direction  
    if(nchar(paste(unique(ana$var), collapse = "")) > 30){
      pop_hist <- pop_hist + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45)) 
    }
    
    
    if(any(c("factor", "character") %in% class_var)){
      pop_hist <- pop_hist + ggplot2::geom_bar() 
    }
    
    if(any(c("numeric", "integer") %in% class_var)){
      pop_hist <- pop_hist + ggplot2::geom_histogram(bins = 5) 
    }  
    
  }else{
    pop_hist <- NULL
  }
  
  list(table = pop_table, n = pop_n, subset = res, listing = listing, histogram = pop_hist)  
}
