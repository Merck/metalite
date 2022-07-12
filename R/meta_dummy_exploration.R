#' Create a data exploration `meta_adam` object
#' 
#' @param data a data frame 
#' @inheritParams define_population
#' 
#' @examples 
#' meta <- meta_dummy_exploration(r2rtf::r2rtf_adsl, group = "TRT01A")
#' collect_n_subject(meta, "ase", "AGE")
#' collect_n_subject(meta, "ase", "SEX")
#' 
#' @export
meta_dummy_exploration <- function(data, 
                                   group,
                                   name = "ase",
                                   subset = NULL, 
                                   label = "All Subjects Enrolled"){
  
  meta <- meta_adam(observation = data)
  
  var <- names(data)  
  var_label <- vapply(data, function(x) attr(x, "label"), FUN.VALUE = 'character')
  names(var_label) <- NULL
  
  # add analysis plan of all variables
  meta <- define_plan(meta, plan = plan(analysis = "exploration", 
                                        population = name, 
                                        observation = "inf", 
                                        parameter = paste(var, collapse = ";")))
  
  # define population 
  meta <- define_population(meta, 
                            name = name, 
                            group = group, 
                            subset = subset,
                            label = label) 
  
  # define observation 
  meta <- define_observation(meta, 
                             name = "inf", 
                             subset = NULL, 
                             label = "All Observations")
  
  # define parameter 
  for(i in seq(var)){
    meta <- define_parameter(meta, name = var[i], var = var[i], label = var_label[i], subset = NULL)
  }
  
  # define analysis 
  meta <- define_analysis(meta, name = "exploration", label = "Data Exploration")
  
  # build metadata 
  meta <- meta_build(meta)
}