#' Create a data exploration `meta_adam` object
#' 
#' @param data a data frame 
#' @param group a character value of group variable name in `data`. 
#' 
#' @export
meta_dummy_exploration <- function(data, 
                                   group,
                                   name = "ase",
                                   subset = NULL, 
                                   label = "All Subjects Enrolled"){
  
  meta <- meta_adam(observation = data)
  
  name <- names(data)  
  label <- vapply(data, function(x) attr(x, "label"), FUN.VALUE = 'character')
  names(label) <- NULL
  
  # add analysis plan of all variables
  meta <- define_plan(meta, plan = plan(analysis = "exploration", 
                                        population = name, 
                                        observation = "inf", 
                                        parameter = paste(name, collapse = ";")))
  
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
  for(i in seq(name)){
    meta <- define_parameter(meta, name = name[i], var = name[i], label = label[i], subset = NULL)
  }
  
  # define analysis 
  meta <- define_analysis(meta, name = "exploration", label = "Data Exploration")
  
  # build metadata 
  meta <- meta_build(meta)
}