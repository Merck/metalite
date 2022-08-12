#' Add duplicate data to enable a total group. 
#' 
#' @param meta a metalite object 
#' @param total a character value of total group name. 
#' 
#' @export
meta_add_total <- function(meta, 
                           total = "Total",
                           remove_blank_group = FALSE){
  
  stopifnot(length(total) == 1)
  
  pop <- meta$data_population
  obs <- meta$data_observation
  
  pop_grp <- vapply(meta$population, "[[", FUN.VALUE = character(1), "group")
  obs_grp <- vapply(meta$population, "[[", FUN.VALUE = character(1), "group")
  
  grp <- unique(c(pop_grp, obs_grp))
  
  for(i in seq(grp)){
    pop[[grp[i]]] <- factor(total)
    obs[[grp[i]]] <- factor(total)
  }
  
  meta$data_population <- rbind(meta$data_population, pop)
  meta$data_observation <- rbind(meta$data_observation, obs)
  
  meta
}
