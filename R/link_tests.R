#' Link tests
#' 2024-24-08

# Output whole plink output - pull out scores separately so we can print output


# Dependencies ------------------------------------------------------------


pacman::p_load(
  eRm,
  plink,
  dplyr
)


link_tests <- function(new_model, baseline_model, method = 'SL') {
  
  # Assume all common for now
  common <- matrix(c(1:24, 1:24), 24, 2)
  
  # Put models into a list for convenience
  models <- list(baseline_model, new_model) %>% 
    setNames(c('baseline_model', 'new_model'))
  
  # Combine pars into the object that plink requires
  pars <- combine.pars(
    x = list(read_erm(x = models$baseline_model), read_erm(x = models$new_model)),
    common = common,
    grp.names = names(models)
  )
    
  #' Define a list of thetas from survey 2a and 2b
  thetas <- map(models, ~ coef(person.parameter(.x)))
  
  out <- plink(
    x = pars,
    rescale = method,
    ability = thetas
  )
  
  return(out)
}