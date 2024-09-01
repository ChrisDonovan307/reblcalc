get_person_fit <- function(model, survey, id_column) {
  pp <- person.parameter(model)
  
  # Now just person scores with person # and prolific ID
  person_scores <- coef(pp) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    setNames(c('person', 'rebl_score')) %>% 
    mutate(!!id_column := survey[[id_column]])
  
  # All other fit statistics for all people. 
  # Separate so that we can pull names from it later
  pfit <- personfit(pp)[1:7] %>% 
    .[names(.) != 'st.res']
  
  # Continue getting person fit, join with person_scores    
  person_fit <- pfit %>% 
    map(as.vector) %>% 
    bind_rows() %>%
    as.data.frame() %>%
    mutate(person = names(pfit$p.fit)) %>% 
    inner_join(person_scores, by = 'person') %>% 
    mutate(across(where(is.numeric), ~ round(., 3))) %>% 
    select(all_of(id_column), rebl_score, everything(), -person)
  
  return(person_fit)
}