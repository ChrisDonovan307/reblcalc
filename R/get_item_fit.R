get_item_fit <- function(model, df, rebl_items = rebl_items) {
  
  # Get DF of percent eco for REBL items
  percent_eco_df <- df %>%
    select(all_of(rebl_items)) %>%
    colMeans(na.rm = TRUE) %>%
    as.data.frame() %>%
    setNames('prop') %>%
    rownames_to_column(var = 'rebl_item')
  
  # Get DF of most item fit statistics
  item_fit_df <- model %>%
    person.parameter() %>%
    itemfit() %>% 
    .[!names(.) %in% c('st.res', 'i.disc', 'i.df')] %>% 
    map(as.vector) %>%
    as.data.frame() %>% 
    mutate(rebl_item = percent_eco_df$rebl_item)
  
  item_difficulty_df <- data.frame(
    rebl_item = str_remove(names(model$betapar), 'beta '),
    eta = c(-model$betapar[[1]], model$etapar),
    eta_se = c(-model$se.beta[[1]], model$se.eta)
  )
    
  # Join all three DFs together
  output <- percent_eco_df %>%
    full_join(item_fit_df, by = 'rebl_item') %>%
    full_join(item_difficulty_df, by = 'rebl_item') %>% 
    mutate(across(where(is.numeric), ~ round(., 3)))
  return(output)
}