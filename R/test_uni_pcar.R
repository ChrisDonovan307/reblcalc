test_uni_pcar <- function(model, 
                          n_vars = length(model$se.beta), 
                          rotate = 'Promax',
                          n_factors = 4) {
  results <- list()
  
  pca <- model %>% 
    person.parameter() %>% 
    itemfit() %>% 
    .$st.res %>% 
    pca(nfactors = n_factors, rotate = rotate)
  
  eigenvalues <- c(pca$values[1:10])
  y_limit <- ifelse(max(eigenvalues) > 2.1, max(eigenvalues), 2.1)
  plot(eigenvalues,
       ylab = "Eigenvalues",
       xlab = "Item Number",
       type = "b",
       ylim = c(1, y_limit)
  )
  abline(h = 2, col = 'red', lwd = 2, lty = 2)
  
  return(pca)
}