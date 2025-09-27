# Test miss ranger
library(missRanger)
df <- read.csv('dev/example_mis10.csv')
str(df)

df <- df %>%
  select(-respondent_id) %>%
  mutate(across(everything(), as.factor))
str(df)

out <- missRanger(
  df,
  seed = 42,
  data_only = FALSE
)
str(out)
out$best_iter
out$pred_errors[2, ]
out$mean_pred_errors

oob <- out$pred_errors[2, ] %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%
  setNames(c('rebl_item', 'oob'))
oob

# Clean it up
str(out)
str(out$data) # clean set
out$data %>%
  mutate(across(everything(), ~ as.integer(.x) - 1)) %>%
  str()
