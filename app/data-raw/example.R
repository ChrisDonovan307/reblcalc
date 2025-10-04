## code to prepare `example` dataset goes here

# Synthetic dataset for example and testing
raw_example <- read.csv("dev/raw_example.csv")
col_names <- names(raw_example)

# Get target proportions
props <- c(
  0.425,
  0.816,
  0.078,
  0.146,
  0.192,
  0.024,
  0.674,
  0.365,
  0.922,
  0.106,
  0.227,
  0.037,
  0.504,
  0.074,
  0.787,
  0.406,
  0.842,
  0.106,
  0.225,
  0.026,
  0.504,
  0.092,
  0.158,
  0.797
) %>%
  round(2)

df <- matrix(
  data = NA,
  nrow = 500,
  ncol = ncol(raw_example)
) %>%
  as.data.frame() %>%
  setNames(c(names(raw_example))) %>%
  mutate(respondent_id = dplyr::row_number())

# Populate based on target props
set.seed(42)
iwalk(col_names[-1], ~ {
  df[[.x]] <<- rbinom(500, size = 1, prob = props[.y])
})
str(df)

# Make sure everyone answered yes and no at least once each
# test <- df %>%
#   rowwise() %>%
#   mutate(sum = sum()


# Save
example <- df
readr::write_csv(example, "dev/example.csv")
usethis::use_data(example, overwrite = TRUE)
