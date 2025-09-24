# Remove 10% of data from example df

set.seed(42)

add_missing <- function(df, id_col = 1, prop = 0.1) {
  # separate ID column
  id_vals <- df[[id_col]]

  # work only on the other columns
  df_data <- select(df, -all_of(id_col))

  n <- nrow(df_data) * ncol(df_data)       # total number of cells (excluding ID)
  n_missing <- ceiling(prop * n)           # number of cells to replace

  # pick random positions
  miss_idx <- arrayInd(sample(n, n_missing), dim(df_data))

  # copy
  df_missing <- df_data

  # replace with NA
  for (i in seq_len(n_missing)) {
    df_missing[miss_idx[i, 1], miss_idx[i, 2]] <- NA
  }

  # put ID back in
  df_missing <- cbind(df[ , id_col, drop = FALSE], df_missing)

  return(df_missing)
}

example_mis10 <- add_missing(example, id_col = "respondent_id", prop = 0.1)

# Add some 1s to 594
example_mis10$foodMeat[example_mis10$respondent_id == '594'] <- 1
# example_mis10$foodMeat[example_mis10$foodOatMilk == '594'] <- 1
# example_mis10 <- example_mis10[-594, ]
# example_mis10 <- example_mis10[1:500, ]

skimr::skim(example_mis10)

readr::write_csv(example_mis10, 'dev/example_mis10.csv')
usethis::use_data(example_mis10, overwrite = TRUE)
