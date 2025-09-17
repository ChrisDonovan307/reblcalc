#' Get Item Fit Statistics
#'
#' This function calculates comprehensive item fit statistics for REBL items,
#' including proportion correct, fit statistics, and item difficulty parameters.
#'
#' @param model An eRm model object (e.g., from RM, RSM, or PCM analysis)
#' @param df A data frame containing the response data
#' @param rebl_items A character vector of REBL item names to analyze
#'
#' @return A data frame containing:
#'   \item{rebl_item}{Item identifier}
#'   \item{prop}{Proportion of correct responses (ecological choice)}
#'   \item{p.fit}{Person fit statistic}
#'   \item{p.df}{Degrees of freedom for person fit}
#'   \item{eta}{Item difficulty parameter}
#'   \item{eta_se}{Standard error of item difficulty parameter}
#'
#' @details The function combines three types of item-level information:
#' 1. Proportion of ecological responses for each item
#' 2. Item fit statistics from the Rasch model
#' 3. Item difficulty parameters (eta) and their standard errors
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have a fitted Rasch model and response data
#' item_stats <- get_item_fit(rasch_model, response_data, rebl_item_names)
#' }
get_item_fit <- function(model, df, rebl_items = rebl_items) {

  # Input validation
  assertthat::assert_that(
    inherits(model, "eRm"),
    msg = "model must be an eRm object"
  )
  assertthat::assert_that(
    is.data.frame(df),
    msg = "df must be a data frame"
  )
  assertthat::assert_that(
    is.character(rebl_items),
    msg = "rebl_items must be a character vector"
  )
  assertthat::assert_that(
    all(rebl_items %in% colnames(df)),
    msg = "All rebl_items must be present in df column names"
  )

  # Get DF of percent eco for REBL items
  percent_eco_df <- df %>%
    dplyr::select(dplyr::all_of(rebl_items)) %>%
    colMeans(na.rm = TRUE) %>%
    as.data.frame() %>%
    setNames('prop') %>%
    tibble::rownames_to_column(var = 'rebl_item')

  # Get DF of most item fit statistics
  item_fit_df <- model %>%
    eRm::person.parameter() %>%
    eRm::itemfit() %>%
    .[!names(.) %in% c('st.res', 'i.disc', 'i.df')] %>%
    purrr::map(as.vector) %>%
    as.data.frame() %>%
    dplyr::mutate(rebl_item = percent_eco_df$rebl_item)

  item_difficulty_df <- data.frame(
    rebl_item = stringr::str_remove(names(model$betapar), 'beta '),
    eta = c(-model$betapar[[1]], model$etapar),
    eta_se = c(-model$se.beta[[1]], model$se.eta)
  )

  # Join all three DFs together
  output <- percent_eco_df %>%
    dplyr::full_join(item_fit_df, by = 'rebl_item') %>%
    dplyr::full_join(item_difficulty_df, by = 'rebl_item') %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(., 3)))
  return(output)
}