box::use(
  assertthat[assert_that],
  eRm[person.parameter, personfit],
  tibble[rownames_to_column],
  dplyr[mutate, select, all_of, inner_join, `%>%`, across, where, everything, bind_rows],
  purrr[map],
  stats[setNames, coef]
)
#' Get Person Fit Statistics
#'
#' This function calculates person fit statistics and REBL scores for individuals
#' in a Rasch model analysis.
#'
#' @param model An eRm model object (e.g., from RM, RSM, or PCM analysis)
#' @param survey A data frame containing survey data with participant identifiers
#' @param id_column A character string specifying the column name containing participant IDs
#'
#' @return A data frame containing:
#'   \item{id_column}{Participant identifier (column name matches id_column parameter)}
#'   \item{rebl_score}{Person parameter estimate (REBL score)}
#'   \item{p.fit}{Person fit statistic}
#'   \item{p.df}{Degrees of freedom for person fit}
#'   \item{outfit}{Outfit mean square statistic}
#'   \item{infit}{Infit mean square statistic}
#'   \item{outfit.z}{Standardized outfit statistic}
#'   \item{infit.z}{Standardized infit statistic}
#'
#' @details This function extracts person parameters and fit statistics from a fitted
#' Rasch model and combines them with participant identifiers from the survey data.
#' All numeric values are rounded to 3 decimal places.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get person fit statistics
#' person_stats <- get_person_fit(rasch_model, survey_data, "participant_id")
#' }
get_person_fit <- function(model, survey, id_column) {

  # Input validation
  assert_that(
    inherits(model, "eRm"),
    msg = "model must be an eRm object"
  )
  assert_that(
    is.data.frame(survey),
    msg = "survey must be a data frame"
  )
  assert_that(
    is.character(id_column) && length(id_column) == 1,
    msg = "id_column must be a single character string"
  )
  assert_that(
    id_column %in% colnames(survey),
    msg = "id_column must be present in survey column names"
  )

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
