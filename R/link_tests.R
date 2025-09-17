#' Link Tests Using Plink
#'
#' This function performs test linking using the plink package to link two Rasch models
#' (baseline and new) using common items.
#'
#' @param new_model An eRm model object representing the new test form
#' @param baseline_model An eRm model object representing the baseline test form
#' @param method Character string specifying the linking method. Default is 'SL' (Stocking-Lord).
#'   Other options include 'MM' (Mean-Mean), 'MS' (Mean-Sigma), 'HB' (Haebara), etc.
#'
#' @return A plink object containing:
#'   \item{constants}{Linking constants (slope and intercept)}
#'   \item{linked.pars}{Linked parameters}
#'   \item{ability}{Linked ability estimates}
#'   \item{descriptives}{Descriptive statistics}
#'
#' @details This function assumes all 24 items are common between the two test forms.
#' It converts eRm model objects to the format required by plink, performs the linking,
#' and returns the complete plink output including linked ability estimates.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Link two test forms
#' link_result <- link_tests(new_model, baseline_model, method = "SL")
#'
#' # Extract linking constants
#' constants <- link_result$constants
#' }
link_tests <- function(new_model, baseline_model, method = 'SL') {

  # Input validation
  assertthat::assert_that(
    inherits(new_model, "eRm"),
    msg = "new_model must be an eRm object"
  )
  assertthat::assert_that(
    inherits(baseline_model, "eRm"),
    msg = "baseline_model must be an eRm object"
  )
  assertthat::assert_that(
    is.character(method) && length(method) == 1,
    msg = "method must be a single character string"
  )
  assertthat::assert_that(
    method %in% c('SL', 'MM', 'MS', 'HB'),
    msg = "method must be one of: 'SL', 'MM', 'MS', 'HB'"
  )

  # Assume all common for now
  common <- matrix(c(1:24, 1:24), 24, 2)

  # Put models into a list for convenience
  models <- list(baseline_model, new_model) %>%
    setNames(c('baseline_model', 'new_model'))

  # Combine pars into the object that plink requires
  pars <- plink::combine.pars(
    x = list(read_erm(x = models$baseline_model), read_erm(x = models$new_model)),
    common = common,
    grp.names = names(models)
  )

  # Define a list of thetas from survey 2a and 2b
  thetas <- purrr::map(models, ~ coef(eRm::person.parameter(.x)))

  out <- plink::plink(
    x = pars,
    rescale = method,
    ability = thetas
  )

  return(out)
}