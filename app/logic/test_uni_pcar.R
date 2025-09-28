box::use(
  assertthat[assert_that],
  eRm[person.parameter, itemfit],
  psych[pca],
  dplyr[`%>%`],
  graphics[abline]
)
#' Test Unidimensionality Using Principal Component Analysis
#'
#' This function performs a principal component analysis on standardized residuals
#' from a Rasch model to test the assumption of unidimensionality.
#'
#' @param model An eRm model object (RM, RSM, or PCM)
#' @param n_vars Numeric. Number of variables to use in the analysis. Default is length(model$se.beta).
#' @param rotate Character string specifying rotation method for PCA. Default is 'Promax'.
#'   Other options include 'varimax', 'quartimax', etc.
#' @param n_factors Numeric. Number of factors to extract. Default is 4.
#'
#' @return A PCA object from the psych package containing:
#'   \item{values}{Eigenvalues}
#'   \item{loadings}{Factor loadings}
#'   \item{scores}{Factor scores}
#'   \item{rotation}{Rotation method used}
#'
#' @details This function tests unidimensionality by:
#' \itemize{
#'   \item Extracting standardized residuals from the Rasch model
#'   \item Performing PCA on these residuals
#'   \item Creating a scree plot with eigenvalues
#'   \item Adding a reference line at eigenvalue = 2
#' }
#'
#' The first eigenvalue should be small (< 2) if the unidimensionality assumption holds.
#' Large eigenvalues suggest the presence of additional dimensions in the data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Test unidimensionality
#' pca_result <- test_uni_pcar(rasch_model)
#'
#' # Custom analysis with different parameters
#' pca_result <- test_uni_pcar(rasch_model, n_factors = 6, rotate = "varimax")
#' }
test_uni_pcar <- function(model,
                          n_vars = length(model$se.beta),
                          rotate = 'Promax',
                          n_factors = 4) {

  # Input validation
  assertthat::assert_that(
    inherits(model, "eRm"),
    msg = "model must be an eRm object"
  )
  assertthat::assert_that(
    is.numeric(n_vars) && length(n_vars) == 1 && n_vars > 0,
    msg = "n_vars must be a positive numeric value"
  )
  assertthat::assert_that(
    is.character(rotate) && length(rotate) == 1,
    msg = "rotate must be a single character string"
  )
  assertthat::assert_that(
    is.numeric(n_factors) && length(n_factors) == 1 && n_factors > 0,
    msg = "n_factors must be a positive numeric value"
  )

  results <- list()

  pca <- model %>%
    eRm::person.parameter() %>%
    eRm::itemfit() %>%
    .$st.res %>%
    psych::pca(nfactors = n_factors, rotate = rotate)

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
