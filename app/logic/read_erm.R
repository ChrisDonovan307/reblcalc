box::use(
  assertthat[assert_that],
  eRm[person.parameter],
  plink[plink, combine.pars, as.poly.mod, as.irt.pars, sep.pars]
)
#' Read eRm Objects for Plink Analysis
#'
#' A modified version of plink::read.erm that actually works correctly with eRm objects.
#' This function converts eRm model objects to the format required by the plink package
#' for test linking and equating procedures.
#'
#' @param x An eRm model object (RM, RSM, or PCM)
#' @param loc.out Logical. Should location parameters be output? Default is FALSE.
#' @param as.irt.pars Logical. Should the output be returned as irt.pars object? Default is TRUE.
#'
#' @return If as.irt.pars = TRUE, returns an irt.pars object suitable for plink functions.
#'   If as.irt.pars = FALSE, returns a list of parameter matrices.
#'
#' @details This function addresses issues with the original plink::read.erm function
#' by properly setting the groups and time variables to 1, which are not part of the
#' standard output from eRm objects. The function:
#' \itemize{
#'   \item Extracts item parameters from eRm objects
#'   \item Converts them to the plink format
#'   \item Handles dichotomous (RM) and polytomous (RSM, PCM) models
#'   \item Returns parameters in irt.pars format for linking
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert eRm model for plink analysis
#' plink_pars <- read_erm(rasch_model)
#'
#' # Get raw parameter matrices
#' raw_pars <- read_erm(rasch_model, as.irt.pars = FALSE)
#' }
read_erm <- function (x,
                      loc.out = FALSE,
                      as.irt.pars = TRUE)
{

  # Input validation
  assertthat::assert_that(
    inherits(x, "eRm"),
    msg = "x must be an eRm model object"
  )
  assertthat::assert_that(
    is.logical(loc.out) && length(loc.out) == 1,
    msg = "loc.out must be a single logical value"
  )
  assertthat::assert_that(
    is.logical(as.irt.pars) && length(as.irt.pars) == 1,
    msg = "as.irt.pars must be a single logical value"
  )

  groups <- 1
  time <- 1
  items <- ncol(x$X) / time
  cat <- apply(x$X, 2, max, na.rm = TRUE)[1:items]
  beta <- x$betapar
  it <- rep(rep(1:items, cat), groups * time)
  t <- rep(1:2, each = sum(cat) * time)
  g <- rep(rep(1:2, each = sum(cat)), time)
  pars <- vector("list", groups * time)
  comb <- expand.grid(list(1:groups, 1:time))
  comb$grp <- 1:nrow(comb)
  for (i in 1:items) {
    for (j in 1:groups) {
      for (k in 1:time) {
        tmp <- c(beta[it == i & g == j & t == k], rep(NA, max(cat) - cat[i]))
        pars[[comb$grp[comb[, 1] == j & comb[, 2] ==
                         k]]] <- rbind(pars[[comb$grp[comb[, 1] ==
                                                        j &
                                                        comb[, 2] == k]]], tmp)
        names(pars)[comb$grp[comb[, 1] == j & comb[, 2] == k]] <- paste("group", j, ".time", k, sep = "")
      }
    }
  }
  cat <- cat + 1
  names(cat) <- NULL
  if (min(cat) == 2 & max(cat == 2)) {
    mod <- "drm"
    tmp.it <- 1:items
  }
  else if (min(cat) == 2 & max(cat > 2)) {
    mod <- c("drm", "gpcm")
    tmp <- 1:items
    tmp.it <- list(tmp[cat == 2], tmp[cat > 2])
  }
  else if (min(cat) > 2 & max(cat > 2)) {
    mod <- "gpcm"
    tmp.it <- 1:items
  }
  pm <- plink::as.poly.mod(items, mod, tmp.it)
  for (i in 1:length(pars)) {
    rownames(pars[[i]]) <- NULL
    colnames(pars[[i]]) <- NULL
    pars[[i]] <- plink::sep.pars(pars[[i]],
                          cat = cat,
                          poly.mod = pm,
                          loc.out = loc.out)
  }
  if (length(pars) == 1) {
    pars <- plink::as.irt.pars(pars[[1]])
  }
  else {
    common <- vector("list", (groups * time) - 1)
    for (i in 1:length(common)) {
      common[[i]] <- matrix(1:items, items, 2)
    }
    pars <- plink::as.irt.pars(pars, common, grp.names = names(pars))
  }
  if (as.irt.pars == FALSE) {
    pars <- pars@pars
  }
  return(pars)
}
