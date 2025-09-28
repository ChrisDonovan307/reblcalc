box::use(
  assertthat[assert_that],
  eRm[thresholds],
  graphics[par, box, plot, axis, mtext, points, layout, segments, lines, text]
)

#' Enhanced Person-Item Map Plotting Function
#'
#' An enhanced version of the eRm plotPImap function with additional customization options
#' and improved visualization features for Rasch model analysis.
#'
#' @param object An eRm model object (RM, RSM, or PCM). LLTM, LRSM, and LPCM are not supported.
#' @param item.subset Character vector specifying which items to include. Use "all" for all items,
#'   or provide a vector of item names/indices. Default is "all".
#' @param sorted Logical. Should items be sorted by difficulty? Default is FALSE.
#' @param main Character string for the main title. Default is "Person-Item Map".
#' @param latdim Character string for the x-axis label. Default is "Latent Dimension".
#' @param pplabel Character string for the person parameter distribution label.
#'   Default is "Person\nParameter\nDistribution".
#' @param cex.gen Numeric value for general text size scaling. Default is 0.7.
#' @param xrange Numeric vector of length 2 specifying x-axis range. If NULL (default),
#'   range is calculated automatically.
#' @param warn.ord Logical. Should warnings for disordered thresholds be displayed? Default is TRUE.
#' @param warn.ord.colour Character string specifying color for disordered threshold warnings.
#'   Default is "black".
#' @param irug Logical. Should item threshold rug be displayed? Default is TRUE.
#' @param pp A person.parameter object. If NULL (default), it will be calculated from the model.
#' @param margins Numeric vector of length 4 specifying plot margins (bottom, left, top, right).
#'   Default is c(2.5, 4, 0, 1).
#'
#' @return No return value. Function creates a plot.
#'
#' @details This function creates a person-item map showing the distribution of person abilities
#' and item difficulties on the same scale. It displays:
#' \itemize{
#'   \item Person parameter distribution (top panel)
#'   \item Item thresholds and difficulties (bottom panel)
#'   \item Warnings for disordered thresholds (if enabled)
#' }
#'
#' The function only works with RM, RSM, and PCM models from the eRm package.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic person-item map
#' plotPImap2(rasch_model)
#'
#' # Customized map with sorted items
#' plotPImap2(rasch_model, sorted = TRUE, main = "My REBL Analysis")
#'
#' # Map with specific item subset
#' plotPImap2(rasch_model, item.subset = c("item1", "item5", "item10"))
#' }
get_pi_map <- function (object,
                        item.subset = "all",
                        sorted = FALSE,
                        main = "Person-Item Map",
                        latdim = "Latent Dimension",
                        pplabel = "Person\nParameter\nDistribution",
                        cex.gen = 0.7,
                        xrange = NULL,
                        warn.ord = TRUE,
                        warn.ord.colour = "black",
                        irug = TRUE,
                        pp = NULL,
                        margins = c(2.5, 4, 0, 1))
{

  # Input validation
  assertthat::assert_that(
    inherits(object, "eRm"),
    msg = "object must be an eRm model object"
  )
  assertthat::assert_that(
    is.logical(sorted),
    msg = "sorted must be logical"
  )
  assertthat::assert_that(
    is.character(main) && length(main) == 1,
    msg = "main must be a single character string"
  )
  assertthat::assert_that(
    is.numeric(cex.gen) && length(cex.gen) == 1 && cex.gen > 0,
    msg = "cex.gen must be a positive numeric value"
  )
  assertthat::assert_that(
    is.null(xrange) || (is.numeric(xrange) && length(xrange) == 2),
    msg = "xrange must be NULL or a numeric vector of length 2"
  )
  assertthat::assert_that(
    is.logical(warn.ord),
    msg = "warn.ord must be logical"
  )
  assertthat::assert_that(
    is.logical(irug),
    msg = "irug must be logical"
  )
  assertthat::assert_that(
    is.numeric(margins) && length(margins) == 4,
    msg = "margins must be a numeric vector of length 4"
  )

  def.par <- par(no.readonly = TRUE)
  if ((object$model == "LLTM") || (object$model == "LRSM") ||
      (object$model == "LPCM"))
    stop("Item-Person Map are computed only for RM, RSM, and PCM!")
  if (object$model == "RM" || max(object$X, na.rm = TRUE) <
      2) {
    dRm <- TRUE
    threshtable <- cbind(object$betapar, object$betapar) *
      -1
    rownames(threshtable) <- substring(rownames(threshtable), first = 6, last = 9999)
  }
  else {
    dRm <- FALSE
    threshtable <- eRm::thresholds(object)$threshtable[[1]]
  }
  tr <- as.matrix(threshtable)
  if (is.character(item.subset)) {
    if (length(item.subset) > 1 && all(item.subset %in%
                                       rownames(threshtable)))
      tr <- tr[item.subset, ]
    else if (length(item.subset) != 1 || !(item.subset ==
                                           "all"))
      stop(
        "item.subset misspecified. Use 'all' or vector of at least two valid item indices/names."
      )
  }
  else {
    if (length(item.subset) > 1 && all(item.subset %in%
                                       1:nrow(tr)))
      tr <- tr[item.subset, ]
    else
      stop(
        "item.subset misspecified. Use 'all' or vector of at least two valid item indices/names."
      )
  }
  if (sorted)
    tr <- tr[order(tr[, 1], decreasing = FALSE), ]
  loc <- as.matrix(tr[, 1])
  tr <- as.matrix(tr[, -1])
  if (is.null(pp))
    suppressWarnings(pp <- eRm::person.parameter(object))
  else if ((!("ppar" %in% class(pp))) || !identical(pp$X, object$X))
    stop("pp is not a person.parameter object which matches the main Rasch data object!")
  theta <- unlist(pp$thetapar)
  tt <- table(theta)
  ttx <- as.numeric(names(tt))
  yrange <- c(0, nrow(tr) + 1)
  if (is.null(xrange))
    xrange <- range(c(tr, theta), na.rm = T)
  nf <- layout(matrix(c(2, 1), 2, 1, byrow = TRUE), heights = c(1, 3), T)

  # mar1 -----
  par(mar = c(margins))
  plot(
    xrange,
    yrange,
    xlim = xrange,
    ylim = yrange,
    main = "",
    ylab = "",
    type = "n",
    yaxt = "n",
    xaxt = "n"
  )
  axis(
    2,
    at = 1:nrow(tr),
    labels = rev(rownames(tr)),
    las = 2,
    cex.axis = cex.gen
  )
  axis(
    1,
    at = seq(floor(xrange[1]), ceiling(xrange[2])),
    cex.axis = cex.gen,
    padj = -1.5
  )
  mtext(latdim, 1, 1.2, cex = cex.gen + 0.1)
  if (irug == TRUE) {
    y.offset <- nrow(tr) * 0.0275
    tr.rug <- as.numeric(tr)
    if (any(is.na(tr.rug)))
      tr.rug <- tr.rug[-which(is.na(tr.rug))]
    segments(tr.rug,
             rep(yrange[2], length(tr.rug)) + y.offset,
             tr.rug,
             rep(yrange[2], length(tr.rug)) + 100)
  }
  warn <- rep(" ", nrow(tr))
  for (j in 1:nrow(tr)) {
    i <- nrow(tr) + 1 - j
    assign("trpoints", tr[i, !is.na(tr[i, ])])
    npnts <- length(trpoints)
    if (!dRm && !all(sort(trpoints) == trpoints))
      ptcol = warn.ord.colour
    else
      ptcol = "black"
    if (npnts > 1)
      points(
        sort(trpoints),
        rep(j, npnts),
        type = "b",
        cex = 1,
        col = ptcol
      )
    if (dRm) {
      lines(xrange * 1.5, rep(j, 2), lty = "dotted")
    }
    else {
      if (npnts > 1)
        text(
          sort(trpoints),
          rep(j, npnts),
          (1:npnts)[order(trpoints)],
          cex = cex.gen,
          pos = 1,
          col = ptcol
        )
      if (!all(sort(trpoints) == trpoints))
        warn[j] <- "*"
    }
    points(loc[i],
           j,
           pch = 20,
           cex = 1.5,
           col = ptcol)
  }
  if (warn.ord)
    axis(
      4,
      at = 1:nrow(tr),
      tick = FALSE,
      labels = warn,
      hadj = 2.5,
      padj = 0.7,
      las = 2
    )

  # mar2 -----
  par(mar = c(0, margins[2], 3, margins[4]))
  plot(
    ttx,
    tt,
    type = "n",
    main = main,
    axes = FALSE,
    ylab = "",
    xlim = xrange,
    ylim = c(0, max(tt))
  )
  points(
    ttx,
    tt,
    type = "h",
    col = "gray",
    lend = 2,
    lwd = 5
  )
  mtext(pplabel, 2, 0.5, las = 2, cex = cex.gen)
  box()
  par(def.par)
}
