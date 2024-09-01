#' read_erm
#' 2024-08-14

#' Modification of function plink::read.erm from that actually works. Just set
#' the groups and time variables within function to 1. These are not part of the
#' standard output from eRm objects.

read_erm <- function (x,
                      loc.out = FALSE,
                      as.irt.pars = TRUE)
{
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
  pm <- as.poly.mod(items, mod, tmp.it)
  for (i in 1:length(pars)) {
    rownames(pars[[i]]) <- NULL
    colnames(pars[[i]]) <- NULL
    pars[[i]] <- sep.pars(pars[[i]],
                          cat = cat,
                          poly.mod = pm,
                          loc.out = loc.out)
  }
  if (length(pars) == 1) {
    pars <- as.irt.pars(pars[[1]])
  }
  else {
    common <- vector("list", (groups * time) - 1)
    for (i in 1:length(common)) {
      common[[i]] <- matrix(1:items, items, 2)
    }
    pars <- as.irt.pars(pars, common, grp.names = names(pars))
  }
  if (as.irt.pars == FALSE) {
    pars <- pars@pars
  }
  return(pars)
}