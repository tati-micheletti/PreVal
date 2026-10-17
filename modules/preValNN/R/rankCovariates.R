#' Covariate ranking for the complexity ladder (per species, on one global model).
#'
#' A global network (same adapter and hyper-parameters as the experiment) is fitted once; the importance of every covariate is its
#' PERMUTATION importance: the rise in loss on held-out cases when that covariate is shuffled. Highly correlated covariates
#' (|r| > groupR on the training cases, connected components) are permuted TOGETHER so that a covariate is not under-ranked because
#' a twin carries the same information; members of a group share the group's importance and are ordered by their own marginal
#' association with the response (absolute Spearman correlation on the training cases).
#'
#' mode = "random":  training and held-out cases are a random split of the years in `rankYears`: the ranking a practitioner with the
#'                   usual random validation would obtain (primary ladder, as in the caribou refit).
#' mode = "forward": training on the first `trainFraction` of the years in `rankYears`, held-out = the later years: the ranking
#'                   PreVal's own logic would give (sensitivity ladder).
#' Neither ranking sees the forecast-year test sets of the regime comparison in any way that differs between regimes: it only fixes the
#' ORDER in which covariates enter, identically for all regimes.
#' @return data.table(Feature, group, Importance, ImportanceSD, marginalAssoc, rank) ordered by rank, with attributes baselineLoss and mode
rankCovariates <- function(store, features, adapter, hyper, seed, mode = c("random", "forward"), rankYears = NULL, heldOutFraction = 0.2,
                           trainFraction = 0.6, nRep = 5L, groupR = 0.9, device = "cpu", zClip = 10, earlyStopPatience = 15L, verbose = FALSE) {
  mode <- match.arg(mode)
  idx <- store$index; yr <- idx$year
  pool <- if (is.null(rankYears)) seq_len(nrow(idx)) else which(yr %in% rankYears)
  if (mode == "random") {
    ho <- withSeed(seed, sample(pool, floor(heldOutFraction * length(pool))))
    trRows <- setdiff(pool, ho)
    # validation for early stopping: a random tenth of the training rows; the held-out rows are used ONLY for the importance
    vaRows <- withSeed(seed + 1L, sample(trRows, floor(0.1 * length(trRows)))); trRows <- setdiff(trRows, vaRows)
  } else {
    ys <- sort(unique(yr[pool])); cut <- ys[max(2L, floor(trainFraction * length(ys)))]
    trAll <- pool[yr[pool] <= cut]; ho <- pool[yr[pool] > cut]
    vaRows <- withSeed(seed + 1L, sample(trAll, floor(0.1 * length(trAll)))); trRows <- setdiff(trAll, vaRows)
  }
  tr <- storeSlice(store, trRows, features, device); va <- storeSlice(store, vaRows, features, device); te <- storeSlice(store, ho, features, device)
  st <- scaleStats(tr$x, features)
  tr$x <- applyScale(tr$x, st, zClip); va$x <- applyScale(va$x, st, zClip); te$x <- applyScale(te$x, st, zClip)
  fit <- fitNet(adapter, hyper, tr, va, store$nUnits, seed = seed, device = device, earlyStopPatience = earlyStopPatience, verbose = verbose)
  net <- fit$net; seen <- fit$trainUnitsSeen
  base <- mean(scoreCases(net, adapter, te$x, te$y, te$id, seen, store$nUnits)$loss)

  # correlation groups (connected components of |r| > groupR on the standardised training cases)
  M <- as.matrix(tr$x$cpu()); Cm <- abs(stats::cor(M)); Cm[is.na(Cm)] <- 0; diag(Cm) <- 0
  comp <- seq_along(features); names(comp) <- features
  for (it in seq_along(features)) {                      # label propagation until stable (cheap: <= ~100 features)
    new <- comp
    for (i in seq_along(features)) { nb <- which(Cm[i, ] > groupR); if (length(nb)) new[i] <- min(comp[c(i, nb)]) }
    if (identical(new, comp)) break
    comp <- new
  }
  grp <- match(comp, unique(comp))
  y <- store$y[trRows]
  assoc <- vapply(seq_along(features), function(i) { r <- suppressWarnings(stats::cor(M[, i], y, method = "spearman")); if (is.na(r)) 0 else abs(r) }, numeric(1))

  n <- te$x$size(1); torch::torch_manual_seed(seed + 2L)
  imp <- vapply(unique(grp), function(gi) {
    cols <- which(grp == gi); d <- numeric(nRep)
    for (r in seq_len(nRep)) {
      perm <- torch::torch_randperm(n) + 1L
      xp <- te$x$clone(); xp[, cols] <- te$x[perm, ][, cols]
      d[r] <- mean(scoreCases(net, adapter, xp, te$y, te$id, seen, store$nUnits)$loss) - base
    }
    c(mean(d), stats::sd(d))
  }, numeric(2))
  out <- data.table::data.table(Feature = features, group = grp, Importance = imp[1, match(grp, unique(grp))],
                                ImportanceSD = imp[2, match(grp, unique(grp))], marginalAssoc = assoc)
  data.table::setorder(out, -Importance, -marginalAssoc)
  out[, rank := seq_len(.N)]
  attr(out, "baselineLoss") <- base; attr(out, "mode") <- mode
  attr(out, "nTrain") <- length(trRows); attr(out, "nHeldOut") <- length(ho)
  out[]
}

#' Features of one ladder level (Inf = all).
featuresForLevel <- function(ranking, k) ranking$Feature[seq_len(if (is.infinite(k)) nrow(ranking) else min(k, nrow(ranking)))]

#' Effective number of covariates: principal components needed for `var` of the variance of the standardised covariates.
effectiveNumber <- function(X, var = 0.9) {
  X <- scale(as.matrix(X)); X[is.na(X)] <- 0
  ev <- stats::prcomp(X, center = FALSE, scale. = FALSE)$sdev^2
  which(cumsum(ev) / sum(ev) >= var)[1]
}
