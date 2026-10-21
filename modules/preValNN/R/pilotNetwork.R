#' Pilot for the network settings of ONE species (dataset-specific structure, chosen once, then FIXED for every regime and covariate level).
#'
#' mode = "random" (default, same logic as the covariate ranking): a random 20% hold-out of ALL cases decides. The settings are the same for every
#' regime, so they cannot favour one regime; they only fix the network's shape (absolute losses may be marginally optimistic, relative ones are not).
#' mode = "balanced" (default for the birds from 2026-10-21): every configuration is judged under the random hold-out AND two forward geometries (train up to
#' the last year minus 1 or 5 years, validate on the last year); the mean validation loss decides (see the comment in the code).
#' mode = "forward" (strict version): only cases of years <= `lastPilotYear` (the year before the first forecast year); train on the earlier years,
#' validate on the last one, so the choice never sees a forecast year (costs the earliest forecast years). Every configuration of the grid is fitted with the same seed; the lowest validation loss wins
#' (ties within `tolerance` go to the smaller network).
#' @param grid data.table/list of configurations; columns hidden (list), embDim, dropout, lr, batchSize, weightDecay
#' @return list(table = all configurations with their validation loss, best = the chosen hyper list, lastPilotYear)
pilotNetwork <- function(store, features, adapter, lastPilotYear, grid = NULL, seed, epochs = 100L, earlyStopPatience = 15L,
                         device = "cpu", zClip = 10, tolerance = 0.002, verbose = FALSE, idMode = c("intercept", "none", "concat"), mode = c("random", "forward", "balanced"),
                         gaps = NULL) {
  idMode <- match.arg(idMode); mode <- match.arg(mode)
  idx <- store$index; n <- nrow(idx)
  # With lastPilotYear (the year before the first forecast year) the pilot sees ONLY years up to it, in every geometry: no forecast year ever informs the network settings.
  poolRows <- if (is.na(lastPilotYear)) seq_len(n) else which(idx$year <= lastPilotYear)
  if (is.null(gaps)) gaps <- if (is.na(lastPilotYear)) c(1L, 5L) else c(1L, 3L)
  # VALIDATION GEOMETRIES the settings are judged under. "balanced" (default of birds from 2026-10-21): the SAME configurations are fitted under a random 20% hold-out
  # (the status quo's geometry) AND under forward geometries (train on years <= Y - gap, validate on year Y = the last year; gaps 1 and 5 years), and the
  # mean validation loss decides. The audit showed that a pilot run only under the random geometry picks the weakest route-intercept shrinkage (the edge of the
  # grid), which suits next-year interpolation and costs most at long horizons, i.e. it was tuned in the status quo's own geometry.
  geoms <- list()
  if (mode %in% c("random", "balanced")) {
    vaRows <- sort(withSeed(seed, sample(poolRows, floor(0.2 * length(poolRows))))); geoms[["random"]] <- list(tr = setdiff(poolRows, vaRows), va = vaRows, year = NA_integer_)
  }
  if (mode == "forward") {
    yrs <- sort(unique(idx$year[idx$year <= lastPilotYear]))
    if (length(yrs) < 3) stop("pilotNetwork: fewer than three pre-test years; cannot split into train and validation years")
    vaYear <- max(yrs); geoms[["forward"]] <- list(tr = which(idx$year < vaYear & idx$year <= lastPilotYear), va = which(idx$year == vaYear), year = vaYear)
  }
  if (mode == "balanced") {
    vaYear <- max(idx$year[poolRows])
    for (g in gaps) geoms[[paste0("forward_gap", g)]] <- list(tr = poolRows[idx$year[poolRows] <= vaYear - g], va = poolRows[idx$year[poolRows] == vaYear], year = vaYear)
  }
  vaYear <- if (mode == "random") NA_integer_ else if (mode == "forward") geoms[["forward"]]$year else max(idx$year[poolRows])
  if (is.null(grid)) {
    grid <- list()
    # the route-information mode (none / intercept / concat) is FIXED by design, not chosen by the pilot; the pilot tunes the rest
    for (h in list(c(32, 16), c(64, 32), c(128, 64), c(128, 64, 32))) for (wd in c(0, 1e-4)) for (dr in c(0, 0.1))
      for (idd in if (idMode == "intercept") c(1e-5, 1e-4, 1e-3, 1e-2) else 0)
        grid[[length(grid) + 1L]] <- list(hidden = h, embDim = if (idMode == "concat") 8L else 0L, idIntercept = idMode == "intercept", idDecay = idd,
                                          weightDecay = wd, dropout = dr, lr = 0.003, batchSize = 128L)
  }
  prepared <- lapply(geoms, function(g) {
    tr <- storeSlice(store, g$tr, features, device); va <- storeSlice(store, g$va, features, device)
    st <- scaleStats(tr$x, features); tr$x <- applyScale(tr$x, st, zClip); va$x <- applyScale(va$x, st, zClip); list(tr = tr, va = va)
  })
  res <- lapply(seq_along(grid), function(i) {
    hp <- grid[[i]]; hp$epochs <- epochs
    fits <- lapply(prepared, function(p) tryCatch(fitNet(adapter, hp, p$tr, p$va, store$nUnits, seed = seed, device = device, earlyStopPatience = earlyStopPatience, verbose = verbose),
                                                  error = function(e) { message("pilot configuration ", i, " failed: ", conditionMessage(e)); NULL }))   # no silent failure
    vl <- vapply(fits, function(f) if (is.null(f)) NA_real_ else f$bestValLoss, numeric(1))
    out <- data.table::data.table(config = i, hidden = paste(hp$hidden, collapse = "-"), embDim = hp$embDim, idIntercept = isTRUE(hp$idIntercept), idDecay = hp$idDecay %||% NA_real_,
                                  weightDecay = hp$weightDecay, dropout = hp$dropout, lr = hp$lr, nParams = sum(hp$hidden),
                                  valLoss = if (anyNA(vl)) NA_real_ else mean(vl),                       # the mean over the geometries decides
                                  bestEpoch = if (is.null(fits[[1]])) NA_integer_ else fits[[1]]$bestEpoch)
    for (k in names(vl)) out[[paste0("valLoss_", k)]] <- vl[[k]]
    out
  })
  tab <- data.table::rbindlist(res)
  ok <- tab[is.finite(valLoss)]
  if (!nrow(ok)) stop("pilotNetwork: no configuration produced a finite validation loss")
  if (nrow(ok) < nrow(tab)) warning("pilotNetwork: ", nrow(tab) - nrow(ok), " of ", nrow(tab), " configurations failed (see the messages above)")
  cand <- ok[valLoss <= min(valLoss) + tolerance]
  win <- cand[order(nParams, embDim, dropout, weightDecay, -idDecay)][1]
  best <- grid[[win$config]]; best$epochs <- epochs
  list(table = tab[order(valLoss)], best = best, lastPilotYear = lastPilotYear, nTrain = length(geoms[[1]]$tr), nVal = length(geoms[[1]]$va), validationYear = vaYear,
       geometries = names(geoms))
}
