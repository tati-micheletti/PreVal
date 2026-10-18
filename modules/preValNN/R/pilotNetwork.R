#' Pilot for the network settings of ONE species (dataset-specific structure, chosen once, then FIXED for every regime and covariate level).
#'
#' Uses the PRE-TEST history only: cases of years <= `lastPilotYear` (the year before the first forecast year of the design). Inside it the
#' pilot follows PreVal's own logic: train on the earlier years, validate on the last one, so the choice is made with forward validation
#' and never sees a forecast year. Every configuration of the grid is fitted with the same seed; the lowest validation loss wins
#' (ties within `tolerance` go to the smaller network).
#' @param grid data.table/list of configurations; columns hidden (list), embDim, dropout, lr, batchSize, weightDecay
#' @return list(table = all configurations with their validation loss, best = the chosen hyper list, lastPilotYear)
pilotNetwork <- function(store, features, adapter, lastPilotYear, grid = NULL, seed, epochs = 100L, earlyStopPatience = 15L,
                         device = "cpu", zClip = 10, tolerance = 0.002, verbose = FALSE, idMode = c("intercept", "none", "concat")) {
  idMode <- match.arg(idMode)
  idx <- store$index
  yrs <- sort(unique(idx$year[idx$year <= lastPilotYear]))
  if (length(yrs) < 3) stop("pilotNetwork: fewer than three pre-test years; cannot split into train and validation years")
  vaYear <- max(yrs)
  trRows <- which(idx$year < vaYear & idx$year <= lastPilotYear); vaRows <- which(idx$year == vaYear)
  if (is.null(grid)) {
    grid <- list()
    # the route-information mode (none / intercept / concat) is FIXED by design, not chosen by the pilot; the pilot tunes the rest
    for (h in list(c(32, 16), c(64, 32), c(128, 64), c(128, 64, 32))) for (wd in c(0, 1e-4)) for (dr in c(0, 0.1))
      for (idd in if (idMode == "intercept") c(1e-4, 1e-3, 1e-2) else 0)
        grid[[length(grid) + 1L]] <- list(hidden = h, embDim = if (idMode == "concat") 8L else 0L, idIntercept = idMode == "intercept", idDecay = idd,
                                          weightDecay = wd, dropout = dr, lr = 0.003, batchSize = 128L)
  }
  tr <- storeSlice(store, trRows, features, device); va <- storeSlice(store, vaRows, features, device)
  st <- scaleStats(tr$x, features); tr$x <- applyScale(tr$x, st, zClip); va$x <- applyScale(va$x, st, zClip)
  res <- lapply(seq_along(grid), function(i) {
    hp <- grid[[i]]; hp$epochs <- epochs
    fit <- tryCatch(fitNet(adapter, hp, tr, va, store$nUnits, seed = seed, device = device, earlyStopPatience = earlyStopPatience, verbose = verbose),
                    error = function(e) NULL)
    data.table::data.table(config = i, hidden = paste(hp$hidden, collapse = "-"), embDim = hp$embDim, idIntercept = isTRUE(hp$idIntercept), idDecay = hp$idDecay %||% NA_real_,
                           weightDecay = hp$weightDecay, dropout = hp$dropout,
                           lr = hp$lr, nParams = sum(hp$hidden), valLoss = if (is.null(fit)) NA_real_ else fit$bestValLoss,
                           bestEpoch = if (is.null(fit)) NA_integer_ else fit$bestEpoch)
  })
  tab <- data.table::rbindlist(res)
  ok <- tab[is.finite(valLoss)]
  if (!nrow(ok)) stop("pilotNetwork: no configuration produced a finite validation loss")
  cand <- ok[valLoss <= min(valLoss) + tolerance]
  win <- cand[order(nParams, embDim, dropout, weightDecay, -idDecay)][1]
  best <- grid[[win$config]]; best$epochs <- epochs
  list(table = tab[order(valLoss)], best = best, lastPilotYear = lastPilotYear, nTrain = length(trRows), nVal = length(vaRows), validationYear = vaYear)
}
