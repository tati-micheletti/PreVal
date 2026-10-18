#' Disjoint, deterministic train/validation/test splits for FIVE validation regimes with EQUAL training size.
#'
#' One split = (window start s, history end e, forecast year T > e). Pool U = all cases of years s..e.
#'  * ONE shared test set: a fixed random fraction (default 50%) of the cases of year T, identical in every regime.
#'  * FutureUnseen (PreVal): train = years s..e-v, validation = the last v years e-v+1..e (v = valYears, default 1; v = 2 is the
#'    two-year-validation variant).
#'  * FutureTainted (status quo): the same pool U, re-allocated at random to train and validation (validation size = PreVal's).
#'  * FutureTaintedGrouped (route-grouped CV): validation = whole units (routes, all their years in U) in random order until the
#'    PreVal validation size is reached (the last unit is trimmed at random); a trimmed unit's other cases are NOT used for training.
#'  * FutureTaintedSpatial (spatial-block CV): validation = whole spatial blocks of U in random order, same size rule; training
#'    excludes the validation blocks and a buffer around them.
#'  * Internal (leaky random reference, only for H1, never compared with PreVal): random draw from years s..T minus the test set.
#' EQUAL TRAINING SIZE: every regime's training set is randomly subsampled to the smallest training pool among the first four
#' regimes (in practice the spatial-block one, which loses its buffer). Splits where that leaves less than `minTrainFraction`
#' of the status-quo training pool are skipped, so the contrast is not confounded by how much data a regime keeps.
REGIMES <- c("FutureUnseen", "FutureTainted", "FutureTaintedGrouped", "FutureTaintedSpatial", "Internal")

# whole groups (units or blocks) in random order until `nVal` cases are reached; the last group is trimmed at random
.pickGroups <- function(poolRows, groupOf, nVal, seed) {
  g <- groupOf[poolRows]
  ord <- withSeed(seed, sample(unique(g)))
  cnt <- as.numeric(table(g)[ord])
  nG <- which(cumsum(cnt) >= nVal)[1]
  if (is.na(nG)) return(NULL)
  chosen <- ord[seq_len(nG)]
  valRows <- poolRows[g %in% chosen]
  if (length(valRows) > nVal) {
    last <- poolRows[g == ord[nG]]
    drop <- last[withSeed(seed + 1L, sample.int(length(last), length(valRows) - nVal))]
    valRows <- setdiff(valRows, drop)
  }
  list(groups = chosen, valRows = sort(valRows))
}

#' @param idx case index from buildCaseIndex()
#' @param spatial NULL or assignBlocks() result (needed for the spatial regime)
#' @param regimes subset of REGIMES to build (FutureUnseen is always built)
buildRegimeSplits <- function(idx, s, e, testYear, seed, spatial = NULL, testFraction = 0.5,
                              minTrain = 200L, minVal = 100L, minTest = 100L, minTrainFraction = 0.8,
                              regimes = REGIMES, valYears = 1L) {
  stopifnot(s <= e - valYears, e < testYear, valYears >= 1L)
  yr <- idx$year
  tp <- which(yr == testYear)
  nTest <- floor(testFraction * length(tp))
  if (nTest < minTest) return(list(ok = FALSE, reason = sprintf("test set too small (%d)", nTest)))
  test <- sort(withSeed(seed + 1L, sample(tp, nTest)))

  trainP <- which(yr >= s & yr <= e - valYears); valP <- which(yr > e - valYears & yr <= e)
  U <- c(trainP, valP); nVal <- length(valP)
  if (length(trainP) < minTrain || nVal < minVal)
    return(list(ok = FALSE, reason = sprintf("train/val too small (%d/%d)", length(trainP), nVal)))

  pools <- list(FutureUnseen = list(train = trainP, val = valP))
  perm <- withSeed(seed + 2L, sample(U))
  pools$FutureTainted <- list(train = perm[(nVal + 1L):length(perm)], val = perm[seq_len(nVal)])

  if ("FutureTaintedGrouped" %in% regimes) {
    pk <- .pickGroups(U, idx$unit, nVal, seed + 10L)
    if (is.null(pk)) return(list(ok = FALSE, reason = "validation size larger than the pool (grouped)"))
    pools$FutureTaintedGrouped <- list(train = U[!idx$unit[U] %in% pk$groups], val = pk$valRows)
  }
  if ("FutureTaintedSpatial" %in% regimes) {
    if (is.null(spatial)) stop("the spatial regime needs `spatial` (assignBlocks)")
    pk <- .pickGroups(U, spatial$key, nVal, seed + 20L)
    if (is.null(pk)) return(list(ok = FALSE, reason = "validation size larger than the pool (spatial)"))
    zone <- blockZone(idx, pk$groups, spatial)
    pools$FutureTaintedSpatial <- list(train = U[!zone[U]], val = pk$valRows, nBuffered = sum(zone[U]) - length(pk$valRows))
  }

  poolSizes <- vapply(pools, function(p) length(p$train), numeric(1))
  nEq <- min(poolSizes)
  if (nEq < minTrain) return(list(ok = FALSE, reason = sprintf("training too small after blocking (%d)", nEq)))
  if (nEq < minTrainFraction * poolSizes[["FutureTainted"]])
    return(list(ok = FALSE, reason = sprintf("blocking leaves only %d of %d status-quo training cases", nEq, poolSizes[["FutureTainted"]])))

  mk <- function(tr, va) data.table::rbindlist(list(
    data.table::data.table(row = sort(tr), set = "train"), data.table::data.table(row = sort(va), set = "val"),
    data.table::data.table(row = test, set = "test")))
  splits <- list()
  for (k in seq_along(pools)) {
    nm <- names(pools)[k]; p <- pools[[nm]]
    tr <- if (length(p$train) > nEq) p$train[withSeed(seed + 30L + k, sample.int(length(p$train), nEq))] else p$train
    splits[[nm]] <- mk(tr, p$val)
  }
  if ("Internal" %in% regimes) {
    intPool <- setdiff(which(yr >= s & yr <= testYear), test)
    if (length(intPool) < nEq + nVal) return(list(ok = FALSE, reason = "Internal pool smaller than train+val"))
    draw <- withSeed(seed + 3L, sample(intPool, nEq + nVal))
    splits$Internal <- mk(draw[(nVal + 1L):length(draw)], draw[seq_len(nVal)])
  }
  list(ok = TRUE, reason = NA_character_, valYears = valYears, splits = splits[intersect(regimes, names(splits))],
       sizes = c(nTrain = nEq, nVal = nVal, nTest = length(test)),
       poolSizes = poolSizes, pools = lapply(pools, function(p) p$train),
       spatialValBlocks = if (!is.null(pools$FutureTaintedSpatial)) unique(spatial$key[pools$FutureTaintedSpatial$val]) else NULL)
}

#' Per-set composition (cases, units, year range) for the audit trail.
summariseRegimeSplit <- function(idx, split, splitId, regime) {
  d <- merge(split, idx[, .(row, unit, year)], by = "row", sort = FALSE)
  d[, .(splitId = splitId, regime = regime, nCases = .N, nUnits = data.table::uniqueN(unit),
        yearMin = min(year), yearMax = max(year)), by = set]
}
