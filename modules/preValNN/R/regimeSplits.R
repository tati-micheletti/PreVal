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
#'  * FutureUnseenSpatial (PreVal + spatial blocks, added 2026-10-21): validation = the validation-year cases inside the SAME blocks as the spatial regime
#'    (later AND in places the training never saw); training = PreVal's training years outside those blocks and their buffer. Its validation set is SMALLER by
#'    design (about 6-20% of PreVal's), and its training pool is 70-89% of the status-quo pool, which sets the common training size of every regime.
#'  * FutureUnseenGrouped (PreVal on UNSEEN ROUTES, added 2026-10-27): validation = the validation-year cases of a random set of whole routes; those routes are removed from training in EVERY
#'    year (no spatial buffer, neighbouring routes stay in training). Same principle as validating on an unseen year, applied to routes. Its validation size equals that of PreVal + spatial
#'    when that regime is built (otherwise 15% of PreVal's).
REGIMES <- c("FutureUnseen", "FutureTainted", "FutureTaintedGrouped", "FutureTaintedSpatial", "FutureUnseenSpatial", "FutureUnseenGrouped", "Internal")

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
                              regimes = REGIMES, valYears = 1L, minValCombined = 10L) {      # production passes 50 (preValNN parameter minValCombined)
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
  pkS <- NULL
  if (any(c("FutureTaintedSpatial", "FutureUnseenSpatial") %in% regimes)) {
    if (is.null(spatial)) stop("the spatial regimes need `spatial` (assignBlocks)")
    pkS <- .pickGroups(U, spatial$key, nVal, seed + 20L)                 # the SAME validation blocks for the spatial regime and the PreVal + spatial regime
    if (is.null(pkS)) return(list(ok = FALSE, reason = "validation size larger than the pool (spatial)"))
    zone <- blockZone(idx, pkS$groups, spatial)
    if ("FutureTaintedSpatial" %in% regimes) pools$FutureTaintedSpatial <- list(train = U[!zone[U]], val = pkS$valRows, nBuffered = sum(zone[U]) - length(pkS$valRows))
    if ("FutureUnseenSpatial" %in% regimes) {
      valC <- valP[spatial$key[valP] %in% pkS$groups]
      if (length(valC) < minValCombined) return(list(ok = FALSE, reason = sprintf("PreVal + spatial validation too small (%d cases)", length(valC))))
      pools$FutureUnseenSpatial <- list(train = trainP[!zone[trainP]], val = valC)
    }
  }

  if ("FutureUnseenGrouped" %in% regimes) {
    mG <- if (!is.null(pools$FutureUnseenSpatial)) length(pools$FutureUnseenSpatial$val) else max(minValCombined, floor(0.15 * nVal))
    pg <- .pickGroups(valP, idx$unit, mG, seed + 50L)
    if (is.null(pg)) return(list(ok = FALSE, reason = "validation size larger than the validation years (unseen routes)"))
    pools$FutureUnseenGrouped <- list(train = trainP[!idx$unit[trainP] %in% pg$groups], val = pg$valRows)      # the held-out routes are absent from training in every year
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
  sp2 <- splits[intersect(regimes, names(splits))]
  list(ok = TRUE, reason = NA_character_, valYears = valYears, splits = sp2,
       sizes = c(nTrain = nEq, nVal = nVal, nTest = length(test)), valSizes = vapply(sp2, function(m) sum(m$set == "val"), numeric(1)),
       trainShare = nEq / poolSizes[["FutureTainted"]], poolSizes = poolSizes, pools = lapply(pools, function(p) p$train),
       spatialValBlocks = if (!is.null(pkS)) pkS$groups else NULL)
}

#' Per-set composition (cases, units, year range) for the audit trail.
summariseRegimeSplit <- function(idx, split, splitId, regime) {
  d <- merge(split, idx[, .(row, unit, year)], by = "row", sort = FALSE)
  d[, .(splitId = splitId, regime = regime, nCases = .N, nUnits = data.table::uniqueN(unit),
        yearMin = min(year), yearMax = max(year)), by = set]
}
