#' SPATIAL-TEST ARM (added 2026-10-21): forecasting into NEW PLACES. Splits for four regimes where the shared test cases lie in spatial blocks that no regime
#' ever trains or validates on (blocks + buffer are removed from the pool of every regime), so nobody can use route memory for the test cases.
#'
#' One split = (s, e, T) as in buildRegimeSplits. Steps:
#'  1. TEST: whole spatial blocks of year T in random order until `testShare` of the year-T cases is reached (the last block is trimmed at random). The test set is
#'     identical in every regime. The blocks and a buffer around them (blockZone) are removed from the pool of EVERY regime, in every year.
#'  2. POOL U' = cases of years s..e outside the test zone. PreVal's training years are s..e-v, its validation years e-v+1..e (v = valYears).
#'  3. Regimes (all with the SAME validation size m and the same training size):
#'     * FutureUnseen        PreVal: train = years s..e-v of U'; validation = a random sample of m cases of the validation years;
#'     * FutureTainted       status quo: U' re-allocated at random, validation = m cases;
#'     * FutureTaintedSpatial spatial-block CV: validation = whole blocks of U' (all years) until m cases; training excludes those blocks and their buffer;
#'     * FutureUnseenSpatial  PreVal + spatial blocks: validation = the validation-year cases inside randomly chosen blocks (m cases); training = years s..e-v of U'
#'                            outside those blocks and their buffer. Validation is both LATER and in UNSEEN PLACES.
#'     m = floor(valShare * number of validation-year cases of U'); it is the size the combined regime can reach without removing too many blocks from training.
#'  4. EQUAL TRAINING SIZE by random subsampling to the smallest training pool (as in the main design).
SPATIALTEST_REGIMES <- c("FutureUnseen", "FutureTainted", "FutureTaintedSpatial", "FutureUnseenSpatial")

buildSpatialTestSplits <- function(idx, s, e, testYear, seed, spatial, testShare = 0.25, valShare = 0.25, minTrain = 200L, minVal = 50L, minTest = 100L,
                                   minTrainFraction = 0.5, valYears = 1L) {
  stopifnot(s <= e - valYears, e < testYear, valYears >= 1L, !is.null(spatial))
  yr <- idx$year; tp <- which(yr == testYear)
  nTestTarget <- floor(testShare * length(tp))
  if (nTestTarget < minTest) return(list(ok = FALSE, reason = sprintf("test set too small (%d)", nTestTarget)))
  pt <- .pickGroups(tp, spatial$key, nTestTarget, seed + 1L)
  if (is.null(pt)) return(list(ok = FALSE, reason = "test size larger than the forecast year"))
  test <- sort(pt$valRows); testZone <- blockZone(idx, pt$groups, spatial)
  trainP <- which(yr >= s & yr <= e - valYears & !testZone); valP <- which(yr > e - valYears & yr <= e & !testZone); U <- c(trainP, valP)
  m <- floor(valShare * length(valP))
  if (length(trainP) < minTrain || m < minVal) return(list(ok = FALSE, reason = sprintf("train/val too small (%d/%d)", length(trainP), m)))
  pools <- list()
  pools$FutureUnseen <- list(train = trainP, val = sort(valP[withSeed(seed + 41L, sample.int(length(valP), m))]))
  perm <- withSeed(seed + 2L, sample(U)); pools$FutureTainted <- list(train = perm[(m + 1L):length(perm)], val = perm[seq_len(m)])
  pk <- .pickGroups(U, spatial$key, m, seed + 20L); if (is.null(pk)) return(list(ok = FALSE, reason = "validation size larger than the pool (spatial)"))
  z <- blockZone(idx, pk$groups, spatial); pools$FutureTaintedSpatial <- list(train = U[!z[U]], val = pk$valRows)
  pc <- .pickGroups(valP, spatial$key, m, seed + 40L); if (is.null(pc)) return(list(ok = FALSE, reason = "validation size larger than the validation years (combined)"))
  z2 <- blockZone(idx, pc$groups, spatial); pools$FutureUnseenSpatial <- list(train = trainP[!z2[trainP]], val = pc$valRows)
  poolSizes <- vapply(pools, function(p) length(p$train), numeric(1)); nEq <- min(poolSizes)
  if (nEq < minTrain) return(list(ok = FALSE, reason = sprintf("training too small after blocking (%d)", nEq)))
  if (nEq < minTrainFraction * poolSizes[["FutureTainted"]]) return(list(ok = FALSE, reason = sprintf("blocking leaves only %d of %d status-quo training cases", nEq, poolSizes[["FutureTainted"]])))
  mk <- function(tr, va) data.table::rbindlist(list(data.table::data.table(row = sort(tr), set = "train"), data.table::data.table(row = sort(va), set = "val"),
                                                    data.table::data.table(row = test, set = "test")))
  splits <- list()
  for (k in seq_along(pools)) { nm <- names(pools)[k]; p <- pools[[nm]]
    tr <- if (length(p$train) > nEq) p$train[withSeed(seed + 30L + k, sample.int(length(p$train), nEq))] else p$train
    splits[[nm]] <- mk(tr, p$val) }
  list(ok = TRUE, reason = NA_character_, valYears = valYears, splits = splits[SPATIALTEST_REGIMES], sizes = c(nTrain = nEq, nVal = m, nTest = length(test)),
       trainShare = nEq / poolSizes[["FutureTainted"]], poolSizes = poolSizes, pools = lapply(pools, function(p) p$train),
       spatialValBlocks = pk$groups, combinedValBlocks = pc$groups, testBlocks = pt$groups, arm = "spatialTest")
}

#' Gates for the spatial-test arm; every check STOPS the run on failure (see verifyRegimeSplits for the main design).
#'   T1 no case in two sets within a regime; T2 identical test set in year T, equal sizes across regimes
#'   T3 NO training or validation case of ANY regime lies in the test blocks or their buffer (the test cases are in new places for everyone)
#'   T4 PreVal: training years in [s, e-v], validation years in (e-v, e]; combined: the same year rules
#'   T5 status quo and spatial: train+val years in [s, e]
#'   T6 spatial and combined: no training case in the regime's own validation blocks or their buffer; validation blocks recovered from the validation rows
verifySpatialTestSplits <- function(idx, bundle, s, e, testYear, splitId, spatial, valYears = bundle$valYears %||% 1L) {
  sp <- bundle$splits; yr <- idx$year; res <- list()
  add <- function(check, ok, detail = "") res[[length(res) + 1L]] <<- data.table::data.table(splitId = splitId, check = check, ok = isTRUE(ok), detail = as.character(detail))
  rowsOf <- function(m, st) m$row[m$set == st]
  for (rg in names(sp)) add(paste0("T1 disjoint sets [", rg, "]"), !anyDuplicated(sp[[rg]]$row))
  tests <- lapply(sp, rowsOf, st = "test")
  add("T2 test set identical across regimes and entirely in the forecast year", all(vapply(tests, function(t) identical(sort(t), sort(tests[[1]])), logical(1))) && all(yr[tests[[1]]] == testYear))
  sz <- vapply(sp, function(m) c(sum(m$set == "train"), sum(m$set == "val"), sum(m$set == "test")), numeric(3))
  add("T2 equal train/val/test sizes across regimes", all(apply(sz, 1, function(v) length(unique(v)) == 1)), paste(apply(sz, 2, paste, collapse = "/"), collapse = " | "))
  testZone <- blockZone(idx, unique(spatial$key[tests[[1]]]), spatial)
  for (rg in names(sp)) add(sprintf("T3 %s: no training/validation case in the test blocks or their buffer", rg), !any(testZone[c(rowsOf(sp[[rg]], "train"), rowsOf(sp[[rg]], "val"))]))
  for (rg in intersect(c("FutureUnseen", "FutureUnseenSpatial"), names(sp))) {
    add(sprintf("T4 %s: train years in [s, e-v], validation years in (e-v, e]", rg),
        all(yr[rowsOf(sp[[rg]], "train")] >= s & yr[rowsOf(sp[[rg]], "train")] <= e - valYears) && all(yr[rowsOf(sp[[rg]], "val")] > e - valYears & yr[rowsOf(sp[[rg]], "val")] <= e))
  }
  for (rg in intersect(c("FutureTainted", "FutureTaintedSpatial"), names(sp))) { tv <- c(rowsOf(sp[[rg]], "train"), rowsOf(sp[[rg]], "val")); add(sprintf("T5 %s years in [s, e]", rg), all(yr[tv] >= s & yr[tv] <= e)) }
  for (rg in intersect(c("FutureTaintedSpatial", "FutureUnseenSpatial"), names(sp))) {
    vb <- unique(spatial$key[rowsOf(sp[[rg]], "val")]); zone <- blockZone(idx, vb, spatial)
    add(sprintf("T6 %s: no training case in a validation block or its buffer", rg), !any(zone[rowsOf(sp[[rg]], "train")]))
  }
  out <- data.table::rbindlist(res)
  if (any(!out$ok)) stop("verifySpatialTestSplits FAILED for ", splitId, ":\n", paste(utils::capture.output(print(out[ok == FALSE])), collapse = "\n"))
  out
}
