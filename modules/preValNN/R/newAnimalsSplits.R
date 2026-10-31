#' NEW-ANIMALS ARM (added 2026-10-31, caribou): forecasting for UNITS (animals) the models have never seen. The shared test strata of a split are the forecast-year strata of a
#' held-out set of animals; ALL strata of those animals, in every year, are removed from the pool of EVERY regime (training and validation), so nobody can use the animal
#' embedding or the animal's earlier years for the test strata. There is no spatial buffer: the unit of separation is the animal, which is what the birds' new-routes arm
#' does for routes (a route is a fixed place; an animal is not, so a spatially blocked "new place" can still contain an animal that was trained on, see caribou/newplaces_animal_overlap.R).
#'
#' HELD-OUT ANIMALS are a function of the WINDOW (s, e) only (newAnimalsHeldOut: seeded animal folds, one fold of nFolds, about 25% of the animals), identical for every forecast
#' year and every regime of the window. That is what keeps the per-window covariate ranking valid: the rank stage excludes the same animals (rankCovariates(excludeUnits = ...)),
#' so neither the ranking nor any model has seen an animal of the test set in any year.
#'
#' One split = (s, e, T) as in buildRegimeSplits. Steps:
#'  1. TEST = the year-T strata of the held-out animals (all of them). Pool U' = strata of years s..e of the animals that are NOT held out.
#'  2. PreVal's training years are s..e-v, its validation years e-v+1..e (v = valYears). m = floor(valShare * number of validation-year strata of U').
#'  3. Regimes (the SAME validation size m and the same training size):
#'     * FutureUnseenGrouped   PreVal: validation = last-year strata of whole animals (random order until m strata; the last animal trimmed); those animals are absent from training in every year;
#'     * FutureTainted         status quo: U' re-allocated at random, validation = m strata;
#'     * FutureTaintedGrouped  animal-grouped CV: validation = whole animals of U' (all years, random order until m strata; the last animal trimmed), training excludes ALL strata of those animals.
#'  4. EQUAL TRAINING SIZE by random subsampling to the smallest training pool (as in the other designs); splits where that leaves less than minTrainFraction of the status-quo pool are skipped.
NEWANIMALS_REGIMES <- c("FutureUnseenGrouped", "FutureTainted", "FutureTaintedGrouped")

#' The animals held out in window s..e: one seeded fold of nFolds over all animals that appear from year s on (so the set does not depend on the forecast year).
newAnimalsHeldOut <- function(idx, s, e, nFolds = 4L) {
  ids <- sort(unique(as.character(idx$unit[idx$year >= s])))
  fold <- withSeed(stringSeed(paste("newAnimalsFolds", s, e)), sample(rep_len(seq_len(nFolds), length(ids))))
  k <- withSeed(stringSeed(paste("newAnimalsFold", s, e)), sample.int(nFolds, 1L))
  ids[fold == k]
}

buildNewAnimalsSplits <- function(idx, s, e, testYear, seed, valShare = 0.30, minTrain = 200L, minVal = 50L, minTest = 100L, minTrainFraction = 0.4, valYears = 1L, nFolds = 4L) {
  stopifnot(s <= e - valYears, e < testYear, valYears >= 1L)
  yr <- idx$year; held <- newAnimalsHeldOut(idx, s, e, nFolds); isHeld <- as.character(idx$unit) %in% held
  test <- sort(which(yr == testYear & isHeld))
  if (length(test) < minTest) return(list(ok = FALSE, reason = sprintf("test set too small (%d)", length(test))))
  trainP <- which(yr >= s & yr <= e - valYears & !isHeld); valP <- which(yr > e - valYears & yr <= e & !isHeld); U <- c(trainP, valP)
  m <- floor(valShare * length(valP))
  if (length(trainP) < minTrain || m < minVal) return(list(ok = FALSE, reason = sprintf("train/val too small (%d/%d)", length(trainP), m)))
  pools <- list()
  pg <- .pickGroups(valP, idx$unit, m, seed + 50L); if (is.null(pg)) return(list(ok = FALSE, reason = "validation size larger than the validation years (unseen animals)"))
  pools$FutureUnseenGrouped <- list(train = trainP[!idx$unit[trainP] %in% pg$groups], val = pg$valRows)
  perm <- withSeed(seed + 2L, sample(U)); pools$FutureTainted <- list(train = perm[(m + 1L):length(perm)], val = perm[seq_len(m)])
  pk <- .pickGroups(U, idx$unit, m, seed + 10L); if (is.null(pk)) return(list(ok = FALSE, reason = "validation size larger than the pool (animal-grouped CV)"))
  pools$FutureTaintedGrouped <- list(train = U[!idx$unit[U] %in% pk$groups], val = pk$valRows)
  poolSizes <- vapply(pools, function(p) length(p$train), numeric(1)); nEq <- min(poolSizes)
  if (nEq < minTrain) return(list(ok = FALSE, reason = sprintf("training too small after grouping (%d)", nEq)))
  if (nEq < minTrainFraction * poolSizes[["FutureTainted"]]) return(list(ok = FALSE, reason = sprintf("blocking leaves only %d of %d status-quo training cases", nEq, poolSizes[["FutureTainted"]])))
  mk <- function(tr, va) data.table::rbindlist(list(data.table::data.table(row = sort(tr), set = "train"), data.table::data.table(row = sort(va), set = "val"),
                                                    data.table::data.table(row = test, set = "test")))
  splits <- list()
  for (k in seq_along(pools)) { nm <- names(pools)[k]; p <- pools[[nm]]
    tr <- if (length(p$train) > nEq) p$train[withSeed(seed + 30L + k, sample.int(length(p$train), nEq))] else p$train
    splits[[nm]] <- mk(tr, p$val) }
  splits <- splits[NEWANIMALS_REGIMES]
  list(ok = TRUE, reason = NA_character_, valYears = valYears, splits = splits, sizes = c(nTrain = nEq, nVal = m, nTest = length(test)),
       valSizes = vapply(splits, function(x) sum(x$set == "val"), numeric(1)), trainShare = nEq / poolSizes[["FutureTainted"]], poolSizes = poolSizes,
       pools = lapply(pools, function(p) p$train), heldAnimals = held, testAnimals = unique(as.character(idx$unit[test])), arm = "newAnimals")
}

#' Gates of the new-animals arm; every check STOPS the run on failure.
#'   N1 no case in two sets within a regime; N2 identical test set in year T, equal train/val/test sizes across regimes
#'   N3 NO training or validation case of ANY regime belongs to a held-out animal (recomputed from the window: the test animals are new for everyone, in every year)
#'   N4 PreVal: training years in [s, e-v], validation years in (e-v, e], and no animal in both training and validation
#'   N5 status quo and animal-grouped CV: train+val years in [s, e]; animal-grouped CV: no animal in both training and validation
verifyNewAnimalsSplits <- function(idx, bundle, s, e, testYear, splitId, valYears = bundle$valYears %||% 1L, nFolds = 4L) {
  sp <- bundle$splits; yr <- idx$year; res <- list()
  add <- function(check, ok, detail = "") res[[length(res) + 1L]] <<- data.table::data.table(splitId = splitId, check = check, ok = isTRUE(ok), detail = as.character(detail))
  rowsOf <- function(m, st) m$row[m$set == st]
  for (rg in names(sp)) add(paste0("N1 disjoint sets [", rg, "]"), !anyDuplicated(sp[[rg]]$row))
  tests <- lapply(sp, rowsOf, st = "test")
  add("N2 test set identical across regimes and entirely in the forecast year", all(vapply(tests, function(t) identical(sort(t), sort(tests[[1]])), logical(1))) && all(yr[tests[[1]]] == testYear))
  sz <- vapply(sp, function(m) c(sum(m$set == "train"), sum(m$set == "val"), sum(m$set == "test")), numeric(3))
  add("N2 equal train/val/test sizes across regimes", all(apply(sz, 1, function(v) length(unique(v)) == 1)), paste(apply(sz, 2, paste, collapse = "/"), collapse = " | "))
  held <- newAnimalsHeldOut(idx, s, e, nFolds); u <- function(r) as.character(idx$unit[r])
  add("N3 every test stratum belongs to a held-out animal of the window", all(u(tests[[1]]) %in% held))
  for (rg in names(sp)) add(sprintf("N3 %s: no training/validation stratum of a held-out animal (any year)", rg), !any(u(c(rowsOf(sp[[rg]], "train"), rowsOf(sp[[rg]], "val"))) %in% held))
  for (rg in intersect("FutureUnseenGrouped", names(sp))) {
    add(sprintf("N4 %s: train years in [s, e-v], validation years in (e-v, e]", rg), all(yr[rowsOf(sp[[rg]], "train")] >= s & yr[rowsOf(sp[[rg]], "train")] <= e - valYears) && all(yr[rowsOf(sp[[rg]], "val")] > e - valYears & yr[rowsOf(sp[[rg]], "val")] <= e))
  }
  for (rg in intersect(c("FutureUnseenGrouped", "FutureTaintedGrouped"), names(sp)))
    add(sprintf("N4 %s: no animal appears in both training and validation", rg), !length(intersect(u(rowsOf(sp[[rg]], "train")), u(rowsOf(sp[[rg]], "val")))))
  for (rg in intersect(c("FutureTainted", "FutureTaintedGrouped"), names(sp))) { tv <- c(rowsOf(sp[[rg]], "train"), rowsOf(sp[[rg]], "val")); add(sprintf("N5 %s years in [s, e]", rg), all(yr[tv] >= s & yr[tv] <= e)) }
  out <- data.table::rbindlist(res)
  if (any(!out$ok)) stop("verifyNewAnimalsSplits FAILED for ", splitId, ":\n", paste(utils::capture.output(print(out[ok == FALSE])), collapse = "\n"))
  out
}
