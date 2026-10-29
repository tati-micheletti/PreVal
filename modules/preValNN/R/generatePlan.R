#' Experimental design for the validation-regime experiment (dataset-agnostic).
#'
#' One SPLIT = (window start s, history end e, forecast year T > e). Each split is fitted under up to five regimes
#' (REGIMES) and every covariate level. All regimes and levels of a split are built from the SAME cases (buildRegimeSplits):
#' same pool, same shared test set, same validation size, same training size.
#' @param idx buildCaseIndex()
#' @param startYear,endYear analysis years (the data may start earlier; nothing before startYear is used)
#' @param minTrainYears minimum number of TRAINING years of PreVal in a window (default 2); a window s..e has at least minTrainYears + valYears years
#' @param valYears number of validation years of PreVal (default 1; 2 = the two-year-validation variant)
#' @param horizons forecast distances T - e to build (e.g. 1:3)
#' @param covariateLevels complexity ladder (Inf = all)
#' @param planRegimes regimes that get models in the plan (default: all built ones). Names ending in "Refit" use the base regime's split and need all
#'   base regimes to be BUILT (equal training size depends on all of them), so `regimes` stays the full set
#' @param nReplicates independent network initialisations per cell (same splits)
#' @param firstForecastYear no split forecasts an earlier year (the years before it form the pre-test history used by the network pilot)
#' @param blockKm,bufferKm,nFolds spatial blocks for the spatial regime (blockKm usually from blockSizeFromAutocorrelation())
#' @param regimes which regimes to build
#' @return list(plan, splitSummary, checks, skipped, bundles, spatial)
generatePlan <- function(idx, startYear, endYear, minTrainYears = 2L, valYears = 1L, horizons = 1L, covariateLevels = c(2, 5, 10, 15, 25, 35, 50, Inf),
                         nReplicates = 1L, firstForecastYear = -Inf, planRegimes = NULL, testFraction = 0.5, blockKm = 100, bufferKm = 10, nFolds = 4L, regimes = REGIMES,
                         minTrain = 200L, minVal = 100L, minTest = 100L, minTrainFraction = 0.8, registryPath = NULL,
                         arm = c("main", "spatialTest"), spatialTestShare = 0.25, spatialValShare = 0.25, minValCombined = 10L, routeHeldOut = FALSE, matchUnits = FALSE) {
  arm <- match.arg(arm)      # spatialTest: the test cases lie in blocks no regime sees; regimes = SPATIALTEST_REGIMES (spatialTestSplits.R)
  # routeHeldOut (2026-10-27): also build FutureUnseenGrouped = PreVal validated on UNSEEN ROUTES (no spatial buffer); in the main arm it is added to the regimes, in the spatialTest arm it is passed on
  if (arm == "spatialTest") { regimes <- SPATIALTEST_REGIMES; if (minVal >= 100L) minVal <- 50L; if (minTrainFraction > 0.5) minTrainFraction <- 0.5 }
  else if (isTRUE(routeHeldOut)) regimes <- union(regimes, "FutureUnseenGrouped") else regimes <- setdiff(regimes, "FutureUnseenGrouped")
  spatialSeed <- stringSeed("spatialBlocks")
  spatial <- assignBlocks(idx, blockKm = blockKm, bufferKm = bufferKm, nFolds = nFolds, seed = spatialSeed)
  registerSeeds(registryPath, "preValNN::design", "spatialBlocks", spatialSeed, "random assignment of spatial blocks to folds")
  grid <- data.table::rbindlist(lapply(startYear:endYear, function(s) data.table::rbindlist(lapply(seq_len(max(0L, endYear - (s + minTrainYears + valYears - 1L))) + s + minTrainYears + valYears - 2L, function(e) {
    Ts <- e + horizons; Ts <- Ts[Ts <= endYear & Ts >= firstForecastYear]
    if (!length(Ts)) return(NULL)
    data.table::data.table(s = s, e = e, testYear = Ts)
  }))))
  grid[, splitId := sprintf("s%d_e%d_T%d", s, e, testYear)]
  grid[, splitSeed := vapply(splitId, stringSeed, integer(1))]
  if (anyDuplicated(grid$splitSeed)) stop("Hash collision between split seeds; change the id scheme.")
  bundles <- list(); checks <- list(); summaries <- list(); skipped <- list()
  for (i in seq_len(nrow(grid))) {
    g <- grid[i]
    b <- if (arm == "spatialTest") buildSpatialTestSplits(idx, g$s, g$e, g$testYear, g$splitSeed, spatial = spatial, testShare = spatialTestShare, valShare = spatialValShare,
                                                         minTrain = minTrain, minVal = minVal, minTest = minTest, minTrainFraction = minTrainFraction, valYears = valYears, withRouteHeldOut = isTRUE(routeHeldOut))
         else buildRegimeSplits(idx, g$s, g$e, g$testYear, g$splitSeed, spatial = spatial, testFraction = testFraction, minTrain = minTrain,
                                minVal = minVal, minTest = minTest, minTrainFraction = minTrainFraction, regimes = regimes, valYears = valYears, minValCombined = minValCombined, matchUnits = matchUnits)
    if (!isTRUE(b$ok)) { skipped[[length(skipped) + 1L]] <- data.table::data.table(splitId = g$splitId, reason = b$reason); next }
    checks[[length(checks) + 1L]] <- if (arm == "spatialTest") verifySpatialTestSplits(idx, b, g$s, g$e, g$testYear, g$splitId, spatial = spatial, valYears = valYears)
                                      else verifyRegimeSplits(idx, b, g$s, g$e, g$testYear, g$splitId, spatial = spatial, valYears = valYears)
    summaries[[length(summaries) + 1L]] <- data.table::rbindlist(lapply(names(b$splits), function(rg) summariseRegimeSplit(idx, b$splits[[rg]], g$splitId, rg)))
    bundles[[g$splitId]] <- b
    registerSeeds(registryPath, "preValNN::splits", g$splitId, g$splitSeed, "split seed (+1 test, +2 status-quo allocation, +3 Internal draw, +10/+20 grouped/spatial pick, +30+k subsample)")
  }
  keep <- grid[splitId %in% names(bundles)]
  if (length(skipped)) message(sprintf("Dropped %d splits:\n%s", length(skipped), paste(utils::capture.output(print(data.table::rbindlist(skipped))), collapse = "\n")))
  rows <- data.table::rbindlist(lapply(seq_len(nrow(keep)), function(i) {
    g <- keep[i]; b <- bundles[[g$splitId]]
    planRg <- if (is.null(planRegimes)) names(b$splits) else planRegimes
    planRg <- planRg[sub("Refit$", "", planRg) %in% names(b$splits)]
    data.table::rbindlist(lapply(planRg, function(rg) data.table::data.table(
      splitId = g$splitId, typeValidation = rg, windowStart = g$s, historyEnd = g$e, testYear = g$testYear, horizon = g$testYear - g$e,
      valYears = valYears, trainStartYear = g$s, trainEndYear = switch(sub("Refit$", "", rg), FutureUnseen = , FutureUnseenSpatial = , FutureUnseenGrouped = g$e - valYears, Internal = g$testYear, g$e),
      nTrainS = b$sizes[["nTrain"]], nValS = if (!is.null(b$valSizes) && sub("Refit$", "", rg) %in% names(b$valSizes)) b$valSizes[[sub("Refit$", "", rg)]] else b$sizes[["nVal"]],   # PreVal + spatial has its own, smaller validation size
      nTestS = b$sizes[["nTest"]], trainShare = b$trainShare, splitSeed = g$splitSeed, arm = arm)))
  }))
  plan <- data.table::rbindlist(lapply(covariateLevels, function(k) data.table::rbindlist(lapply(seq_len(nReplicates), function(r) {
    x <- data.table::copy(rows); x[, `:=`(numberOfCovariates = k, replicate = r)]; x }))))
  plan[, groupId := paste0("Grp_", numberOfCovariates, "_", windowStart, "_", historyEnd, "_", testYear, ifelse(replicate > 1, paste0("_r", replicate), ""), ifelse(valYears > 1, paste0("_v", valYears), ""))]
  plan[, modelName := paste0(groupId, "_", typeValidation)]
  plan[, modelSeed := mapply(function(sid, k, r) stringSeed(paste(sid, "cov", k, "rep", r)), splitId, numberOfCovariates, replicate)]
  if (anyDuplicated(unique(plan[, .(splitId, numberOfCovariates, replicate, modelSeed)])$modelSeed)) stop("Hash collision between model seeds.")
  if (anyDuplicated(plan$modelName)) stop("Duplicated model names in the plan.")
  cat(sprintf("Plan: %d models = %d splits x %d regimes x %d covariate levels x %d replicate(s).\n", nrow(plan), nrow(keep),
              data.table::uniqueN(plan$typeValidation), length(covariateLevels), nReplicates))
  list(plan = plan[], splitSummary = data.table::rbindlist(summaries), checks = data.table::rbindlist(checks),
       skipped = if (length(skipped)) data.table::rbindlist(skipped) else NULL, bundles = bundles, spatial = spatial)
}

#' Write manifests (row, set) once; refuse to overwrite a different existing one.
writeManifests <- function(bundles, manifestDir) {
  dir.create(manifestDir, recursive = TRUE, showWarnings = FALSE)
  for (sid in names(bundles)) for (rg in names(bundles[[sid]]$splits)) {
    m <- data.table::copy(bundles[[sid]]$splits[[rg]]); p <- file.path(manifestDir, paste0(sid, "__", rg, ".rds"))
    if (file.exists(p)) {
      if (!isTRUE(all.equal(readRDS(p)[order(set, row)], m[order(set, row)], check.attributes = FALSE)))
        stop("Existing manifest differs from the newly built one (code, data or seed changed): ", p)
    } else saveRDS(m, p)
  }
  invisible(manifestDir)
}
readManifest <- function(manifestDir, splitId, regime) {
  p <- file.path(manifestDir, paste0(splitId, "__", regime, ".rds"))
  if (!file.exists(p)) stop("Manifest not found: ", p)
  readRDS(p)
}

#' Re-verify ALL manifests on disk against the case index (post-hoc audit of what was run).
auditManifests <- function(idx, plan, manifestDir, spatial) {
  out <- list()
  for (sid in unique(plan$splitId)) {
    p <- plan[splitId == sid][1]
    rgs <- sub(paste0("^", sid, "__(.*)[.]rds$"), "\\1", list.files(manifestDir, pattern = paste0("^", sid, "__.*[.]rds$")))   # every regime stored for this split
    splits <- stats::setNames(lapply(rgs, function(rg) readManifest(manifestDir, sid, rg)[, .(row, set)]), rgs)
    sv <- if ("FutureTaintedSpatial" %in% rgs) unique(spatial$key[splits$FutureTaintedSpatial[set == "val"]$row]) else NULL
    v <- if (!is.null(p$valYears) && !is.na(p$valYears)) p$valYears else 1L
    if (identical(p$arm, "spatialTest")) {            # (also holds when FutureUnseenGrouped is present)            # spatial-test arm: its own gates (the pool is not a plain window, the test cases are in new places)
      out[[sid]] <- verifySpatialTestSplits(idx, list(ok = TRUE, splits = splits, valYears = v), p$windowStart, p$historyEnd, p$testYear, sid, spatial = spatial, valYears = v); next
    }
    pools <- list(FutureUnseen = idx[year >= p$windowStart & year <= p$historyEnd - v]$row)
    out[[sid]] <- verifyRegimeSplits(idx, list(ok = TRUE, splits = splits, pools = pools, spatialValBlocks = sv, valYears = v),
                                     p$windowStart, p$historyEnd, p$testYear, sid, spatial = spatial, valYears = v)
  }
  data.table::rbindlist(out)
}
