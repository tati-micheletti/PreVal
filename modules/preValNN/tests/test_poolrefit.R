# Test of the POOL refit (added 2026-11-27, simulation stage 1y): PreVal's stopping epoch and learning-rate trajectory, trained on the STATUS QUO's pool of the same split
# (its training + validation cases), tested on the same test cases. Built on the structure of test_refit.R.
suppressMessages({library(data.table); library(torch)})
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
for (f in c("seeds.R", "caseIndex.R", "regimeSplits.R", "verifyRegimes.R", "adapters.R", "fitNet.R", "runModel.R", "runRefit.R", "generatePlan.R", "analyzeRegimes.R"))
  source(file.path(here, "..", "R", f))
nFail <- 0L
check <- function(cond, msg) { ok <- isTRUE(cond); cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) nFail <<- nFail + 1L }

check(identical(refitBase(c("FutureTaintedRefit", "FutureUnseenGroupedPoolRefit", "FutureUnseenRefit", "FutureTainted")), c("FutureTainted", "FutureUnseenGrouped", "FutureUnseen", "FutureTainted")), "refitBase maps every refit name to its base regime and leaves plain regimes alone")

set.seed(5)
nUnit <- 250; years <- 2005:2016
units <- data.table(SITE_ID = sprintf("r%03d", 1:nUnit), x = runif(nUnit, 4e6, 4.6e6), y = runif(nUnit, 2.7e6, 3.3e6), a = rnorm(nUnit), b = rnorm(nUnit))
tab <- rbindlist(lapply(years, function(yy) { u <- units[runif(nUnit) < 0.6]; u[, year := yy]; u }))
tab[, `:=`(n1 = rnorm(.N), count = 0L)]; tab[, count := rnbinom(.N, mu = exp(0.4 + 0.6 * a - 0.4 * b), size = 2)]
feats <- c("a", "b", "n1"); idx <- buildCaseIndex(tab)
common <- list(idx, startYear = 2005, endYear = 2016, minTrainYears = 4, horizons = 1, covariateLevels = 2, blockKm = 100, bufferKm = 10, minTrain = 100, minVal = 50, minTest = 50, routeHeldOut = TRUE)
pl <- do.call(generatePlan, common)
rr <- c("FutureUnseenGroupedPoolRefit", "FutureTaintedRefit")
plR <- do.call(generatePlan, c(common, list(planRegimes = rr)))
check("FutureUnseenGrouped" %in% names(pl$bundles[[1]]$splits), "the grouped (unseen-routes) regime is built")
check(setequal(unique(plR$plan$typeValidation), rr) && nrow(plR$plan) == 2 * uniqueN(pl$plan$splitId), "the refit plan has only the pool refit and the status-quo refit, one row each per split")
check(identical(pl$bundles, plR$bundles), "the refit plan is built on exactly the same splits as the main plan")
check(all(plR$plan[typeValidation == "FutureUnseenGroupedPoolRefit"]$trainEndYear == pl$plan[typeValidation == "FutureUnseenGrouped"]$trainEndYear), "the pool refit row carries the grouped regime's year rules")
md <- file.path(tempdir(), "mfp"); writeManifests(pl$bundles, md); writeManifests(plR$bundles, md)
check(all(auditManifests(idx, plR$plan, md, plR$spatial)$ok), "audit works for a plan with a pool refit")

st <- buildCaseStore(tab, idx, feats, "count"); ad <- getAdapter("count")
hyper <- list(hidden = c(16, 8), embDim = 0L, idIntercept = TRUE, idDecay = 1e-3, lr = 0.01, batchSize = 64L, epochs = 40L, weightDecay = 0)
mainDir <- file.path(tempdir(), "mainp"); refDir <- file.path(tempdir(), "refitp"); dir.create(mainDir, showWarnings = FALSE); dir.create(refDir, showWarnings = FALSE)
sid <- pl$plan$splitId[1]
for (rg in c("FutureUnseenGrouped", "FutureTainted")) {
  pr <- pl$plan[splitId == sid & typeValidation == rg]
  assign(paste0("base_", rg), runModel(st, readManifest(md, sid, rg), pr, feats, ad, hyper, mainDir, reRun = TRUE))
}
mT <- readManifest(md, sid, "FutureTainted"); mG <- readManifest(md, sid, "FutureUnseenGrouped")
pr <- plR$plan[splitId == sid & typeValidation == "FutureUnseenGroupedPoolRefit"]
r <- runModel(st, mG, pr, feats, ad, hyper, refDir, reRun = TRUE, mainModelDir = mainDir, poolManifest = mT)
bP <- base_FutureUnseenGrouped; bT <- base_FutureTainted
poolN <- sum(mT$set %in% c("train", "val"))
check(r$nTrain == poolN && r$nTrain > bP$nTrain + bP$nVal, sprintf("the pool refit trains on the status-quo pool (%d cases), more than PreVal's own training + validation (%d)", r$nTrain, bP$nTrain + bP$nVal))
check(r$bestEpoch == bP$bestEpoch && r$epochsRun == bP$bestEpoch && is.finite(r$testLossMean), sprintf("it stops at PreVal's best epoch (%d), not the status quo's (%d)", r$bestEpoch, bT$bestEpoch))
check(isTRUE(all.equal(r$valLossBest, bP$valLossBest)), "reported loss = what PreVal's own validation showed")
a <- readRDS(file.path(mainDir, paste0(bP$modelName, "_perCase.rds"))); b <- readRDS(file.path(refDir, paste0(r$modelName, "_perCase.rds")))
check(setequal(a[set == "test"]$row, b[set == "test"]$row), "scored on exactly the same test cases as the base model")
check(!any(mT$row[mT$set %in% c("train", "val")] %in% mT$row[mT$set == "test"]), "no test case is in the pool (no leakage)")
check(max(idx$year[mT$row[mT$set %in% c("train", "val")]]) <= pr$historyEnd, "the pool holds no case later than the end of the window")
r2 <- runModel(st, readManifest(md, sid, "FutureTainted"), plR$plan[splitId == sid & typeValidation == "FutureTaintedRefit"], feats, ad, hyper, refDir, reRun = TRUE, mainModelDir = mainDir)
check(r2$nTrain == r$nTrain, "the status-quo refit and the pool refit train on exactly the same number of cases (same rows)")
check(inherits(try(runModel(st, mG, pr, feats, ad, hyper, refDir, reRun = TRUE, mainModelDir = mainDir), silent = TRUE), "try-error"), "a pool refit refuses to run without the status quo's manifest")
cat(sprintf("\n%d failure(s)\n", nFail)); if (nFail) quit(status = 1)
