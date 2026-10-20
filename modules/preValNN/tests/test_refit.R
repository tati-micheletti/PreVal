# Test of the refit arm: same splits as the main design, refit = base training + validation cases, stops at the base model's best epoch
suppressMessages({library(data.table); library(torch)})
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
for (f in c("seeds.R", "caseIndex.R", "regimeSplits.R", "verifyRegimes.R", "adapters.R", "fitNet.R", "runModel.R", "runRefit.R", "generatePlan.R", "analyzeRegimes.R"))
  source(file.path(here, "..", "R", f))
nFail <- 0L
check <- function(cond, msg) { ok <- isTRUE(cond); cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) nFail <<- nFail + 1L }

set.seed(5)
nUnit <- 250; years <- 2005:2016
units <- data.table(SITE_ID = sprintf("r%03d", 1:nUnit), x = runif(nUnit, 4e6, 4.6e6), y = runif(nUnit, 2.7e6, 3.3e6), a = rnorm(nUnit), b = rnorm(nUnit))
tab <- rbindlist(lapply(years, function(yy) { u <- units[runif(nUnit) < 0.6]; u[, year := yy]; u }))
tab[, `:=`(n1 = rnorm(.N), count = 0L)]; tab[, count := rnbinom(.N, mu = exp(0.4 + 0.6 * a - 0.4 * b), size = 2)]
feats <- c("a", "b", "n1"); idx <- buildCaseIndex(tab)
common <- list(idx, startYear = 2005, endYear = 2016, minTrainYears = 4, horizons = 1, covariateLevels = 2, blockKm = 100, bufferKm = 10, minTrain = 100, minVal = 50, minTest = 50)
pl <- do.call(generatePlan, common)
plR <- do.call(generatePlan, c(common, list(planRegimes = c("FutureUnseenRefit", "FutureTaintedRefit"))))
check(setequal(unique(plR$plan$typeValidation), c("FutureUnseenRefit", "FutureTaintedRefit")) && nrow(plR$plan) == 2 * uniqueN(pl$plan$splitId), "refit plan has only the two refit regimes, one row each per split")
check(identical(pl$bundles, plR$bundles), "the refit plan is built on exactly the same splits as the main plan (same seeds, all five regimes built)")
md <- file.path(tempdir(), "mfr"); writeManifests(pl$bundles, md); writeManifests(plR$bundles, md)
check(all(auditManifests(idx, plR$plan, md, plR$spatial)$ok), "audit works for a refit plan (uses every stored regime of the split)")

st <- buildCaseStore(tab, idx, feats, "count"); ad <- getAdapter("count")
hyper <- list(hidden = c(16, 8), embDim = 0L, idIntercept = TRUE, idDecay = 1e-3, lr = 0.01, batchSize = 64L, epochs = 40L, weightDecay = 0)
mainDir <- file.path(tempdir(), "main"); refDir <- file.path(tempdir(), "refit"); dir.create(mainDir, showWarnings = FALSE); dir.create(refDir, showWarnings = FALSE)
sid <- pl$plan$splitId[1]
for (rg in c("FutureUnseen", "FutureTainted")) {
  pr <- pl$plan[splitId == sid & typeValidation == rg]
  assign(paste0("base_", rg), runModel(st, readManifest(md, sid, rg), pr, feats, ad, hyper, mainDir, reRun = TRUE))
}
for (rg in c("FutureUnseen", "FutureTainted")) {
  base <- get(paste0("base_", rg)); pr <- plR$plan[splitId == sid & typeValidation == paste0(rg, "Refit")]
  r <- runModel(st, readManifest(md, sid, rg), pr, feats, ad, hyper, refDir, reRun = TRUE, mainModelDir = mainDir)
  check(r$nTrain == base$nTrain + base$nVal && r$nTest == base$nTest, sprintf("%s: the refit trains on training + validation cases (%d = %d + %d)", rg, r$nTrain, base$nTrain, base$nVal))
  check(r$bestEpoch == base$bestEpoch && r$epochsRun == base$bestEpoch && is.finite(r$testLossMean), sprintf("%s: stops at the base model's best epoch (%d) and gives a finite test loss", rg, r$bestEpoch))
  check(isTRUE(all.equal(r$valLossBest, base$valLossBest)), sprintf("%s: reported loss = what the base validation showed", rg))
  a <- readRDS(file.path(mainDir, paste0(base$modelName, "_perCase.rds"))); b <- readRDS(file.path(refDir, paste0(r$modelName, "_perCase.rds")))
  check(setequal(a[set == "test"]$row, b[set == "test"]$row), sprintf("%s: scored on exactly the same test cases as the base model", rg))
}
pr <- plR$plan[splitId == sid & typeValidation == "FutureUnseenRefit"]
check(inherits(try(runModel(st, readManifest(md, sid, "FutureUnseen"), pr, feats, ad, hyper, refDir, reRun = TRUE, mainModelDir = file.path(tempdir(), "nope")), silent = TRUE), "try-error"), "refit refuses to run without the base model's result")
cat(sprintf("\n%d failure(s)\n", nFail)); if (nFail) quit(status = 1)
