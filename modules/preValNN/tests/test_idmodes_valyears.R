# Tests: (1) two-year validation variant (valYears = 2) in splits, gates, plan and manifests; (2) route-information modes of the network
suppressMessages({library(data.table); library(torch)})
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
for (f in c("seeds.R", "caseIndex.R", "regimeSplits.R", "verifyRegimes.R", "adapters.R", "fitNet.R", "runModel.R", "generatePlan.R"))
  source(file.path(here, "..", "R", f))
nFail <- 0L
check <- function(cond, msg) { ok <- isTRUE(cond); cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) nFail <<- nFail + 1L }

set.seed(21)
nUnit <- 300; years <- 2005:2018
units <- data.table(SITE_ID = sprintf("r%03d", 1:nUnit), x = runif(nUnit, 4e6, 4.6e6), y = runif(nUnit, 2.7e6, 3.3e6), a = rnorm(nUnit), u = rnorm(nUnit, sd = 0.8))
tab <- rbindlist(lapply(years, function(yy) { z <- units[runif(nUnit) < 0.55]; z[, year := yy]; z }))
tab[, n1 := rnorm(.N)]
tab[, count := rnbinom(.N, mu = exp(0.5 + 0.4 * a + u), size = 3)]     # u = a route effect the covariates do not know
idx <- buildCaseIndex(tab); sp <- assignBlocks(idx, 100, 10, 4L, 1L)

# ---- two-year validation ----
b <- buildRegimeSplits(minTest = 50L, minVal = 50L, minTrain = 100L, idx, s = 2007, e = 2012, testYear = 2013, seed = 5L, spatial = sp, minTrainFraction = 0.5, valYears = 2L)
check(isTRUE(b$ok), "valYears = 2 split builds")
u <- b$splits$FutureUnseen; yr <- idx$year
check(all(yr[u[set == "val"]$row] %in% c(2011, 2012)) && all(yr[u[set == "train"]$row] <= 2010), "PreVal: validation = the last two years, training before them")
check(all(verifyRegimeSplits(idx, b, 2007, 2012, 2013, "v2", sp, valYears = 2L)$ok), "gates pass with two validation years")
check(inherits(try(verifyRegimeSplits(idx, b, 2007, 2012, 2013, "v2bad", sp, valYears = 1L), silent = TRUE), "try-error"), "gates stop if the split is checked as one-year validation (rule is enforced)")
pl <- generatePlan(idx, 2005, 2018, minTrainYears = 2, valYears = 2, horizons = 1:2, covariateLevels = 2, blockKm = 100, bufferKm = 10, minTrain = 100, minVal = 50, minTest = 50, minTrainFraction = 0.5)
check(all(pl$plan$historyEnd - pl$plan$windowStart + 1L >= 4L) && all(pl$plan$valYears == 2L), "plan: every window has at least 2 training + 2 validation years")
md <- file.path(tempdir(), "mv2"); writeManifests(pl$bundles, md)
check(all(auditManifests(idx, pl$plan, md, pl$spatial)$ok), "audit of two-year-validation manifests passes")
pl1 <- generatePlan(idx, 2005, 2018, minTrainYears = 2, valYears = 1, horizons = 1, covariateLevels = 2, blockKm = 100, bufferKm = 10, minTrain = 100, minVal = 50, minTest = 50, minTrainFraction = 0.5)
check(min(pl1$plan$historyEnd - pl1$plan$windowStart + 1L) == 3L, "valYears = 1 with 2 training years gives windows of at least 3 years")

plF <- generatePlan(idx, 2005, 2018, minTrainYears = 2, valYears = 1, horizons = 1:3, covariateLevels = 2, firstForecastYear = 2010, blockKm = 100, bufferKm = 10, minTrain = 100, minVal = 50, minTest = 50, minTrainFraction = 0.5)
check(min(plF$plan$testYear) >= 2010, "firstForecastYear: no split forecasts a year in the pilot's pre-test history")

# ---- route-information modes ----
hyper <- list(hidden = c(16, 8), embDim = 0L, lr = 0.01, batchSize = 128L, epochs = 60L, weightDecay = 0)
runMode <- function(mode, regime, seed = 3L) {
  h <- hyper
  if (mode == "intercept") { h$idIntercept <- TRUE; h$idDecay <- 1e-3 }
  if (mode == "concat") h$embDim <- 8L
  b <- buildRegimeSplits(minTest = 50L, minVal = 50L, minTrain = 100L, idx, s = 2007, e = 2012, testYear = 2013, seed = 5L, spatial = sp, minTrainFraction = 0.5)
  m <- b$splits[[regime]]
  st <- buildCaseStore(tab, idx, c("a", "n1"), "count")
  od <- file.path(tempdir(), paste0("od_", mode, "_", regime)); dir.create(od, showWarnings = FALSE)
  pr <- data.table(modelName = paste0("m_", mode, "_", regime), splitId = "x", typeValidation = regime, numberOfCovariates = 2, replicate = 1L, groupId = "g",
                   windowStart = 2007L, historyEnd = 2012L, testYear = 2013L, horizon = 1L, nTrainS = b$sizes[["nTrain"]], nValS = b$sizes[["nVal"]], nTestS = b$sizes[["nTest"]],
                   splitSeed = 1L, modelSeed = seed, valYears = 1L)
  runModel(st, m, pr, c("a", "n1"), getAdapter("count"), h, od, reRun = TRUE)
}
r0 <- runMode("none", "FutureTainted"); r1 <- runMode("intercept", "FutureTainted"); r2 <- runMode("concat", "FutureTainted")
check(r1$valLossBest < r0$valLossBest - 0.05, sprintf("status quo: a route intercept lowers the validation loss when routes carry their own level (%.3f < %.3f)", r1$valLossBest, r0$valLossBest))
check(r2$valLossBest < r0$valLossBest - 0.05, sprintf("status quo: the concatenated embedding also does (%.3f < %.3f)", r2$valLossBest, r0$valLossBest))
g0 <- runMode("none", "FutureTaintedGrouped"); g1 <- runMode("intercept", "FutureTaintedGrouped")
check(g1$valLossBest < g0$valLossBest + 0.05, sprintf("route-grouped validation (unseen routes): the intercept does not hurt (%.3f vs %.3f)", g1$valLossBest, g0$valLossBest))
check(r0$valLossBest - r1$valLossBest > g0$valLossBest - g1$valLossBest + 0.03, "the gain from route information is larger where routes are shared between train and validation (the leak channel)")
# the intercept is shrunk: with a very strong penalty it collapses to the no-ID model
h <- hyper; h$idIntercept <- TRUE; h$idDecay <- 5
st <- buildCaseStore(tab, idx, c("a", "n1"), "count")
b <- buildRegimeSplits(minTest = 50L, minVal = 50L, minTrain = 100L, idx, s = 2007, e = 2012, testYear = 2013, seed = 5L, spatial = sp, minTrainFraction = 0.5); m <- b$splits$FutureTainted
tr <- storeSlice(st, m$row[m$set == "train"], c("a", "n1")); va <- storeSlice(st, m$row[m$set == "val"], c("a", "n1"))
fit <- fitNet(getAdapter("count"), h, tr, va, st$nUnits, seed = 3L)
check(max(abs(as.matrix(fit$net$idInt$weight$detach()))) < 0.25, sprintf("a strong penalty shrinks the route intercepts toward zero (max |intercept| %.3f)", max(abs(as.matrix(fit$net$idInt$weight$detach())))))
cat(sprintf("\n%d failure(s)\n", nFail)); if (nFail) quit(status = 1)
