# Tests for the spatial-test arm: forecasting into NEW places (test blocks no regime sees) with the PreVal + spatial-block regime
suppressMessages({library(data.table); library(torch)})
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
for (f in c("seeds.R", "caseIndex.R", "regimeSplits.R", "verifyRegimes.R", "spatialTestSplits.R", "generatePlan.R", "adapters.R", "fitNet.R", "runModel.R")) source(file.path(here, "..", "R", f))
nFail <- 0L
check <- function(cond, msg) { ok <- isTRUE(cond); cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) nFail <<- nFail + 1L }
set.seed(5)
nUnit <- 1200; years <- 2005:2016
units <- data.table(SITE_ID = sprintf("r%04d", 1:nUnit), x = runif(nUnit, 4e6, 4.6e6), y = runif(nUnit, 2.7e6, 3.3e6), a = rnorm(nUnit))
tab <- rbindlist(lapply(years, function(yy) { u <- units[runif(nUnit) < 0.6]; u[, year := yy]; u }))
tab[, count := rnbinom(.N, mu = exp(0.5 + 0.5 * a), size = 2)]
idx <- buildCaseIndex(tab)
spatial <- assignBlocks(idx, blockKm = 60, bufferKm = 8, nFolds = 4L, seed = 3L)
b <- buildSpatialTestSplits(idx, 2005, 2012, 2014, seed = 77L, spatial = spatial)
check(isTRUE(b$ok), sprintf("split builds (train %d, val %d, test %d)", b$sizes[["nTrain"]], b$sizes[["nVal"]], b$sizes[["nTest"]]))
v <- verifySpatialTestSplits(idx, b, 2005, 2012, 2014, "s2005_e2012_T2014", spatial)
check(all(v$ok) && nrow(v) > 15, sprintf("all %d gate checks pass", nrow(v)))
sp <- b$splits; rowsOf <- function(m, st) m$row[m$set == st]
tz <- blockZone(idx, unique(spatial$key[rowsOf(sp$FutureUnseen, "test")]), spatial)
check(all(vapply(sp, function(m) !any(tz[c(rowsOf(m, "train"), rowsOf(m, "val"))]), logical(1))), "the test cases are in NEW places: no regime trains or validates inside the test blocks or their buffer")
check(length(unique(vapply(sp, function(m) paste(sum(m$set == "train"), sum(m$set == "val")), character(1)))) == 1, "equal training size and equal validation size in all four regimes")
cmb <- sp$FutureUnseenSpatial
check(all(idx$year[rowsOf(cmb, "val")] == 2012) && all(idx$year[rowsOf(cmb, "train")] <= 2011), "PreVal + spatial: validation = the last year, training = earlier years")
vz <- blockZone(idx, unique(spatial$key[rowsOf(cmb, "val")]), spatial)
check(!any(vz[rowsOf(cmb, "train")]) && length(unique(spatial$key[rowsOf(cmb, "val")])) >= 1, "PreVal + spatial: validation blocks and their buffer are removed from training")
# the gates really stop a leak: put a training case of the status quo into the test zone
bad <- b; m <- data.table::copy(bad$splits$FutureTainted); k <- setdiff(which(tz & idx$year == 2010), m$row)[1]
m[which(m$set == "train")[1], row := k]; bad$splits$FutureTainted <- m
er <- tryCatch({ verifySpatialTestSplits(idx, bad, 2005, 2012, 2014, "bad", spatial); "no error" }, error = function(e) conditionMessage(e))
check(grepl("T3", er) && grepl("FutureTainted", er), "the gate stops a split with a training case inside the test zone (check T3)")
# the plan: arm = spatialTest builds the four regimes, the audit of the manifests passes, and the manifests of every regime are accepted by the per-model check
pl <- generatePlan(idx, startYear = 2005, endYear = 2016, minTrainYears = 4, horizons = 1:2, covariateLevels = c(2), blockKm = 60, bufferKm = 8, minTrain = 100, minVal = 30, minTest = 50, arm = "spatialTest")
check(nrow(pl$plan) > 0 && setequal(unique(pl$plan$typeValidation), SPATIALTEST_REGIMES) && all(pl$checks$ok), sprintf("plan: %d models, four regimes, all %d gate checks pass", nrow(pl$plan), nrow(pl$checks)))
md <- file.path(tempdir(), "mst"); writeManifests(pl$bundles, md); a <- auditManifests(idx, pl$plan, md, pl$spatial)
check(all(a$ok), "audit of the stored manifests passes (spatial-test gates)")
pr <- pl$plan[typeValidation == "FutureUnseenSpatial"][1]; man <- readManifest(md, pr$splitId, "FutureUnseenSpatial")
ok <- tryCatch({ verifyRegimeManifest(idx, man, "FutureUnseenSpatial", pr$windowStart, pr$historyEnd, pr$testYear, c(nTrain = pr$nTrainS, nVal = pr$nValS, nTest = pr$nTestS), pr$valYears); TRUE }, error = function(e) FALSE)
check(ok, "the per-model manifest check accepts the PreVal + spatial regime")
# one model of the new regime trains through the engine
store <- buildCaseStore(tab, idx, c("a"), "count"); ad <- getAdapter("count"); od <- file.path(tempdir(), "mst_models"); dir.create(od, showWarnings = FALSE)
hyper <- list(hidden = c(8, 4), embDim = 0L, idIntercept = TRUE, idDecay = 1e-3, lr = 0.01, batchSize = 64L, epochs = 8L, weightDecay = 0)
r <- runModel(store, man, pr, "a", ad, hyper, od)
check(is.finite(r$testLossMean) && is.finite(r$testLossCal) && r$nVal == pr$nValS, "a PreVal + spatial model trains and gives finite raw and calibrated test losses")
cat(sprintf("\n%d failure(s)\n", nFail)); if (nFail) quit(status = 1)
