# Tests for the covariate-novelty analysis (DI): the measure itself, and the whole chain on a small real experiment (plan, manifests, rankings, trained models, per-case files)
suppressMessages({library(data.table); library(torch)})
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
for (f in c("seeds.R", "caseIndex.R", "regimeSplits.R", "verifyRegimes.R", "spatialTestSplits.R", "generatePlan.R", "adapters.R", "fitNet.R", "runModel.R", "rankCovariates.R", "analyzeRegimes.R", "novelty.R"))
  source(file.path(here, "..", "R", f))
nFail <- 0L
check <- function(cond, msg) { ok <- isTRUE(cond); cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) nFail <<- nFail + 1L }

# ---- 1. the measure ----
set.seed(3); Zr <- matrix(rnorm(2000 * 5), ncol = 5)
same <- noveltyDI(Zr, Zr[1:20, ] + 1e-6, nSample = 500L, seed = 1L)
far <- noveltyDI(Zr, matrix(8, 20, 5), nSample = 500L, seed = 1L)
new <- noveltyDI(Zr, matrix(rnorm(300 * 5), ncol = 5), nSample = 500L, seed = 1L)
check(all(same$di < 0.01), "a test case that duplicates a reference case has DI near 0")
check(all(far$di > 3) && median(new$di) > 0.3 && median(new$di) < 1.5, sprintf("a far-away case has a large DI (%.1f), new cases from the same distribution about 1 (median %.2f)", min(far$di), median(new$di)))
check(identical(noveltyDI(Zr, Zr[1:5, ], 500L, 7L)$di, noveltyDI(Zr, Zr[1:5, ], 500L, 7L)$di), "DI is deterministic for a given seed")

# ---- 2. the chain on a small experiment ----
set.seed(11); nUnit <- 300; years <- 2005:2016
units <- data.table(SITE_ID = sprintf("r%03d", 1:nUnit), x = runif(nUnit, 4e6, 4.5e6), y = runif(nUnit, 2.7e6, 3.2e6), a = rnorm(nUnit), b = rnorm(nUnit))
tab <- rbindlist(lapply(years, function(yy) { u <- units[runif(nUnit) < 0.7]; u[, year := yy]; u }))
tab[, `:=`(n1 = rnorm(.N), n2 = rnorm(.N), n3 = rnorm(.N))]; tab[, count := rnbinom(.N, mu = exp(0.4 + 0.6 * a - 0.4 * b), size = 2)]
feats <- c("a", "b", "n1", "n2", "n3"); tab <- tab[, c("SITE_ID", "year", "x", "y", feats, "count"), with = FALSE]; idx <- buildCaseIndex(tab)
pl <- generatePlan(idx, startYear = 2005, endYear = 2016, minTrainYears = 4, horizons = 1, covariateLevels = c(2, 4), blockKm = 100, bufferKm = 10, minTrain = 100, minVal = 50, minTest = 50,
                   minTrainFraction = 0.5, minValCombined = 10)
sel <- unique(pl$plan$splitId)[1:2]; plan <- pl$plan[splitId %in% sel]
runDir <- file.path(tempdir(), "novrun"); dir.create(file.path(runDir, "testedModels"), recursive = TRUE, showWarnings = FALSE); dir.create(file.path(runDir, "rankings"), showWarnings = FALSE)
writeManifests(pl$bundles[sel], file.path(runDir, "splits")); data.table::fwrite(plan, file.path(runDir, "experimentPlan.csv")); saveRDS(idx, file.path(runDir, "caseIndex.rds"))
store <- buildCaseStore(tab, idx, feats, "count"); ad <- getAdapter("count")
hyper <- list(hidden = c(8, 4), embDim = 0L, idIntercept = TRUE, idDecay = 1e-3, lr = 0.01, batchSize = 64L, epochs = 6L, weightDecay = 0)
rkList <- list()
for (i in seq_len(nrow(unique(plan[, .(windowStart, historyEnd, valYears)])))) { w <- unique(plan[, .(windowStart, historyEnd, valYears)])[i]
  r <- rankCovariates(store, feats, ad, hyper, seed = 5L + i, mode = "random", rankYears = w$windowStart:(w$historyEnd - w$valYears), nRep = 2L)
  data.table::fwrite(r, file.path(runDir, "rankings", sprintf("s%d_e%d.csv", w$windowStart, w$historyEnd))); rkList[[sprintf("s%d_e%d", w$windowStart, w$historyEnd)]] <- r }
for (j in seq_len(nrow(plan))) { pr <- plan[j]; rk <- rkList[[sprintf("s%d_e%d", pr$windowStart, pr$historyEnd)]]
  if (!pr$typeValidation %in% names(NOVELTY_LABELS)) next
  runModel(store, readManifest(file.path(runDir, "splits"), pr$splitId, pr$typeValidation), pr, featuresForLevel(rk, pr$numberOfCovariates), ad, hyper, file.path(runDir, "testedModels"), reRun = TRUE) }
cs <- noveltyCases(runDir, tab, levels = c(2, 4), nonFeatures = c("SITE_ID", "year", "x", "y", "count"), nSample = 200L, verbose = FALSE)
check(nrow(cs) > 0 && all(is.finite(cs$DI)) && all(cs$DI >= 0) && all(is.finite(cs$lossCal)), sprintf("novelty and calibrated loss for %d test-case rows", nrow(cs)))
check(setequal(unique(cs$regime), c("FutureOnly", "StatusQuo", "Spatial", "PreValSpatial")) && uniqueN(cs$splitId) == 2 && setequal(unique(cs$level), c(2, 4)), "all four regimes, both splits and both levels are in the table")
one <- cs[splitId == sel[1] & level == 4]; nn <- one[, .(DIs = uniqueN(round(DI, 10))), by = .(regime, row)]
check(all(nn$DIs == 1) && one[regime == "FutureOnly", .N] == one[regime == "StatusQuo", .N] && setequal(one[regime == "FutureOnly"]$row, one[regime == "Spatial"]$row), "a test case has ONE novelty value and the same test cases appear in every regime")
st <- noveltyStats(cs)
check(all(c("DI_summary", "tercile_means", "tercile_contrasts", "slope_on_logDI") %in% names(st)) && all(c("estimate", "lo", "hi", "nYears", "nPositive", "nNegative") %in% names(st$slope_on_logDI)), "statistics tables are produced with the year-level test columns")
check(setequal(unique(st$tercile_contrasts$contrast), sapply(Filter(function(p) all(p %in% c("FutureOnly", "StatusQuo", "Spatial", "PreValSpatial")), NOVELTY_PAIRS), function(p) paste(p[1], "minus", p[2]))) && all(st$tercile_means$tercile %in% c("low", "mid", "high")), "all five regime contrasts and the three terciles are reported")
# the models' own result files agree with the per-case table (calibrated test loss = mean of the per-case calibrated losses)
mm <- data.table::rbindlist(lapply(list.files(file.path(runDir, "testedModels"), "_finalDT[.]csv$", full.names = TRUE), data.table::fread, fill = TRUE))
chk <- merge(mm[numberOfCovariates == 4 & splitId == sel[1], .(regime = NOVELTY_LABELS[typeValidation], testLossCal)], cs[splitId == sel[1] & level == 4, .(m = mean(lossCal)), by = regime], by = "regime")
check(nrow(chk) == 4 && all(abs(chk$testLossCal - chk$m) < 1e-9), "the mean per-case calibrated loss equals the model's saved calibrated test loss")
cat(sprintf("\n%d failure(s)\n", nFail)); if (nFail) quit(status = 1)
