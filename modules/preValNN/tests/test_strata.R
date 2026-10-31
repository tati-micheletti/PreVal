# Stratum layout (caribou through the standardised engine): the adapter reproduces the old caribou numbers, the split builder reproduces the old splits, the gates pass,
# animals unseen in training get the mean embedding, and a small chain (design, ranking, training, analysis) runs end to end. Synthetic data only.
suppressMessages({library(data.table); library(torch)})
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
for (f in c("seeds.R", "caseIndex.R", "regimeSplits.R", "verifyRegimes.R", "spatialTestSplits.R", "generatePlan.R", "adapters.R", "strataLayout.R", "fitNet.R", "runModel.R", "rankCovariates.R", "analyzeRegimes.R", "novelty.R"))
  source(file.path(here, "..", "R", f))
old <- new.env(); for (f in c("stratumNet.R", "buildSplits.R")) sys.source(file.path(here, "..", "..", "caribouNN", "R", f), envir = old)      # the OLD caribou code, in its own environment (same function names as the engine)
nFail <- 0L
check <- function(cond, msg) { ok <- isTRUE(cond); cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) nFail <<- nFail + 1L }

# ---- synthetic long stratum table: 11 rows per stratum, the observed step first; feature f1 is informative, f2 weakly, f3-f6 noise
set.seed(7); nAn <- 40; years <- 2013:2019; steps <- 11L; feats <- paste0("f", 1:6)
an <- data.table(id = sprintf("a%02d", sample(nAn)), x = runif(nAn, 0, 4e5), y = runif(nAn, 0, 4e5))
st <- rbindlist(lapply(years, function(yy) an[, .(year = yy, k = seq_len(sample(8:14, 1))), by = .(id, x, y)]))
st[, `:=`(indiv_step_id = sprintf("s%05d", .I), x1_ = x + rnorm(.N, 0, 5e3), y1_ = y + rnorm(.N, 0, 5e3), burst_ = paste0("b", k))]
long <- st[rep(seq_len(.N), each = steps)]; long[, case_ := rep(c(TRUE, rep(FALSE, steps - 1L)), nrow(st))]
for (f in feats) long[, (f) := rnorm(.N)]
long[case_ == TRUE, `:=`(f1 = f1 + 1.5, f2 = f2 + 0.4)]
long[, idIndex := as.numeric(as.factor(id))]
idx <- buildStrataCaseIndex(long)
check(nrow(idx) == nrow(st) && nrow(long) == steps * nrow(st) && !anyDuplicated(idx$indiv_step_id) && all(c("row", "unit", "year", "x", "y") %in% names(idx)), sprintf("strata case index: %d strata, one row each, unit = animal", nrow(idx)))
bad <- copy(long); bad[1, case_ := FALSE]; check(inherits(try(buildStrataCaseIndex(bad), silent = TRUE), "try-error"), "a stratum whose first row is not the observed step is refused")
store <- buildCaseStore(long, idx, feats, "case_", layout = "strata")
check(store$layout == "strata" && identical(dim(store$X$shape), NULL) || length(store$X$shape) == 3L, "store is one 3-D tensor [strata, 11, features]")
sl <- storeSlice(store, c(5L, 1L, 9L), c("f2", "f1")); check(identical(as.integer(sl$x$shape), c(3L, 11L, 2L)) && as.numeric(sl$x[1, 1, 2]) == as.numeric(store$X[5, 1, 1]) , "storeSlice selects strata and features by name")

# ---- the standardisation agrees with the old caribou code
tr <- storeSlice(store, which(idx$year <= 2016), feats)
sNew <- scaleStats(tr$x, feats); sOld <- old$scaleStats(tr$x, feats)
check(identical(sNew, sOld) && isTRUE(all.equal(as.array(applyScale(tr$x, sNew, 10)), as.array(old$applyScale(tr$x, sOld, 10)))), "scaleStats / applyScale of the stratum tensor equal the old caribou functions")

# ---- the network and the loss equal the old stratumNet on the same seed
ad <- adapterStratum()
torch_manual_seed(11); netE <- ad$makeNet(6L, store$nUnits, list(embDim = 8L)); torch_manual_seed(11); netO <- old$makeStratumNet()(nIn = 6L, nAnimals = store$nUnits)
x <- applyScale(tr$x, sNew, 10)[1:50, , ]; id <- tr$id[1:50]
oE <- netE(x, id); oO <- netO(x, id)
check(isTRUE(all.equal(as.array(oE), as.array(oO), tolerance = 0)), "engine network and old network give identical scores for the same seed (identical initialisation)")
lE <- as.numeric(ad$caseLoss(oE, tr$y[1:50], netE)); lO <- as.numeric(nnf_cross_entropy(oO, torch_ones(50, dtype = torch_long()), reduction = "none"))
check(max(abs(lE - lO)) < 1e-6 && abs(mean(lE) - as.numeric(nnf_cross_entropy(oO, torch_ones(50, dtype = torch_long())))) < 1e-6, sprintf("adapter loss equals the old cross-entropy per stratum (max difference %.1e)", max(abs(lE - lO))))
va <- storeSlice(store, which(idx$year == 2017), feats); va$x <- applyScale(va$x, sNew, 10)
sc <- scoreCases(netE, ad, va$x, va$y, va$id, sort(unique(as.integer(as.array(tr$id)))), store$nUnits); so <- old$scoreStrata(netO, va$x, va$id, sort(unique(as.integer(as.array(tr$id)))), store$nUnits)
check(max(abs(sc$loss - so$loss)) < 1e-6 && identical(sc$rankTrue, so$rankTrue) && identical(sc$seenUnit, so$seen), "scoreCases equals the old scoreStrata (loss, rank of the observed step, seen flag)")

# ---- animals unseen in training are scored with the MEAN of the trained animals' embedding rows
seenIds <- sort(unique(as.integer(as.array(tr$id)))); stranger <- setdiff(seq_len(store$nUnits), seenIds)
if (!length(stranger)) { seenIds <- seenIds[-1]; stranger <- setdiff(seq_len(store$nUnits), seenIds) }
vv <- storeSlice(store, 1:20, feats); vv$x <- applyScale(vv$x, sNew, 10); vv$id[1:20] <- stranger[1]
a <- scoreCases(netE, ad, vv$x, vv$y, vv$id, seenIds, store$nUnits)
W <- as.matrix(netE$idEmb$weight$detach()); avg <- colMeans(W[seenIds, , drop = FALSE])
check(all(!a$seenUnit) && max(abs(W[store$nUnits + 1L, ] - avg)) < 1e-6, "an unseen animal is scored with the mean embedding of the trained animals")

# ---- the split builder reproduces the OLD caribou splits (same seed, same rows)
oldIdx <- cbind(idx[, .(row, id = unit, year, x1_ = x, y1_ = y)], idIndex = store$unitIndex, case_ = TRUE, indiv_step_id = idx$indiv_step_id, burstKey = idx$burstKey)
seed <- 4242L; s0 <- 2013; e0 <- 2016; T0 <- 2018
bo <- old$buildGroupSplits(oldIdx, s0, e0, T0, seed, minTrain = 100L, minVal = 50L, minTest = 50L, matchAnimals = TRUE)
bn <- buildRegimeSplits(idx, s0, e0, T0, seed, minTrain = 100L, minVal = 50L, minTest = 50L, regimes = c("FutureUnseen", "FutureTainted", "Internal"), matchUnits = TRUE)
same <- function(a, b) identical(a[order(set, row)][, .(row, set)], b[order(set, row)][, .(row, set)])
check(isTRUE(bn$ok) && all(vapply(c("FutureUnseen", "FutureTainted", "Internal"), function(r) same(bn$splits[[r]], bo$splits[[r]]), logical(1))),
      "engine splits (PreVal-old, status quo, Internal matched on animals) are row-for-row the old caribou splits for the same seed")
check(all(verifyRegimeSplits(idx, bn, s0, e0, T0, "toy")$ok), "the gates pass on the stratum splits")
bg <- buildRegimeSplits(idx, s0, e0, T0, seed, minTrain = 100L, minVal = 50L, minTest = 50L, regimes = c("FutureUnseen", "FutureTainted", "FutureUnseenGrouped", "Internal"), matchUnits = TRUE, minValCombined = 10L)
gm <- bg$splits$FutureUnseenGrouped
check(isTRUE(bg$ok) && length(intersect(idx$unit[gm[set == "train"]$row], idx$unit[gm[set == "val"]$row])) == 0 && all(idx$year[gm[set == "val"]$row] == e0), "PreVal on unseen animals: the validation animals are absent from training in every year; validation = the last year")

# ---- within-stratum shuffle (importance): the values of a stratum are kept, only their step positions change; a single column keeps its dimension
xg <- store$X[1:200, , 1:2]; sh <- .shuffleWithinStrata(xg); sh1 <- .shuffleWithinStrata(store$X[1:50, , 3, drop = FALSE])
check(isTRUE(all.equal(as.array(torch_sort(xg, dim = 2)[[1]]), as.array(torch_sort(sh, dim = 2)[[1]]))) && mean(as.array(sh[, 1, 1]) == as.array(xg[, 1, 1])) < 0.3 && identical(as.integer(sh$shape), c(200L, 11L, 2L)) && identical(as.integer(sh1$shape), c(50L, 11L, 1L)),
      "importance shuffle: the same values within every stratum, in a new order (also for one column)")
# ---- ranking hook: the informative covariate comes first
hy <- list(embDim = 8L, lr = 0.01, batchSize = 64L, epochs = 6L, weightDecay = 0)
rk <- rankCovariates(store, feats, ad, hy, seed = 3L, mode = "random", rankYears = 2013:2017, nRep = 2L)
check(nrow(rk) == 6 && rk$Feature[1] == "f1" && setequal(rk$Feature, feats) && !anyNA(rk$marginalAssoc), sprintf("covariate ranking on the stratum tensor: the informative covariate is first (%s)", paste(rk$Feature, collapse = " ")))

# ---- a small chain: design, training of PreVal / status quo, raw and calibrated loss, analysis
pl <- generatePlan(idx, startYear = 2013, endYear = 2019, minTrainYears = 2, horizons = 1, covariateLevels = c(2, 6), minTrain = 100L, minVal = 50L, minTest = 50L, minTrainFraction = 0.3, minValCombined = 10L,
                   regimes = c("FutureUnseen", "FutureTainted", "FutureUnseenGrouped", "Internal"), routeHeldOut = TRUE, matchUnits = TRUE, splitSuffix = "_temporal")
check(nrow(pl$plan) > 0 && all(grepl("_temporal$", pl$plan$splitId)) && all(pl$checks$ok), sprintf("plan with the old split-id suffix: %d models, all gates pass", nrow(pl$plan)))
dir <- file.path(tempdir(), "strata_run"); dir.create(file.path(dir, "testedModels"), recursive = TRUE); writeManifests(pl$bundles, file.path(dir, "splits")); saveRDS(idx, file.path(dir, "caseIndex.rds"))
sel <- unique(pl$plan$splitId)[1:2]; plan <- pl$plan[splitId %in% sel & typeValidation %in% c("FutureUnseenGrouped", "FutureTainted")]
for (j in seq_len(nrow(plan))) { pr <- plan[j]; runModel(store, readManifest(file.path(dir, "splits"), pr$splitId, pr$typeValidation), pr, rk$Feature[seq_len(pr$numberOfCovariates)], ad, hy, file.path(dir, "testedModels"), reRun = TRUE) }
fd <- rbindlist(lapply(list.files(file.path(dir, "testedModels"), "_finalDT[.]csv$", full.names = TRUE), fread))
pc <- readRDS(list.files(file.path(dir, "testedModels"), "_perCase[.]rds$", full.names = TRUE)[1])
check(nrow(fd) == nrow(plan) && all(is.finite(fd$testLossMean)) && all(is.finite(fd$testLossCal)) && all(c("testAccuracy", "testLossSeen", "testLossUnseen") %in% names(fd)) && !any(paste0("s", 1:11) %in% names(pc)) && all(c("pred", "loss", "lossCal") %in% names(pc)),
      "models train; raw and temperature-calibrated losses are saved; score columns are dropped from the per-case files")
check(all(fd$testLossMean < log(11) + 0.2) && abs(mean(pc[set == "test"]$loss) - fd[modelName == pc$modelName[1]]$testLossMean) < 1e-9, "the raw loss is the mean cross-entropy of the test strata (chance = log 11)")
vp <- pc[set == "val"]; sV <- fd[modelName == pc$modelName[1]]
check(abs(mean(vp$lossCal) - sV$valLossCal) < 1e-9 && sV$valLossCal <= sV$valLossBest + 1e-9, "the temperature is fitted on the regime's own validation strata: calibrated validation loss <= raw")
an2 <- analyzeRegimes(file.path(dir, "testedModels"), file.path(dir, "analysis"), nCovariatesAll = 6, lossType = "fitted")
check(file.exists(file.path(dir, "analysis", "allModels.csv")) && fread(file.path(dir, "analysis", "allModels.csv"))[1]$lossType == "fitted", "the analysis runs on the raw (fitted) loss as primary")
# ---- novelty (DI) on strata: reference = PreVal's (unseen animals) training strata, case vector = mean over the 11 steps of each stratum
dir.create(file.path(dir, "rankings"), showWarnings = FALSE)
for (w in unique(plan[, paste(windowStart, historyEnd)])) { ww <- strsplit(w, " ")[[1]]; fwrite(copy(rk)[, rankYears := "x"], file.path(dir, "rankings", sprintf("s%s_e%s.csv", ww[1], ww[2]))) }
fwrite(plan, file.path(dir, "experimentPlan.csv"))
Xc <- as.matrix(as.array(store$X$mean(dim = 2L))); colnames(Xc) <- feats
cs <- suppressWarnings(noveltyCases(dir, NULL, levels = c(2, 6), nSample = 200L, verbose = FALSE, refRegime = "FutureUnseenGrouped", Xcases = Xc))
check(nrow(cs) > 0 && all(is.finite(cs$DI)) && all(cs$DI >= 0) && all(is.finite(cs$lossCal)) && setequal(unique(cs$regime), c("PreVal", "StatusQuo")), sprintf("novelty on strata: %d test-case rows, DI finite, reference = PreVal on unseen animals", nrow(cs)))
cat(sprintf("\n%d failure(s)\n", nFail)); if (nFail) quit(status = 1)
