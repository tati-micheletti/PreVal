# ACCEPTANCE TEST of the caribou engine (local, real data): the OLD caribou module and the NEW engine (strata layout) on the SAME splits, seeds and covariates.
#   1. Splits: buildRegimeSplits(regimes FutureUnseen / FutureTainted / Internal, matchUnits) must give row-for-row the splits of the old buildGroupSplits (same seed).
#   2. Models: for a few splits of the first pass (results_firstpass/experimentPlan.csv: same split seed, model seed, covariates = top k of the old global ranking) the old
#      trainingExperimentNN and the engine's runModel are trained on the same machine; the per-model numbers are compared (bitwise = identical).
#   3. The numbers saved by the old first pass on EVE (results_firstpass/analysis/allModels.csv) are shown next to them (a different machine: float noise is expected).
# Run from the repo root after the prep stage (outputs/caribou_engine/caribou/final_caribou.rds):  Rscript caribou/acceptance_old_vs_engine.R
# Settings used by the old runs (designParams.rds): Adam lr 0.001 halved on plateau, batch 128, cap 50 epochs (first pass), early stop after 10 epochs without a new best.
suppressMessages({library(data.table); library(torch)})
args <- commandArgs(TRUE); outDir <- if (length(args)) args[1] else file.path(tempdir(), "caribou_acceptance"); dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
for (f in list.files("modules/preValNN/R", "[.]R$", full.names = TRUE)) source(f)
old <- new.env(); for (f in c("stratumNet.R", "buildSplits.R", "strataStore.R", "verifySplits.R", "trainingExperimentNN.R", "theExperiment.R")) sys.source(file.path("modules/caribouNN/R", f), envir = old)
torch_set_num_threads(1)
fin <- readRDS("outputs/caribou_engine/caribou/final_caribou.rds"); rk <- fread("results_firstpass/featureTable.csv"); feats <- rk$Feature
oldPlan <- fread("results_firstpass/experimentPlan.csv"); oldRes <- fread("results_firstpass/analysis/allModels.csv")
fin[, idIndex := as.numeric(as.factor(id))]
idx <- buildStrataCaseIndex(fin); store <- buildCaseStore(fin, idx, feats, "case_", layout = "strata")
oldIdx <- old$buildStrataIndex(fin); oldStore <- old$buildStrataStore(fin, feats)
cat(sprintf("strata %d | animals %d | features %d | same animal numbering: %s | same case order: %s\n", nrow(idx), store$nUnits, length(feats),
            identical(as.integer(store$unitIndex), as.integer(oldIdx$idIndex)), identical(idx$indiv_step_id, oldIdx$indiv_step_id)))
hyper <- list(embDim = 8L, lr = 0.001, batchSize = 128L, epochs = 50L, weightDecay = 0); ad <- adapterStratum()
splits <- c("s2013_e2014_T2015_temporal", "s2014_e2016_T2018_temporal"); ks <- c(2, 10); rgs <- c("FutureUnseen", "FutureTainted")
rows <- list()
for (sid in splits) {
  pr0 <- oldPlan[splitId == sid & numberOfCovariates == ks[1] & typeValidation == "FutureUnseen"]; sd <- pr0$splitSeed; s <- pr0$windowStart; e <- pr0$historyEnd; T <- pr0$testYear
  bo <- old$buildGroupSplits(oldIdx, s, e, T, sd, minTrain = 500L, minVal = 200L, minTest = 200L, matchAnimals = TRUE)
  bn <- buildRegimeSplits(idx, s, e, T, sd, minTrain = 500L, minVal = 200L, minTest = 200L, regimes = c("FutureUnseen", "FutureTainted", "Internal"), matchUnits = TRUE)
  same <- vapply(c("FutureUnseen", "FutureTainted", "Internal"), function(r) identical(bo$splits[[r]][order(set, row)][, .(row, set)], bn$splits[[r]][order(set, row)][, .(row, set)]), logical(1))
  cat(sprintf("%s: split sizes engine %s | old plan %d/%d/%d | splits identical row for row (PreVal-old, status quo, Internal): %s\n", sid, paste(bn$sizes, collapse = "/"),
              pr0$nTrainS, pr0$nValS, pr0$nTestS, paste(same, collapse = "/")))
  for (k in ks) for (rg in rgs) {
    pr <- oldPlan[splitId == sid & numberOfCovariates == k & typeValidation == rg]; ff <- feats[seq_len(k)]
    dO <- file.path(outDir, "old"); dN <- file.path(outDir, "engine"); dir.create(dO, showWarnings = FALSE); dir.create(dN, showWarnings = FALSE)
    t0 <- Sys.time(); ro <- old$trainingExperimentNN(oldStore, bo$splits[[rg]], pr, ff, batchSize = 128L, learningRate = 0.001, epochs = 50L, outputDir = dO, reRun = TRUE, verbose = FALSE, earlyStopPatience = 10)
    t1 <- Sys.time(); prN <- copy(pr); prN[, valYears := 1L]
    rn <- runModel(store, bn$splits[[rg]], prN, ff, ad, hyper, dN, reRun = TRUE, earlyStopPatience = 10); t2 <- Sys.time()
    sv <- oldRes[modelName == pr$modelName]
    rows[[length(rows) + 1L]] <- data.table(split = sid, k = k, regime = rg, bestEpoch_old = ro$bestEpoch, bestEpoch_engine = rn$bestEpoch, bestEpoch_EVE = sv$bestEpoch,
      valLoss_old = ro$valLossBest, valLoss_engine = rn$valLossBest, valLoss_EVE = sv$valLossBest, testLoss_old = ro$testLossMean, testLoss_engine = rn$testLossMean, testLoss_EVE = sv$testLossMean,
      trainLoss_old = ro$trainLossBest, trainLoss_engine = rn$trainLossBest, testLossCal_engine = rn$testLossCal, secOld = round(as.numeric(t1 - t0, units = "secs")), secEngine = round(as.numeric(t2 - t1, units = "secs")))
    cat(sprintf("  %s k=%d %-13s best epoch old/engine/EVE %d/%d/%d | test loss old %.10f engine %.10f EVE %.6f\n", sid, k, rg, ro$bestEpoch, rn$bestEpoch, sv$bestEpoch, ro$testLossMean, rn$testLossMean, sv$testLossMean))
  }
}
res <- rbindlist(rows); fwrite(res, file.path(outDir, "acceptance_results.csv"))
cat("\nmax |engine - old| (same machine):  valLoss", signif(max(abs(res$valLoss_engine - res$valLoss_old)), 3), " testLoss", signif(max(abs(res$testLoss_engine - res$testLoss_old)), 3), " trainLoss", signif(max(abs(res$trainLoss_engine - res$trainLoss_old)), 3),
    " | best epochs identical:", all(res$bestEpoch_engine == res$bestEpoch_old), "\n")
cat("max |engine - EVE first pass|:     valLoss", signif(max(abs(res$valLoss_engine - res$valLoss_EVE)), 3), " testLoss", signif(max(abs(res$testLoss_engine - res$testLoss_EVE)), 3), " | best epochs identical to EVE:", all(res$bestEpoch_engine == res$bestEpoch_EVE), "\n")
