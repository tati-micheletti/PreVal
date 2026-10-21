#' Fit and evaluate ONE model of the experiment (one regime, one covariate level, one replicate of one split).
#'
#' Train/validation/test cases come from a verified manifest; standardisation uses TRAINING statistics only; best-epoch weights are
#' saved, reloaded into a fresh network and re-scored (error if different); per-case test and validation losses are saved with
#' unit and year; seeds, RNG kind and software versions are written next to the model. No silent fallbacks.
#' @param store buildCaseStore()
#' @param manifest data.table(row, set) of this model's regime
#' @param planRow one row of the plan (see generatePlan)
#' @param features covariates of this complexity level (ordered by importance)
#' @param adapter getAdapter(...)
#' @param hyper list(hidden, embDim, dropout, lr, batchSize, epochs, weightDecay), fixed per species
runModel <- function(store, manifest, planRow, features, adapter, hyper, outputDir, reRun = FALSE, device = "cpu", zClip = 10,
                     earlyStopPatience = 15L, verbose = FALSE, mainModelDir = NULL) {
  if (grepl("Refit$", planRow$typeValidation)) {      # refit regimes: see runRefit.R
    if (is.null(mainModelDir)) stop("refit regimes need mainModelDir (the folder with the base models)")
    return(runModelRefit(store, manifest, planRow, features, adapter, hyper, outputDir, mainModelDir, reRun = reRun, device = device, zClip = zClip))
  }
  modelName <- planRow$modelName
  finalPath <- file.path(outputDir, paste0(modelName, "_finalDT.csv"))
  if (!reRun && file.exists(finalPath)) { message(modelName, ": results exist and reRun = FALSE; loading."); return(data.table::fread(finalPath)) }
  t0 <- Sys.time(); idx <- store$index
  verifyRegimeManifest(idx, manifest, planRow$typeValidation, planRow$windowStart, planRow$historyEnd, planRow$testYear,
                       c(nTrain = planRow$nTrainS, nVal = planRow$nValS, nTest = planRow$nTestS), valYears = planRow$valYears %||% 1L)
  trRows <- manifest$row[manifest$set == "train"]; vaRows <- manifest$row[manifest$set == "val"]; teRows <- manifest$row[manifest$set == "test"]
  tr <- storeSlice(store, trRows, features, device); va <- storeSlice(store, vaRows, features, device); te <- storeSlice(store, teRows, features, device)
  st <- scaleStats(tr$x, features)
  tr$x <- applyScale(tr$x, st, zClip); va$x <- applyScale(va$x, st, zClip); te$x <- applyScale(te$x, st, zClip)
  saveRDS(st, file.path(outputDir, paste0(modelName, "_scl.rds")))

  fit <- fitNet(adapter, hyper, tr, va, store$nUnits, seed = planRow$modelSeed, device = device, earlyStopPatience = earlyStopPatience,
                verbose = verbose, diag = te)        # test loss per epoch is logged for diagnosis, never used for stopping/selection
  weightsPath <- file.path(outputDir, paste0(modelName, "_BW.pt"))
  checkWeightsRoundTrip(adapter, hyper, fit$bestState, weightsPath, length(features), store$nUnits, va, fit$trainUnitsSeen, fit$bestValLoss, device)

  sc <- function(set, d, rows) {
    r <- scoreCases(fit$net, adapter, d$x, d$y, d$id, fit$trainUnitsSeen, store$nUnits)
    ix <- idx[match(rows, idx$row)]
    cbind(data.table::data.table(modelName = modelName, splitId = planRow$splitId, typeValidation = planRow$typeValidation,
                                 numberOfCovariates = planRow$numberOfCovariates, set = set, row = rows, unit = ix$unit, year = ix$year), r)
  }
  teT <- sc("test", te, teRows); vaT <- sc("val", va, vaRows)
  dispFit <- if ("theta" %in% names(vaT)) vaT$theta[1] else NA_real_             # dispersion learned on the training rows
  dispCal <- if (is.function(adapter$calibrate)) adapter$calibrate(vaT$pred, vaT$obs) else NA_real_   # re-estimated on this regime's validation cases (adapters.R)
  if (is.finite(dispCal)) { teT[, lossCal := adapter$nllWith(pred, obs, dispCal)]; vaT[, lossCal := adapter$nllWith(pred, obs, dispCal)] }
  trLoss <- mean(scoreCases(fit$net, adapter, tr$x, tr$y, tr$id, fit$trainUnitsSeen, store$nUnits)$loss)
  saveRDS(rbind(teT, vaT, fill = TRUE), file.path(outputDir, paste0(modelName, "_perCase.rds")))
  data.table::fwrite(fit$history, file.path(outputDir, paste0(modelName, "_history.csv")))
  saveRDS(list(modelName = modelName, splitId = planRow$splitId, splitSeed = planRow$splitSeed, modelSeed = planRow$modelSeed,
               torchSeedUse = "torch_manual_seed(modelSeed): weight init + batch order; split sampling is separate (splitSeed)",
               RNGkind = paste(RNGkind(), collapse = "/"), R = R.version.string, torch = as.character(utils::packageVersion("torch")),
               device = device, host = Sys.info()[["nodename"]], slurmJob = Sys.getenv("SLURM_JOB_ID", NA_character_),
               slurmTask = Sys.getenv("SLURM_ARRAY_TASK_ID", NA_character_), features = features, adapter = adapter$name, hyper = hyper,
               time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")), file.path(outputDir, paste0(modelName, "_provenance.rds")))
  nU <- function(rows) data.table::uniqueN(idx$unit[match(rows, idx$row)])
  finalDT <- data.table::data.table(
    groupId = planRow$groupId, splitId = planRow$splitId, typeValidation = planRow$typeValidation, modelName = modelName,
    numberOfCovariates = planRow$numberOfCovariates, replicate = planRow$replicate,
    windowStart = planRow$windowStart, historyEnd = planRow$historyEnd, testYear = planRow$testYear, horizon = planRow$horizon,
    valYears = planRow$valYears %||% 1L, trainShare = planRow$trainShare %||% NA_real_,
    nTrain = length(trRows), nVal = length(vaRows), nTest = length(teRows), nUnitsTrain = nU(trRows), nUnitsTest = nU(teRows),
    splitSeed = planRow$splitSeed, modelSeed = planRow$modelSeed,
    trainLossBest = trLoss, valLossBest = fit$bestValLoss, bestEpoch = fit$bestEpoch, epochsRun = nrow(fit$history), epochCap = hyper$epochs %||% 100L,
    testLossMean = mean(teT$loss), testLossSD = stats::sd(teT$loss), testLossSE = stats::sd(teT$loss) / sqrt(nrow(teT)),
    testMAE = mean(teT$absErr), testRMSE = sqrt(mean(teT$sqErr)), seenShare = mean(teT$seenUnit),
    dispFit = dispFit, dispCal = dispCal,
    valLossCal = if (is.finite(dispCal)) mean(vaT$lossCal) else NA_real_, testLossCal = if (is.finite(dispCal)) mean(teT$lossCal) else NA_real_,
    secondsTotal = as.numeric(difftime(Sys.time(), t0, units = "secs")), modelWeight = weightsPath)
  data.table::fwrite(finalDT, finalPath)
  rm(tr, va, te, fit); gc(verbose = FALSE)
  finalDT
}
