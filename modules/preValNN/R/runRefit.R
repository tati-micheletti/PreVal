#' REFIT arm (post-hoc, added 2026-10-20 after the first look at the greenfinch results; see ANALYSIS_PLAN_BIRDS.md amendment 12).
#'
#' In the main design a model is trained on the training cases only; the validation cases are used only to decide WHEN to stop (best epoch).
#' In practice people refit after that choice: train again on training + validation cases and stop at the epoch the validation chose.
#' No information leaks: the validation cases come from years up to e, the forecast year T is later than e, and the test cases are never touched.
#' The refit regimes are "FutureUnseenRefit" (PreVal) and "FutureTaintedRefit" (status quo). They use the manifest of their base regime,
#' the best epoch and the learning-rate trajectory of the SAME base model from the main run (`mainModelDir`), so nothing is tuned again.
#' The refit model sees nVal more cases than the base models (that is the point of refitting); nTrain in the result is training + validation.

#' Train for a fixed number of epochs with a given learning-rate trajectory (no validation, no early stopping).
fitFixedEpochs <- function(adapter, hyper, tr, nUnits, seed, lrSchedule, device = "cpu") {
  torch::torch_manual_seed(seed)
  net <- adapter$makeNet(tr$x$size(2), nUnits, hyper)$to(device = device)
  batchSize <- hyper$batchSize %||% 128L
  pn <- names(net$parameters); isId <- grepl("^idInt", pn)
  groups <- list(list(params = unname(net$parameters[!isId]), weight_decay = hyper$weightDecay %||% 0))
  if (any(isId)) groups[[2]] <- list(params = unname(net$parameters[isId]), weight_decay = hyper$idDecay %||% 1e-3)
  opt <- torch::optim_adam(groups, lr = lrSchedule[1])
  n <- tr$x$size(1)
  for (ep in seq_along(lrSchedule)) {
    for (g in seq_along(opt$param_groups)) opt$param_groups[[g]]$lr <- lrSchedule[ep]
    net$train()
    perm <- torch::torch_randperm(n, device = device) + 1L
    for (a in seq(1L, n, by = batchSize)) {
      z <- min(a + batchSize - 1L, n); ii <- perm[a:z]
      opt$zero_grad()
      loss <- adapter$caseLoss(net(tr$x[ii, ], tr$id[ii]), tr$y[ii], net)$mean()
      loss$backward(); opt$step()
    }
  }
  list(net = net, trainUnitsSeen = sort(unique(as.integer(as.array(tr$id$cpu())))))
}

runModelRefit <- function(store, manifest, planRow, features, adapter, hyper, outputDir, mainModelDir, reRun = FALSE, device = "cpu", zClip = 10) {
  modelName <- planRow$modelName; regime <- planRow$typeValidation; base <- sub("Refit$", "", regime)
  finalPath <- file.path(outputDir, paste0(modelName, "_finalDT.csv"))
  if (!reRun && file.exists(finalPath)) { message(modelName, ": results exist and reRun = FALSE; loading."); return(data.table::fread(finalPath)) }
  t0 <- Sys.time(); idx <- store$index
  verifyRegimeManifest(idx, manifest, base, planRow$windowStart, planRow$historyEnd, planRow$testYear,
                       c(nTrain = planRow$nTrainS, nVal = planRow$nValS, nTest = planRow$nTestS), valYears = planRow$valYears %||% 1L)
  baseName <- sub(paste0(regime, "$"), base, modelName)
  bf <- file.path(mainModelDir, paste0(baseName, "_finalDT.csv")); hf <- file.path(mainModelDir, paste0(baseName, "_history.csv"))
  if (!file.exists(bf) || !file.exists(hf)) stop("Refit needs the base model's result and history in ", mainModelDir, ": ", baseName)
  b <- data.table::fread(bf); h <- data.table::fread(hf)
  bestEpoch <- as.integer(b$bestEpoch[1]); lrSched <- h$lr[seq_len(bestEpoch)]
  if (length(lrSched) != bestEpoch || anyNA(lrSched)) stop("Base model history does not cover its best epoch: ", baseName)
  trRows <- c(manifest$row[manifest$set == "train"], manifest$row[manifest$set == "val"]); teRows <- manifest$row[manifest$set == "test"]
  stopifnot(!anyDuplicated(trRows), !any(trRows %in% teRows))
  tr <- storeSlice(store, trRows, features, device); te <- storeSlice(store, teRows, features, device)
  st <- scaleStats(tr$x, features); tr$x <- applyScale(tr$x, st, zClip); te$x <- applyScale(te$x, st, zClip)
  fit <- fitFixedEpochs(adapter, hyper, tr, store$nUnits, planRow$modelSeed, lrSched, device)
  ix <- idx[match(teRows, idx$row)]
  r <- scoreCases(fit$net, adapter, te$x, te$y, te$id, fit$trainUnitsSeen, store$nUnits)
  teT <- cbind(data.table::data.table(modelName = modelName, splitId = planRow$splitId, typeValidation = regime, numberOfCovariates = planRow$numberOfCovariates,
                                      set = "test", row = teRows, unit = ix$unit, year = ix$year), r)
  trLoss <- mean(scoreCases(fit$net, adapter, tr$x, tr$y, tr$id, fit$trainUnitsSeen, store$nUnits)$loss)
  saveRDS(teT, file.path(outputDir, paste0(modelName, "_perCase.rds")))
  nU <- function(rows) data.table::uniqueN(idx$unit[match(rows, idx$row)])
  finalDT <- data.table::data.table(
    groupId = planRow$groupId, splitId = planRow$splitId, typeValidation = regime, modelName = modelName, numberOfCovariates = planRow$numberOfCovariates,
    replicate = planRow$replicate, windowStart = planRow$windowStart, historyEnd = planRow$historyEnd, testYear = planRow$testYear, horizon = planRow$horizon,
    valYears = planRow$valYears %||% 1L, trainShare = planRow$trainShare %||% NA_real_, nTrain = length(trRows), nVal = 0L, nTest = length(teRows),
    nUnitsTrain = nU(trRows), nUnitsTest = nU(teRows), splitSeed = planRow$splitSeed, modelSeed = planRow$modelSeed,
    trainLossBest = trLoss, valLossBest = b$valLossBest[1],            # what the practitioner saw when choosing the stopping epoch (the base model's validation loss)
    bestEpoch = bestEpoch, epochsRun = bestEpoch, epochCap = b$epochCap[1],
    testLossMean = mean(teT$loss), testLossSD = stats::sd(teT$loss), testLossSE = stats::sd(teT$loss) / sqrt(nrow(teT)),
    testMAE = mean(teT$absErr), testRMSE = sqrt(mean(teT$sqErr)), seenShare = mean(teT$seenUnit),
    secondsTotal = as.numeric(difftime(Sys.time(), t0, units = "secs")), modelWeight = NA_character_)
  data.table::fwrite(finalDT, finalPath)
  finalDT
}
