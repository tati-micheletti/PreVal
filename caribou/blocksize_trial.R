# Block-size trial for the caribou NEW-PLACES design (design stage only; nothing is trained, nothing in the existing run folders is touched).
# For each (blockKm, bufferKm) the splits are built once with a permissive minimum training share (0.2) and the share is applied afterwards (the kept share is stored per split):
# splits kept at 0.5 / 0.4 / 0.3, forecast years and horizons covered, common training size as a share of the status-quo pool, validation sizes, skip reasons.
# Run from the repo root:  Rscript caribou/blocksize_trial.R   (writes caribou/blocksize_trial_results.csv and caribou/blocksize_trial_2015.csv)
suppressMessages(library(data.table))
for (f in list.files("modules/preValNN/R", pattern = "[.]R$", full.names = TRUE)) source(f)
idx <- readRDS("outputs/caribou_engine/caribou/run_newPlaces/caseIndex.rds")
cfg <- rbindlist(list(data.table(blockKm = c(100, 50, 25), bufferKm = 10), data.table(blockKm = 25, bufferKm = 5)))
res <- list(); early <- list()
for (i in seq_len(nrow(cfg))) {
  bk <- cfg$blockKm[i]; bf <- cfg$bufferKm[i]
  pl <- suppressMessages(generatePlan(idx, startYear = 2013, endYear = 2022, minTrainYears = 1L, valYears = 1L, horizons = 1:8, covariateLevels = 2, nReplicates = 1L, blockKm = bk, bufferKm = bf, nFolds = 4L,
                                      minTrain = 500L, minVal = 200L, minTest = 200L, minTrainFraction = 0.2, arm = "spatialTest", spatialTestShare = 0.25, spatialValShare = 0.25, routeHeldOut = TRUE))
  b <- pl$bundles; grid <- unique(pl$plan[, .(splitId, windowStart, historyEnd, testYear, horizon)])
  sk <- if (is.data.table(pl$skipped)) pl$skipped else if (length(pl$skipped)) rbindlist(pl$skipped) else data.table(splitId = character(), reason = character())
  S <- rbindlist(lapply(names(b), function(sid) data.table(splitId = sid, trainShare = b[[sid]]$trainShare, nTrain = b[[sid]]$sizes[["nTrain"]], nVal = b[[sid]]$sizes[["nVal"]], nTest = b[[sid]]$sizes[["nTest"]],
                                                          poolSQ = b[[sid]]$poolSizes[["FutureTainted"]], valPreVal = sum(b[[sid]]$splits$FutureUnseenGrouped$set == "val"),
                                                          nAnimalsValPreVal = uniqueN(idx$unit[b[[sid]]$splits$FutureUnseenGrouped[set == "val"]$row]))))
  S <- merge(S, grid, by = "splitId")
  nAll <- 120L
  for (th in c(0.5, 0.4, 0.3)) {
    K <- S[trainShare >= th]
    res[[length(res) + 1L]] <- data.table(blockKm = bk, bufferKm = bf, minTrainFraction = th, splitsKept = nrow(K), of = nAll, years = paste(sort(unique(K$testYear)), collapse = ","), nYears = uniqueN(K$testYear),
                                          horizons = paste(sort(unique(K$horizon)), collapse = ","), medianTrainShare = round(median(K$trainShare), 3), medianNTrain = median(K$nTrain), medianValPerRegime = median(K$nVal),
                                          medianPreValAnimals = median(K$nAnimalsValPreVal))
  }
  # skip reasons with the minimum share 0.5 / 0.4: splits that exist in S but fail the share, plus the splits skipped for other reasons
  other <- sk[, .N, by = .(reason = sub("[0-9]+", "N", reason))]
  cat(sprintf("\n== block %g km, buffer %g km: %d splits built at share 0.2; skipped even at 0.2: %d\n", bk, bf, nrow(S), nrow(sk))); if (nrow(other)) print(other)
  S[, `:=`(blockKm = bk, bufferKm = bf)]; early[[i]] <- S[testYear <= 2017][order(testYear, horizon)]
}
R <- rbindlist(res); fwrite(R, "caribou/blocksize_trial_results.csv"); print(R)
E <- rbindlist(early); fwrite(E[, .(blockKm, bufferKm, splitId, testYear, horizon, nTrain, poolSQ, trainShare = round(trainShare, 3), nVal, nTest)], "caribou/blocksize_trial_2015.csv")
cat("\nEarly test years (2015-2017) at 100 km / buffer 10 km:\n"); print(E[blockKm == 100 & bufferKm == 10, .(splitId, testYear, horizon, poolSQ, nTrain, trainShare = round(trainShare, 3), nVal)])
