# Why is test year 2015 (and 2016) missing in the caribou new-places design? Pool sizes per regime for the early splits, 100 km blocks, buffer 10 km (and 25 km / 5 km for comparison).
suppressMessages(library(data.table))
for (f in list.files("modules/preValNN/R", pattern = "[.]R$", full.names = TRUE)) source(f)
idx <- readRDS("outputs/caribou_engine/caribou/run_newPlaces/caseIndex.rds")
cat("strata per year:\n"); print(idx[, .N, by = year][order(year)][, paste(year, N, collapse = " | ")])
for (cfg in list(c(100, 10), c(25, 5))) {
  sp <- assignBlocks(idx, blockKm = cfg[1], bufferKm = cfg[2], nFolds = 4L, seed = stringSeed("spatialBlocks"))
  cat(sprintf("\n=== blocks %g km, buffer %g km ===\n", cfg[1], cfg[2]))
  for (sid in c("s2013_e2014_T2015", "s2013_e2015_T2016", "s2014_e2015_T2016", "s2013_e2016_T2017")) {
    g <- as.integer(regmatches(sid, regexec("s([0-9]+)_e([0-9]+)_T([0-9]+)", sid))[[1]][2:4]); seed <- stringSeed(sid)
    b <- buildSpatialTestSplits(idx, g[1], g[2], g[3], seed, spatial = sp, testShare = 0.25, valShare = 0.25, minTrain = 500L, minVal = 50L, minTest = 200L, minTrainFraction = 0.0, withRouteHeldOut = TRUE)
    if (!isTRUE(b$ok)) { cat(sid, "-> ", b$reason, "\n"); next }
    yr <- idx$year; trainP <- which(yr >= g[1] & yr <= g[2] - 1L); valP <- which(yr == g[2])
    ps <- b$poolSizes; cat(sprintf("%s: train-year strata %d, val-year strata %d | pools: %s | common training %d = %.0f%% of the status-quo pool (%d)\n", sid, length(trainP), length(valP),
        paste(names(ps), format(ps, big.mark = ","), collapse = "; "), b$sizes[["nTrain"]], 100 * b$trainShare, ps[["FutureTainted"]]))
  }
}
