# Caribou through the STANDARDISED validation engine (modules/preValNN, dataLayout = "strata"): same regimes, gates, plan, ranking and analysis as the birds.
# Plan: caribou/ANALYSIS_PLAN_CARIBOU_NN.md. Naming (2026-10-30): "PreVal" = the regime FutureUnseenGrouped (validation on the LAST year AND on whole animals absent from training);
# FutureUnseen (validation year with the animals still in training) is built only for the REPLICATION arm (the old design) and is not a comparison regime.
#
# Settings (environment variables; stage = prep | design | rank | train | analyze | all):
#   PREVAL_STAGE      stage (default all)
#   PREVAL_OUT        output root (default outputs/caribou_engine); results go to <out>/caribou/run<tag>
#   PREVAL_FINAL      folder with final_caribou.rds (written by the prep stage into <out>/caribou if empty)
#   PREVAL_PRESET     replication | main | newPlaces | newPlaces25 | newAnimals (new animals: test strata of held-out animals only, see plan section 14) (new places with 25 km blocks and a 5 km buffer, sensitivity design B). Sets the defaults below (any explicit variable wins):
#       replication   the OLD design as it was: regimes FutureUnseen, FutureTainted, Internal (matched on animals), full pools, split ids with suffix _temporal (same split and model seeds),
#                     the old global ranking (no per-window ranking), levels 2,5,10,20,Inf (the second pass). Tag "replication". The acceptance arm: it must reproduce the old numbers up to float noise.
#       main          same animals, new design (2026-10-31): PreVal (FutureUnseenGrouped), status quo, Internal (matched on animals); NO spatial regimes; per-window ranking on the training years only; levels 2,5,10,20,30.
#       newPlaces     test strata in blocks no regime trains or validates on: PreVal, status quo, spatial CV, PreVal + spatial (arm spatialTest, tag newPlaces); plan excludes the old FutureUnseen and Internal.
#   PREVAL_TAG, PREVAL_REGIMES, PREVAL_PLANREGIMES, PREVAL_MATCHUNITS, PREVAL_ROUTEHELDOUT, PREVAL_SPLITSUFFIX, PREVAL_RANKPERWINDOW, PREVAL_RANKFROM, PREVAL_ARM,
#   PREVAL_COMPLEXITY, PREVAL_EPOCHS (300; the first old pass used 50), PREVAL_PATIENCE (10, as in the old runs), PREVAL_REPLICATES (1; the old second pass had 3), PREVAL_START (2013), PREVAL_END (2022), PREVAL_MINTRAIN (1 year), PREVAL_HORIZONS (1..8),
#   PREVAL_UNSEENVALSHARE (validation share of PreVal on unseen animals; main 0.30, else 0.15), PREVAL_MINTRAINFRAC (0.5; newPlaces 0.4), PREVAL_SPATIALTESTSHARE / PREVAL_SPATIALVALSHARE (new places: 0.25 / 0.25),
#   PREVAL_BLOCKKM (100), PREVAL_BUFFERKM (10), PREVAL_SLICE_ID / PREVAL_SLICE_COUNT, PREVAL_THREADS, PREVAL_ONLY_MISSING, PREVAL_NEW_PLAN
#   prep stage only: PREVAL_CARIBOU_PREPARED (default results_firstpass/preparedModelInputs.csv: the saved result of prepareNNdata), PREVAL_RANKING_CSV (global ranking, default
#   results_firstpass/featureTable.csv), PREVAL_CARIBOU_NSTRATA (smoke tests: read only this many strata)
# The primary loss is the RAW cross-entropy of the observed step (folder analysis); the temperature-calibrated loss is next to it (analysis_calibrated).
if (nzchar(Sys.getenv("LOCALAPPDATA"))) .libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))
if (nzchar(Sys.getenv("SLURM_JOB_ID"))) {          # cluster job: only add the project library made by the one-time install (as runMe.R does)
  libs <- Sys.glob(file.path(path.expand("~"), ".local", "share", "R", "PreVal", "packages", "*", "*"))
  if (length(libs)) .libPaths(c(libs, .libPaths()))
}
envNum <- function(name, default) { v <- Sys.getenv(name, ""); if (nzchar(v)) as.numeric(v) else default }
envChr <- function(name, default) { v <- Sys.getenv(name, ""); if (nzchar(v)) v else default }
envLgl <- function(name, default) { v <- Sys.getenv(name, ""); if (nzchar(v)) v %in% c("1", "TRUE", "true") else default }
preset <- envChr("PREVAL_PRESET", "main"); stopifnot(preset %in% c("replication", "main", "newPlaces", "newPlaces25", "newAnimals"))
P <- list(
  replication = list(tag = "replication", regimes = "FutureUnseen,FutureTainted,Internal", plan = "", matchUnits = TRUE, routeHeld = FALSE, suffix = "_temporal", rankPerWindow = FALSE, arm = "main", complexity = "2,5,10,20,Inf", unseenShare = 0.15, minTrainFrac = 0.5),
  # main REDEFINED 2026-10-31 (Tati: two-arm structure): NO spatial regimes in the same-animals arm, so no random cut to the smallest (buffered) pool and all splits / forecast years survive.
  # FutureUnseen (validation year with the animals still in training) is built as a by-product only; it is NOT in the training plan.
  main        = list(tag = "main", regimes = "FutureUnseen,FutureTainted,FutureUnseenGrouped,Internal",
                     plan = "FutureTainted,FutureUnseenGrouped,Internal", matchUnits = TRUE, routeHeld = TRUE, suffix = "", rankPerWindow = TRUE, arm = "main", complexity = "2,5,10,20,30", unseenShare = 0.30, minTrainFrac = 0.5),
  newPlaces   = list(tag = "newPlaces", regimes = "FutureUnseen,FutureTainted,FutureTaintedSpatial,FutureUnseenSpatial,Internal", plan = "FutureTainted,FutureTaintedSpatial,FutureUnseenSpatial,FutureUnseenGrouped", matchUnits = FALSE, routeHeld = TRUE, suffix = "", rankPerWindow = TRUE, arm = "spatialTest", complexity = "2,5,10,20,30", unseenShare = 0.15, minTrainFrac = 0.4),
  # newPlaces25 (2026-10-31, sensitivity design B): the same new-places arm with 25 km blocks and a 5 km buffer (the 100 km / 10 km of newPlaces is design A, the primary one)
  newPlaces25 = list(tag = "newPlaces25", regimes = "FutureUnseen,FutureTainted,FutureTaintedSpatial,FutureUnseenSpatial,Internal", plan = "FutureTainted,FutureTaintedSpatial,FutureUnseenSpatial,FutureUnseenGrouped", matchUnits = FALSE, routeHeld = TRUE, suffix = "", rankPerWindow = TRUE, arm = "spatialTest", complexity = "2,5,10,20,30", unseenShare = 0.15, minTrainFrac = 0.4, blockKm = 25, bufferKm = 5),
  # newAnimals (2026-10-31): test strata = forecast-year strata of held-out ANIMALS (one seeded fold of 4 per window), absent from every pool of every regime in every year; no spatial blocks.
  # PreVal (30% validation share), status quo, animal-grouped CV. Per-window rankings are recomputed WITHOUT the held-out animals (PREVAL_RANKFROM must stay empty).
  newAnimals  = list(tag = "newAnimals", regimes = "FutureUnseenGrouped,FutureTainted,FutureTaintedGrouped", plan = "FutureUnseenGrouped,FutureTainted,FutureTaintedGrouped", matchUnits = FALSE, routeHeld = FALSE, suffix = "", rankPerWindow = TRUE, arm = "newAnimals", complexity = "2,5,10,20,30", unseenShare = 0.30, minTrainFrac = 0.4))[[preset]]      # decisions of 2026-10-31: main 30% of the last-year strata as PreVal validation; new places minimum training share 0.4
if (is.null(P$blockKm)) P$blockKm <- 100; if (is.null(P$bufferKm)) P$bufferKm <- 10      # blocks and buffer of the spatial regimes (new-places designs A: 100 / 10, B: 25 / 5)
stage <- envChr("PREVAL_STAGE", "all"); stopifnot(stage %in% c("prep", "design", "rank", "train", "analyze", "all"))
outPath <- envChr("PREVAL_OUT", "outputs/caribou_engine"); species <- "caribou"; speciesDir <- file.path(outPath, species)
finalDir <- envChr("PREVAL_FINAL", speciesDir); runTag <- envChr("PREVAL_TAG", P$tag)
sliceTask <- if (stage %in% c("train", "rank")) {
  if (nzchar(Sys.getenv("PREVAL_SLICE_COUNT"))) c(envNum("PREVAL_SLICE_ID", 1), envNum("PREVAL_SLICE_COUNT", 1)) else c(envNum("SLURM_ARRAY_TASK_ID", 1), envNum("SLURM_ARRAY_TASK_COUNT", 1))
} else c(NA_real_, NA_real_)

if (stage %in% c("prep", "all")) {            # final table + global ranking + network settings (the original network, never tuned: no pilot)
  suppressMessages(library(data.table)); source("modules/preValNN/R/seeds.R"); source("modules/preValPrep_Caribou/R/prepCaribou.R")
  prepared <- envChr("PREVAL_CARIBOU_PREPARED", "results_firstpass/preparedModelInputs.csv")
  ns <- envNum("PREVAL_CARIBOU_NSTRATA", NA_real_)
  res <- buildCaribouFinal(preparedCsv = prepared, startYear = envNum("PREVAL_START", 2013), nStrataMax = if (is.na(ns)) NULL else ns)
  message(sprintf("caribou final table: %d strata, %d animals, years %d-%d, %d candidate covariates", res$nStrata, res$nAnimals, res$years[1], res$years[2], length(res$featureCandidates)))
  writeCaribouInputs(res, speciesDir, rankingCsv = envChr("PREVAL_RANKING_CSV", file.path(dirname(prepared), "featureTable.csv")))
}
if (stage %in% c("design", "rank", "train", "analyze", "all")) {
  suppressMessages({library(SpaDES.core); library(data.table)})
  options(spades.useRequire = FALSE, spades.moduleCodeChecks = FALSE, reproducible.useMemoise = TRUE)
  finalTable <- readRDS(file.path(finalDir, "final_caribou.rds"))
  sim <- simInit(times = list(start = 1, end = 1), modules = "preValNN", objects = list(finalTable = finalTable), paths = list(modulePath = "modules", outputPath = outPath, inputPath = "data_birds", cachePath = file.path(outPath, "cache")),
    params = list(preValNN = list(
      species = species, response = "case_", responseType = "stratum", unitCol = "id", yearCol = "year", dataLayout = "strata", primaryLoss = "fitted",
      stage = c(prep = "design", design = "design", rank = "rank", train = "train", analyze = "analyze", all = "all")[[stage]],
      rankPerWindow = envLgl("PREVAL_RANKPERWINDOW", P$rankPerWindow), rankingsFromTag = envChr("PREVAL_RANKFROM", ""),
      startYear = envNum("PREVAL_START", 2013), endYear = envNum("PREVAL_END", 2022), minTrainYears = envNum("PREVAL_MINTRAIN", 1), valYears = 1, horizons = envChr("PREVAL_HORIZONS", paste(1:8, collapse = ",")),
      complexityLevels = envChr("PREVAL_COMPLEXITY", P$complexity), epoch = envNum("PREVAL_EPOCHS", 300), earlyStopPatience = envNum("PREVAL_PATIENCE", 10), nReplicates = envNum("PREVAL_REPLICATES", 1),
      useSavedPlan = Sys.getenv("PREVAL_NEW_PLAN") != "1", onlyMissing = Sys.getenv("PREVAL_ONLY_MISSING") == "1", runSlice = sliceTask, torchThreads = envNum("PREVAL_THREADS", 1),
      rankingFile = "featureTable.csv", runTag = runTag, planRegimes = envChr("PREVAL_PLANREGIMES", P$plan), regimes = envChr("PREVAL_REGIMES", P$regimes),
      blockKm = envNum("PREVAL_BLOCKKM", P$blockKm), bufferKm = envNum("PREVAL_BUFFERKM", P$bufferKm), nFolds = 4, minTrain = 500, minVal = 200, minTest = 200, minTrainFraction = envNum("PREVAL_MINTRAINFRAC", P$minTrainFrac), unseenValShare = envNum("PREVAL_UNSEENVALSHARE", P$unseenShare),
      spatialTestShare = envNum("PREVAL_SPATIALTESTSHARE", 0.25), spatialValShare = envNum("PREVAL_SPATIALVALSHARE", 0.25),
      arm = envChr("PREVAL_ARM", P$arm), routeHeldOut = envLgl("PREVAL_ROUTEHELDOUT", P$routeHeld), matchUnits = envLgl("PREVAL_MATCHUNITS", P$matchUnits), splitSuffix = envChr("PREVAL_SPLITSUFFIX", P$suffix))))
  invisible(spades(sim))
}
