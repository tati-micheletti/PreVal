# Birds: run the generic validation engine for ONE species (each species is its own experiment).
# Same conventions as runMe.R (caribou): stage and settings come from PREVAL_* environment variables, one SLURM array task = one slice.
#   PREVAL_SPECIES   Chloris_chloris | Dendrocopos_major          (default Chloris_chloris)
#   PREVAL_STAGE     prep (network pilot + ranking + block size, then design) | design | train | analyze | all
#   PREVAL_OUT       output folder (default outputs/birds)
#   PREVAL_FINAL     folder with final_<species>.rds (default data_birds)
#   PREVAL_LADDER    "" (primary ladder) | forward (sensitivity ladder from featureTable_forward.csv)
#   PREVAL_COMPLEXITY, PREVAL_EPOCHS, PREVAL_PATIENCE, PREVAL_REPLICATES, PREVAL_START, PREVAL_END, PREVAL_MINTRAIN (default 2),
#   PREVAL_VALYEARS (default 1; 2 = two-year validation arm, run with PREVAL_TAG=val2), PREVAL_HORIZONS (default 1..19 = every distance the data allow),
#   PREVAL_IDMODE (intercept | none | concat; default intercept), PREVAL_PILOTMODE (random = all years, default | forward = strict, needs
#   PREVAL_LASTPILOT, e.g. 2008),
#   PREVAL_MINTRAINFRAC (sanity limit: skip splits where blocking keeps less of the status-quo training pool; default 0.5, which excludes
#   nothing for the birds; the kept share is stored per split), PREVAL_MINTRAIN_CASES (absolute minimum training cases, default 200),
#   PREVAL_PILOT_EPOCHS, PREVAL_ONLY_MISSING, PREVAL_THREADS, PREVAL_NEW_PLAN
if (nzchar(Sys.getenv("LOCALAPPDATA"))) .libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))
if (nzchar(Sys.getenv("SLURM_JOB_ID"))) {          # cluster job: only add the project library made by the one-time install (as runMe.R does)
  libs <- Sys.glob(file.path(path.expand("~"), ".local", "share", "R", "PreVal", "packages", "*", "*"))
  if (length(libs)) .libPaths(c(libs, .libPaths()))
  birdLibs <- Sys.glob(file.path(path.expand("~"), ".local", "share", "R", "birdMonitor", "packages", "*", "*"))
  if (length(birdLibs)) .libPaths(c(.libPaths(), birdLibs))
}
envNum <- function(name, default) { v <- Sys.getenv(name, ""); if (nzchar(v)) as.numeric(v) else default }
envChr <- function(name, default) { v <- Sys.getenv(name, ""); if (nzchar(v)) v else default }
species <- envChr("PREVAL_SPECIES", "Chloris_chloris")
stage <- envChr("PREVAL_STAGE", "all"); stopifnot(stage %in% c("prep", "design", "train", "analyze", "all"))
outPath <- envChr("PREVAL_OUT", "outputs/birds"); finalDir <- envChr("PREVAL_FINAL", "data_birds")
ladder <- envChr("PREVAL_LADDER", "")
sliceTask <- if (stage == "train") c(envNum("SLURM_ARRAY_TASK_ID", 1), envNum("SLURM_ARRAY_TASK_COUNT", 1)) else c(NA_real_, NA_real_)
startYear <- envNum("PREVAL_START", 2005); endYear <- envNum("PREVAL_END", 2024); minTrain <- envNum("PREVAL_MINTRAIN", 2); valYears <- envNum("PREVAL_VALYEARS", 1); runTag <- envChr("PREVAL_TAG", "")
horizons <- envChr("PREVAL_HORIZONS", paste(1:19, collapse = ",")); idMode <- envChr("PREVAL_IDMODE", "intercept")
# Network pilot: random hold-out over all years (default, like the covariate ranking) or strict forward (only years <= lastPilotYear;
# the design then refuses any forecast year <= lastPilotYear).
pilotMode <- envChr("PREVAL_PILOTMODE", "random"); lastPilotYear <- envNum("PREVAL_LASTPILOT", NA_real_)

suppressMessages({library(SpaDES.core); library(data.table)})
options(spades.useRequire = FALSE, spades.moduleCodeChecks = FALSE, reproducible.useMemoise = TRUE)
finalTable <- readRDS(file.path(finalDir, sprintf("final_%s.rds", species)))
dir.create(outPath, recursive = TRUE, showWarnings = FALSE)

runStage <- function(mods, params) {
  sim <- simInit(times = list(start = 1, end = 1), modules = mods, params = params, objects = list(finalTable = finalTable),
                 paths = list(modulePath = "modules", outputPath = outPath, inputPath = "data_birds", cachePath = file.path(outPath, "cache")))
  invisible(spades(sim))
}
common <- list(species = species, response = "Reviere", responseType = "count")
if (stage %in% c("prep", "all")) {
  runStage("preValNN_Global", list(preValNN_Global = c(common, list(pilotMode = pilotMode, lastPilotYear = lastPilotYear, pilotEpochs = envNum("PREVAL_PILOT_EPOCHS", 100),
                                                                     rankEpochs = envNum("PREVAL_EPOCHS", 100), crs = 3035, idMode = idMode))))
}
if (stage %in% c("prep", "design", "train", "analyze", "all")) {
  runStage("preValNN", list(preValNN = c(common, list(
    stage = c(prep = "design", design = "design", train = "train", analyze = "analyze", all = "all")[[stage]],
    startYear = startYear, endYear = endYear, minTrainYears = minTrain, valYears = valYears, horizons = horizons,
    complexityLevels = envChr("PREVAL_COMPLEXITY", "2,5,10,15,25,35,50,Inf"),
    epoch = envNum("PREVAL_EPOCHS", 100), earlyStopPatience = envNum("PREVAL_PATIENCE", 15), nReplicates = envNum("PREVAL_REPLICATES", 1),
    useSavedPlan = Sys.getenv("PREVAL_NEW_PLAN") != "1", onlyMissing = Sys.getenv("PREVAL_ONLY_MISSING") == "1",
    runSlice = sliceTask, torchThreads = envNum("PREVAL_THREADS", 1),
    rankingFile = if (nzchar(ladder)) sprintf("featureTable_%s.csv", ladder) else "featureTable.csv", ladderTag = ladder, runTag = runTag,
    minTrainFraction = envNum("PREVAL_MINTRAINFRAC", 0.5), minTrain = envNum("PREVAL_MINTRAIN_CASES", 200)))))
}
