defineModule(sim, list(
  name = "preValNN",
  description = paste0("Generic validation engine: disjoint splits for five validation regimes (PreVal, status quo, route-grouped CV, spatial-block CV, ",
                       "leaky random reference) with equal training size, leak gates, plan, one model per cell, pre-specified analysis. The response ",
                       "type is handled by an adapter (R/adapters.R)."),
  keywords = c("validation", "forecasting", "neural network", "leakage"),
  authors = structure(list(list(given = "Tati", family = "Micheletti", role = c("aut", "cre"), email = "tati.micheletti@gmail.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(preValNN = "0.1.0"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list("README.md"),
  reqdPkgs = list("SpaDES.core (>= 3.0.4)", "data.table", "torch"),
  parameters = bindrows(
    defineParameter(".plots", "character", "screen", NA, NA, "Used by Plots function"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should caching of events or module be used?"),
    defineParameter("species", "character", NA_character_, NA, NA, "File stem of the species / dataset; outputs go to <outputPath>/<species>/."),
    defineParameter("response", "character", "Reviere", NA, NA, "Response column."),
    defineParameter("responseType", "character", "count", NA, NA, "Adapter: 'count' or 'continuous'."),
    defineParameter("unitCol", "character", "SITE_ID", NA, NA, "Unit that repeats across years (route)."),
    defineParameter("yearCol", "character", "year", NA, NA, "Year column."),
    defineParameter("startYear", "numeric", 2005, NA, NA, "First analysis year."),
    defineParameter("endYear", "numeric", 2024, NA, NA, "Last year with data."),
    defineParameter("minTrainYears", "numeric", 2, 1, NA, "Minimum number of training years of PreVal in a window s..e."),
    defineParameter("valYears", "numeric", 1, 1, 5, "Number of validation years of PreVal (the last years of the window). 2 = two-year-validation variant."),
    defineParameter("runTag", "character", "", NA, NA, "Suffix of the run folder, e.g. val2 for the two-year-validation arm (the ladder file is chosen separately)."),
    defineParameter("horizons", "character", paste(1:19, collapse = ","), NA, NA, "Comma-separated forecast distances T - e (default: every distance the data allow, 1 to 19)."),
    defineParameter("complexityLevels", "character", "2,5,10,15,25,35,50,Inf", NA, NA, "Comma-separated covariate counts (Inf = all)."),
    defineParameter("nReplicates", "numeric", 1, 1, 20, "Independent network initialisations per cell (same splits)."),
    defineParameter("testFraction", "numeric", 0.5, 0.05, 1, "Share of the forecast year's cases used as the shared test set."),
    defineParameter("blockKm", "numeric", NA_real_, 1, NA, "Spatial block size (km). NA = the value found by preValNN_Global (blockSize.rds)."),
    defineParameter("bufferKm", "numeric", NA_real_, 0, NA, "Buffer around validation blocks (km). NA = blockKm / 8."),
    defineParameter("nFolds", "numeric", 4, 2, NA, "Number of spatial folds (blocks are assigned to folds at random)."),
    defineParameter("minTrain", "numeric", 200, 10, NA, "Minimum training cases of a split."),
    defineParameter("minVal", "numeric", 100, 10, NA, "Minimum validation cases of a split."),
    defineParameter("minTest", "numeric", 100, 10, NA, "Minimum test cases of a split."),
    defineParameter("minTrainFraction", "numeric", 0.5, 0, 1, "Sanity limit: skip splits where blocking leaves less than this share of the status-quo training pool (the share is stored per split)."),
    defineParameter("rankingFile", "character", "featureTable.csv", NA, NA, "Ranking (ladder) file from preValNN_Global; e.g. featureTable_forward.csv for the sensitivity ladder."),
    defineParameter("ladderTag", "character", "", NA, NA, "Suffix for the output folders of a sensitivity run (e.g. 'forward'); empty = primary."),
    defineParameter("epoch", "numeric", 100, 1, 1000, "Maximum epochs per model (best validation epoch is kept)."),
    defineParameter("earlyStopPatience", "numeric", 15, 1, Inf, "Stop after this many epochs without a new best validation loss."),
    defineParameter("zClip", "numeric", 10, 1, Inf, "Standardised covariates are clipped to +/- zClip."),
    defineParameter("regimes", "character", "FutureUnseen,FutureTainted,FutureTaintedGrouped,FutureTaintedSpatial,Internal", NA, NA, "Regimes to build."),
    defineParameter("runSlice", "numeric", NA_real_, NA, NA, "c(taskId, nTasks) to run a share of the models in this process (SLURM array). NA runs everything."),
    defineParameter("torchThreads", "numeric", 1, 1, NA, "Threads per process for torch."),
    defineParameter("stopOnError", "logical", TRUE, NA, NA, "Raise an error after the run if any model failed."),
    defineParameter("reRunModels", "logical", FALSE, NA, NA, "Re-run models with existing results?"),
    defineParameter("useSavedPlan", "logical", TRUE, NA, NA, "Use the saved plan and manifests if they exist (never re-sample silently)."),
    defineParameter("onlyMissing", "logical", FALSE, NA, NA, "Mop-up: run only models without a result."),
    defineParameter("modComplex", "character", "all", NA, NA, "Run only this number of covariates."),
    defineParameter("useGPU", "logical", FALSE, NA, NA, "Use a GPU if available."),
    defineParameter("stage", "character", "all", NA, NA, "'all' | 'design' (plan, manifests, store; once) | 'train' (one SLURM task; needs runSlice) | 'analyze'.")
  ),
  inputObjects = bindrows(
    expectsInput("finalTable", "data.table", "Final table for modeling", sourceURL = NA),
    expectsInput("featurePriority", "data.table", "Covariates ordered by importance (preValNN_Global)", sourceURL = NA),
    expectsInput("networkSettings", "list", "Fixed network settings (preValNN_Global)", sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput("experimentPlan", "data.table", "The experiment plan (one row per model)"),
    createsOutput("modelComparisons", "list", "Pre-specified analysis tables")
  )
))

doEvent.preValNN <- function(sim, eventTime, eventType) {
  stage <- P(sim)$stage
  if (!stage %in% c("all", "design", "train", "analyze")) stop("preValNN parameter 'stage' must be one of: all, design, train, analyze.")
  num <- function(s) as.numeric(strsplit(s, ",")[[1]])
  switch(eventType,
    init = {
      if (stage %in% c("all", "design", "train")) sim <- scheduleEvent(sim, time(sim), "preValNN", "prepareExperiment")
      if (stage %in% c("all", "train")) sim <- scheduleEvent(sim, time(sim), "preValNN", "trainExperiment")
      if (stage %in% c("all", "analyze")) sim <- scheduleEvent(sim, time(sim), "preValNN", "compareExperiment")
    },
    prepareExperiment = {
      sp <- P(sim)$species; tag <- paste0(if (nzchar(P(sim)$ladderTag)) paste0("_", P(sim)$ladderTag) else "", if (nzchar(P(sim)$runTag)) paste0("_", P(sim)$runTag) else "")
      root <- file.path(outputPath(sim), sp); outDir <- file.path(root, paste0("run", tag)); dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
      planPath <- file.path(outDir, "experimentPlan.csv"); manifestDir <- file.path(outDir, "splits")
      slice <- if (anyNA(P(sim)$runSlice)) NULL else P(sim)$runSlice
      have <- all(P(sim)$useSavedPlan, file.exists(planPath), dir.exists(manifestDir), file.exists(file.path(outDir, "designParams.rds")))
      if (have) {
        message("Saved plan and split manifests found; using them.")
        sim$experimentPlan <- data.table::fread(planPath)
      } else {
        if (stage == "train") stop("stage = 'train' needs the output of stage = 'design' in ", outDir)
        tab <- data.table::as.data.table(sim$finalTable); idx <- buildCaseIndex(tab, P(sim)$unitCol, P(sim)$yearCol, "x", "y")
        bk <- P(sim)$blockKm
        if (is.na(bk)) bk <- readRDS(file.path(root, "blockSize.rds"))$blockKm
        bf <- P(sim)$bufferKm; if (is.na(bf)) bf <- bk / 8
        des <- generatePlan(idx, startYear = P(sim)$startYear, endYear = P(sim)$endYear, minTrainYears = P(sim)$minTrainYears, valYears = P(sim)$valYears, horizons = num(P(sim)$horizons),
                            covariateLevels = num(P(sim)$complexityLevels), nReplicates = P(sim)$nReplicates, testFraction = P(sim)$testFraction,
                            firstForecastYear = if (is.null(sim$networkSettings$lastPilotYear) || is.na(sim$networkSettings$lastPilotYear)) -Inf else sim$networkSettings$lastPilotYear + 1,
                            blockKm = bk, bufferKm = bf, nFolds = P(sim)$nFolds, regimes = trimws(strsplit(P(sim)$regimes, ",")[[1]]),
                            minTrain = P(sim)$minTrain, minVal = P(sim)$minVal, minTest = P(sim)$minTest, minTrainFraction = P(sim)$minTrainFraction,
                            registryPath = file.path(outDir, "seedRegistry.csv"))
        lp <- sim$networkSettings$lastPilotYear   # strict (forward) pilot only: it must never have seen a forecast year
        if (!is.null(lp) && !is.na(lp) && min(des$plan$testYear) <= lp) stop("The design contains forecast year ", min(des$plan$testYear), " but the network pilot used years up to ", lp, ". Re-run preValNN_Global with a smaller lastPilotYear.")
        writeManifests(des$bundles, manifestDir)
        data.table::fwrite(des$plan, planPath); data.table::fwrite(des$splitSummary, file.path(outDir, "splitSummary.csv"))
        data.table::fwrite(des$checks, file.path(outDir, "splitChecks.csv"))
        if (!is.null(des$skipped)) data.table::fwrite(des$skipped, file.path(outDir, "splitsSkipped.csv"))
        saveRDS(list(spatial = des$spatial, params = P(sim), blockKm = bk, bufferKm = bf), file.path(outDir, "designParams.rds"))
        saveRDS(idx, file.path(outDir, "caseIndex.rds"))
        au <- auditManifests(idx, des$plan, manifestDir, des$spatial); data.table::fwrite(au, file.path(outDir, "splitChecks_reaudit.csv"))
        sim$experimentPlan <- des$plan
      }
      if (!P(sim)$modComplex %in% c("all", as.character(sim$experimentPlan$numberOfCovariates)))
        stop("modComplex = ", P(sim)$modComplex, ". Available: all, ", paste(unique(sim$experimentPlan$numberOfCovariates), collapse = ", "))
    },
    trainExperiment = {
      sp <- P(sim)$species; tag <- paste0(if (nzchar(P(sim)$ladderTag)) paste0("_", P(sim)$ladderTag) else "", if (nzchar(P(sim)$runTag)) paste0("_", P(sim)$runTag) else "")
      root <- file.path(outputPath(sim), sp); outDir <- file.path(root, paste0("run", tag))
      slice <- if (anyNA(P(sim)$runSlice)) NULL else P(sim)$runSlice
      tab <- data.table::as.data.table(sim$finalTable); idx <- readRDS(file.path(outDir, "caseIndex.rds"))
      rk <- data.table::fread(file.path(root, P(sim)$rankingFile))
      feats <- rk$Feature
      store <- buildCaseStore(tab, idx, feats, P(sim)$response)
      device <- if (isTRUE(P(sim)$useGPU) && torch::cuda_is_available()) "cuda" else "cpu"
      torch::torch_set_num_threads(as.integer(P(sim)$torchThreads))
      hyper <- sim$networkSettings; hyper$epochs <- P(sim)$epoch
      adapter <- getAdapter(P(sim)$responseType)
      modelDir <- file.path(outDir, "testedModels"); dir.create(file.path(modelDir, "errors"), recursive = TRUE, showWarnings = FALSE)
      work <- data.table::copy(sim$experimentPlan)
      if (P(sim)$modComplex != "all") work <- work[numberOfCovariates == as.numeric(P(sim)$modComplex)]
      data.table::setorder(work, -nTrainS, modelName)
      if (isTRUE(P(sim)$onlyMissing)) work <- work[!file.exists(file.path(modelDir, paste0(modelName, "_finalDT.csv")))]
      if (!is.null(slice)) work <- work[(seq_len(.N) - 1L) %% slice[2] == (slice[1] - 1L)]
      message(sprintf("This process runs %d of %d models (device %s).", nrow(work), nrow(sim$experimentPlan), device))
      manifests <- new.env(); errs <- list()
      for (i in seq_len(nrow(work))) {
        pr <- work[i]; key <- paste(pr$splitId, pr$typeValidation)
        if (is.null(manifests[[key]])) manifests[[key]] <- readManifest(file.path(outDir, "splits"), pr$splitId, pr$typeValidation)
        ff <- featuresForLevel(rk, pr$numberOfCovariates)
        message(sprintf("[%d/%d] %s", i, nrow(work), pr$modelName))
        res <- tryCatch(runModel(store, manifests[[key]], pr, ff, adapter, hyper, modelDir, reRun = P(sim)$reRunModels, device = device,
                                 zClip = P(sim)$zClip, earlyStopPatience = P(sim)$earlyStopPatience), error = function(e) e)
        if (inherits(res, "error")) {
          writeLines(c(pr$modelName, conditionMessage(res)), file.path(modelDir, "errors", paste0(pr$modelName, ".txt")))
          errs[[length(errs) + 1L]] <- data.table::data.table(modelName = pr$modelName, error = conditionMessage(res))
          message("!!! FAILED: ", pr$modelName, ": ", conditionMessage(res))
        }
        gc(verbose = FALSE)
      }
      if (length(errs) && P(sim)$stopOnError) stop(length(errs), " model(s) failed (see ", file.path(modelDir, "errors"), ")")
    },
    compareExperiment = {
      sp <- P(sim)$species; tag <- paste0(if (nzchar(P(sim)$ladderTag)) paste0("_", P(sim)$ladderTag) else "", if (nzchar(P(sim)$runTag)) paste0("_", P(sim)$runTag) else "")
      root <- file.path(outputPath(sim), sp); outDir <- file.path(root, paste0("run", tag))
      plan <- data.table::fread(file.path(outDir, "experimentPlan.csv"))
      done <- list.files(file.path(outDir, "testedModels"), pattern = "_finalDT\\.csv$")
      missing <- setdiff(paste0(plan$modelName, "_finalDT.csv"), done)
      if (length(missing)) { writeLines(missing, file.path(outDir, "modelsMissing.txt")); stop(length(missing), " models have no result (see modelsMissing.txt).") }
      dp <- readRDS(file.path(outDir, "designParams.rds")); idx <- readRDS(file.path(outDir, "caseIndex.rds"))
      au <- auditManifests(idx, plan, file.path(outDir, "splits"), dp$spatial); data.table::fwrite(au, file.path(outDir, "splitChecks_final.csv"))
      rk <- data.table::fread(file.path(root, P(sim)$rankingFile))
      sim$modelComparisons <- analyzeRegimes(file.path(outDir, "testedModels"), file.path(outDir, "analysis"), nCovariatesAll = nrow(rk))
      tab <- data.table::as.data.table(sim$finalTable)
      cs <- coefficientStability(tab[tab[[P(sim)$yearCol]] <= P(sim)$endYear], P(sim)$response, featuresForLevel(rk, 10), P(sim)$yearCol)
      if (!is.null(cs)) data.table::fwrite(cs, file.path(outDir, "analysis", "coefficientStability_top10.csv"))
    },
    warning(noEventWarning(sim)))
  invisible(sim)
}

.inputObjects <- function(sim) {
  if (P(sim)$stage %in% c("all", "design", "train") && !suppliedElsewhere("finalTable", sim = sim)) stop("preValNN: finalTable is not supplied (run preValPrep_* first).")
  if (!suppliedElsewhere("networkSettings", sim = sim)) {
    f <- file.path(outputPath(sim), P(sim)$species, "networkSettings.rds")
    if (!file.exists(f)) stop("networkSettings not supplied and ", f, " not found. Run preValNN_Global first.")
    sim$networkSettings <- readRDS(f)
  }
  invisible(sim)
}
