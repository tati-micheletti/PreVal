defineModule(sim, list(
  name = "preValNN_Global",
  description = paste0("Per species (or dataset): network settings by a pilot on the pre-test history, covariate ranking by permutation ",
                       "importance (correlated covariates grouped), spatial block size from residual autocorrelation. Uses the generic ",
                       "engine in the preValNN module (R/ folder of preValNN is sourced; nothing is duplicated)."),
  keywords = c("validation", "neural network", "permutation importance", "forecasting"),
  authors = structure(list(list(given = "Tati", family = "Micheletti", role = c("aut", "cre"), email = "tati.micheletti@gmail.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(preValNN_Global = "0.1.0"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list("README.md"),
  reqdPkgs = list("SpaDES.core (>= 3.0.4)", "data.table", "torch", "MASS", "sf", "blockCV"),
  parameters = bindrows(
    defineParameter(".plots", "character", "screen", NA, NA, "Used by Plots function"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should caching of events or module be used?"),
    defineParameter("species", "character", NA_character_, NA, NA, "File stem of the species, e.g. 'Chloris_chloris'. Outputs go to <outputPath>/<species>/."),
    defineParameter("response", "character", "Reviere", NA, NA, "Response column of the final table."),
    defineParameter("responseType", "character", "count", NA, NA, "Adapter: 'count' (negative binomial) or 'continuous'."),
    defineParameter("nonFeatureCols", "character", "SITE_ID,year,Reviere,x,y", NA, NA, "Comma-separated columns of the final table that are NOT covariates."),
    defineParameter("unitCol", "character", "SITE_ID", NA, NA, "Column of the unit that repeats across years (route)."),
    defineParameter("yearCol", "character", "year", NA, NA, "Year column."),
    defineParameter("crs", "numeric", 3035, NA, NA, "EPSG code of the x/y coordinates (projected, metres)."),
    defineParameter("pilotMode", "character", "random", NA, NA, "random: the pilot (and the block size) use all years with a random hold-out, as the ranking does; forward: only years <= lastPilotYear (strict, never sees a forecast year)."),
    defineParameter("lastPilotYear", "numeric", NA_real_, NA, NA, "Only for pilotMode = forward: last pre-test year (the year before the first forecast year of the design)."),
    defineParameter("pilotEpochs", "numeric", 100, 10, 500, "Epoch cap in the pilot."),
    defineParameter("idMode", "character", "intercept", NA, NA, paste0("Route information in the network, FIXED by design: intercept (one shrunk number per route added to the log-mean; analogue of a random intercept), none (covariates only) or concat (8-d route embedding fed with the covariates).")),
    defineParameter("rankEpochs", "numeric", 100, 10, 500, "Epoch cap of the global ranking model."),
    defineParameter("rankingModes", "character", "random,forward", NA, NA, "Comma-separated ranking modes to compute; the first is the primary ladder."),
    defineParameter("rankYearsForward", "numeric", NA_real_, NA, NA, "Forward-mode ranking uses years <= this (default: lastPilotYear = strictly pre-test)."),
    defineParameter("nRepImportance", "numeric", 5, 1, 50, "Permutation repeats per covariate group."),
    defineParameter("groupR", "numeric", 0.9, 0.5, 1, "|r| above which covariates are permuted together."),
    defineParameter("globalSeed", "numeric", 20261017, NA, NA, "Seed of the pilot and the ranking model."),
    defineParameter("zClip", "numeric", 10, 1, Inf, "Standardised covariates are clipped to +/- zClip."),
    defineParameter("useGPU", "logical", FALSE, NA, NA, "Use a GPU if available."),
    defineParameter("rerun", "logical", FALSE, NA, NA, "Recompute even if the outputs exist.")
  ),
  inputObjects = bindrows(
    expectsInput("finalTable", "data.table", "Final table for modeling (one row per unit and year), made by preValPrep_*", sourceURL = NA),
    expectsInput("enginePath", "character", "Folder with the engine R files (default: <modules>/preValNN/R)", sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput("featurePriority", "data.table", "Covariates ordered by importance (primary ladder)"),
    createsOutput("networkSettings", "list", "Fixed network settings (hyper) chosen by the pilot"),
    createsOutput("blockKm", "numeric", "Spatial block size (km) from the residual autocorrelation range")
  )
))

doEvent.preValNN_Global <- function(sim, eventTime, eventType) {
  switch(eventType,
    init = { sim <- scheduleEvent(sim, time(sim), "preValNN_Global", "rank") },
    rank = {
      eng <- if (is.null(sim$enginePath)) file.path(modulePath(sim), "preValNN", "R") else sim$enginePath
      env <- environment(doEvent.preValNN_Global)
      for (f in list.files(eng, pattern = "[.]R$", full.names = TRUE)) sys.source(f, envir = env)
      sp <- P(sim)$species; if (is.na(sp)) stop("preValNN_Global: parameter 'species' is required")
      out <- file.path(outputPath(sim), sp); dir.create(out, recursive = TRUE, showWarnings = FALSE)
      done <- file.path(out, c("featureTable.csv", "networkSettings.rds", "blockSize.rds"))
      if (!P(sim)$rerun && all(file.exists(done))) {
        message("preValNN_Global: outputs for ", sp, " exist; loading (rerun = TRUE recomputes).")
        sim$featurePriority <- data.table::fread(done[1]); sim$networkSettings <- readRDS(done[2]); sim$blockKm <- readRDS(done[3])$blockKm
        return(invisible(sim))
      }
      tab <- data.table::as.data.table(sim$finalTable)
      nonF <- trimws(strsplit(P(sim)$nonFeatureCols, ",")[[1]])
      feats <- setdiff(names(tab), nonF)
      const <- feats[vapply(feats, function(v) { s <- stats::sd(tab[[v]]); is.na(s) || s == 0 }, logical(1))]
      if (length(const)) { message("Dropping constant covariates: ", paste(const, collapse = ", ")); feats <- setdiff(feats, const) }
      idx <- buildCaseIndex(tab, P(sim)$unitCol, P(sim)$yearCol, "x", "y")
      store <- buildCaseStore(tab, idx, feats, P(sim)$response)
      adapter <- getAdapter(P(sim)$responseType)
      device <- if (isTRUE(P(sim)$useGPU) && torch::cuda_is_available()) "cuda" else "cpu"
      lastPilot <- P(sim)$lastPilotYear; if (P(sim)$pilotMode == "forward" && is.na(lastPilot)) stop("preValNN_Global: pilotMode = forward needs lastPilotYear")
      seed <- as.integer(P(sim)$globalSeed)
      registerSeeds(file.path(out, "seedRegistry.csv"), "preValNN_Global", "globalSeed", seed, "pilot and ranking model seeds (+1/+2 internal)")

      # 1. network settings: pilot on the pre-test history only
      message("preValNN_Global [", sp, "]: network pilot (", P(sim)$pilotMode, ") ...")
      pil <- pilotNetwork(store, feats, adapter, lastPilotYear = lastPilot, mode = P(sim)$pilotMode, seed = seed, epochs = P(sim)$pilotEpochs, device = device, zClip = P(sim)$zClip, idMode = P(sim)$idMode)
      data.table::fwrite(pil$table, file.path(out, "networkPilot.csv"))
      hyper <- pil$best; hyper$epochs <- P(sim)$rankEpochs
      saveRDS(c(hyper, list(idMode = P(sim)$idMode, pilotMode = P(sim)$pilotMode, lastPilotYear = lastPilot, validationYear = pil$validationYear, nTrain = pil$nTrain, nVal = pil$nVal)), file.path(out, "networkSettings.rds"))

      # 2. ranking(s): primary = first mode
      modes <- trimws(strsplit(P(sim)$rankingModes, ",")[[1]]); rks <- list()
      for (m in modes) {
        message("preValNN_Global [", sp, "]: ranking, mode = ", m, " ...")
        ry <- if (m == "forward") { fy <- if (is.na(P(sim)$rankYearsForward)) lastPilot else P(sim)$rankYearsForward; if (is.na(fy)) NULL else sort(unique(idx$year[idx$year <= fy])) } else NULL   # forward: train on the earlier 60% of the years, hold out the later ones
        rk <- rankCovariates(store, feats, adapter, hyper, seed = seed + 1L, mode = m, rankYears = ry, nRep = P(sim)$nRepImportance,
                             groupR = P(sim)$groupR, device = device, zClip = P(sim)$zClip)
        rk[, mode := m]; rk[, baselineLoss := attr(rk, "baselineLoss")]
        rks[[m]] <- rk
        data.table::fwrite(rk, file.path(out, if (m == modes[1]) "featureTable.csv" else sprintf("featureTable_%s.csv", m)))
      }
      prim <- rks[[1]]
      # effective number of covariates at each ladder level (principal components for 90% of the variance, on pre-test rows)
      ev <- data.table::rbindlist(lapply(c(2, 5, 10, 15, 25, 35, 50, Inf), function(k) {
        ff <- featuresForLevel(prim, k); data.table::data.table(level = k, nominal = length(ff), effective90 = effectiveNumber(tab[, ..ff])) }))
      data.table::fwrite(ev, file.path(out, "effectiveNumberOfCovariates.csv"))

      # 3. block size from the residual spatial autocorrelation of a baseline model (pre-test rows only)
      pre <- if (is.na(lastPilot)) tab else tab[tab[[P(sim)$yearCol]] <= lastPilot]
      bs <- blockSizeFromAutocorrelation(pre, P(sim)$response, featuresForLevel(prim, 10), unitCol = P(sim)$unitCol, yearCol = P(sim)$yearCol,
                                         crs = P(sim)$crs, family = if (P(sim)$responseType == "count") "nb" else "gaussian")   # no silent fallback: an error stops the run
      saveRDS(bs, file.path(out, "blockSize.rds"))
      message(sprintf("preValNN_Global [%s]: block size %.0f km (autocorrelation range %.0f km); network %s, route information: %s.", sp, bs$blockKm, bs$rangeKm,
                      paste(hyper$hidden, collapse = "-"), P(sim)$idMode))
      sim$featurePriority <- prim; sim$networkSettings <- hyper; sim$blockKm <- bs$blockKm
    },
    warning(noEventWarning(sim)))
  invisible(sim)
}

.inputObjects <- function(sim) {
  if (!suppliedElsewhere("finalTable", sim = sim)) stop("preValNN_Global: finalTable is not supplied (run preValPrep_* first).")
  invisible(sim)
}
