# Post-analysis of the corrected birds experiment, run on EVE next to the results (one species per call; parts chosen by PREVAL_POST):
#   routes          PreVal on UNSEEN ROUTES (regime FutureUnseenGrouped) against the other regimes of the same-places and new-places runs
#   arms            cross-arm comparisons from the analysis folders: refit against the main run, two-year validation against one year, route information off against on (matched splits)
#   nov:<runfolder> covariate-novelty (DI) analysis of the plan (amendments 27 and 29) for one run folder, e.g. nov:run (same places) or nov:run_spatialTest (new places)
# Settings: PREVAL_SPECIES, PREVAL_OUT (default outputs/birds_v4), PREVAL_FINAL (default data_birds/v2), PREVAL_POST (default all parts).
# Outputs: <out>/<species>/analysis_extra/<comparison>/*.csv and <out>/<species>/<runfolder>/analysis_novelty/*.csv. Nothing existing is modified.
if (nzchar(Sys.getenv("LOCALAPPDATA"))) .libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))
if (nzchar(Sys.getenv("SLURM_JOB_ID"))) {          # cluster job: the project library made by the one-time install (as runMe_birds.R does)
  libs <- Sys.glob(file.path(path.expand("~"), ".local", "share", "R", "PreVal", "packages", "*", "*")); if (length(libs)) .libPaths(c(libs, .libPaths()))
  birdLibs <- Sys.glob(file.path(path.expand("~"), ".local", "share", "R", "birdMonitor", "packages", "*", "*")); if (length(birdLibs)) .libPaths(c(.libPaths(), birdLibs))
}
suppressMessages(library(data.table))
for (f in list.files("modules/preValNN/R", pattern = "[.]R$", full.names = TRUE)) source(f)
envChr <- function(name, default) { v <- Sys.getenv(name, ""); if (nzchar(v)) v else default }
sp <- envChr("PREVAL_SPECIES", ""); if (!nzchar(sp)) stop("set PREVAL_SPECIES")
out <- envChr("PREVAL_OUT", "outputs/birds_v4"); finalDir <- envChr("PREVAL_FINAL", "data_birds/v2")
parts <- trimws(strsplit(envChr("PREVAL_POST", "arms,nov:run,nov:run_spatialTest"), ",")[[1]])
root <- file.path(out, sp); ex <- file.path(root, "analysis_extra"); dir.create(ex, recursive = TRUE, showWarnings = FALSE)
logf <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")
logf("post-analysis", sp, "| parts:", paste(parts, collapse = ", "), "| out:", out)

if ("arms" %in% parts) {
  A <- file.path(root, "run", "analysis")
  cons <- c("H2_PreVal_minus_Tainted", "P3_PreVal_minus_Spatial", "P3_Spatial_minus_Tainted", "P4_Spatial_minus_PreVal", "P6_Combined_minus_PreVal", "P6_Combined_minus_Spatial", "P6_Combined_minus_Tainted")
  rd <- file.path(root, "run_refit", "analysis")
  if (file.exists(file.path(rd, "allModels.csv"))) { r <- compareRefit(A, rd, file.path(ex, "refit")); logf("refit comparison:", nrow(r), "rows") } else logf("NOTE: no refit analysis found")
  for (arm in c("val2", "none")) { d <- file.path(root, paste0("run_", arm), "analysis")
    if (file.exists(file.path(d, "pairedContrasts_perSplit.csv"))) { r <- compareValidationArms(A, d, file.path(ex, arm), contrasts = cons); logf(arm, "against the main run:", nrow(r), "contrasts") } else logf("NOTE: no analysis for", arm) }
}

if ("routes" %in% parts) {          # PreVal on UNSEEN ROUTES (regime FutureUnseenGrouped, trained in run_routes / run_routesNew) against the regimes of the same-places / new-places runs
  for (pr in list(c("run_routes", "run", "routes_samePlaces"), c("run_routesNew", "run_spatialTest", "routes_newPlaces"))) {
    a <- file.path(root, pr[1], "analysis"); b <- file.path(root, pr[2], "analysis")
    if (file.exists(file.path(a, "allModels.csv"))) { r <- compareAddedRegime(b, a, file.path(ex, pr[3])); logf("unseen routes", pr[3], ":", nrow(r), "rows") } else logf("NOTE: no analysis found for", pr[1])
  }
}

for (p in grep("^nov:", parts, value = TRUE)) {
  run <- sub("^nov:", "", p); d <- file.path(root, run); od <- file.path(d, "analysis_novelty"); dir.create(od, showWarnings = FALSE)
  tab <- readRDS(file.path(finalDir, sprintf("final_%s.rds", sp))); logf("novelty:", run, "- computing DI and reading the per-case files (levels 10 and 50)")
  cs <- noveltyCases(d, tab, levels = c(10, 50)); logf("novelty: ", nrow(cs), "case rows;", uniqueN(cs$splitId), "splits")
  st <- noveltyStats(cs); for (nm in names(st)) fwrite(st[[nm]], file.path(od, paste0("novelty_", nm, ".csv")))
  writeLines(c("Covariate novelty (DI), plan amendments 27 and 29.",
    "DI = distance from a test case to its nearest REFERENCE case (PreVal's training cases of that split), in the standardised covariates of the level weighted by the window's permutation importance, divided by the mean nearest-neighbour distance among reference cases (after Meyer and Pebesma 2021).",
    "Terciles of DI over all test cases of this run folder and level. Per-case losses are the calibrated ones. Unit of inference = the forecast year (estimate, t interval, nPositive/nNegative years, exact sign-flip p).",
    "tercile_means: mean loss by regime and tercile; tercile_contrasts: regime differences within terciles (first minus second: negative = the first is better); slope_on_logDI: per-year slope of the loss difference on log(DI) (positive = the first regime gets relatively worse as cases become more novel)."), file.path(od, "README.txt"))
  logf("novelty:", run, "done ->", od)
}
logf("finished", sp)
