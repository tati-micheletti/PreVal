# Local end-to-end test of the SpaDES route (the same entry point EVE uses), on a SMALL subset of the real table:
# stage prep -> two train tasks -> analyze. Takes a few minutes. Needs SpaDES.core, data.table, torch.
#   Rscript tests/run_spades_stages_local.R <path to extractedFeatures_*.csv> [output folder]
args <- commandArgs(trailingOnly = TRUE)
tbl <- args[1]; out <- if (length(args) >= 2) args[2] else file.path(tempdir(), "spadesTest")
if (is.na(tbl) || !file.exists(tbl)) stop("give the path to the extracted-features table")
fileArg <- grep("--file=", commandArgs(FALSE), value = TRUE)
root <- normalizePath(file.path(dirname(normalizePath(sub("--file=", "", fileArg[1]))), ".."))
rscript <- file.path(R.home("bin"), "Rscript")
base <- c(PREVAL_ROOT = root, PREVAL_OUT = out, PREVAL_TABLE = tbl, PREVAL_EPOCHS = "2", PREVAL_GLOBAL_EPOCHS = "2",
          PREVAL_START_YEAR = "2013", PREVAL_END_YEAR = "2016", PREVAL_COMPLEXITY = "2,Inf",
          PREVAL_SPATIAL_YEARS = "2016", PREVAL_STRATA_FRACTION = "0.25", PREVAL_LIB = Sys.getenv("PREVAL_LIB"))
run <- function(stage, extra = character()) {
  env <- c(base, PREVAL_STAGE = stage, extra)
  cat(sprintf("\n=== stage %s %s ===\n", stage, paste(names(extra), extra, sep = "=", collapse = " ")))
  do.call(Sys.setenv, as.list(env))   # the child process inherits these (values may contain spaces)
  st <- system2(rscript, c("--vanilla", shQuote(file.path(root, "tests", "spades_driver_local.R"))))
  if (st != 0) stop("stage ", stage, " failed")
}
run("prep")
n <- nrow(data.table::fread(file.path(out, "experimentPlan.csv")))
cat("planned models:", n, "\n")
run("train", c(SLURM_ARRAY_TASK_ID = "1", SLURM_ARRAY_TASK_COUNT = "2"))
run("train", c(SLURM_ARRAY_TASK_ID = "2", SLURM_ARRAY_TASK_COUNT = "2"))
run("analyze")
# feature-set arm (re-ordered / ablated covariate sets) on the finished design: design step, two train tasks, analysis
arm <- c(PREVAL_FEATURE_SETS = "1", PREVAL_FS_SETS = "habitatOnly,randomA", PREVAL_FS_LEVELS = "2,5")
run("prep", arm)
nArm <- nrow(data.table::fread(file.path(out, "experimentPlan_featureSets.csv")))
cat("feature-set arm models:", nArm, "\n")
run("train", c(arm, SLURM_ARRAY_TASK_ID = "1", SLURM_ARRAY_TASK_COUNT = "2"))
run("train", c(arm, SLURM_ARRAY_TASK_ID = "2", SLURM_ARRAY_TASK_COUNT = "2"))
run("analyze", arm)
needArm <- c("featureSets.csv", "experimentPlan_featureSets.csv", "analysis_featureSets/FS_penalty.csv")
okArm <- file.exists(file.path(out, needArm))
print(data.frame(file = needArm, exists = okArm))
doneArm <- length(list.files(file.path(out, "testedModels_featureSets"), pattern = "_finalDT\\.csv$"))
cat(sprintf("feature-set models with results: %d of %d\n", doneArm, nArm))
if (!all(okArm) || doneArm != nArm) { cat("SPADES STAGES TEST (feature-set arm): FAILED\n"); quit(status = 1) }
need <- c("experimentPlan.csv", "splitChecks_final.csv", "seedRegistry.csv", "featureTable.csv", "analysis/H1_forecast_vs_reference.csv")
ok <- file.exists(file.path(out, need))
print(data.frame(file = need, exists = ok))
done <- length(list.files(file.path(out, "testedModels"), pattern = "_finalDT\\.csv$"))
cat(sprintf("models with results: %d of %d\n", done, n))
if (!all(ok) || done != n) { cat("SPADES STAGES TEST: FAILED\n"); quit(status = 1) }
cat("SPADES STAGES TEST: ALL OK\n")
