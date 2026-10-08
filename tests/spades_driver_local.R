# TEST-ONLY SpaDES driver (uses SpaDES.core directly; SpaDES.project is not installed on the test PC). The real entry point is runMe.R.
#
# Stage is chosen with the environment variable PREVAL_STAGE:
#   prep     caribouNN_Global (prepare data, global ranking model, ranking) + caribouNN stage = "design"
#            (disjoint splits, shared test sets, spatial arm, verification gate, tensor store). Run ONCE.
#   train    caribouNN stage = "train": one slice of the models. On SLURM, the slice comes from the array
#            (SLURM_ARRAY_TASK_ID / SLURM_ARRAY_TASK_COUNT). Safe to resubmit: finished models are skipped.
#   analyze  caribouNN stage = "analyze": completeness check, re-audit of the splits, pre-specified analysis.
#   all      everything in one session (small tests only)
#
# Environment variables (all optional except PREVAL_OUT and, for prep/all, PREVAL_TABLE):
#   PREVAL_ROOT    repository root (default: folder of this file)
#   PREVAL_TABLE   extractedFeatures_*.csv (read only)
#   PREVAL_OUT     output folder (everything is written here)
#   PREVAL_LIB     extra R library folder (torch etc.)
#   PREVAL_EPOCHS (50)  PREVAL_LR (0.001)  PREVAL_PATIENCE (10)  PREVAL_THREADS (1)  PREVAL_GPU (unset)
#   PREVAL_START_YEAR (2013)  PREVAL_END_YEAR (2022)  PREVAL_COMPLEXITY ("2,5,10,Inf")
#   PREVAL_SPATIAL_YEARS ("2018,2020,2022")   PREVAL_GLOBAL_EPOCHS (100)
#   PREVAL_STRATA_FRACTION (1; <1 keeps a random fraction of strata: tests only)
envv <- function(name, default) { v <- Sys.getenv(name, ""); if (nzchar(v)) v else default }
numv <- function(s) as.numeric(strsplit(s, ",")[[1]])
lib <- envv("PREVAL_LIB", "")
if (nzchar(lib)) .libPaths(c(lib, .libPaths()))

fileArg <- grep("--file=", commandArgs(FALSE), value = TRUE)
here <- if (length(fileArg)) dirname(normalizePath(sub("--file=", "", fileArg[1]))) else getwd()
root <- normalizePath(envv("PREVAL_ROOT", here))
outDir <- envv("PREVAL_OUT", file.path(root, "outputs", "refit"))
dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
stage <- envv("PREVAL_STAGE", "all")
stopifnot(stage %in% c("prep", "train", "analyze", "all"))

suppressMessages({ library(SpaDES.core); library(data.table) })
options(spades.useRequire = FALSE,            # never install packages from inside a job
        spades.moduleCodeChecks = FALSE, spades.recoveryMode = 0, reproducible.useCache = FALSE,
        spades.useRequire = FALSE)

# ---- parameters --------------------------------------------------------------------------------------
slice <- c(NA_real_, NA_real_)
if (stage == "train") {
  slice <- as.numeric(c(envv("SLURM_ARRAY_TASK_ID", "1"), envv("SLURM_ARRAY_TASK_COUNT", "1")))
}
common <- list(
  startYear = as.numeric(envv("PREVAL_START_YEAR", "2013")),
  zClip = 10)
caribouNN <- list(
  stage = switch(stage, prep = "design", train = "train", analyze = "analyze", all = "all"),
  epoch = as.numeric(envv("PREVAL_EPOCHS", "50")),
  learningRate = as.numeric(envv("PREVAL_LR", "0.001")),       # the value used in the original runs
  earlyStopPatience = as.numeric(envv("PREVAL_PATIENCE", "10")),
  startYear = common$startYear, endYear = as.numeric(envv("PREVAL_END_YEAR", "2022")),
  complexityLevels = numv(envv("PREVAL_COMPLEXITY", "2,5,10,Inf")),
  spatialTestYears = numv(envv("PREVAL_SPATIAL_YEARS", "2018,2020,2022")),
  runSlice = slice, torchThreads = as.numeric(envv("PREVAL_THREADS", "1")),
  useGPU = nzchar(Sys.getenv("PREVAL_GPU", "")))
global <- list(startYear = common$startYear, epoch = as.numeric(envv("PREVAL_GLOBAL_EPOCHS", "100")),
               useGPU = nzchar(Sys.getenv("PREVAL_GPU", "")), scheduleGlobalRSS = FALSE)

modules <- if (stage %in% c("prep", "all")) c("caribouNN_Global", "caribouNN") else "caribouNN"
objects <- list()
if (stage %in% c("prep", "all")) {
  tbl <- Sys.getenv("PREVAL_TABLE")
  if (!file.exists(tbl)) stop("PREVAL_TABLE not found: ", tbl)
  message("Reading the table (read only): ", tbl)
  ev <- fread(tbl)
  frac <- as.numeric(envv("PREVAL_STRATA_FRACTION", "1"))
  if (frac < 1) {                                   # tests only: keep a random fraction of whole strata
    ids <- unique(ev$indiv_step_id); set.seed(1)
    ev <- ev[indiv_step_id %in% sample(ids, floor(length(ids) * frac))]
  }
  objects$extractedVariables <- ev
}

mySim <- simInit(
  times = list(start = 1, end = 1),
  modules = modules,
  params = list(caribouNN = caribouNN, caribouNN_Global = global),
  objects = objects,
  paths = list(modulePath = file.path(root, "modules"), inputPath = file.path(outDir, "inputs"),
               outputPath = outDir, cachePath = file.path(outDir, "cache")),
  loadOrder = modules)
simOut <- spades(mySim, debug = FALSE)
message("Stage '", stage, "' finished.")
if (nzchar(Sys.getenv("SLURM_JOB_ID"))) {
  mem <- tryCatch(as.numeric(gsub("[^0-9]", "", grep("^VmHWM", readLines("/proc/self/status"), value = TRUE))) / 1024,
                  error = function(e) NA)
  message("Peak memory (MB): ", round(mem))
}
