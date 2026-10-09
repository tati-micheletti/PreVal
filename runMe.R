getOrUpdatePkg <- function(p, minVer = "0") {
  if (!isFALSE(try(packageVersion(p) < minVer, silent = TRUE) )) {
    repo <- c("predictiveecology.r-universe.dev", getOption("repos"))
    install.packages(p, repos = repo)
  }
}
# REFIT (same pattern as birdMonitor): packages are installed ONCE from an EVE login node with
#   PREVAL_ON_EVE=1 PREVAL_INSTALL_ONLY=1 Rscript runMe.R
# into the project library that setupProject() creates (~/.local/share/R/PreVal/packages/...). A cluster job
# (SLURM_JOB_ID set) installs nothing: it only adds that library to .libPaths() and loads what is there.
installOnly <- Sys.getenv("PREVAL_INSTALL_ONLY") == "1"
isJob <- nzchar(Sys.getenv("SLURM_JOB_ID"))
birdLibs <- Sys.glob(file.path(path.expand("~"), ".local", "share", "R", "birdMonitor", "packages", "*", "*"))
if (isJob) {
  libs <- Sys.glob(file.path(path.expand("~"), ".local", "share", "R", "PreVal", "packages", "*", "*"))
  if (length(libs)) .libPaths(c(libs, .libPaths()))
  # fallback only (searched LAST): packages that exist in birdMonitor's working library but not in PreVal's
  if (length(birdLibs)) .libPaths(c(.libPaths(), birdLibs))
}
# REFIT: inside a cluster job nothing is installed (compute nodes have throttled internet); the one-time install
# (eve/setup_eve.sh, login node) guarantees these versions
if (!isJob) {
  # Non-interactive R (Rscript on a cluster) has no default CRAN mirror, and EVE's system library is read-only
  # (same handling as birdMonitor/runMe.R)
  cranRepo <- getOption("repos")["CRAN"]
  if (is.null(cranRepo) || is.na(cranRepo) || cranRepo == "@CRAN@") options(repos = c(CRAN = "https://cloud.r-project.org"))
  if (!any(file.access(.libPaths(), 2) == 0)) {
    userLib <- Sys.getenv("R_LIBS_USER")
    if (!nzchar(userLib)) userLib <- file.path("~", "R", "library")
    dir.create(userLib, recursive = TRUE, showWarnings = FALSE)
    .libPaths(c(userLib, .libPaths()))
    message("No writable R library found; using ", userLib)
  }
  getOrUpdatePkg("Require", "1.0.1.9020")
  getOrUpdatePkg("SpaDES.project", "0.1.1.9036")
}

################### SETUP

# REFIT: on EVE (inside a SLURM job, or set PREVAL_ON_EVE=1 on the login node) nothing is installed or downloaded
onEVE <- nzchar(Sys.getenv("SLURM_JOB_ID")) || nzchar(Sys.getenv("PREVAL_ON_EVE"))
# REFIT: stage: prep (global ranking + design, once) | train (one SLURM array task) | analyze | all (small tests)
stage <- Sys.getenv("PREVAL_STAGE", "all")
stopifnot(stage %in% c("prep", "train", "analyze", "all"))
envNum <- function(name, default) { v <- Sys.getenv(name, ""); if (nzchar(v)) as.numeric(v) else default }
sliceTask <- if (stage == "train") c(envNum("SLURM_ARRAY_TASK_ID", 1), envNum("SLURM_ARRAY_TASK_COUNT", 1)) else c(NA_real_, NA_real_)

if (SpaDES.project::user("tmichele")){ # ON BC
  scratchPath <- Require::checkPath("~/scratch", create = TRUE)
  if (getwd() != "/home/tmichele/projects/PreVal" &&
      getwd() != "/export/home/tmichele/projects/PreVal") setwd("~/projects/PreVal/")
}
if (SpaDES.project::user("Tati")){  # ON MY WINDOWS MACHINE
  scratchPath <- Require::checkPath("scratch", create = TRUE)
}
if (onEVE){  # REFIT: ON EVE (code in ~/projects/PreVal; data and outputs on /work, never in /home)
  workPath <- Sys.getenv("PREVAL_WORK", file.path("/work", Sys.getenv("USER"), "preval"))
  scratchPath <- Require::checkPath(file.path(workPath, "scratch"), create = TRUE)
  if (basename(getwd()) != "PreVal") setwd("~/projects/PreVal/")
}

runName <- "NN"
# centralPoint <- c(64.024641, -122.356419)
# REFIT: where outputs go and where the (read-only) extracted-features table is
outPath <- if (nzchar(Sys.getenv("PREVAL_OUT"))) Sys.getenv("PREVAL_OUT") else file.path("outputs", runName)
tablePath <- if (nzchar(Sys.getenv("PREVAL_TABLE"))) Sys.getenv("PREVAL_TABLE") else
  file.path("outputs", runName, "extractedFeatures_498a1edc8c19988e843def7542411d3e_2007_2022.csv")

out <- SpaDES.project::setupProject(
  Restart = FALSE, # REFIT (as in birdMonitor): Rscript cannot restart R after the first package install
  runName = runName,
  paths = list(projectPath = "PreVal",
               scratchPath = scratchPath,
               outputPath = outPath),
  modules =c(
    # "tati-micheletti/caribouLocPrep@main",
    # 'tati-micheletti/prepTracks@main',
    # 'tati-micheletti/prepLandscape@main',
    # 'tati-micheletti/extractLand@main',
    # REFIT: the refit modules are used from the local modules/ folder (feature/refit-disjoint-sets).
    # GitHub references would download over (and could overwrite) local, uncommitted work.
    if (stage %in% c("prep", "all")) "caribouNN_Global",
    "caribouNN"
  ),
  options = list(future.globals.maxSize = 6000*1024^2,
                 tempdir = scratchPath, # terra::terraOptions
                 spades.allowInitDuringSimInit = TRUE,
                 reproducible.cacheSaveFormat = "rds",
                 gargle_oauth_email = if (user("tmichele")||user("Tati")) "tati.micheletti@gmail.com" else NULL,
                 gargle_oauth_cache = ".secrets",
                 gargle_oauth_client_type = "web", # Without "web", google authentication didn't work when running non-interactively! "installed" should be used in non-server systems
                 use_oob = TRUE, # TRUE
                 repos = "https://cloud.r-project.org",
                 spades.project.fast = FALSE,
                 spades.recoveryMode = 0,
                 spades.useRequire = !isJob, # REFIT: never install packages from inside a cluster job
                 spades.scratchPath = scratchPath,
                 reproducible.gdalwarp = TRUE,
                 reproducible.inputPaths = if (user("tmichele")) "~/data" else paths[["inputPath"]],
                 reproducible.destinationPath = if (user("tmichele")) "~/data" else paths[["outputPath"]],
                 reproducible.useMemoise = TRUE,
                 reproducible.showSimilar =FALSE,
                 terra_default = list(memfrac = 0)
  ),
  times = list(start = 2025,
               end = 2025),
  # authorizeGDrive = googledrive::drive_auth(cache = ".secrets"),
  params = list(
    caribouiSSA = list(
      jurisdiction = "NT"),
    caribouLocPrep = list(
      jurisdiction = "NT",
      herdNT = "Dehcho Boreal Woodland Caribou"),
    prepTracks = if (requireNamespace("amt", quietly = TRUE)) list( # REFIT: amt is not installed yet on a fresh EVE library
      minyr = 2007,
      maxyr = 2022,
      rate = amt::hours(8),
      tolerance = amt::minutes(240),
      probsfilter = 0.9,
      aggrNonAnnualData = "middle"),
    extractLand = list(
      histLandYears = 2007:2022,
      checkExistingExtracted = TRUE,
      hashExtracted = "run01",
      .saveInitialTime = 1),
    prepLandscape = list(
      histLandYears = 2007:2022), # first year of caribou data that matches landcover and 5year interval
    # In theory, should be 2008, not 2007. However, 2008 does not have enough data!
    .globals = list(
      .plots = c("png"),
      .studyAreaName=  "NT",
      jurisdiction = "NT",
      .useCache = c(".inputObjects")
    ),
    caribouNN_Global = list(
      rerunPrepData = FALSE,
      scheduleGlobalRSS = FALSE,
      startYear = 2013,                              # REFIT: 2012 and earlier are not used
      epoch = envNum("PREVAL_GLOBAL_EPOCHS", 100)
    ),
    caribouNN = list(
      stage = c(prep = "design", train = "train", analyze = "analyze", all = "all")[[stage]], # REFIT
      learningRate = envNum("PREVAL_LR", 0.001),     # the value used in the original runs
      epoch = envNum("PREVAL_EPOCHS", 50),
      earlyStopPatience = envNum("PREVAL_PATIENCE", 10),
      useSavedPlan = Sys.getenv("PREVAL_NEW_PLAN") != "1", # REFIT: PREVAL_NEW_PLAN=1 rebuilds the plan (e.g. to add a complexity level); existing models are kept
      complexityLevels = if (nzchar(Sys.getenv("PREVAL_COMPLEXITY"))) as.numeric(strsplit(Sys.getenv("PREVAL_COMPLEXITY"), ",")[[1]]) else c(2, 5, 10, Inf),
      featureSetArm = Sys.getenv("PREVAL_FEATURE_SETS") == "1", # REFIT: follow-up arm with re-ordered/ablated covariate sets (needs a finished design)
      featureSetTag = Sys.getenv("PREVAL_FS_TAG"),
      featureSetNames = if (nzchar(Sys.getenv("PREVAL_FS_SETS"))) Sys.getenv("PREVAL_FS_SETS") else "habitatOnly,habitatFirst,movementFirst,randomA,randomB",
      featureSetLevels = if (nzchar(Sys.getenv("PREVAL_FS_LEVELS"))) Sys.getenv("PREVAL_FS_LEVELS") else "2,5,10,20",
      nReplicates = envNum("PREVAL_REPLICATES", 1), # REFIT: independent network initialisations per cell (same splits)
      onlyMissing = Sys.getenv("PREVAL_ONLY_MISSING") == "1", # REFIT: mop-up of models without a result
      extendFrom = envNum("PREVAL_EXTEND_FROM", NA), # REFIT: re-train models that stopped at this epoch cap (with PREVAL_EPOCHS larger)
      startYear = 2013,
      runSlice = sliceTask,                          # REFIT: one SLURM array task = one slice of the models
      torchThreads = envNum("PREVAL_THREADS", 1),
      modComplex = "all")
  ),
  packages = if (isJob) NULL else if (onEVE) c(  # REFIT: on EVE only what the refit modules need (amt needs gmp/gsl, unused here)
               "PredictiveEcology/SpaDES.core@development") else c("terra", "purrr", "amt",
               "PredictiveEcology/SpaDES.core@development"# REFIT: was @box; @development is what birdMonitor installs on EVE and what the refit modules were tested with (SpaDES.core 3.2.x)
  ),
  useGit = FALSE, # REFIT: modules are local (see above)
  loadOrder = c(
    # "caribouLocPrep",
    # "prepTracks",
    # "prepLandscape",
    # "extractLand",
    if (stage %in% c("prep", "all")) "caribouNN_Global",
    "caribouNN"
  ),
  # REFIT: the table is only needed (and read, never written) by the prep stage
  objects = if (stage %in% c("prep", "all") && !installOnly) list(extractedVariables = data.table::fread(tablePath)) else list() # SHORTCUTTING!!!
  # The right way would be to generate the table + run the model. However: the name extractedVariables in
  # the iSSA is actually extractedLand produced by extractLand module. This needs fixing (i.e., maybe
  # just providing a synonym?)
)

# REFIT: PREVAL_INSTALL_ONLY=1: setupProject() above has just installed the packages of runMe.R and of the modules.
# Add libtorch (CPU), check, and stop without running anything (run once from an EVE login node).
if (installOnly) {
  pe <- "https://predictiveecology.r-universe.dev"
  needed <- c("data.table", "torch", "SpaDES.core", "SpaDES.project", "reproducible")
  # setupProject() can skip packages or fail on one of them without stopping (same as in birdMonitor/runMe.R):
  # install whatever is still missing here, with visible errors
  missing <- Filter(function(p) !requireNamespace(p, quietly = TRUE), needed)
  if (length(missing)) {
    message("Installing packages that setupProject() did not install: ", paste(missing, collapse = ", "))
    for (p in missing) tryCatch(install.packages(p, repos = c(PE = pe, CRAN = "https://cloud.r-project.org")),
                                error = function(e) message("install.packages(", p, ") failed: ", conditionMessage(e)))
  }
  if (requireNamespace("torch", quietly = TRUE) && !torch::torch_is_installed()) torch::install_torch()
  stillMissing <- Filter(function(p) !requireNamespace(p, quietly = TRUE), needed)
  if (length(stillMissing)) {
    for (p in stillMissing) message("WHY ", p, " cannot be loaded: ",
                                    tryCatch({ loadNamespace(p); "no error?" }, error = function(e) conditionMessage(e)))
    if (length(birdLibs)) {
      message("Trying birdMonitor's library as a fallback (searched last): ", paste(birdLibs, collapse = " | "))
      .libPaths(c(.libPaths(), birdLibs))
      stillMissing <- Filter(function(p) !requireNamespace(p, quietly = TRUE), needed)
    }
  }
  if (length(stillMissing)) stop("Could not install: ", paste(stillMissing, collapse = ", "),
                                 ". Library paths: ", paste(.libPaths(), collapse = " | "))
  message("PREVAL_INSTALL_ONLY=1: packages and libtorch are installed (torch ", as.character(packageVersion("torch")),
          ", SpaDES.core ", as.character(packageVersion("SpaDES.core")), " from ", find.package("SpaDES.core"),
          "). Nothing was run.")
  quit(save = "no", status = 0)
}

if (!onEVE) { # REFIT: needs the internet; not used by the refit modules anyway
  source("https://raw.githubusercontent.com/tati-micheletti/PreVal/refs/heads/main/R/checkMovebankCredentials.R")
  checkMovebankCredentials(out)
}

# a<-SpaDES.core::restartSpades()

finalResults <- SpaDES.core::simInitAndSpades2(out)
