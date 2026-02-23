
print("RUNNING 5 COVARIATE MODELS...")

getOrUpdatePkg <- function(p, minVer = "0") {
  if (!isFALSE(try(packageVersion(p) < minVer, silent = TRUE) )) {
    repo <- c("predictiveecology.r-universe.dev", getOption("repos"))
    install.packages(p, repos = repo)
  }
}
getOrUpdatePkg("Require", "1.0.1.9020")
getOrUpdatePkg("SpaDES.project", "0.1.1.9036")

################### SETUP

if (SpaDES.project::user("tmichele")){ # ON BC
  scratchPath <- Require::checkPath("~/scratch", create = TRUE)
  if (getwd() != "/home/tmichele/projects/PreVal") setwd("~/projects/preVal/")
} 
if (SpaDES.project::user("Tati")){  # ON MY WINDOWS MACHINE
  scratchPath <- Require::checkPath("scratch", create = TRUE)
} 

runName <- "NN"
# centralPoint <- c(64.024641, -122.356419)

out <- SpaDES.project::setupProject(
  runName = runName,
  paths = list(projectPath = "PreVal",
               scratchPath = scratchPath,
               outputPath = file.path("outputs", runName)),
  modules =c(
    # "tati-micheletti/caribouLocPrep@main",
    # 'tati-micheletti/prepTracks@main',
    # 'tati-micheletti/prepLandscape@main',
    # 'tati-micheletti/extractLand@main',
    'tati-micheletti/caribouNN_Global@main',
    'tati-micheletti/caribouNN@main'
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
                 spades.useRequire = TRUE,
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
    prepTracks = list(
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
      scheduleGlobalRSS = FALSE
    ),
    caribouNN = list(
      learningRate = 0.001,
      useSavedPlan = TRUE,
      maxClu = Inf,
      modComplex = 5,
      useFuture = TRUE) # If any changes in experiment or capping is done, this needs to be FALSE
  ),
  packages = c("terra", "purrr", "amt",
               "PredictiveEcology/SpaDES.core@box"# # OLDER VERSIONS: 2.1.5.9022 # (>= 2.1.6.9002)
  ),
  useGit = "both",
  loadOrder = c(
    "caribouNN_Global",
    "caribouNN"
  ),
  objects = list(extractedVariables = data.table::fread(file.path("outputs", runName, "extractedFeatures_498a1edc8c19988e843def7542411d3e_2007_2022.csv"))) # SHORTCUTTING!!! 
)

source("https://raw.githubusercontent.com/tati-micheletti/PreVal/refs/heads/main/R/checkMovebankCredentials.R")
checkMovebankCredentials(out)

# a<-SpaDES.core::restartSpades()

finalResults <- SpaDES.core::simInitAndSpades2(out)
