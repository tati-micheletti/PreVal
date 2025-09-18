getOrUpdatePkg <- function(p, minVer = "0") {
  if (!isFALSE(try(packageVersion(p) < minVer, silent = TRUE) )) {
    repo <- c("predictiveecology.r-universe.dev", getOption("repos"))
    install.packages(p, repos = repo)
  }
}
getOrUpdatePkg("Require", "1.0.1")
getOrUpdatePkg("SpaDES.project", "0.1.1.9036")

################### SETUP

if (SpaDES.project::user("tmichele")){ # ON BC
  setwd("~/projects/preVal/")
  scratchPath <- Require::checkPath("~/scratch", create = TRUE)
  terra::terraOptions(tempdir = scratchPath)
} 
if (SpaDES.project::user("Tati")){  # ON MY WINDOWS MACHINE
  scratchPath <- Require::checkPath("scratch", create = TRUE)
  terra::terraOptions(tempdir = scratchPath)
} 

runName <- "RSF"

out <- SpaDES.project::setupProject(
  runName = runName,
  paths = list(projectPath = "PreVal",
               scratchPath = scratchPath,
               outputPath = file.path("outputs", runName)),
  modules =c(
    "tati-micheletti/getReadySimulationFiles@main",
    "tati-micheletti/anthroDisturbance_DataPrep@main",
    "tati-micheletti/potentialResourcesNT_DataPrep@main",
    "tati-micheletti/anthroDisturbance_Generator@main"
  ),
  options = list(spades.allowInitDuringSimInit = TRUE,
                 reproducible.cacheSaveFormat = "rds",
                 gargle_oauth_email = if (user("tmichele")) "tati.micheletti@gmail.com" else NULL,
                 gargle_oauth_cache = ".secrets",
                 gargle_oauth_client_type = "web", # Without this, google authentication didn't work when running non-interactively!
                 use_oob = FALSE,
                 repos = "https://cloud.r-project.org",
                 spades.project.fast = FALSE,
                 spades.recoveryMode = 0,
                 spades.scratchPath = scratchPath,
                 reproducible.gdalwarp = TRUE,
                 reproducible.inputPaths = if (user("tmichele")) "~/data" else paths[["inputPath"]],
                 reproducible.destinationPath = if (user("tmichele")) "~/data" else paths[["outputPath"]],
                 reproducible.useMemoise = FALSE
  ),
  times = list(start = 2011,
               end = 2051),
  functions = "tati-micheletti/anthropogenicDisturbance_Demo@main/R/studyAreaMakers.R",
  authorizeGDrive = googledrive::drive_auth(cache = ".secrets"),
  shortProvinceName = shortProvinceName,
  studyArea = reproducible::Cache(studyAreaGenerator, centralPoint = centralPoint, 
                                  totalArea = 10^10, typeArea = "circle"),
  rasterToMatch = reproducible::Cache(rtmGenerator, studyArea = studyArea), 
  params = list(getReadySimulationFiles = list(gDriveFolder = "1lqIjwQQ8CU6l5GJezC9tVgs0Uz0dv-FD", 
                                               climateScenario = climateScenario, 
                                               replicateRun = replicateRun,
                                               lastYearSimulations = times[["end"]],
                                               runInterval = 10),
                anthroDisturbance_Generator = list(.inputFolderFireLayer = paths[["outputPath"]],
                                                   .runName = runName,
                                                   growthStepGenerating = 0.01,
                                                   totalDisturbanceRate = distMod
                )
  ),
  packages = c("googledrive", 'RCurl', 'XML', 'igraph', 'qs', 'usethis',
               "SpaDES.tools",
               "PredictiveEcology/SpaDES.core@box",# 2.1.5.9022
               "PredictiveEcology/reproducible@AI",# 2.1.2.9046
               "PredictiveEcology/Require@development (>= 1.0.1)"),
  useGit = "both",
  loadOrder = c(
    "getReadySimulationFiles",
    "anthroDisturbance_DataPrep", "potentialResourcesNT_DataPrep", "anthroDisturbance_Generator"
  )
)
