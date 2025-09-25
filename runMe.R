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
  setwd("~/projects/preVal/")
  scratchPath <- Require::checkPath("~/scratch", create = TRUE)
  terra::terraOptions(tempdir = scratchPath)
} 
if (SpaDES.project::user("Tati")){  # ON MY WINDOWS MACHINE
  scratchPath <- Require::checkPath("scratch", create = TRUE)
  terra::terraOptions(tempdir = scratchPath)
} 

runName <- "RSF"
centralPoint <- c(64.024641, -122.356419)

out <- SpaDES.project::setupProject(
  runName = runName,
  paths = list(projectPath = "PreVal",
               scratchPath = scratchPath,
               outputPath = file.path("outputs", runName)),
  modules =c(
    "tati-micheletti/caribouLocPrep@main"
  ),
  options = list(spades.allowInitDuringSimInit = TRUE,
                 reproducible.cacheSaveFormat = "rds",
                 gargle_oauth_email = if (user("tmichele")||user("Tati")) "tati.micheletti@gmail.com" else NULL,
                 gargle_oauth_cache = ".secrets",
                 gargle_oauth_client_type = "installed", # Without "web", google authentication didn't work when running non-interactively!
                 use_oob = TRUE, # TRUE
                 repos = "https://cloud.r-project.org",
                 spades.project.fast = FALSE,
                 spades.recoveryMode = 0,
                 spades.useRequire = TRUE,
                 spades.scratchPath = scratchPath,
                 reproducible.gdalwarp = TRUE,
                 reproducible.inputPaths = if (user("tmichele")) "~/data" else paths[["inputPath"]],
                 reproducible.destinationPath = if (user("tmichele")) "~/data" else paths[["outputPath"]],
                 reproducible.useMemoise = FALSE
  ),
  times = list(start = 2025,
               end = 2025),
  authorizeGDrive = googledrive::drive_auth(cache = ".secrets"),
  params = list(caribouLocPrep =
                  list(jurisdiction = "NT")
                ),
  packages = c(
               "googledrive", 'RCurl', 'XML', 'igraph', 'qs', 'usethis',
               "SpaDES.tools",
               "PredictiveEcology/SpaDES.core@box (HEAD)",# 2.1.5.9022
               "PredictiveEcology/reproducible@AI (HEAD)",# 2.1.2.9046
               "PredictiveEcology/Require@development (>= 1.0.1)"
               ),
  useGit = "both",
  loadOrder = c(
    "caribouLocPrep"
  )
)

resultsRSF <- SpaDES.core::simInitAndSpades2(out)


