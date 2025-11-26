getOrUpdatePkg <- function(p, minVer = "0") {
  if (!isFALSE(try(packageVersion(p) < minVer, silent = TRUE) )) {
    repo <- c("predictiveecology.r-universe.dev", getOption("repos"))
    install.packages(p, repos = repo)
  }
}
getOrUpdatePkg("Require", "1.0.1.9020")
getOrUpdatePkg("SpaDES.project", "0.1.1.9036")
getOrUpdatePkg("terra")

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

runName <- "iSSA"
# centralPoint <- c(64.024641, -122.356419)

out <- SpaDES.project::setupProject(
  runName = runName,
  paths = list(projectPath = "PreVal",
               scratchPath = scratchPath,
               outputPath = file.path("outputs", runName)),
  modules =c(
    "tati-micheletti/caribouLocPrep@main",
    'tati-micheletti/prepTracks@main',
    'tati-micheletti/prepLandscape@main',
    'tati-micheletti/extractLand@main'
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
  # authorizeGDrive = googledrive::drive_auth(cache = ".secrets"),
  params = list(caribouLocPrep = list(
                       jurisdiction = "NT",
                       herdNT = "Dehcho Boreal Woodland Caribou"),
                prepTracks = list(
                       rate = amt::hours(8),
                       tolerance = amt::minutes(240),
                       probsfilter = 0.9),
                extractLand = list(
                       histLandYears = 2005:2021), # go back all the way to first year of caribou data!),
                .globals = list(
                       .plots = c("png"),
                       .studyAreaName=  "NT",
                       jurisdiction = "NT",
                       .useCache = c(".inputObjects")
                )
                ),
  packages = c(#"googledrive", 'RCurl', 'XML', 'igraph', 'qs', 'usethis',
               #"SpaDES.tools",
               "purrr", "amt", # 1. First time to run: purrr was not installed so I added here. Restart the session after that.
               "PredictiveEcology/SpaDES.core@box"# # OLDER VERSIONS: 2.1.5.9022 # (>= 2.1.6.9002)
               ),
  useGit = "both",
  loadOrder = c(
    "caribouLocPrep",
    "prepTracks",
    "prepLandscape",
    "extractLand"
  )
)

iSSA <- SpaDES.core::simInitAndSpades2(out)



