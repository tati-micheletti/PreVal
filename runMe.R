getOrUpdatePkg <- function(p, minVer = "0") {
  if (!isFALSE(try(packageVersion(p) < minVer, silent = TRUE) )) {
    repo <- c("predictiveecology.r-universe.dev", getOption("repos"))
    install.packages(p, repos = repo)
  }
}
getOrUpdatePkg("Require", "1.0.1")
getOrUpdatePkg("SpaDES.project", "0.1.1.9036")

################### SETUP

if (SpaDES.project::user("tmichele")) setwd("~/projects/preVal/")
scratchPath <- Require::checkPath("~/scratch", create = TRUE) #
if (SpaDES.project::user("tmichele")) terra::terraOptions(tempdir = scratchPath)
