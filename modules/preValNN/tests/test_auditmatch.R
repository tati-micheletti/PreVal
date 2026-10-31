# Post-hoc audit of a design with matched Internal draws (matchUnits), also for plans written before the plan carried a matchUnits column (the EVE full-pool run of 2026-10-30)
suppressMessages(library(data.table))
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
for (f in c("seeds.R", "caseIndex.R", "regimeSplits.R", "verifyRegimes.R", "spatialTestSplits.R", "generatePlan.R")) source(file.path(here, "..", "R", f))
nFail <- 0L
check <- function(cond, msg) { ok <- isTRUE(cond); cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) nFail <<- nFail + 1L }
set.seed(7); nUnit <- 300; years <- 2005:2016
units <- data.table(SITE_ID = sprintf("r%03d", 1:nUnit), x = runif(nUnit, 4e6, 4.5e6), y = runif(nUnit, 2.7e6, 3.2e6))
tab <- rbindlist(lapply(years, function(yy) { u <- units[runif(nUnit) < 0.35 + 0.04 * (yy - 2005)]; u[, year := yy]; u }))
tab[, `:=`(a = rnorm(.N), count = rpois(.N, 3))]; idx <- buildCaseIndex(tab)
pl <- generatePlan(idx, startYear = 2005, endYear = 2016, minTrainYears = 4, horizons = 1, covariateLevels = c(2, 4), regimes = c("FutureUnseen", "FutureTainted", "Internal"),
                   minTrain = 100, minVal = 50, minTest = 50, minTrainFraction = 0.5, matchUnits = TRUE)
check("matchUnits" %in% names(pl$plan) && all(pl$plan$matchUnits), "a design made with matchUnits writes the matchUnits column into the plan")
dir <- file.path(tempdir(), "am"); dir.create(dir); writeManifests(pl$bundles, dir)
a1 <- auditManifests(idx, pl$plan, dir, pl$spatial)
check(all(a1$ok), "audit with the column in the plan passes")
old <- copy(pl$plan); old[, matchUnits := NULL]
a2 <- auditManifests(idx, old, dir, pl$spatial, matchUnits = TRUE)
check(all(a2$ok), "audit of an OLD plan without the column passes when the design setting is passed")
check(isTRUE(all(auditManifests(idx, old, dir, pl$spatial, matchUnits = TRUE)[grepl("matchUnits", check)]$ok)) && any(grepl("matchUnits", a2$check)), "the matched-Internal check is part of that audit")
cat(sprintf("\n%d failure(s)\n", nFail)); if (nFail) quit(status = 1)
