# Unit tests for the rules in buildFinalTable.R that need no real raster
suppressMessages(library(data.table))
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(here, "..", "R", "buildFinalTable.R"))
nFail <- 0L
check <- function(cond, msg) { ok <- isTRUE(cond); cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) nFail <<- nFail + 1L }
check(identical(nearestEpoch(c(2005, 2009, 2010, 2015, 2016, 2024)), c(2006L, 2006L, 2012L, 2012L, 2018L, 2018L)), "nearest epoch, ties to the earlier one (2009 -> 2006, 2015 -> 2012)")
lu <- data.table(year = c(2010, 2018), grassland_km1 = c(0.20, 0.15), orchards_and_berries_km1 = c(0.014, 0.005), fallow_km1 = c(0.015, 0.008), hedges_km1 = c(0.07, 0.07), maize_km1 = c(0.05, 0.05))
h <- harmoniseLanduse(data.table::copy(lu), "merged", "_km1")
check(abs(h$green_permanent_km1[1] - (0.20 + 0.014 + 0.015)) < 1e-9, "no-hedge year: the carried-forward hedge value is NOT added (it is already inside grassland)")
check(abs(h$green_permanent_km1[2] - (0.15 + 0.005 + 0.008 + 0.07)) < 1e-9, "hedge year: the real hedge share is added back")
check(!any(c("grassland_km1", "hedges_km1", "orchards_and_berries_km1", "fallow_km1") %in% names(h)) && "maize_km1" %in% names(h), "merged classes replace their parts; other classes stay")
d <- data.table(year = c(2005, 2013, 2022), x_e2006 = 1, x_e2012 = 2, x_e2018 = 3)
p <- pickEpoch(data.table::copy(d))
check(identical(p$x, c(1, 2, 3)) && !any(grepl("_e20", names(p))), "pickEpoch takes the nearest epoch's column and drops the epoch columns")
cat(sprintf("\n%d failure(s)\n", nFail)); if (nFail) quit(status = 1)
