# Unit tests for the layer features on synthetic data (no real raster)
suppressMessages(library(data.table))
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(here, "..", "R", "layerFeatures.R"))
nFail <- 0L
check <- function(cond, msg) { ok <- isTRUE(cond); cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) nFail <<- nFail + 1L }
set.seed(3)
yrs <- 2004:2024; sites <- c("a1", "b2", "c3")
mo <- CJ(SITE_ID = sites, year = yrs, month = 1:12)
mo[, ndvi := 0.5 + 0.3 * sin((month - 3) / 12 * 2 * pi) + 0.01 * (year - 2004) + rnorm(.N, 0, .01)]
mo[, evi := ndvi * 0.6]
mo[SITE_ID == "a1" & month %in% c(12, 1, 2), ndvi := NA_real_]                     # snow: winter always missing for one route
mo[SITE_ID == "b2" & year == 2010, `:=`(ndvi = NA_real_, evi = NA_real_)]           # a whole year missing for one route
f <- modisSeasonFeatures(mo, 2005:2024)
check(nrow(f) == 3 * 20 && !anyNA(f), "one row per route and year, no missing value after filling")
check(all(f[SITE_ID == "c3"]$ndvi_breeding > f[SITE_ID == "c3"]$ndvi_winter), "breeding-season NDVI is above winter NDVI (seasonal cycle recovered)")
check(abs(f[SITE_ID == "c3" & year == 2010]$ndvi_breeding - mean(mo[SITE_ID == "c3" & year == 2010 & month %in% 4:6]$ndvi)) < 1e-9, "breeding mean is the April-June mean of that year")
check(abs(f[SITE_ID == "b2" & year == 2010]$ndvi_annual - median(f[SITE_ID == "b2" & year != 2010]$ndvi_annual)) < 1e-9, "a whole missing route-year is filled with the route's median of the other years")
check(attr(f, "filled")[["ndvi_winter"]] == 20, "the number of filled winter values is reported (route a1, 20 years)")
check(abs(mean(f[SITE_ID == "c3"]$ndvi_breeding_anom)) < 1e-9, "anomaly is centred on the route's own mean")
l <- f[SITE_ID == "c3"][order(year)]; check(all(abs(l$ndvi_breeding_lag1[-1] - l$ndvi_breeding[-nrow(l)]) < 1e-12), "lag-1 equals the previous year's breeding NDVI")
t44 <- CJ(SITE_ID = sites, year = 2004:2024)[, tcover := 30 + 0.5 * (year - 2004)]; t44[SITE_ID == "a1" & year == 2012, tcover := NA_real_]
tc <- treeCoverFeatures(t44, 2005:2024)
check(!anyNA(tc) && nrow(tc) == 60, "tree cover: no missing value, one row per route-year")
cl <- rbind(CJ(SITE_ID = sites, year = 2018:2024, layer = c("TCD", "BCD", "CCD"))[, `:=`(mean = 40, sd = 12)])
cs <- clmsStaticFeatures(cl)
check(nrow(cs) == 3 && all(c("tcd_mean100_static", "bcd_sd100_static", "ccd_mean100_static") %in% names(cs)) && !anyNA(cs), "Copernicus layers become route-level static covariates")
cat(sprintf("\n%d failure(s)\n", nFail)); if (nFail) quit(status = 1)
