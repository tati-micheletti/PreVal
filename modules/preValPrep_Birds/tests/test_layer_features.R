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
check(abs(f[SITE_ID == "b2" & year == 2010]$ndvi_annual - mean(c(f[SITE_ID == "b2" & year == 2009]$ndvi_annual, f[SITE_ID == "b2" & year == 2011]$ndvi_annual))) < 1e-9, "a whole missing route-year is filled with the mean of the neighbouring years of the same route")
z <- fillNeighbours(data.table(SITE_ID = "q", year = 1:5, x = c(NA, 2, NA, 4, NA)), "x")
check(identical(as.numeric(z$x), c(2, 2, 3, 4, 4)), "interpolation between neighbours, nearest value carried at both ends of the series")
check(attr(f, "filled")[["ndvi_winter"]] == 20, "the number of filled winter values is reported (route a1, 20 years)")
check(abs(mean(f[SITE_ID == "c3"]$ndvi_breeding_anom)) < 1e-9, "anomaly is centred on the route's own mean")
l <- f[SITE_ID == "c3"][order(year)]; check(all(abs(l$ndvi_breeding_lag1[-1] - l$ndvi_breeding[-nrow(l)]) < 1e-12), "lag-1 equals the previous year's breeding NDVI")
t44 <- CJ(SITE_ID = sites, year = 2004:2024)[, tcover := 30 + 0.5 * (year - 2004)]; t44[SITE_ID == "a1" & year == 2012, tcover := NA_real_]
tc <- treeCoverFeatures(t44, 2005:2024)
check(!anyNA(tc) && nrow(tc) == 60, "tree cover: no missing value, one row per route-year")
check(abs(tc[SITE_ID == "a1" & year == 2012]$tcover_modis - 34) < 1e-9, "tree cover: the missing year is interpolated between 2011 and 2013")
cl <- CJ(SITE_ID = sites, year = 2018:2024, layer = c("TCD", "BCD", "CCD"))[, `:=`(mean = 40 + (year - 2018), sd = 12)]
cl[SITE_ID == "a1" & year == 2020 & layer == "TCD", mean := NA_real_]
cs <- clmsYearFeatures(cl, 2005:2024)
check(nrow(cs) == 3 * 20 && all(c("tcd_mean100", "bcd_sd100", "ccd_mean100") %in% names(cs)) && !anyNA(cs), "Copernicus layers: one row per route-year, no missing value")
check(all(cs[year <= 2018]$tcd_mean100 == 40) && all(abs(cs[year == 2021]$tcd_mean100 - 43) < 1e-9) && all(abs(cs[year == 2024]$tcd_mean100 - 46) < 1e-9), "Copernicus layers: each year keeps its own map, 2005-2017 take the nearest map (2018)")
check(abs(cs[SITE_ID == "a1" & year == 2020]$tcd_mean100 - 42) < 1e-9, "Copernicus layers: a route-year without a value is interpolated from the other maps")
gh <- CJ(SITE_ID = sites, epoch = c(2005, 2010, 2015, 2020))[, `:=`(mean = 1000 * (epoch - 2000) / 5, sd = 500)]
gf <- ghsEpochFeatures(gh, 2005:2024)
check(nrow(gf) == 60 && !anyNA(gf), "GHS built-up: one row per route-year, no missing value")
check(all(abs(gf[year %in% 2005:2009]$ghs_built_share - 0.1) < 1e-9) && all(abs(gf[year %in% 2010:2014]$ghs_built_share - 0.2) < 1e-9) &&
      all(abs(gf[year %in% 2015:2019]$ghs_built_share - 0.3) < 1e-9) && all(abs(gf[year %in% 2020:2024]$ghs_built_share - 0.4) < 1e-9), "GHS built-up: every year takes the most recent epoch at or before it (never a later one), value scaled to a share 0-1")
# ---- DWD, disturbance atlas, Hansen, DLR tree species, ETH canopy height (2026-10-21) ----
dw <- CJ(SITE_ID = sites, year = 2005:2024, variable = c("snowcover_days", "frost_days", "ice_days", "vegetation_begin", "vegetation_end"))[, value := 50 + (year - 2005)]
dw[SITE_ID == "a1" & year == 2010 & variable == "frost_days", value := NA_real_]
df <- dwdFeatures(dw, 2005:2024)
check(nrow(df) == 60 && !anyNA(df) && all(c("dwd_snow_days", "dwd_veg_length", "dwd_frost_days_lag1") %in% names(df)), "DWD: one row per route-year, no missing value, lag and vegetation length present")
check(abs(df[SITE_ID == "a1" & year == 2010]$dwd_frost_days - 55) < 1e-9 && abs(df[SITE_ID == "b2" & year == 2012]$dwd_snow_days_lag1 - 56) < 1e-9, "DWD: a missing year is interpolated and the lag is the previous year")
fd <- CJ(SITE_ID = sites, year = 1985:2023)[, share := ifelse(year == 2010, 0.2, 0.01)]
ff <- fdaFeatures(fd, data.table(SITE_ID = sites, forestShare = c(0.1, 0.5, 0.9)), 2005:2024)
check(nrow(ff) == 60 && !anyNA(ff), "disturbance atlas: one row per route-year")
check(abs(ff[SITE_ID == "a1" & year == 2011]$fda_disturbed_prev - 0.2) < 1e-12 && abs(ff[SITE_ID == "a1" & year == 2010]$fda_disturbed_prev - 0.01) < 1e-12, "disturbance atlas: the disturbance of year t - 1 enters year t, not year t itself")
check(abs(ff[SITE_ID == "a1" & year == 2012]$fda_disturbed_5y - mean(c(0.2, 0.01, 0.01, 0.01, 0.01))) < 1e-12, "disturbance atlas: the 5-year mean covers t - 5 ... t - 1")
hh <- rbind(data.table(SITE_ID = sites, lossYear = 0L, nCells = 900L, tcSum = 900 * 50), data.table(SITE_ID = sites, lossYear = 10L, nCells = 100L, tcSum = 100 * 80))   # 10% of cells lost in 2010
hf <- hansenFeatures(hh, 2005:2024)
check(abs(hf[SITE_ID == "a1" & year == 2010]$hansen_loss_cum) < 1e-12 && abs(hf[SITE_ID == "a1" & year == 2011]$hansen_loss_cum - 0.1) < 1e-12, "Hansen: loss in 2010 counts from 2011 on (known at the end of the previous year)")
check(abs(hf[SITE_ID == "a1" & year == 2011]$hansen_loss_prev - 0.1) < 1e-12 && abs(hf[SITE_ID == "a1" & year == 2012]$hansen_loss_prev) < 1e-12 && abs(hf[SITE_ID == "a1" & year == 2013]$hansen_loss_recent3 - 0.1) < 1e-12, "Hansen: previous-year and 3-year loss shares")
check(abs(hf[SITE_ID == "a1" & year == 2010]$hansen_tcover_est - 53) < 1e-9 && abs(hf[SITE_ID == "a1" & year == 2012]$hansen_tcover_est - 45) < 1e-9, "Hansen: estimated tree cover drops by the cover of the lost cells")
dl <- rbind(data.table(SITE_ID = sites, epoch = 2016L, cls = c(5, 5, NA), n = c(500L, 100L, 1000L)), data.table(SITE_ID = sites, epoch = 2022L, cls = 0, n = 250L),
            data.table(SITE_ID = sites, epoch = 2016L, cls = NA, n = 500L)[1:2], data.table(SITE_ID = sites, epoch = 2022L, cls = NA, n = 750L))
dlf <- dlrFeatures(dl, 2005:2024)
check(nrow(dlf) == 60 && !anyNA(dlf), "DLR: one row per route-year, no missing value (routes without tree cells get zeros)")
check(abs(dlf[SITE_ID == "a1" & year == 2021]$dlr_beech - 0.5) < 1e-12 && abs(dlf[SITE_ID == "a1" & year == 2022]$dlr_pine - 0.25) < 1e-12 && abs(dlf[SITE_ID == "a1" & year == 2022]$dlr_conifer - 0.25) < 1e-12 && abs(dlf[SITE_ID == "a1" & year == 2005]$dlr_beech - 0.5) < 1e-12,
      "DLR: 2005-2021 take the 2016 map (the only one before 2022), 2022-2024 the 2022 map; shares of the route area")
a2 <- expandNearest(data.table(SITE_ID = "q", year = c(2006L, 2012L, 2018L), v = c(1, 2, 3)), 2005:2019)
check(identical(as.numeric(a2$v), c(rep(1, 7), rep(2, 6), rep(3, 2))), "as-of rule: a year uses the latest map at or before it; years before the first map take the first map")
et <- data.table(SITE_ID = sites, nCells = 1000L, nValid = c(900L, 0L, 800L), mean = c(10, NaN, 20), sd = c(2, NaN, 3), p90 = c(14, NA, 25))
ef <- ethFeatures(et, 2005:2024)
check(nrow(ef) == 60 && !anyNA(ef) && abs(ef[SITE_ID == "b2"]$eth_canopy_mean[1] - 15) < 1e-9, "ETH: a route without a valid cell takes the median of the other routes")
fa <- rbind(data.table(SITE_ID = "a1", type = "PF", age = c(10L, 40L, NA), n = c(100L, 100L, 800L)), data.table(SITE_ID = "a1", type = "NF", age = c(30L, NA), n = c(200L, 800L)))
ffa <- forestAgeFeatures(fa, 2005:2024)
check(nrow(ffa) == 20 && !anyNA(ffa), "forest age: one row per route-year, no missing value")
check(abs(ffa[year == 2024]$fage_forest_share - 0.4) < 1e-12 && abs(ffa[year == 2024]$fage_mean_age - 27.5) < 1e-9 && abs(ffa[year == 2024]$fage_young_share - 0.1) < 1e-12 && abs(ffa[year == 2024]$fage_planted_share - 0.2) < 1e-12,
      "forest age: shares and mean age in 2024 (the map year)")
check(abs(ffa[year == 2015]$fage_mean_age - 18.5) < 1e-9 && abs(ffa[year == 2010]$fage_forest_share - 0.3) < 1e-12, "forest age: earlier years subtract the elapsed time, and stands younger than that do not exist yet")
cat(sprintf("\n%d failure(s)\n", nFail)); if (nFail) quit(status = 1)
