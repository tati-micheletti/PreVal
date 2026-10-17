# Unit test for climateFeatures() on a tiny synthetic CHELSA table (no real data needed)
suppressMessages(library(data.table))
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(here, "..", "R", "climateFeatures.R"))
nFail <- 0L
check <- function(cond, msg) { ok <- isTRUE(cond); cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) nFail <<- nFail + 1L }
set.seed(1)
g <- CJ(SITE_ID = c("a1", "b2"), year = 2000:2024, month = 1:12, variable = c("prec", "tasmin", "tasmax"))
g[, value := fifelse(variable == "prec", 50, fifelse(variable == "tasmin", 273.15 + 5, 273.15 + 15)) + rnorm(.N, 0, 0.1)]
g[SITE_ID == "a1" & year == 2022 & month == 4 & variable == "tasmin", value := 0]   # corrupt file
cf <- climateFeatures(g)
check(all(c("tmean_winter", "tmin_winter", "cold_sum_winter", "tmean_breeding_lag1", "tmean_breeding_anom") %in% names(cf)), "expected features exist")
check(nrow(cf) == 2 * 20, "one row per route and year (2005-2024)")
check(abs(cf[SITE_ID == "a1" & year == 2022, tmean_breeding] - 10) < 0.3, "corrupt April 2022 minimum temperature is imputed (breeding-season mean about 10 C)")
check(abs(cf[SITE_ID == "a1" & year == 2010, tmean_annual] - 10) < 0.2, "annual mean temperature is (5 + 15) / 2 = 10 C")
check(abs(cf[SITE_ID == "a1" & year == 2010, prec_annual] - 600) < 5, "annual rain is 12 x 50 mm")
check(abs(cf[SITE_ID == "a1" & year == 2011, tmean_breeding_lag1] - cf[SITE_ID == "a1" & year == 2010, tmean_breeding]) < 1e-9, "lag-1 equals the previous year's value")
check(abs(mean(cf[SITE_ID == "a1", tmean_winter_anom])) < 0.1, "anomalies are centred")
cat(sprintf("\n%d failure(s)\n", nFail)); if (nFail) quit(status = 1)
