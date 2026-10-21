# Final tables v2: the v1 table (95 covariates) plus the covariates from the new layers. v1 is never modified; v2 goes to data_birds/v2/.
#   Rscript birds/build_final_tables_v2.R
# New covariates (per route):
#   MODIS NDVI/EVI (1 km cell at the route centre, monthly 2005-2024): breeding (Apr-Jun), summer (Jun-Aug), winter (Dec-Feb), annual, maximum, amplitude,
#     EVI breeding/annual/maximum, NDVI breeding lag 1 and anomaly                                                    [vary by year]
#   MODIS percent tree cover (route polygon, annual 2005-2024) and its lag 1                                          [vary by year]
#   Copernicus tree cover density, broadleaved and coniferous cover density (100 m, mean and SD in the route, yearly maps 2018-2024; 2005-2017 take the 2018 map) [vary by year from 2018]
#   GHS-BUILT-S built-up surface (JRC, 100 m): share of built-up surface in the route and its SD, epochs 2005/2010/2015/2020, each year takes the nearest epoch
#   DWD annual 1 km grids (snow-cover, frost, ice days, start/end of vegetation; year t and t-1), European Forest Disturbance Atlas (shares disturbed up to t-1), Hansen forest loss
#   (cover and loss up to t-1), DLR tree species (2016/2022), ETH canopy height (2020). Same-year DWD counts include months after the breeding season (like tmean_annual): known only afterwards.
#   Gaps (cloud, snow, no value) are filled by interpolation between the neighbouring years of the same route.
.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))
suppressMessages(library(data.table))
source("modules/preValPrep_Birds/R/layerFeatures.R")
fd <- file.path("data_birds", "newLayers", "features")
readAll <- function(pat) { f <- list.files(fd, pattern = pat, full.names = TRUE); f <- f[!grepl("_test", f)]; if (!length(f)) stop("no files for ", pat); cat(pat, ":", length(f), "file(s)\n")
  rbindlist(lapply(f, function(x) { d <- fread(x); if (grepl("route_clms_by_year_", x) && !"layer" %in% names(d)) d[, layer := sub("^route_clms_by_year_([A-Z]+)_.*$", "\\1", basename(x))]; d })) }  # files written before the extraction fix lack the layer column: take it from the file name
mo <- readAll("^route_mod13a3_monthly_y.*[.]csv$"); t44 <- readAll("^route_mod44b_by_year_y.*[.]csv$"); cl <- readAll("^route_clms_by_year_.*[.]csv$")
stopifnot(!anyDuplicated(mo[, .(SITE_ID, year, month)]), !anyDuplicated(t44[, .(SITE_ID, year)]), !anyDuplicated(cl[, .(SITE_ID, year, layer)]))
cat("months per route in the NDVI table: median", median(mo[, .N, by = SITE_ID]$N), "(expected 240)\n")
modis <- modisSeasonFeatures(mo, 2005:2024); tc <- treeCoverFeatures(t44, 2005:2024); cs <- clmsYearFeatures(cl, 2005:2024)
gh <- fread(file.path(fd, "route_ghs_by_epoch.csv")); stopifnot(!anyDuplicated(gh[, .(SITE_ID, epoch)])); gs <- ghsEpochFeatures(gh, 2005:2024)   # GHS-BUILT-S built-up surface, nearest epoch
rd <- function(f) { p <- file.path(fd, f); if (!file.exists(p)) stop("missing ", p, " (run birds/extract_open_layers.R)"); fread(p) }
dwf <- dwdFeatures(rd("route_dwd_by_year.csv"), 2005:2024)                                      # DWD annual 1 km grids
fdf <- fdaFeatures(rd("route_fda_by_year.csv"), rd("route_fda_forest.csv"), 2005:2024)          # forest disturbance atlas
hnf <- hansenFeatures(rd("route_hansen_by_lossyear.csv"), 2005:2024)                           # Hansen tree cover 2000 + loss year
dlf <- dlrFeatures(rd("route_dlr_by_epoch.csv"), 2005:2024)                                    # DLR tree species 2016 / 2022
etf <- ethFeatures(rd("route_eth_canopy.csv"), 2005:2024)                                      # ETH canopy height 2020
extra <- list(dwf, fdf, hnf, dlf, etf)
agePath <- file.path(fd, "route_forest_age_by_age.csv")                                         # forest age (planted + natural forest), if extracted
if (file.exists(agePath)) extra[[length(extra) + 1L]] <- forestAgeFeatures(fread(agePath), 2005:2024) else message("NOTE: no forest-age table yet (", agePath, "); v2 is built without it")
cat("values filled (interpolated from neighbouring years; a route missing in every year takes the all-route median):\n")
print(c(attr(modis, "filled"), attr(tc, "filled"), attr(cs, "filled"), attr(gs, "filled"), unlist(lapply(extra, attr, "filled"))))
dir.create(file.path("data_birds", "v2"), showWarnings = FALSE)
for (sp in c("Chloris_chloris", "Dendrocopos_major")) {
  v1 <- readRDS(file.path("data_birds", sprintf("final_%s.rds", sp)))
  d <- merge(v1, modis, by = c("SITE_ID", "year"), all.x = TRUE, sort = FALSE)
  d <- merge(d, tc, by = c("SITE_ID", "year"), all.x = TRUE, sort = FALSE)
  d <- merge(d, cs, by = c("SITE_ID", "year"), all.x = TRUE, sort = FALSE)
  d <- merge(d, gs, by = c("SITE_ID", "year"), all.x = TRUE, sort = FALSE)
  for (x in extra) d <- merge(d, x, by = c("SITE_ID", "year"), all.x = TRUE, sort = FALSE)
  newCols <- setdiff(names(d), names(v1))
  if (nrow(d) != nrow(v1)) stop("row count changed: ", nrow(v1), " -> ", nrow(d))
  na <- colSums(is.na(d[, ..newCols])); if (any(na > 0)) { cat("missing values in new columns:\n"); print(na[na > 0]); stop("fix the missing values before using the table") }
  setorder(d, SITE_ID, year); saveRDS(d, file.path("data_birds", "v2", sprintf("final_%s.rds", sp)))
  cat(sprintf("%s: %d rows, %d covariates in v1 + %d new = %d\n", sp, nrow(d), ncol(v1) - 5, length(newCols), ncol(d) - 5))
}
fwrite(data.table(covariate = newCols, source = ifelse(grepl("^ndvi|^evi", newCols), "MODIS MOD13A3", ifelse(grepl("^tcover", newCols), "MODIS MOD44B", ifelse(grepl("^ghs", newCols), "GHS-BUILT-S 100 m (epochs 2005/2010/2015/2020, nearest epoch)",
ifelse(grepl("^dwd", newCols), "DWD annual 1 km grids (year t and t-1)",
ifelse(grepl("^fda", newCols), "European Forest Disturbance Atlas 30 m (years up to t-1)",
ifelse(grepl("^hansen", newCols), "Hansen Global Forest Change 30 m (losses up to t-1)",
ifelse(grepl("^dlr", newCols), "DLR tree species 10 m (2016 / 2022, nearest map)",
ifelse(grepl("^eth", newCols), "ETH canopy height 10 m (2020, static)", ifelse(grepl("^fage", newCols), "Forest age map 30 m (planted + natural, stand age at year t)", "Copernicus HRL 100 m (yearly 2018-2024, nearest map before)")))))))))),
       file.path("data_birds", "v2", "new_covariates.csv"))
