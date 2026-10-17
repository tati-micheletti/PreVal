# 200 m cells inside every route polygon (1 km2 = about 25 cells): MEAN and SD of every processed 200 m layer.
# Read-only on birdMonitor's processed rasters. Output (git-ignored): data_birds/route200m_landuse_by_year.csv and route200m_static.csv
#   Rscript birds/extract_200m_route_summaries.R
.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))
suppressMessages({library(terra); library(sf); library(data.table)})
bm <- "C:/Users/michelet/Documents/GitHub/birdMonitor/inputs"
d <- file.path(bm, "predictors/processed/scale_02"); out <- "data_birds"; dir.create(out, showWarnings = FALSE)
s <- st_read(file.path(bm, "response/raw/MhB/MhB_Probeflaechen_DE_S2637_epsg25832.shp"), quiet = TRUE)
sid <- tolower(as.character(s$SITE_ID))
v <- vect(s); v <- project(v, "EPSG:3035")
zs <- function(r) {                        # mean and SD of the cells inside each polygon, one row per route
  m <- extract(r, v, fun = mean, na.rm = TRUE); sdv <- extract(r, v, fun = sd, na.rm = TRUE)
  nm <- names(r); res <- data.table(SITE_ID = sid)
  for (i in seq_along(nm)) { res[[paste0(nm[i], "_mean200")]] <- m[[i + 1]]; res[[paste0(nm[i], "_sd200")]] <- sdv[[i + 1]] }
  res
}
t0 <- Sys.time()
lu <- rbindlist(lapply(2005:2024, function(y) {
  f <- file.path(d, sprintf("landuse_%d_habitat_scale_02.tif", y)); if (!file.exists(f)) { message("missing ", f); return(NULL) }
  x <- zs(rast(f)); x[, year := y]; message(sprintf("landuse %d done (%.1f min)", y, as.numeric(difftime(Sys.time(), t0, units = "mins")))); x
}), fill = TRUE)
fwrite(lu, file.path(out, "route200m_landuse_by_year.csv"))
st <- zs(c(rast(file.path(d, "elevation_habitat_scale_02.tif")), rast(file.path(d, "slope_habitat_scale_02.tif")), rast(file.path(d, "solar_radiation_habitat_scale_02.tif"))))
for (y in c(2006, 2012, 2018)) {
  lc <- zs(rast(file.path(d, sprintf("landcover_%d_habitat_scale_02.tif", y)))); setnames(lc, setdiff(names(lc), "SITE_ID"), paste0(setdiff(names(lc), "SITE_ID"), "_", y))
  dw <- zs(rast(file.path(d, sprintf("dist_to_woodland_%d_habitat_scale_02.tif", y)))); setnames(dw, setdiff(names(dw), "SITE_ID"), paste0(setdiff(names(dw), "SITE_ID"), "_", y))
  st <- merge(merge(st, lc, by = "SITE_ID"), dw, by = "SITE_ID")
}
fwrite(st, file.path(out, "route200m_static.csv"))
cat("landuse rows:", nrow(lu), " static cols:", ncol(st), " NA share landuse mean:", round(mean(is.na(lu$grassland_mean200)), 4), " minutes:", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "\n")
