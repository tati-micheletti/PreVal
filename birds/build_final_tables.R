# Birds, step 3: FINAL TABLE FOR MODELING, one per species (one row per route and year), written to data_birds/final_<species>.rds
# Reads (read-only): birdMonitor processed rasters (1 km, 50 km), our route-year tables, CHELSA at routes, 200 m summaries.
#   Rscript birds/build_final_tables.R
.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))
suppressMessages({library(data.table); library(terra)})
for (f in c("climateFeatures.R", "buildFinalTable.R")) source(file.path("modules/preValPrep_Birds/R", f))
bm <- "C:/Users/michelet/Documents/GitHub/birdMonitor/inputs/predictors/processed"
chelsa <- fread("data_birds/chelsa_monthly_routes.csv")
lu200 <- fread("data_birds/route200m_landuse_by_year.csv"); st200 <- fread("data_birds/route200m_static.csv")
for (sp in c("Chloris_chloris", "Dendrocopos_major")) {
  tab <- buildFinalTable(sp, "data_birds/territories", chelsa, lu200, st200, file.path(bm, "scale_50"), file.path(bm, "scale_1"))
  saveRDS(tab, file.path("data_birds", sprintf("final_%s.rds", sp)))
  message(sp, ": ", nrow(tab), " rows, ", uniqueN(tab$SITE_ID), " routes, ", ncol(tab) - 5, " covariate columns; NA share by column (top 8):")
  print(head(sort(colMeans(is.na(tab)), decreasing = TRUE), 8))
}
