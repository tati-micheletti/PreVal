# Birds, step 1 (REUSE of birdMonitor): route-year tables of Reviere (territory counts) with the 1 km landscape covariates.
# Calls birdMonitor's own occurrencePrepGerLandscape() READ-ONLY (its R files are sourced, never modified) for the two planned
# species, with spatial thinning OFF, and writes into PreVal_refit/data_birds/territories (git-ignored).
#   Rscript birds/prepare_route_year_tables.R
.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))
suppressMessages({library(sf); library(terra); library(dplyr); library(tidyr); library(readxl)})
bm <- "C:/Users/michelet/Documents/GitHub/birdMonitor"
# the module's R/ folder only defines functions: source them all (read-only)
for (p in list.files(file.path(bm, "modules/dataPrep_Monitor/R"), pattern = "[.]R$", full.names = TRUE)) source(p)
if (exists("warmUpSfProj")) warmUpSfProj()
out <- "data_birds"; dir.create(file.path(out, "cache"), recursive = TRUE, showWarnings = FALSE)
sp <- c("Chloris chloris", "Dendrocopos major")
res <- occurrencePrepGerLandscape(
  ddaTerritoriesXlsxPath = file.path(bm, "inputs/response/raw/territories/BirdStats_Daten2005-2024D_alle.xlsx"),
  ddaVisitsXlsxPath = file.path(bm, "inputs/response/raw/territories/BirdStats_Visits2005-2024D.xlsx"),
  probeflaechenShpPath = file.path(bm, "inputs/response/raw/MhB/MhB_Probeflaechen_DE_S2637_epsg25832.shp"),
  processedRoot = file.path(bm, "inputs/predictors/processed"), outputDir = file.path(out, "territories"),
  species = sp, landscapeYears = setNames(list(2005:2024, 2005:2024), sp), resolutionConfig = NULL, sharedResolutionM = 1000,
  localeCtype = Sys.getlocale("LC_CTYPE"), useThinning = FALSE, perSpeciesDataSource = NULL,
  germanNames = c("Chloris chloris" = "Gr\u00fcnfink", "Dendrocopos major" = "Buntspecht"), cachePath = file.path(out, "cache"))
print(basename(res)[1:6]); cat("files:", length(res), "\n")
