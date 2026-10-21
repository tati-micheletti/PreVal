# Extract the new layers inside every route polygon (about 1 km2): READ-ONLY on the downloaded files, writes CSVs into data_birds/newLayers/features/.
#   Rscript birds/extract_new_layers.R clms      # Copernicus tree cover density (TCD), broadleaved (BCD) and coniferous (CCD) cover density, 100 m, 2018-2024
#   Rscript birds/extract_new_layers.R mod44b    # MODIS percent tree cover (250 m), annual 2005-2024
#   Rscript birds/extract_new_layers.R mod13a3   # MODIS NDVI and EVI (1 km), monthly 2005-2024
#   (add  test  as a second argument to run one year only)
.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))
suppressMessages({library(terra); library(sf); library(data.table)})
args <- commandArgs(trailingOnly = TRUE); what <- args[1]; testRun <- "test" %in% args
yr <- sub("^y=", "", grep("^y=", args, value = TRUE)); yr <- if (length(yr)) as.integer(strsplit(yr, "-")[[1]]) else c(2005L, 2024L)
prod <- if (grepl("^clms_", what)) sub("^clms_", "", what) else NA_character_; if (!is.na(prod)) what <- "clms"
stopifnot(what %in% c("clms", "mod44b", "mod13a3", "ghs"))
dl <- file.path("data_birds", "downloads"); outDir <- file.path("data_birds", "newLayers", "features"); dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
ndviDir <- list.dirs(file.path("data_birds", "newLayers"), recursive = FALSE); ndviDir <- ndviDir[grepl("MOD13A3", ndviDir)][1]
shp <- "C:/Users/michelet/Documents/GitHub/birdMonitor/inputs/response/raw/MhB/MhB_Probeflaechen_DE_S2637_epsg25832.shp"
routes <- st_read(shp, quiet = TRUE); sid <- tolower(as.character(routes$SITE_ID)); stopifnot(!anyDuplicated(sid))
polys <- vect(routes)
logf <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")
years <- if (testRun) 2020L else yr[1]:yr[2]
tag <- paste0(if (!is.na(prod)) paste0("_", prod) else "", sprintf("_y%d-%d", min(years), max(years)), if (testRun) "_test" else "")

if (what == "clms") {
  out <- list()
  codes <- c(TCD = "hrl_tcd", BCD = "hrl_bcd", CCD = "hrl_ccd"); if (!is.na(prod)) codes <- codes[prod]
  for (lname in names(codes)) for (yr in intersect(years, 2018:2024)) {
    code <- codes[[lname]]                                                       # (fixed: names(code) was NULL inside the old loop, so the layer column was dropped)
    f <- list.files(file.path(dl, code, "tif"), pattern = sprintf("_S%d_.*[.]tif$", yr), full.names = TRUE)
    if (length(f) != 69) stop("expected 69 tiles for ", code, " ", yr, " but found ", length(f))
    v <- vrt(f); p <- project(polys, crs(v))
    em <- terra::extract(v, p, fun = mean, na.rm = TRUE)[[2]]                   # mean of the 100 m cells of the route polygon
    es <- terra::extract(v, p, fun = sd, na.rm = TRUE)[[2]]                     # spread of the 100 m cells inside the route
    if (max(em, na.rm = TRUE) > 100.001) stop("values above 100 in ", code, " ", yr, ": a nodata code needs masking")
    out[[length(out) + 1L]] <- data.table(SITE_ID = sid, year = yr, layer = lname, mean = em, sd = es)
    logf(lname, yr, "done; routes without a value:", sum(is.na(em)))
  }
  res <- rbindlist(out); fwrite(res, file.path(outDir, paste0("route_clms_by_year", tag, ".csv")))
}

if (what == "mod44b") {
  fs <- list.files(file.path(dl, "modis_mod44b"), pattern = "[.]hdf$", full.names = TRUE)
  info <- data.table(f = fs, year = as.integer(sub("^MOD44B[.]A(\\d{4}).*$", "\\1", basename(fs))), tile = sub("^.*[.](h\\d\\dv\\d\\d)[.].*$", "\\1", basename(fs)))
  info <- info[year %in% years]; out <- list()
  for (i in seq_len(nrow(info))) {
    d <- describe(info$f[i], sds = TRUE); k <- grep("^\"?Percent_Tree_Cover\"?$", d$var)[1]
    r <- sds(info$f[i])[[k]]; r <- ifel(r > 100, NA, r)                           # 200 = water, other codes = no data
    p <- project(polys, crs(r)); inside <- which(relate(p, as.polygons(ext(r), crs = crs(r)), "intersects")[, 1])
    e <- terra::extract(r, p[inside], fun = mean, na.rm = TRUE, exact = TRUE)
    out[[i]] <- data.table(SITE_ID = sid[inside], year = info$year[i], tile = info$tile[i], tcover = e[[2]])
    logf("MOD44B", info$year[i], info$tile[i], "routes in tile:", length(inside))
  }
  res <- rbindlist(out)[, .(tcover = mean(tcover, na.rm = TRUE), nTiles = .N), by = .(SITE_ID, year)]
  fwrite(res, file.path(outDir, paste0("route_mod44b_by_year", tag, ".csv")))
}

if (what == "mod13a3") {
  fs <- list.files(ndviDir, pattern = "[.]hdf$", full.names = TRUE)
  info <- data.table(f = fs, year = as.integer(sub("^MOD13A3[.]A(\\d{4})\\d{3}.*$", "\\1", basename(fs))), doy = as.integer(sub("^MOD13A3[.]A\\d{4}(\\d{3}).*$", "\\1", basename(fs))),
                     tile = sub("^.*[.](h\\d\\dv\\d\\d)[.].*$", "\\1", basename(fs)))
  info[, month := as.integer(format(as.Date(doy - 1L, origin = paste0(year, "-01-01")), "%m"))]
  info <- info[year %in% years]; setorder(info, year, month, tile); out <- list()
  inTile <- list()                                                                # polygons that touch each tile (the grid is the same every month)
  r0 <- sds(info$f[1])[[1]]; p0 <- project(polys, crs(r0)); cen0 <- centroids(p0)
  for (t in unique(info$tile)) { r1 <- sds(info[tile == t]$f[1])[[1]]; inTile[[t]] <- which(relate(p0, as.polygons(ext(r1), crs = crs(r0)), "intersects")[, 1]) }
  cnt <- 0L
  for (i in seq_len(nrow(info))) {
    s <- sds(info$f[i]); idx <- inTile[[info$tile[i]]]; vals <- list()
    for (nm in c("ndvi", "evi")) {
      lay <- c(ndvi = 1L, evi = 2L)[[nm]]
      r <- s[[lay]]; r <- ifel(r < -2e7, NA, r)                                  # terra multiplies the HDF integer by its scale of 10000: DN = value / 1e4, index = DN / 1e4; fill DN = -3000
      e <- terra::extract(r, cen0[idx], method = "simple")[[2]] / 1e8            # the 1 km cell that contains the route centre (the route is about 1 km2, i.e. one cell); fast
      vals[[nm]] <- e
    }
    out[[i]] <- data.table(SITE_ID = sid[idx], year = info$year[i], month = info$month[i], ndvi = vals$ndvi, evi = vals$evi)
    cnt <- cnt + 1L; if (cnt %% 12 == 0) logf("MOD13A3", info$year[i], "month", info$month[i], "-", cnt, "of", nrow(info), "files")
  }
  res <- rbindlist(out)[, .(ndvi = mean(ndvi, na.rm = TRUE), evi = mean(evi, na.rm = TRUE)), by = .(SITE_ID, year, month)]
  res[is.nan(ndvi), ndvi := NA_real_][is.nan(evi), evi := NA_real_]
  fwrite(res, file.path(outDir, paste0("route_mod13a3_monthly", tag, ".csv")))
}
if (what == "ghs") {                                                              # GHS-BUILT-S (JRC): built-up surface in m2 per 100 m cell, epochs 2005/2010/2015/2020, 4 tiles each
  out <- list()
  for (ep in c(2005L, 2010L, 2015L, 2020L)) {
    d <- file.path(dl, "ghs_built_s", paste0("E", ep)); td <- file.path(d, "tif"); dir.create(td, showWarnings = FALSE)
    for (z in list.files(d, pattern = "[.]zip$", full.names = TRUE)) {
      nm <- grep("[.]tif$", unzip(z, list = TRUE)$Name, value = TRUE); if (!all(file.exists(file.path(td, nm)))) unzip(z, files = nm, exdir = td)
    }
    f <- list.files(td, pattern = "[.]tif$", full.names = TRUE); if (length(f) != 4) stop("expected 4 tiles for epoch ", ep, " but found ", length(f))
    v <- vrt(f); p <- project(polys, crs(v))
    em <- terra::extract(v, p, fun = mean, na.rm = TRUE)[[2]]; es <- terra::extract(v, p, fun = sd, na.rm = TRUE)[[2]]
    if (max(em, na.rm = TRUE) > 10000.001) stop("values above 10000 m2 per cell in epoch ", ep, ": a nodata code needs masking")
    out[[length(out) + 1L]] <- data.table(SITE_ID = sid, epoch = ep, mean = em, sd = es)
    logf("GHS-BUILT-S", ep, "done; routes without a value:", sum(is.na(em)), "; mean built-up share", round(mean(em, na.rm = TRUE) / 1e4, 4))
  }
  fwrite(rbindlist(out), file.path(outDir, "route_ghs_by_epoch.csv"))
}
logf("finished", what, if (testRun) "(test)" else "")
