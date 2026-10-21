# Route summaries of the open-data layers (READ-ONLY on the downloads; writes CSVs into data_birds/newLayers/features/). Routes are the MhB polygons (about 1 km2).
#   Rscript birds/extract_open_layers.R dwd      # DWD annual 1 km grids: snow-cover, frost and ice days, start and end of vegetation (value in the 1 km cell of the route centre)
#   Rscript birds/extract_open_layers.R fda      # European Forest Disturbance Atlas, Germany: share of the route disturbed in each year 1985-2023 + forest-mask share
#   Rscript birds/extract_open_layers.R hansen   # Hansen Global Forest Change v1.12: per route and loss year (0 = none, 1-24 = 2001-2024) the number of 30 m cells and their tree cover 2000
#   Rscript birds/extract_open_layers.R dlr      # DLR tree species (10 m), 2016 and 2022: cell counts per class per route
#   Rscript birds/extract_open_layers.R eth      # ETH canopy height 2020 (10 m): mean, SD, 90th percentile per route
#   Rscript birds/extract_open_layers.R age      # global forest age map (30 m; planted + natural forest): cell counts per route, forest type and age at 2024
#   (add  test  as a second argument to run the first 100 routes only)
.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))
suppressMessages({library(terra); library(sf); library(data.table)})
args <- commandArgs(trailingOnly = TRUE); what <- args[1]; testRun <- "test" %in% args
stopifnot(what %in% c("dwd", "fda", "hansen", "dlr", "eth", "age"))
dl <- file.path("data_birds", "downloads"); outDir <- file.path("data_birds", "newLayers", "features"); dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
shp <- "C:/Users/michelet/Documents/GitHub/birdMonitor/inputs/response/raw/MhB/MhB_Probeflaechen_DE_S2637_epsg25832.shp"
routes <- st_read(shp, quiet = TRUE); sid <- tolower(as.character(routes$SITE_ID)); stopifnot(!anyDuplicated(sid))
polys <- vect(routes); if (testRun) { polys <- polys[1:100]; sid <- sid[1:100] }
logf <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")
chunks <- split(seq_along(sid), ceiling(seq_along(sid) / 150))
tag <- if (testRun) "_test" else ""
cellValues <- function(r, p) { d <- as.data.table(extract(r, p)); d }                 # every cell whose centre lies in the polygon

if (what == "dwd") {
  vars <- c("snowcover_days", "frost_days", "ice_days", "vegetation_begin", "vegetation_end"); out <- list()
  cen <- project(centroids(polys), "EPSG:31467")                                        # the DWD grids are in Gauss-Krueger zone 3 (no .prj shipped)
  tmp <- tempfile("dwd"); dir.create(tmp)
  for (v in vars) for (f in list.files(file.path(dl, "dwd_annual", v), pattern = "[.]asc[.]gz$", full.names = TRUE)) {
    yr <- as.integer(sub("^.*_(\\d{4})(_17)?[.]asc[.]gz$", "\\1", basename(f)))
    asc <- file.path(tmp, sub("[.]gz$", "", basename(f)))
    con <- gzfile(f, "rb"); wcon <- file(asc, "wb")                                    # decompress in chunks until the end of the file (a fixed size cut some grids off)
    repeat { b <- readBin(con, "raw", n = 1e7); if (!length(b)) break; writeBin(b, wcon) }
    close(con); close(wcon)
    r <- rast(asc); crs(r) <- "EPSG:31467"
    val <- extract(r, cen, search_radius = 5000)[[2]]                                  # a coastal / lake-edge route whose own 1 km cell has no value takes the nearest cell with a value (within 5 km)
    out[[length(out) + 1L]] <- data.table(SITE_ID = sid, year = yr, variable = v, value = val); r <- NULL; invisible(gc()); unlink(asc)
  }
  res <- rbindlist(out); logf("DWD: rows", nrow(res), "; routes without a value:", res[is.na(value), uniqueN(SITE_ID)])
  fwrite(res, file.path(outDir, paste0("route_dwd_by_year", tag, ".csv")))
}

if (what == "fda") {
  uz <- file.path(dl, "forest_disturbance_atlas", "unz")
  r <- rast(file.path(uz, "annual_disturbances_1985_2023_germany.tif")); fm <- rast(file.path(uz, "forest_mask_germany.tif"))
  pr <- project(polys, crs(r)); out <- list(); fo <- list()
  for (ii in chunks) {
    # both layers are 1/0 inside forest and NaN OUTSIDE it, so the share of the route AREA is the count of 1s over ALL cells of the route (mean(na.rm = TRUE) would give the share of forest cells)
    share1 <- function(x) sum(x == 1, na.rm = TRUE) / length(x)
    e <- extract(r, pr[ii], fun = share1); m <- as.matrix(e[, -1, drop = FALSE])
    out[[length(out) + 1L]] <- data.table(SITE_ID = rep(sid[ii], times = ncol(m)), year = rep(1985:2023, each = length(ii)), share = as.vector(m))
    fo[[length(fo) + 1L]] <- data.table(SITE_ID = sid[ii], forestShare = extract(fm, pr[ii], fun = share1)[[2]]); logf("FDA chunk", ii[1], "-", ii[length(ii)])
  }
  fwrite(rbindlist(out), file.path(outDir, paste0("route_fda_by_year", tag, ".csv"))); fwrite(rbindlist(fo), file.path(outDir, paste0("route_fda_forest", tag, ".csv")))
}

if (what == "hansen") {
  d0 <- file.path(dl, "hansen_gfc"); tiles <- c("50N_000E", "50N_010E", "60N_000E", "60N_010E")
  tc <- vrt(file.path(d0, sprintf("Hansen_GFC-2024-v1.12_treecover2000_%s.tif", tiles))); ly <- vrt(file.path(d0, sprintf("Hansen_GFC-2024-v1.12_lossyear_%s.tif", tiles)))
  st <- c(tc, ly); pr <- project(polys, "EPSG:4326"); out <- list()
  for (ii in chunks) {
    d <- cellValues(st, pr[ii]); setnames(d, c("ID", "tc", "ly")); d <- d[!is.na(tc) & !is.na(ly)]
    a <- d[, .(nCells = .N, tcSum = sum(tc)), by = .(ID, lossYear = ly)]; a[, SITE_ID := sid[ii][ID]]; a[, ID := NULL]
    out[[length(out) + 1L]] <- a; logf("Hansen chunk", ii[1], "-", ii[length(ii)])
  }
  fwrite(rbindlist(out), file.path(outDir, paste0("route_hansen_by_lossyear", tag, ".csv")))
}

if (what == "dlr") {
  out <- list()
  for (ep in c(2016L, 2022L)) {
    r <- rast(file.path(dl, "dlr_treespecies", sprintf("treespecies_de_%d.tif", ep))); pr <- project(polys, crs(r))
    for (ii in chunks) {
      d <- cellValues(r, pr[ii]); setnames(d, c("ID", "cls")); a <- d[, .(n = .N), by = .(ID, cls)]; a[, `:=`(SITE_ID = sid[ii][ID], epoch = ep)]; a[, ID := NULL]
      out[[length(out) + 1L]] <- a; logf("DLR", ep, "chunk", ii[1], "-", ii[length(ii)])
    }
  }
  fwrite(rbindlist(out), file.path(outDir, paste0("route_dlr_by_epoch", tag, ".csv")))
}

if (what == "eth") {
  fs <- list.files(file.path(dl, "eth_canopy_height"), pattern = "[.]tif$", full.names = TRUE); v <- vrt(fs); pr <- project(polys, crs(v)); out <- list()
  for (ii in chunks) {
    d <- cellValues(v, pr[ii]); setnames(d, c("ID", "h")); d[!is.na(h) & h > 100, h := NA_real_]            # 255 = no data
    a <- d[, .(nCells = .N, nValid = sum(!is.na(h)), mean = mean(h, na.rm = TRUE), sd = sd(h, na.rm = TRUE), p90 = quantile(h, 0.9, na.rm = TRUE, names = FALSE)), by = ID]
    a[, SITE_ID := sid[ii][ID]]; a[, ID := NULL]; out[[length(out) + 1L]] <- a; logf("ETH chunk", ii[1], "-", ii[length(ii)])
  }
  fwrite(rbindlist(out), file.path(outDir, paste0("route_eth_canopy", tag, ".csv")))
}
if (what == "age") {
  out <- list(); pr <- project(polys, "EPSG:4326")
  for (ty in c("PF", "NF")) {
    fs <- list.files(file.path(dl, "forest_age", paste0("unz_", ty)), pattern = "[.]tif$", full.names = TRUE, recursive = TRUE); if (!length(fs)) stop("no unpacked tiles for ", ty)
    bb <- ext(pr); fs <- fs[vapply(fs, function(f) { e <- ext(rast(f)); !(e[2] < bb[1] || e[1] > bb[2] || e[4] < bb[3] || e[3] > bb[4]) }, logical(1))]   # only the tiles that touch the routes
    cat("forest age", ty, ":", length(fs), "tiles touch the routes\n")
    v <- vrt(fs)
    for (ii in chunks) {
      d <- cellValues(v, pr[ii]); setnames(d, c("ID", "age")); a <- d[, .(n = .N), by = .(ID, age)]; a[, `:=`(SITE_ID = sid[ii][ID], type = ty)]; a[, ID := NULL]
      out[[length(out) + 1L]] <- a; logf("forest age", ty, "chunk", ii[1], "-", ii[length(ii)])
    }
  }
  fwrite(rbindlist(out), file.path(outDir, paste0("route_forest_age_by_age", tag, ".csv")))
}
logf("finished", what, if (testRun) "(test)" else "")
