#' Build the FINAL TABLE FOR MODELING for one bird species (one row per route and year).
#'
#' Reuses what birdMonitor already prepared (read-only): the route-year tables of Reviere with the 1 km land-use covariates
#' (occurrencePrepGerLandscape), the processed 200 m, 1 km and 50 km rasters; adds the climate features and the 200 m summaries
#' (mean and SD of the cells inside each route polygon). Every covariate in a row belongs to THAT row's year (never to other years).
#'
#' Rules (decided with the project lead):
#'  * Reviere = -1 (observer could not give an exact count) is excluded from the count response.
#'  * Land cover (CORINE 2006, 2012, 2018) and distance to woodland use the most recent epoch at or before the year (2005 uses 2006: nothing earlier exists).
#'  * Solar radiation is not used (birdMonitor dropped it for every species, DECISIONS 2026-09-26).
#'  * Nothing from birdMonitor is modified.
#'
#' Hedges (a data problem found here): the land-use product has a hedges class only in 2017-2021 and 2024. In those years hedges are
#' carved out of grassland, orchards, fallow and some cereals, so the class proportions have a structural break that would act like a
#' non-transferable covariate. landuseMode = "merged" (default) adds the real hedge share back into one class
#' `green_permanent` = grassland + orchards_and_berries + fallow + hedges (hedges only in the years where they are real), and keeps a
#' static hedge density per route; "raw" keeps the classes as delivered.

#' The epoch used for a year: the most recent epoch AT OR BEFORE the year; a year before the first epoch (2005) takes the first one (nothing earlier exists).
#' (Changed 2026-10-21. It used to be the NEAREST epoch, which gave 2010, 2011, 2016 and 2017 a LATER map: information from after the year.)
nearestEpoch <- function(year, epochs = c(2006L, 2012L, 2018L)) {
  vapply(year, function(y) if (any(epochs <= y)) max(epochs[epochs <= y]) else min(epochs), integer(1))
}

HEDGE_YEARS <- c(2017:2021, 2024L)

#' @param lu data.table with the 14 land-use class columns (named by class) plus `year`
harmoniseLanduse <- function(lu, mode = c("merged", "raw"), suffix = "", classes = NULL) {
  mode <- match.arg(mode)
  hn <- paste0("hedges", suffix); gn <- paste0("grassland", suffix); on <- paste0("orchards_and_berries", suffix); fn <- paste0("fallow", suffix)
  if (mode == "raw") return(lu)
  hedgeReal <- ifelse(lu$year %in% HEDGE_YEARS & !is.na(lu[[hn]]), lu[[hn]], 0)
  lu[[paste0("green_permanent", suffix)]] <- lu[[gn]] + lu[[on]] + lu[[fn]] + hedgeReal
  lu[, c(gn, on, fn, hn) := NULL]
  lu
}

#' extract mean values of a raster at points (bilinear), named
extractAt <- function(path, xy, prefix = NULL) {
  r <- terra::rast(path); v <- terra::extract(r, xy, method = "bilinear")
  if (identical(names(v)[1], "ID")) v <- v[, -1, drop = FALSE]      # a matrix of points returns no ID column; a SpatVector does
  if (!is.null(prefix)) names(v) <- paste0(prefix, names(v))
  data.table::as.data.table(v)
}

#' 50 km bioclim window ending in `year` (6 years), 19 layers, at the route centroids
bioclimAtRoutes <- function(bioclimDir, sites, years = 2005:2024, windowLength = 6L) {
  out <- lapply(years, function(y) {
    f <- file.path(bioclimDir, sprintf("bioclim_%d-%d_scale_50.tif", y - windowLength + 1L, y))
    if (!file.exists(f)) stop("bioclim window missing: ", f)
    v <- extractAt(f, as.matrix(sites[, .(x, y)]), "bio50_")
    cbind(data.table::data.table(SITE_ID = sites$SITE_ID, year = y), v)
  })
  data.table::rbindlist(out)
}

#' Static 1 km layers at the route centroids: elevation, slope, and the CORINE / distance-to-woodland epochs
staticAt1km <- function(scale1Dir, sites, epochs = c(2006L, 2012L, 2018L)) {
  xy <- as.matrix(sites[, .(x, y)])
  res <- data.table::data.table(SITE_ID = sites$SITE_ID)
  res <- cbind(res, extractAt(file.path(scale1Dir, "elevation_landscape_scale_1.tif"), xy, "km1_"),
               extractAt(file.path(scale1Dir, "slope_landscape_scale_1.tif"), xy, "km1_"))
  for (e in epochs) {
    lc <- extractAt(file.path(scale1Dir, sprintf("landcover_%d_landscape_scale_1.tif", e)), xy, "km1_"); names(lc) <- paste0(names(lc), "_e", e)
    dw <- extractAt(file.path(scale1Dir, sprintf("dist_to_woodland_%d_landscape_scale_1.tif", e)), xy, "km1_"); names(dw) <- paste0(names(dw), "_e", e)
    res <- cbind(res, lc, dw)
  }
  res
}

#' Pick, for every row, the columns of the nearest epoch and rename them without the epoch suffix
pickEpoch <- function(dt, yearCol = "year", epochs = c(2006L, 2012L, 2018L), epochPrefix = "_e") {
  ep <- nearestEpoch(dt[[yearCol]], epochs)
  bases <- unique(sub(paste0(epochPrefix, "[0-9]{4}$"), "", grep(paste0(epochPrefix, "[0-9]{4}$"), names(dt), value = TRUE)))
  for (b in bases) {
    cols <- paste0(b, epochPrefix, epochs)
    m <- as.matrix(dt[, ..cols]); dt[[b]] <- m[cbind(seq_len(nrow(dt)), match(ep, epochs))]
  }
  drop <- grep(paste0(epochPrefix, "[0-9]{4}$"), names(dt), value = TRUE)
  dt[, (drop) := NULL]
  dt
}

#' @param species file stem, e.g. "Chloris_chloris"
buildFinalTable <- function(species, territoriesDir, chelsa, route200Landuse, route200Static, bioclimDir, scale1Dir,
                            years = 2005:2024, landuseMode = "merged") {
  dt <- data.table::rbindlist(lapply(years, function(y) {
    f <- file.path(territoriesDir, sprintf("%s_landscape_%d.rds", species, y))
    if (!file.exists(f)) return(NULL)
    x <- data.table::as.data.table(readRDS(f))
    cls <- c("grassland", "winter_cereals", "summer_cereals", "maize", "root_crops", "rapeseed", "sunflower", "vegetables", "legumes",
             "hedges", "fallow", "grapevine", "hops", "orchards_and_berries")
    keep <- data.table::data.table(SITE_ID = x$ROUTENCODE, year = y, Reviere = x$Reviere, x = x$x, y = x$y)
    for (k in cls) { cn <- paste0(k, "_", y); keep[[paste0(k, "_km1")]] <- if (cn %in% names(x)) x[[cn]] else NA_real_ }
    keep
  }))
  n0 <- nrow(dt); dt <- dt[Reviere >= 0]
  message(species, ": ", n0, " route-years, ", n0 - nrow(dt), " with Reviere = -1 excluded")
  dt <- harmoniseLanduse(dt, landuseMode, suffix = "_km1")
  sites <- unique(dt[, .(SITE_ID, x, y)])
  # hedge density per route at 200 m from the hedge maps that exist (2017-2021, 2024): each year uses the mean of the real hedge maps up to that year
  # (the first map, 2017, for the years before it: nothing earlier exists). It used to be the mean over ALL real years, i.e. it used maps from after the year.
  hedMaps <- route200Landuse[year %in% HEDGE_YEARS, .(SITE_ID, mapYear = year, h = hedges_mean200)]
  hed <- data.table::rbindlist(lapply(years, function(t) {
    use <- hedMaps[mapYear <= max(t, min(HEDGE_YEARS))]; use[, .(hedges_static_mean200 = mean(h, na.rm = TRUE), year = t), by = SITE_ID] }))
  lu200 <- data.table::copy(route200Landuse)
  mcols <- grep("_(mean|sd)200$", names(lu200), value = TRUE)
  if (landuseMode == "merged") {
    for (s in c("mean200", "sd200")) {
      h <- paste0("hedges_", s); g <- paste0("grassland_", s); o <- paste0("orchards_and_berries_", s); f <- paste0("fallow_", s)
      real <- ifelse(lu200$year %in% HEDGE_YEARS & !is.na(lu200[[h]]), lu200[[h]], 0)
      if (s == "mean200") lu200[[paste0("green_permanent_", s)]] <- lu200[[g]] + lu200[[o]] + lu200[[f]] + real
      else lu200[[paste0("green_permanent_", s)]] <- sqrt(lu200[[g]]^2 + lu200[[o]]^2 + lu200[[f]]^2 + real^2)   # spread of the merged class: approximate (quadrature)
      lu200[, c(g, o, f, h) := NULL]
    }
  }
  dt <- merge(dt, lu200, by = c("SITE_ID", "year"), all.x = TRUE)
  dt <- merge(dt, hed, by = c("SITE_ID", "year"), all.x = TRUE)
  st200 <- data.table::copy(route200Static)
  st200[, c("solar_radiation_mean200", "solar_radiation_sd200") := NULL]
  names(st200) <- sub("_(mean200|sd200)_(2006|2012|2018)$", "_\\1_e\\2", names(st200))
  dt <- merge(dt, st200, by = "SITE_ID", all.x = TRUE)
  dt <- pickEpoch(dt)
  st1 <- staticAt1km(scale1Dir, sites)
  dt <- merge(dt, st1, by = "SITE_ID", all.x = TRUE)
  dt <- pickEpoch(dt)
  dt <- merge(dt, climateFeatures(chelsa, years), by = c("SITE_ID", "year"), all.x = TRUE)
  dt <- merge(dt, bioclimAtRoutes(bioclimDir, sites, years), by = c("SITE_ID", "year"), all.x = TRUE)
  data.table::setcolorder(dt, c("SITE_ID", "year", "Reviere", "x", "y"))
  data.table::setorder(dt, SITE_ID, year)
  dt[]
}
