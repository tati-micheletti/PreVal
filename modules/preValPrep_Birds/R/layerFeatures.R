#' Covariates from the extra layers (MODIS NDVI/EVI monthly, MODIS percent tree cover annual, Copernicus tree-cover layers 2018-2024).
#' Every function returns one row per route and year with NO missing values: a missing monthly value (cloud, snow) is dropped from the seasonal
#' mean; a feature that is missing for a whole route-year is filled with that route's median over the other years, and the share filled is reported.

fillRouteMedian <- function(dt, cols, by = "SITE_ID") {
  filled <- list()
  for (cc in cols) {
    na <- is.na(dt[[cc]]); filled[[cc]] <- sum(na)
    if (any(na)) { med <- dt[, stats::median(get(cc), na.rm = TRUE), by = by]; data.table::setnames(med, "V1", ".med")
      dt <- merge(dt, med, by = by, all.x = TRUE, sort = FALSE); dt[is.na(get(cc)), (cc) := .med]; dt[, .med := NULL] }
    if (anyNA(dt[[cc]])) {                                                   # missing in EVERY year of a route (e.g. snow-covered all winters): use the median of all routes in the same year
      if (!"year" %in% names(dt)) stop("fillRouteMedian: ", cc, " is missing for a whole route and there is no year column to fall back on")
      ym <- dt[, list(.ym = stats::median(get(cc), na.rm = TRUE)), by = "year"]; dt <- merge(dt, ym, by = "year", all.x = TRUE, sort = FALSE)
      dt[is.na(get(cc)), (cc) := .ym]; dt[, .ym := NULL]
      if (anyNA(dt[[cc]])) stop("fillRouteMedian: ", cc, " is missing for all routes in some year")
    }
  }
  data.table::setattr(dt, "filled", unlist(filled)); dt
}

#' @param monthly data.table(SITE_ID, year, month, ndvi, evi), one row per route and month
modisSeasonFeatures <- function(monthly, years = 2005:2024) {
  m <- data.table::copy(monthly)[year %in% (min(years) - 1L):max(years)]
  m[month == 12L, `:=`(year = year + 1L)]                                    # December counts for the winter of the NEXT year (as for the CHELSA features)
  agg <- function(months, fun, nm, col) m[month %in% months, stats::setNames(list(fun(get(col))), nm), by = .(SITE_ID, year)]
  f <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  mx <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
  parts <- list(agg(4:6, f, "ndvi_breeding", "ndvi"), agg(6:8, f, "ndvi_summer", "ndvi"), agg(c(12L, 1:2), f, "ndvi_winter", "ndvi"),
                agg(1:11, f, "ndvi_annual", "ndvi"), agg(1:11, mx, "ndvi_max", "ndvi"), agg(1:11, function(x) if (all(is.na(x))) NA_real_ else diff(range(x, na.rm = TRUE)), "ndvi_amplitude", "ndvi"),
                agg(4:6, f, "evi_breeding", "evi"), agg(1:11, f, "evi_annual", "evi"), agg(1:11, mx, "evi_max", "evi"))
  out <- Reduce(function(a, b) merge(a, b, by = c("SITE_ID", "year"), all = TRUE), parts)
  out <- out[year %in% years]
  allCombos <- data.table::CJ(SITE_ID = unique(out$SITE_ID), year = years); out <- merge(allCombos, out, by = c("SITE_ID", "year"), all.x = TRUE)
  cols <- setdiff(names(out), c("SITE_ID", "year")); out <- fillRouteMedian(out, cols); filled <- attr(out, "filled")
  data.table::setorder(out, SITE_ID, year)
  out[, ndvi_breeding_lag1 := data.table::shift(ndvi_breeding, 1L, type = "lag"), by = SITE_ID]
  out[, ndvi_breeding_anom := ndvi_breeding - mean(ndvi_breeding), by = SITE_ID]
  out[is.na(ndvi_breeding_lag1), ndvi_breeding_lag1 := ndvi_breeding]        # first year of a route: no earlier year, use the same year
  data.table::setattr(out, "filled", filled); out[]
}

#' @param t44 data.table(SITE_ID, year, tcover) from the MOD44B extraction
treeCoverFeatures <- function(t44, years = 2005:2024) {
  out <- t44[year %in% years, .(SITE_ID, year, tcover_modis = tcover)]
  out <- merge(data.table::CJ(SITE_ID = unique(out$SITE_ID), year = years), out, by = c("SITE_ID", "year"), all.x = TRUE)
  out <- fillRouteMedian(out, "tcover_modis"); filled <- attr(out, "filled"); data.table::setorder(out, SITE_ID, year)
  out[, tcover_modis_lag1 := data.table::shift(tcover_modis, 1L, type = "lag"), by = SITE_ID][is.na(tcover_modis_lag1), tcover_modis_lag1 := tcover_modis]
  data.table::setattr(out, "filled", filled); out[]
}

#' Copernicus layers exist only for 2018-2024: used as ROUTE-LEVEL averages (static covariates), so there is no missing value for 2005-2017.
#' @param clms data.table(SITE_ID, year, layer = TCD/BCD/CCD, mean, sd)
clmsStaticFeatures <- function(clms) {
  a <- clms[, .(mean100 = mean(mean, na.rm = TRUE), sd100 = mean(sd, na.rm = TRUE)), by = .(SITE_ID, layer)]
  w <- data.table::dcast(a, SITE_ID ~ layer, value.var = c("mean100", "sd100"))
  data.table::setnames(w, names(w), sub("^(mean100|sd100)_(.*)$", "\\2_\\1_static", names(w)))
  data.table::setnames(w, names(w), tolower(names(w)))
  data.table::setnames(w, "site_id", "SITE_ID")
  w[, lapply(.SD, function(x) { x[is.nan(x)] <- NA_real_; x }), by = SITE_ID]
}
