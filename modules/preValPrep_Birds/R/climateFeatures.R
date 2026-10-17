#' Climate features per route and year from CHELSA monthly values (rain, minimum and maximum temperature).
#'
#' Input: the long table written by birds/extract_chelsa_monthly.R (SITE_ID, year, month, variable, value; temperatures in Kelvin,
#' rain in mm per month). A value of a corrupt file (tasmin below 200 K, e.g. tasmin April 2022) is treated as missing and imputed
#' from the same month's maximum temperature minus that route's mean day-to-night range in the same calendar month of the other years.
#' Features use only information up to the end of the target year (previous winter, spring, breeding season, summer of the year and
#' lags of one year), so they are known at forecast time.
#' @return data.table(SITE_ID, year, <features>)
climateFeatures <- function(chelsa, years = 2005:2024, minYear = 2000L) {
  stopifnot(requireNamespace("data.table", quietly = TRUE))
  d <- data.table::as.data.table(chelsa)
  w <- data.table::dcast(d, SITE_ID + year + month ~ variable, value.var = "value")
  stopifnot(all(c("prec", "tasmin", "tasmax") %in% names(w)))
  bad <- is.na(w$tasmin) | w$tasmin < 200
  if (any(bad)) {
    w[, range := ifelse(bad, NA_real_, tasmax - tasmin)]
    w[, meanRange := mean(range, na.rm = TRUE), by = .(SITE_ID, month)]
    w[bad, tasmin := tasmax - meanRange]
    w[, c("range", "meanRange") := NULL]
    message("climateFeatures: ", sum(bad), " corrupt minimum-temperature values imputed (", paste(unique(w[bad, paste(year, month, sep = "-")]), collapse = ", "), ")")
  }
  w[, `:=`(tmin = tasmin - 273.15, tmax = tasmax - 273.15)]
  w[, tmean := (tmin + tmax) / 2]
  # a season = months with the year shift of the season year (Dec belongs to the NEXT winter)
  agg <- function(var, months, shift, fun, name, lag = 0L) {
    x <- w[month %in% months]
    x[, seasonYear := year + shift[match(month, months)] + lag]
    x[, .(value = fun(get(var))), by = .(SITE_ID, year = seasonYear)][, feature := name][]
  }
  specs <- list(
    list("tmean", c(12, 1, 2), c(1, 0, 0), mean, "tmean_winter"),
    list("tmin", c(12, 1, 2), c(1, 0, 0), min, "tmin_winter"),
    list("prec", c(12, 1, 2), c(1, 0, 0), sum, "prec_winter"),
    list("tmean", c(3, 4, 5), c(0, 0, 0), mean, "tmean_spring"),
    list("prec", c(3, 4, 5), c(0, 0, 0), sum, "prec_spring"),
    list("tmean", c(4, 5, 6), c(0, 0, 0), mean, "tmean_breeding"),
    list("tmax", c(4, 5, 6), c(0, 0, 0), max, "tmax_breeding"),
    list("prec", c(4, 5, 6), c(0, 0, 0), sum, "prec_breeding"),
    list("tmean", c(6, 7, 8), c(0, 0, 0), mean, "tmean_summer"),
    list("tmax", c(6, 7, 8), c(0, 0, 0), mean, "tmax_summer"),
    list("prec", c(6, 7, 8), c(0, 0, 0), sum, "prec_summer"),
    list("tmean", c(9, 10, 11), c(0, 0, 0), mean, "tmean_autumn"),
    list("prec", c(9, 10, 11), c(0, 0, 0), sum, "prec_autumn"),
    list("tmean", 1:12, rep(0, 12), mean, "tmean_annual"),
    list("prec", 1:12, rep(0, 12), sum, "prec_annual"))
  feats <- data.table::rbindlist(lapply(specs, function(s) agg(s[[1]], s[[2]], s[[3]], s[[4]], s[[5]])))
  # winter harshness: degree-months below freezing and number of frost months, November (previous year) to March
  w2 <- w[month %in% c(11, 12, 1, 2, 3)]; w2[, seasonYear := year + (month >= 11)]
  harsh <- w2[, .(cold_sum_winter = sum(pmax(0, -tmean)), frost_months_winter = as.numeric(sum(tmin < 0))), by = .(SITE_ID, year = seasonYear)]
  hw <- data.table::melt(harsh, id.vars = c("SITE_ID", "year"), variable.name = "feature", value.name = "value")
  feats <- data.table::rbindlist(list(feats, hw), use.names = TRUE)
  wide <- data.table::dcast(feats, SITE_ID + year ~ feature, value.var = "value")
  data.table::setorder(wide, SITE_ID, year)
  # lags of one year (previous breeding season, previous summer heat, previous rain)
  for (f in c("tmean_breeding", "prec_breeding", "tmax_summer", "prec_annual"))
    wide[, paste0(f, "_lag1") := data.table::shift(get(f), 1L), by = SITE_ID]
  # anomalies against the route's own climatology of the available years
  for (f in c("tmean_breeding", "prec_breeding", "tmean_winter"))
    wide[, paste0(f, "_anom") := get(f) - mean(get(f), na.rm = TRUE), by = SITE_ID]
  wide <- wide[year %in% years]
  wide[]
}
