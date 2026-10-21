#' Covariates from the extra layers (MODIS NDVI/EVI monthly, MODIS percent tree cover annual, Copernicus tree-cover layers 2018-2024).
#' Every function returns one row per route and year with NO missing values: a missing monthly value (cloud, snow) is dropped from the seasonal
#' mean; a feature that is missing for a whole route-year is filled by linear interpolation between the nearest earlier and later year of the SAME route
#' (at the ends of the series the nearest value is carried), and the number filled is reported.

fillNeighbours <- function(dt, cols, by = "SITE_ID") {
  if (!"year" %in% names(dt)) stop("fillNeighbours: the table needs a year column")
  data.table::setorderv(dt, c(by, "year"))
  interp <- function(y, x) {
    ok <- !is.na(x); if (all(ok) || !any(ok)) return(x)
    x[!ok] <- if (sum(ok) == 1L) x[ok] else stats::approx(y[ok], x[ok], xout = y[!ok], rule = 2)$y   # rule 2: carry the nearest value at the ends
    x
  }
  filled <- list()
  for (cc in cols) {
    na <- is.na(dt[[cc]]); filled[[cc]] <- sum(na)
    if (any(na)) dt[, (cc) := interp(year, get(cc)), by = by]
    if (anyNA(dt[[cc]])) {                                                   # missing in EVERY year of a route (e.g. snow-covered all winters): use the median of all routes in the same year
      ym <- dt[, list(.ym = stats::median(get(cc), na.rm = TRUE)), by = "year"]; dt <- merge(dt, ym, by = "year", all.x = TRUE, sort = FALSE)
      dt[is.na(get(cc)), (cc) := .ym]; dt[, .ym := NULL]
      if (anyNA(dt[[cc]])) stop("fillNeighbours: ", cc, " is missing for all routes in some year")
      data.table::setorderv(dt, c(by, "year"))
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
  cols <- setdiff(names(out), c("SITE_ID", "year")); out <- fillNeighbours(out, cols); filled <- attr(out, "filled")
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
  out <- fillNeighbours(out, "tcover_modis"); filled <- attr(out, "filled"); data.table::setorder(out, SITE_ID, year)
  out[, tcover_modis_lag1 := data.table::shift(tcover_modis, 1L, type = "lag"), by = SITE_ID][is.na(tcover_modis_lag1), tcover_modis_lag1 := tcover_modis]
  data.table::setattr(out, "filled", filled); out[]
}

#' Copernicus layers exist only for 2018-2024. They vary from year to year there, so each year keeps its own map; a year without a map takes the NEAREST
#' available year (2005-2017 -> 2018: nothing earlier exists), the same as-of rule as for the other land-cover layers. Route-years with no value in a map are
#' filled from the same route's other maps before this expansion.
#' @param clms data.table(SITE_ID, year, layer = TCD/BCD/CCD, mean, sd)
clmsYearFeatures <- function(clms, years = 2005:2024) {
  w <- data.table::dcast(clms, SITE_ID + year ~ layer, value.var = c("mean", "sd"))
  data.table::setnames(w, names(w), sub("^(mean|sd)_(.*)$", "\\2_\\1100", names(w)))
  data.table::setnames(w, names(w), tolower(names(w))); data.table::setnames(w, "site_id", "SITE_ID")
  cols <- setdiff(names(w), c("SITE_ID", "year")); w <- fillNeighbours(w, cols); filled <- attr(w, "filled")
  out <- expandNearest(w, years); data.table::setattr(out, "filled", filled); out[]
}

#' One row per route and available year (or epoch) -> one row per route and target year.
#' rule "latestBefore" (default, changed 2026-10-21): the most recent available map at or before the year; a year before the first map takes the first map (nothing earlier exists).
#' rule "nearest": the nearest available year, a tie going to the earlier one (used before: it let 2008-2009 use the 2010 map, 2013-2014 the 2015 map and so on, i.e. later information).
expandNearest <- function(w, years, rule = c("latestBefore", "nearest")) {
  rule <- match.arg(rule); have <- sort(unique(w$year))
  pick <- function(y) if (rule == "nearest") have[which.min(abs(have - y))] else if (any(have <= y)) max(have[have <= y]) else min(have)
  map <- data.table::data.table(year = years, src = vapply(years, pick, numeric(1)))
  out <- merge(data.table::CJ(SITE_ID = unique(w$SITE_ID), year = years), map, by = "year")
  data.table::setnames(w, "year", "src"); out <- merge(out, w, by = c("SITE_ID", "src"), all.x = TRUE, sort = FALSE); out[, src := NULL]
  data.table::setorder(out, SITE_ID, year); out[]
}

#' GHS-BUILT-S built-up surface (JRC, 100 m): epochs 2005, 2010, 2015, 2020; each year takes the most recent epoch at or before it (2005-2009 -> 2005, 2010-2014 -> 2010, 2015-2019 -> 2015, 2020-2024 -> 2020).
#' @param ghs data.table(SITE_ID, epoch, mean, sd): mean and SD over the 100 m cells of the route of the built-up surface in m2 per cell (0-10000)
ghsEpochFeatures <- function(ghs, years = 2005:2024) {
  w <- data.table::data.table(SITE_ID = ghs$SITE_ID, year = ghs$epoch, ghs_built_share = ghs$mean / 1e4, ghs_built_sd = ghs$sd / 1e4)
  w <- fillNeighbours(w, c("ghs_built_share", "ghs_built_sd")); filled <- attr(w, "filled")
  out <- expandNearest(w, years); data.table::setattr(out, "filled", filled); out[]
}

#' DWD annual 1 km grids: snow-cover days, frost days, ice days, start and end of the vegetation period (calendar year t), plus the previous year's values.
#' The annual counts of year t include the months after the breeding season, so they are same-year information (like tmean_annual); the lagged versions are known at season start.
#' @param dwd data.table(SITE_ID, year, variable, value)
dwdFeatures <- function(dwd, years = 2005:2024) {
  nm <- c(snowcover_days = "dwd_snow_days", frost_days = "dwd_frost_days", ice_days = "dwd_ice_days", vegetation_begin = "dwd_veg_begin", vegetation_end = "dwd_veg_end")
  w <- data.table::dcast(dwd[variable %in% names(nm)], SITE_ID + year ~ variable, value.var = "value"); data.table::setnames(w, names(nm), unname(nm))
  w <- merge(data.table::CJ(SITE_ID = unique(w$SITE_ID), year = sort(unique(c(years, min(years) - 1L)))), w, by = c("SITE_ID", "year"), all.x = TRUE)
  w <- fillNeighbours(w, unname(nm)); filled <- attr(w, "filled")
  w[, dwd_veg_length := dwd_veg_end - dwd_veg_begin]
  for (cc in unname(nm)) w[, (paste0(cc, "_lag1")) := data.table::shift(get(cc), 1L, type = "lag"), by = SITE_ID]
  lagc <- paste0(unname(nm), "_lag1"); for (cc in lagc) w[is.na(get(cc)), (cc) := get(sub("_lag1$", "", cc))]
  out <- w[year %in% years]; data.table::setorder(out, SITE_ID, year); data.table::setattr(out, "filled", filled); out[]
}

#' European Forest Disturbance Atlas (annual disturbance, 30 m, 1985-2023): share of the route disturbed in the PREVIOUS year, mean annual share over the previous 5 years,
#' summed annual shares over the previous 20 years, and the share of the route that is forest (static).
#' @param fda data.table(SITE_ID, year, share), @param forest data.table(SITE_ID, forestShare)
fdaFeatures <- function(fda, forest, years = 2005:2024) {
  res <- lapply(years, function(t) {
    p <- fda[year == t - 1L, .(SITE_ID, fda_disturbed_prev = share)]
    r5 <- fda[year %in% (t - 5L):(t - 1L), .(fda_disturbed_5y = mean(share)), by = SITE_ID]
    r20 <- fda[year %in% (t - 20L):(t - 1L), .(fda_disturbed_sum20 = sum(share)), by = SITE_ID]
    r <- Reduce(function(a, b) merge(a, b, by = "SITE_ID"), list(p, r5, r20)); r[, year := t]; r })
  out <- merge(data.table::rbindlist(res), forest[, .(SITE_ID, fda_forest_share = forestShare)], by = "SITE_ID", all.x = TRUE)
  out <- merge(data.table::CJ(SITE_ID = unique(out$SITE_ID), year = years), out, by = c("SITE_ID", "year"), all.x = TRUE)
  out <- fillNeighbours(out, setdiff(names(out), c("SITE_ID", "year"))); filled <- attr(out, "filled"); data.table::setorder(out, SITE_ID, year); data.table::setattr(out, "filled", filled); out[]
}

#' Hansen Global Forest Change: tree cover in 2000 and the year of loss (1 = 2001 ... 24 = 2024). For year t only losses up to the end of t - 1 count: estimated tree cover
#' (2000 cover minus the cover of cells lost by then), cumulative lost share, share lost in year t - 1 and in the 3 years before t.
#' @param h data.table(SITE_ID, lossYear, nCells, tcSum): per route and loss year (0 = never lost) the number of 30 m cells and the sum of their 2000 tree cover (0-100)
hansenFeatures <- function(h, years = 2005:2024) {
  tot <- h[, .(nAll = sum(nCells), tcAll = sum(tcSum)), by = SITE_ID]; l <- h[lossYear > 0]
  res <- lapply(years, function(t) {
    k <- t - 2001L
    a <- l[lossYear <= k, .(nLostCum = sum(nCells), tcLostCum = sum(tcSum)), by = SITE_ID]
    b <- l[lossYear == k, .(nLostPrev = sum(nCells)), by = SITE_ID]
    c3 <- l[lossYear > k - 3L & lossYear <= k, .(nLost3 = sum(nCells)), by = SITE_ID]
    r <- Reduce(function(x, y) merge(x, y, by = "SITE_ID", all.x = TRUE), list(tot, a, b, c3)); r[, year := t]; r })
  r <- data.table::rbindlist(res); for (cc in c("nLostCum", "tcLostCum", "nLostPrev", "nLost3")) r[is.na(get(cc)), (cc) := 0]
  r[, `:=`(hansen_tree2000 = tcAll / nAll, hansen_tcover_est = (tcAll - tcLostCum) / nAll, hansen_loss_cum = nLostCum / nAll, hansen_loss_prev = nLostPrev / nAll, hansen_loss_recent3 = nLost3 / nAll)]
  r[, c("nAll", "tcAll", "nLostCum", "tcLostCum", "nLostPrev", "nLost3") := NULL]; data.table::setorder(r, SITE_ID, year); r[]
}

#' DLR tree species maps (10 m) for 2016 and 2022: share of the route's area in each species group, conifers, broadleaves, all tree-species cells, and canopy-cover loss.
#' Each year takes the most recent map at or before it (2005-2021 -> 2016, 2022-2024 -> 2022; the years before 2016 can only use the 2016 map).
#' @param dlr data.table(SITE_ID, epoch, cls, n): cell counts per class (cls NA or other = no tree species), summed over all cells of the route
dlrFeatures <- function(dlr, years = 2005:2024) {
  map <- c(`0` = "pine", `1` = "spruce", `2` = "douglas", `3` = "larch", `4` = "fir", `5` = "beech", `6` = "oak", `7` = "birch", `8` = "alder", `9` = "other_tree", `666` = "canopy_loss")
  tot <- dlr[, .(nAll = sum(n)), by = .(SITE_ID, epoch)]
  d <- dlr[as.character(cls) %in% names(map)]; d[, nm := paste0("dlr_", map[as.character(cls)])]
  w <- data.table::dcast(d, SITE_ID + epoch ~ nm, value.var = "n", fun.aggregate = sum, fill = 0)
  w <- merge(tot, w, by = c("SITE_ID", "epoch"), all.x = TRUE); cc <- paste0("dlr_", unname(map)); for (c1 in cc) { if (!c1 %in% names(w)) w[, (c1) := 0]; w[is.na(get(c1)), (c1) := 0] }
  for (c1 in cc) w[, (c1) := get(c1) / nAll]
  w[, `:=`(dlr_conifer = dlr_pine + dlr_spruce + dlr_douglas + dlr_larch + dlr_fir, dlr_broadleaf = dlr_beech + dlr_oak + dlr_birch + dlr_alder + dlr_other_tree)]
  w[, dlr_tree_species := dlr_conifer + dlr_broadleaf]; w[, nAll := NULL]; data.table::setnames(w, "epoch", "year")
  expandNearest(w, years)
}

#' ETH global canopy height 2020 (10 m): mean, SD and 90th percentile of the canopy height in the route (static). A route without a valid cell takes the median of all routes.
#' @param eth data.table(SITE_ID, nCells, nValid, mean, sd, p90)
ethFeatures <- function(eth, years = 2005:2024) {
  w <- eth[, .(SITE_ID, eth_canopy_mean = mean, eth_canopy_sd = sd, eth_canopy_p90 = p90)]
  for (cc in c("eth_canopy_mean", "eth_canopy_sd", "eth_canopy_p90")) w[is.nan(get(cc)) | is.infinite(get(cc)), (cc) := NA_real_]
  out <- merge(data.table::CJ(SITE_ID = unique(w$SITE_ID), year = years), w, by = "SITE_ID")
  out <- fillNeighbours(out, c("eth_canopy_mean", "eth_canopy_sd", "eth_canopy_p90")); filled <- attr(out, "filled"); data.table::setorder(out, SITE_ID, year); data.table::setattr(out, "filled", filled); out[]
}

#' Forest age (global 30 m forest age map 1985-2024, natural and planted forest as separate maps, age in years at 2024, 1-40; 40 = present since the start of the record).
#' For year t the stand age is age2024 - (2024 - t); a cell is forest in year t if that is >= 1. Ages of cells capped at 40 are lower bounds.
#' Features: share of the route area that is forest, mean stand age of that forest, share of young forest (age 1-10), share of planted forest (of the route area).
#' @param age data.table(SITE_ID, type = "PF"/"NF", age (NA = no forest), n): cell counts per route, forest type and age, summed over ALL cells of the route
forestAgeFeatures <- function(age, years = 2005:2024, mapYear = 2024L) {
  tot <- age[, .(nAll = sum(n)), by = .(SITE_ID, type)][, .(nAll = max(nAll)), by = SITE_ID]       # the planted and the natural map cover the same cells
  a <- age[!is.na(age)]
  res <- lapply(years, function(t) {
    x <- a[age - (mapYear - t) >= 1L][, `:=`(at = age - (mapYear - t))]
    r <- x[, .(forestCells = sum(n), ageSum = sum(n * at), young = sum(n[at <= 10L]), planted = sum(n[type == "PF"])), by = SITE_ID]; r[, year := t]; r })
  r <- merge(data.table::CJ(SITE_ID = tot$SITE_ID, year = years), data.table::rbindlist(res), by = c("SITE_ID", "year"), all.x = TRUE)
  for (cc in c("forestCells", "ageSum", "young", "planted")) r[is.na(get(cc)), (cc) := 0]
  r <- merge(r, tot, by = "SITE_ID")
  r[, `:=`(fage_forest_share = forestCells / nAll, fage_mean_age = ifelse(forestCells > 0, ageSum / forestCells, 0), fage_young_share = young / nAll, fage_planted_share = planted / nAll)]
  r[, c("forestCells", "ageSum", "young", "planted", "nAll") := NULL]; data.table::setorder(r, SITE_ID, year); r[]
}
