#' Spatial block size from the residual spatial autocorrelation of a baseline model (blockCV::cv_spatial_autocor).
#'
#' Baseline = a Gaussian-link-free, fast model: Pearson residuals of a negative-binomial GLM (MASS::glm.nb) of the response on the given
#' baseline covariates plus a year factor. Residuals are averaged per unit (route) so repeated visits at identical coordinates do not
#' produce a zero-distance spike, then an automatic variogram is fitted. The fitted RANGE is the distance beyond which residuals are
#' no longer correlated; the block size is that range (blockCV's recommendation). Uses PRE-TEST history only (rows passed in).
#' @param tab final table (data.table) restricted to the rows allowed (pre-test years)
#' @param xy columns with projected coordinates in metres
#' @param crs EPSG code of the coordinates
#' @param family "nb" or "gaussian"
#' @return list(rangeKm, blockKm, variogram, nUnits)
blockSizeFromAutocorrelation <- function(tab, response, baselineFeatures, unitCol = "SITE_ID", yearCol = "year", xy = c("x", "y"),
                                         crs = 3035, family = c("nb", "gaussian"), minKm = 20, maxKm = 400, numSample = 5000L) {
  family <- match.arg(family)
  d <- data.table::as.data.table(tab)
  fml <- stats::as.formula(paste(response, "~", paste(c(baselineFeatures, sprintf("factor(%s)", yearCol)), collapse = " + ")))
  fit <- if (family == "nb") MASS::glm.nb(fml, data = d, control = stats::glm.control(maxit = 100)) else stats::lm(fml, data = d)
  d[, resid := stats::residuals(fit, type = if (family == "nb") "pearson" else "response")]
  u <- d[, .(resid = mean(resid), x = mean(get(xy[1])), y = mean(get(xy[2]))), by = c(unitCol)]
  pts <- sf::st_as_sf(u, coords = c("x", "y"), crs = crs)
  ac <- suppressWarnings(blockCV::cv_spatial_autocor(x = pts, column = "resid", num_sample = min(numSample, nrow(u)), plot = FALSE, progress = FALSE))
  rangeKm <- as.numeric(ac$range) / 1000
  list(rangeKm = rangeKm, blockKm = min(maxKm, max(minKm, rangeKm)), nUnits = nrow(u), variogram = ac$variograms)
}
