#' Pre-specified analysis of the five-regime experiment (see birds/ANALYSIS_PLAN_BIRDS.md). Everything is computed from the per-model
#' result files; nothing is tuned after seeing results.
#'
#' Quantities per model (loss = negative log-likelihood per case; lower = better):
#'   reported = loss on the regime's own validation set at the best epoch (what that regime would report)
#'   realized = loss on the shared test set (the forecast year)
#'   optimism = realized - reported
#' Inference: the independent unit is the FORECAST YEAR (splits sharing a forecast year share their test cases). Every estimate is first
#' averaged within forecast year; across years we report the mean, a t interval and an EXACT sign-flip p-value (no bootstrap over ~15 clusters).
#' Contrasts (all paired on identical test cases; negative = the first regime forecasts better):
#'   H1   FutureTainted - Internal                 (>0: the status quo forecasts worse than the leaky reference says)
#'   H2   FutureUnseen (PreVal) - FutureTainted    (<0)
#'   P3   PreVal - Spatial and Spatial - Tainted   (both <0)
#'   P4   Tainted - Grouped, Grouped - Spatial, Spatial - PreVal   (each >= 0: the weak ordering of the leak removal)
#'   P4b  optimism per regime (largest for the status quo)
#'   P5   covariate ladder: penalty vs the simplest level, END and PEAK per regime and the regime differences
#' PreVal is NEVER compared with Internal (Internal is leaky by construction and enters only H1).
#' @param nCovariatesAll how many covariates `Inf` stands for (for the x axis and the log2 slope)
analyzeRegimes <- function(modelDir, outDir, nCovariatesAll, margin = 0.005) {
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(modelDir, pattern = "_finalDT\\.csv$", full.names = TRUE)
  if (!length(files)) stop("No result files in ", modelDir)
  M <- data.table::rbindlist(lapply(files, data.table::fread), fill = TRUE)
  M[, `:=`(reported = valLossBest, realized = testLossMean)]
  M[, `:=`(optimism = realized - reported, trainTestGap = realized - trainLossBest,
           complexity = ifelse(is.infinite(numberOfCovariates), nCovariatesAll, numberOfCovariates),
           windowLength = historyEnd - windowStart + 1L, capBound = epochsRun >= epochCap)]
  data.table::fwrite(M, file.path(outDir, "allModels.csv"))
  out <- list()

  yearTest <- function(x, equivMargin = NULL) {
    x <- x[is.finite(x)]; n <- length(x)
    if (n == 0) return(data.table::data.table(estimate = NA_real_, lo = NA_real_, hi = NA_real_, pSignFlip = NA_real_, nYears = 0L, nPositive = 0L, equivalent = NA))
    est <- mean(x); se <- if (n > 1) stats::sd(x) / sqrt(n) else NA_real_
    tc <- if (n > 1) stats::qt(0.975, n - 1) else NA_real_
    p <- NA_real_
    if (n >= 2 && n <= 16) { sg <- as.matrix(expand.grid(rep(list(c(-1, 1)), n))); p <- mean(abs(sg %*% x / n) >= abs(est) - 1e-12) }
    eq <- NA
    if (!is.null(equivMargin) && n > 1) { t90 <- stats::qt(0.95, n - 1); eq <- (est - t90 * se > -equivMargin) && (est + t90 * se < equivMargin) }
    data.table::data.table(estimate = est, lo = est - tc * se, hi = est + tc * se, pSignFlip = p, nYears = n, nPositive = sum(x > 0), equivalent = eq)
  }
  byYear <- function(dt, col, by, equivMargin = NULL) { y <- dt[, .(v = mean(get(col), na.rm = TRUE)), by = c(by, "testYear")]; y[, yearTest(v, equivMargin), by = by] }
  perYear <- function(dt, col, by) dt[, .(value = mean(get(col), na.rm = TRUE)), by = c(by, "testYear")]

  id <- c("splitId", "complexity", "replicate", "testYear", "horizon", "windowLength")
  W <- data.table::dcast(M, stats::as.formula(paste(paste(id, collapse = " + "), "~ typeValidation")), value.var = c("realized", "reported", "optimism"))
  has <- function(...) all(paste0("realized_", c(...)) %in% names(W))
  contrast <- function(name, a, b) if (has(a, b)) W[, (name) := get(paste0("realized_", a)) - get(paste0("realized_", b))]
  contrast("H1_Tainted_minus_Internal", "FutureTainted", "Internal")
  contrast("H2_PreVal_minus_Tainted", "FutureUnseen", "FutureTainted")
  contrast("P3_PreVal_minus_Spatial", "FutureUnseen", "FutureTaintedSpatial")
  contrast("P3_Spatial_minus_Tainted", "FutureTaintedSpatial", "FutureTainted")
  contrast("P4_Tainted_minus_Grouped", "FutureTainted", "FutureTaintedGrouped")
  contrast("P4_Grouped_minus_Spatial", "FutureTaintedGrouped", "FutureTaintedSpatial")
  contrast("P4_Spatial_minus_PreVal", "FutureTaintedSpatial", "FutureUnseen")
  cn <- grep("^(H1|H2|P3|P4)_", names(W), value = TRUE)
  data.table::fwrite(W, file.path(outDir, "pairedContrasts_perSplit.csv"))
  Wl <- data.table::melt(W, id.vars = id, measure.vars = cn, variable.name = "contrast", value.name = "diff")
  out$contrasts_pooled <- byYear(Wl, "diff", "contrast")
  out$contrasts_by_complexity <- byYear(Wl, "diff", c("contrast", "complexity"))
  out$contrasts_perYear <- perYear(Wl, "diff", c("contrast", "complexity"))
  Wl[, horizonBin := cut(horizon, c(0, 1, 2, 4, Inf), labels = c("1", "2", "3-4", "5+"))]
  out$contrasts_by_horizon <- byYear(Wl, "diff", c("contrast", "horizonBin"))

  out$table1_design <- M[, .(nTrain = mean(nTrain), nVal = mean(nVal), nTest = mean(nTest), unitsTrain = mean(nUnitsTrain), unitsTest = mean(nUnitsTest), models = .N), by = typeValidation]
  out$reported_realized <- data.table::rbindlist(lapply(c("reported", "realized", "optimism"), function(v) byYear(M, v, c("typeValidation", "complexity"))[, measure := v]))
  out$optimism_pooled <- byYear(M, "optimism", "typeValidation")
  if (all(c("optimism_FutureTainted", "optimism_FutureUnseen") %in% names(W))) {
    W[, optGapTainted := optimism_FutureTainted - optimism_FutureUnseen]
    out$P4b_optimism_gap_Tainted_minus_PreVal <- byYear(W, "optGapTainted", "complexity")
  }

  # ---- P5: covariate ladder ----------------------------------------------------------------------------------
  M2 <- data.table::copy(M)
  M2[, penalty := realized - realized[which.min(complexity)], by = .(typeValidation, splitId, replicate)]
  out$P5_penalty_vs_simplest <- byYear(M2, "penalty", c("typeValidation", "complexity"))
  lv <- sort(unique(M$complexity))
  if (length(lv) >= 3) {
    kmin <- as.character(min(lv)); kmax <- as.character(max(lv)); kmid <- as.character(setdiff(lv, c(min(lv), max(lv))))
    wd <- data.table::dcast(M, typeValidation + splitId + replicate + testYear + horizon ~ complexity, value.var = "realized")
    wd[, end := get(kmax) - get(kmin)]
    wd[, peak := rowMeans(.SD) - get(kmin), .SDcols = kmid]
    out$P5_shape_per_regime <- data.table::rbindlist(lapply(c("end", "peak"), function(v) byYear(wd, v, "typeValidation")[, measure := v]))
    slopes <- M[, .(slope = if (.N >= 3) unname(stats::coef(stats::lm(realized ~ log2(complexity)))[2]) else NA_real_, testYear = testYear[1]),
                by = .(typeValidation, splitId, replicate)]
    out$P5_slope_per_regime <- byYear(slopes[is.finite(slope)], "slope", "typeValidation", equivMargin = margin)
    sh <- data.table::dcast(wd, splitId + replicate + testYear ~ typeValidation, value.var = c("end", "peak"))
    dif <- list()
    for (m in c("end", "peak")) for (pr in list(c("FutureUnseen", "FutureTainted"), c("FutureUnseen", "FutureTaintedSpatial"), c("FutureTaintedSpatial", "FutureTainted"))) {
      a <- paste0(m, "_", pr[1]); b <- paste0(m, "_", pr[2])
      if (all(c(a, b) %in% names(sh))) { sh[, d := get(a) - get(b)]; dif[[paste(m, pr[1], "minus", pr[2])]] <- byYear(sh, "d", character(0))[, contrast := paste(m, pr[1], "minus", pr[2])] }
    }
    if (length(dif)) out$P5_shape_difference <- data.table::rbindlist(dif)
    out$P5_penalty_perSplit <- data.table::melt(wd, id.vars = c("typeValidation", "splitId", "replicate", "testYear", "horizon"), measure.vars = as.character(lv),
                                                variable.name = "complexity", value.name = "loss")
    # mixed model: regime x complexity interaction (partial pooling across forecast years)
    if (requireNamespace("lme4", quietly = TRUE)) {
      rows <- list()
      for (cmp in intersect(c("FutureTainted", "FutureTaintedSpatial", "FutureTaintedGrouped"), unique(M$typeValidation))) {
        d <- M[typeValidation %in% c("FutureUnseen", cmp)]
        if (uniqueN(d$testYear) < 3 || uniqueN(d$splitId) < 10) next
        d[, `:=`(regime = factor(typeValidation, c("FutureUnseen", cmp), c("PreVal", "Comparator")), k = factor(complexity))]
        fit <- tryCatch(lme4::lmer(realized ~ regime * k + (1 | testYear) + (1 | splitId), data = d), error = function(e) NULL)
        if (is.null(fit)) next
        cf <- summary(fit)$coefficients; x <- cf[grepl("regimeComparator:k", rownames(cf)), , drop = FALSE]
        rows[[cmp]] <- data.table::data.table(comparator = cmp, term = rownames(x), estimate = x[, 1], lo = x[, 1] - 1.96 * x[, 2], hi = x[, 1] + 1.96 * x[, 2], z = x[, 3])
      }
      if (length(rows)) out$P5_mixed_interactions <- data.table::rbindlist(rows)
    }
  }
  out$training_behaviour <- M[, .(share_stopped_at_cap = mean(capBound), median_best_epoch = stats::median(bestEpoch), median_epochs_run = stats::median(epochsRun), models = .N), by = typeValidation]
  for (nm in names(out)) data.table::fwrite(out[[nm]], file.path(outDir, paste0(nm, ".csv")))
  invisible(out)
}

#' Pre-declared diagnostic: per-year stability of each covariate's coefficient (negative-binomial GLM per year on standardised covariates).
#' A covariate whose coefficient changes sign or size across years (large I-squared) is the kind whose signal does not transfer in time.
#' @return data.table(Feature, meanCoef, I2, Q, pQ, nYears, signFlips, shareYearsPositive) ordered by I2
coefficientStability <- function(tab, response, features, yearCol = "year", years = NULL, minCases = 200L) {
  d <- data.table::as.data.table(tab); yrs <- if (is.null(years)) sort(unique(d[[yearCol]])) else years
  Z <- d[, lapply(.SD, function(v) { s <- stats::sd(v); if (is.na(s) || s == 0) rep(0, length(v)) else (v - mean(v)) / s }), .SDcols = features]
  rows <- lapply(yrs, function(yy) {
    i <- which(d[[yearCol]] == yy); if (length(i) < minCases) return(NULL)
    dd <- cbind(y = d[[response]][i], Z[i]); fml <- stats::as.formula(paste("y ~", paste(sprintf("`%s`", features), collapse = " + ")))
    fit <- tryCatch(suppressWarnings(MASS::glm.nb(fml, data = dd, control = stats::glm.control(maxit = 100))), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    cf <- summary(fit)$coefficients; f <- rownames(cf) != "(Intercept)"
    data.table::data.table(year = yy, Feature = gsub("`", "", rownames(cf)[f]), b = cf[f, 1], se = cf[f, 2])
  })
  r <- data.table::rbindlist(rows)
  if (!nrow(r)) return(NULL)
  r[, w := 1 / se^2]
  out <- r[, { bw <- sum(w * b) / sum(w); Q <- sum(w * (b - bw)^2); k <- .N
               .(meanCoef = bw, Q = Q, pQ = stats::pchisq(Q, k - 1, lower.tail = FALSE), I2 = max(0, (Q - (k - 1)) / Q) * 100, nYears = k,
                 signFlips = sum(diff(sign(b)) != 0), shareYearsPositive = mean(b > 0)) }, by = Feature]
  out[order(-I2)][]
}
