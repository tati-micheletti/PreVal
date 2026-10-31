#' Covariate-novelty (out-of-distribution) analysis, pre-specified in ANALYSIS_PLAN_BIRDS.md (amendments 27 and 29).
#'
#' For every test case of the models at the chosen covariate levels, novelty = DI after Meyer and Pebesma (2021, Methods in Ecology and Evolution; area of applicability):
#' the weighted-Euclidean distance (in the standardised covariates of the level, weighted by the window's permutation importance) from the test case to its nearest
#' REFERENCE case, divided by the mean nearest-neighbour distance among reference cases. The reference set is PreVal's TRAINING cases of that split (its manifest), so a test
#' case has ONE novelty value that does not depend on the regime. Per-case losses are the calibrated ones (lossCal).
# Labels since 2026-10-30: "PreVal" = FutureUnseenGrouped (validation on the last year AND on unseen units); the old FutureUnseen (units still in training) is "FutureOnly".
# Novelty tables written before that date (birds runs of 2026-10-26) use the old labels: "PreVal" there = FutureUnseen, "PreValRoutes" = FutureUnseenGrouped.
NOVELTY_LABELS <- c(FutureUnseen = "FutureOnly", FutureTainted = "StatusQuo", FutureTaintedSpatial = "Spatial", FutureUnseenSpatial = "PreValSpatial", FutureUnseenGrouped = "PreVal")
NOVELTY_PAIRS <- list(c("PreVal", "StatusQuo"), c("Spatial", "StatusQuo"), c("PreVal", "Spatial"), c("PreValSpatial", "Spatial"), c("PreValSpatial", "StatusQuo"),
                      c("PreVal", "FutureOnly"), c("PreValSpatial", "PreVal"), c("FutureOnly", "StatusQuo"), c("PreValSpatial", "FutureOnly"))

#' @param Zref,Ztest standardised and importance-weighted covariate matrices (reference cases, test cases)
#' @param nSample number of reference cases (random, seeded) used to estimate the mean nearest-neighbour distance among reference cases (leave-one-out)
#' @return list(di, dbar, nRef)
noveltyDI <- function(Zref, Ztest, nSample = 1000L, seed = 1L) {
  nn <- function(A, B, selfIdx = NULL) {
    d2 <- outer(rowSums(A^2), rowSums(B^2), "+") - 2 * tcrossprod(A, B)
    if (!is.null(selfIdx)) d2[cbind(seq_len(nrow(A)), selfIdx)] <- Inf          # a reference case is not its own neighbour
    sqrt(pmax(apply(d2, 1, min), 0))
  }
  n <- nrow(Zref); smp <- if (n > nSample) withSeed(seed, sample.int(n, nSample)) else seq_len(n)
  dbar <- mean(nn(Zref[smp, , drop = FALSE], Zref, selfIdx = smp))
  if (!is.finite(dbar) || dbar <= 0) dbar <- 1                                    # degenerate reference (all cases identical): DI is then the raw distance
  list(di = nn(Ztest, Zref) / dbar, dbar = dbar, nRef = n)
}

#' Novelty and calibrated per-case loss of every test case of every regime, for the given covariate levels of ONE run folder.
#' @param runDir run folder (experimentPlan.csv, caseIndex.rds, splits/, rankings/, testedModels/*_perCase.rds)
#' @param tab final table (rows aligned with caseIndex.rds)
#' @return data.table(splitId, testYear, level, regime, row, DI, lossCal)
noveltyCases <- function(runDir, tab, levels = c(10, 50), nonFeatures = c("SITE_ID", "year", "Reviere", "x", "y"), nSample = 1000L, verbose = TRUE,
                         refRegime = "FutureUnseen", Xcases = NULL) {      # refRegime / Xcases (2026-10-30): caribou uses PreVal on unseen animals as the reference and a case matrix = mean over the 11 steps of each stratum
  plan <- data.table::fread(file.path(runDir, "experimentPlan.csv")); idx <- readRDS(file.path(runDir, "caseIndex.rds"))
  if (is.null(Xcases) && nrow(tab) != nrow(idx)) stop("noveltyCases: the final table and the case index of ", runDir, " have different numbers of rows")
  if (!is.null(Xcases) && nrow(Xcases) != nrow(idx)) stop("noveltyCases: Xcases and the case index of ", runDir, " have different numbers of rows")
  if (is.null(Xcases)) { feats <- setdiff(names(tab), nonFeatures); X <- as.matrix(as.data.frame(tab)[, feats, drop = FALSE]); storage.mode(X) <- "double" } else X <- Xcases
  plan <- plan[numberOfCovariates %in% levels & typeValidation %in% names(NOVELTY_LABELS)]
  splits <- unique(plan[, .(splitId, windowStart, historyEnd)]); res <- list(); nMissing <- 0L
  for (i in seq_len(nrow(splits))) {
    sid <- splits$splitId[i]; rk <- data.table::fread(file.path(runDir, "rankings", sprintf("s%d_e%d.csv", splits$windowStart[i], splits$historyEnd[i])))
    man <- readManifest(file.path(runDir, "splits"), sid, refRegime); ref <- man$row[man$set == "train"]; tst <- man$row[man$set == "test"]
    for (k in levels) {
      pk <- plan[splitId == sid & numberOfCovariates == k]; if (!nrow(pk)) next
      ff <- featuresForLevel(rk, k); w <- pmax(rk$Importance[match(ff, rk$Feature)], 0); w <- if (mean(w) > 0) w / mean(w) else rep(1, length(ff))
      Xr <- X[ref, ff, drop = FALSE]; mu <- colMeans(Xr); sg <- apply(Xr, 2, stats::sd); sg[!is.finite(sg) | sg == 0] <- 1
      zs <- function(M) sweep(sweep(sweep(M, 2, mu), 2, sg, "/"), 2, w, "*")
      di <- noveltyDI(zs(Xr), zs(X[tst, ff, drop = FALSE]), nSample = nSample, seed = stringSeed(paste("novelty", sid, k)))$di
      dd <- data.table::data.table(row = tst, DI = di)
      for (j in seq_len(nrow(pk))) {
        f <- file.path(runDir, "testedModels", paste0(pk$modelName[j], "_perCase.rds"))
        if (!file.exists(f)) { nMissing <- nMissing + 1L; next }
        pc <- readRDS(f); pc <- pc[set == "test", .(row, lossCal)]
        if (!setequal(pc$row, tst)) stop("noveltyCases: the saved test cases of ", pk$modelName[j], " differ from the manifest's test set")
        res[[length(res) + 1L]] <- cbind(data.table::data.table(splitId = sid, testYear = pk$testYear[j], level = k, regime = unname(NOVELTY_LABELS[pk$typeValidation[j]])), merge(pc, dd, by = "row"))
      }
    }
    if (verbose && i %% 50 == 0) message(sprintf("novelty: %d of %d splits", i, nrow(splits)))
  }
  if (nMissing) warning("noveltyCases: ", nMissing, " per-case files were missing and skipped")
  data.table::rbindlist(res)
}

#' Statistics of the plan (amendment 27c): terciles of DI, regime contrasts within terciles, slopes of the loss difference on log(DI); the forecast year is the unit.
#' @param cs noveltyCases() output (several arms/levels may be stacked; levels are analysed separately)
noveltyStats <- function(cs, pairs = NOVELTY_PAIRS) {
  cs <- data.table::copy(cs)
  u <- unique(cs[, .(splitId, level, row, DI)])
  br <- u[, .(b1 = stats::quantile(DI, 1 / 3, names = FALSE), b2 = stats::quantile(DI, 2 / 3, names = FALSE)), by = level]
  cs <- merge(cs, br, by = "level"); cs[, tercile := factor(ifelse(DI <= b1, "low", ifelse(DI <= b2, "mid", "high")), c("low", "mid", "high"))][, c("b1", "b2") := NULL]
  out <- list()
  out$DI_summary <- u[, .(cases = .N, p10 = stats::quantile(DI, .1, names = FALSE), median = stats::median(DI), p90 = stats::quantile(DI, .9, names = FALSE), max = max(DI)), by = level]
  ym <- cs[, .(m = mean(lossCal)), by = .(level, tercile, regime, testYear)]
  out$tercile_means <- ym[, .yearTest(m), by = .(level, tercile, regime)]
  wide <- data.table::dcast(cs, splitId + testYear + level + row + DI + tercile ~ regime, value.var = "lossCal")
  wide[, lDI := log(pmax(DI, 1e-6))]
  tc <- list(); sl <- list()
  for (p in pairs) if (all(p %in% names(wide))) {
    nm <- paste(p[1], "minus", p[2]); wide[, d := get(p[1]) - get(p[2])]
    tc[[nm]] <- wide[, .(m = mean(d)), by = .(level, tercile, testYear)][, .yearTest(m), by = .(level, tercile)][, contrast := nm]
    sl[[nm]] <- wide[, .(slope = stats::cov(lDI, d) / stats::var(lDI)), by = .(level, testYear)][is.finite(slope), .yearTest(slope), by = level][, contrast := nm]
  }
  out$tercile_contrasts <- data.table::rbindlist(tc); out$slope_on_logDI <- data.table::rbindlist(sl)
  out
}
