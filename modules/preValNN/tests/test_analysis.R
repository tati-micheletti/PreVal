# Test of the analysis on a synthetic results table with KNOWN effects, and of the coefficient-stability diagnostic
suppressMessages({library(data.table)})
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(here, "..", "R", "analyzeRegimes.R"))
nFail <- 0L
check <- function(cond, msg) { ok <- isTRUE(cond); cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) nFail <<- nFail + 1L }

set.seed(9)
tests <- 2012:2023; ks <- c(2, 5, 10, 15, 25, Inf)
# true forecast loss: PreVal 1.00, Spatial 1.04, Grouped 1.07, Tainted 1.10, Internal 0.90; status quo gets a hump in complexity, PreVal none
rows <- list(); i <- 0L
for (T in tests) { sid <- sprintf("s2005_e%d_T%d", T - 1, T); ye <- rnorm(1, 0, 0.03)
  for (k in ks) for (rg in c("FutureUnseen", "FutureTaintedSpatial", "FutureTaintedGrouped", "FutureTainted", "Internal")) {
    base <- c(FutureUnseen = 1.00, FutureTaintedSpatial = 1.04, FutureTaintedGrouped = 1.07, FutureTainted = 1.10, Internal = 0.90)[[rg]]
    hump <- if (rg %in% c("FutureTainted", "FutureTaintedGrouped")) 0.05 * dnorm(log2(ifelse(is.infinite(k), 50, k)), 3.5, 1) / dnorm(0, 0, 1) else 0
    i <- i + 1L
    rep <- base + hump + ye + rnorm(1, 0, 0.01)
    rows[[i]] <- data.table(groupId = paste(sid, k), splitId = sid, typeValidation = rg, modelName = paste(sid, k, rg), numberOfCovariates = k, replicate = 1L,
      windowStart = 2005L, historyEnd = T - 1L, testYear = T, horizon = 1L, nTrain = 1000L, nVal = 200L, nTest = 300L, nUnitsTrain = 500L, nUnitsTest = 150L,
      trainLossBest = rep - 0.1, valLossBest = rep - c(FutureUnseen = 0.02, FutureTaintedSpatial = 0.06, FutureTaintedGrouped = 0.09, FutureTainted = 0.12, Internal = 0.01)[[rg]],
      bestEpoch = 10L, epochsRun = 25L, epochCap = 100L, testLossMean = rep) } }
d <- file.path(tempdir(), "res"); dir.create(d, showWarnings = FALSE)
for (r in rows) fwrite(r, file.path(d, paste0(r$modelName, "_finalDT.csv")))
o <- analyzeRegimes(d, file.path(tempdir(), "ana"), nCovariatesAll = 50)
cp <- o$contrasts_pooled
g <- function(n) cp[contrast == n]
check(abs(g("H1_Tainted_minus_Internal")$estimate - 0.20) < 0.03 && g("H1_Tainted_minus_Internal")$pSignFlip < 0.01, sprintf("H1 contrast recovered (%.3f, truth about 0.20)", g("H1_Tainted_minus_Internal")$estimate))
check(abs(g("H2_PreVal_minus_Tainted")$estimate + 0.10) < 0.04 && g("H2_PreVal_minus_Tainted")$estimate < 0, sprintf("H2 contrast recovered (%.3f, truth about -0.10 incl. hump)", g("H2_PreVal_minus_Tainted")$estimate))
check(g("P3_PreVal_minus_Spatial")$estimate < 0 && g("P3_Spatial_minus_Tainted")$estimate < 0, "P3: PreVal < Spatial < Tainted recovered")
check(all(cp[grepl("^P4", contrast)]$estimate > 0), "P4: the weak ordering Tainted >= Grouped >= Spatial >= PreVal recovered")
check(!any(grepl("PreVal_minus_Internal|Internal_minus_PreVal", cp$contrast)), "PreVal is never contrasted with Internal")
check(all(o$optimism_pooled[typeValidation == "FutureTainted"]$estimate > o$optimism_pooled[typeValidation == "FutureUnseen"]$estimate), "P4b: status-quo optimism exceeds PreVal's")
pk <- o$P5_shape_per_regime[measure == "peak"]
check(pk[typeValidation == "FutureTainted"]$estimate > 0.01 && abs(pk[typeValidation == "FutureUnseen"]$estimate) < 0.01, "P5: the hump is found under the status quo and not under PreVal")
check(nrow(o$contrasts_perYear) > 0 && o$contrasts_pooled[1]$nYears == length(tests), "forecast year is the unit (12 years)")

# coefficient stability: one covariate flips sign over the years, one is stable
set.seed(2); n <- 600
st <- rbindlist(lapply(2005:2016, function(yy) data.table(year = yy, stable = rnorm(n), flip = rnorm(n), noise = rnorm(n))[, mu := exp(0.5 + 0.5 * stable + ifelse(yy %% 2 == 0, 0.5, -0.5) * flip)]))
st[, y := rnbinom(.N, mu = mu, size = 3)]
cs <- coefficientStability(st, "y", c("stable", "flip", "noise"))
check(cs$Feature[1] == "flip" && cs[Feature == "flip"]$I2 > 80 && cs[Feature == "stable"]$I2 < 40, sprintf("stability diagnostic flags the covariate whose coefficient changes sign (I2 %.0f vs %.0f)", cs[Feature == "flip"]$I2, cs[Feature == "stable"]$I2))
check(cs[Feature == "flip"]$signFlips >= 8 && cs[Feature == "stable"]$signFlips == 0, "sign flips counted")
# matched comparison of two arms (e.g. one vs two validation years): a built-in extra PreVal advantage of 0.03 in arm B must be recovered
dB <- file.path(tempdir(), "resB"); dir.create(dB, showWarnings = FALSE)
for (r in rows) { r2 <- data.table::copy(r); if (r2$typeValidation == "FutureUnseen") { r2[, testLossMean := testLossMean - 0.03] }; r2[, trainShare := 0.9]
  data.table::fwrite(r2, file.path(dB, paste0(r2$modelName, "_finalDT.csv"))) }
oB <- analyzeRegimes(dB, file.path(tempdir(), "anaB"), nCovariatesAll = 50)
cmp <- compareValidationArms(file.path(tempdir(), "ana"), file.path(tempdir(), "anaB"))
h2 <- cmp[contrast == "H2_PreVal_minus_Tainted"]
check(abs(h2$estimate + 0.03) < 0.005 && h2$pSignFlip < 0.01 && h2$nYears == length(tests), sprintf("arm comparison recovers the extra PreVal advantage (%.3f, truth -0.030, %d years)", h2$estimate, h2$nYears))
check(!is.null(oB$trainShare_summary) && oB$trainShare_summary$share80 == 1 && !is.null(oB$contrasts_pooled_share80), "share-restricted sensitivity and the training-share summary are written")
check(!is.null(o$EXPLORATORY_selection_by_regime) && !is.null(o$EXPLORATORY_selection_contrasts), "exploratory covariate-selection tables are written and labelled EXPLORATORY")
cat(sprintf("\n%d failure(s)\n", nFail)); if (nFail) quit(status = 1)
