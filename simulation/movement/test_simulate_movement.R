# Unit tests of simulateMovement(): format of the caribou table, strata of 1 + 10 steps, determinism and common random numbers, the truth (stable / drift / flip / movement block, individual
# deviations, year fingerprint, trend), recovery of the true coefficients by a conditional logit, recovery of the yearly signs, and the ladders.
# Run from anywhere:  Rscript simulation/movement/test_simulate_movement.R   (about 5-8 minutes: three small movement worlds are simulated step by step)
if (nzchar(Sys.getenv("LOCALAPPDATA"))) .libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))
suppressMessages({ library(data.table); library(survival) })
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(here, "simulate_movement.R"))
repoRoot <- normalizePath(file.path(here, "..", ".."))
nFail <- 0L; nPass <- 0L
check <- function(cond, msg) { ok <- isTRUE(cond); cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (ok) nPass <<- nPass + 1L else nFail <<- nFail + 1L }
meta <- c("indiv_step_id", "id", "year", "x1_", "y1_", "case_", "burst_")
tiny <- function(extra = list()) utils::modifyList(list(years = 2013:2018, animalsPerYear = rep(3L, 6), newPerYear = c(3L, 1L, 1L, 1L, 1L, 1L), stepsPerAnimalYear = 4, stepsSdLog = 0, minSteps = 2L, warmup = 0L), extra)

# ---------------- format (a small world with the real animal numbers per year and a few steps each)
wf <- simulateMovement(list(seed = 3L, stepsPerAnimalYear = 3, stepsSdLog = 0, minSteps = 3L, warmup = 0L)); tb <- wf$table
cv <- setdiff(names(tb), meta)
check(is.data.table(tb) && identical(names(tb)[1:7], meta) && is.character(tb$indiv_step_id) && is.character(tb$id) && is.integer(tb$year) && is.logical(tb$case_) && is.integer(tb$burst_) && is.numeric(tb$x1_),
      "table has the caribou engine's columns: indiv_step_id, id (character), year (integer), x1_, y1_, case_ (logical), burst_, then numeric covariates")
check(length(cv) == 30 && all(grepl("^mv_cov_", cv)) && !anyNA(tb) && all(vapply(tb[, cv, with = FALSE], is.numeric, logical(1))), "30 candidate columns (9 layers x 3 + logSl, cosTa, sinTa) with neutral names, no NA")
nS <- nrow(tb) / 11; first <- seq(1L, nrow(tb), by = 11L)
check(nrow(tb) %% 11 == 0 && all(tb$indiv_step_id[first] == tb$indiv_step_id[first + 10L]) && sum(tb$case_) == nS && all(tb$case_[first]) && !anyDuplicated(tb$indiv_step_id[first]) &&
        all(tabulate(match(tb$indiv_step_id, tb$indiv_step_id[first])) == 11L), sprintf("every stratum is 1 observed + 10 available steps in a contiguous block of 11 rows (%d strata)", nS))
check(setequal(tb$year, 2013:2022) && identical(as.integer(tb[case_ == TRUE, uniqueN(id), by = year][order(year)]$V1), defaultMovementParams()$animalsPerYear), "all 10 years present with the real number of animals per year (26, 26, 34, 33, 36, 48, 55, 64, 61, 58)")
nw <- sapply(2013:2022, function(y) { ids <- unique(tb[year == y]$id); prev <- if (y > 2013) unique(tb[year < y]$id) else character(0); length(setdiff(ids, prev)) })
check(identical(as.integer(nw), defaultMovementParams()$newPerYear) && uniqueN(tb$id) == 126, "new animals per year follow the real composition (26, 6, 10, 3, 14, 16, 8, 19, 10, 14; 126 animals)")
ct <- wf$covariates; byRole <- function(r) ct[role == r]$covariate
check(setequal(ct$covariate, cv) && all(table(ct$role)[c("cosTa", "end", "inter", "logSl", "sinTa", "start")] == c(1, 9, 9, 1, 1, 9)), "truth table lists every column with its role")
sc <- as.matrix(tb[, byRole("start"), with = FALSE]); ws <- apply(sc, 2, function(v) max(abs(v - rep(v[first], each = 11L))))
check(max(ws) < 1e-12, "the `_start` columns are constant inside a stratum (they cancel in the conditional logit)")
for (l in 1:9) { s <- ct[role == "start" & layer == l]$covariate; i <- ct[role == "inter" & layer == l]$covariate; lg <- ct[role == "logSl"]$covariate
  if (!isTRUE(all.equal(tb[[i]], tb[[lg]] * tb[[s]]))) { check(FALSE, sprintf("interaction of layer %d = logSl x start value", l)); break } else if (l == 9) check(TRUE, "interaction columns = logSl x the `_start` value of their layer (all 9)") }
wv1 <- function(v) mean((colSums(matrix(v, 11L)^2) - colSums(matrix(v, 11L))^2 / 11) / 10)
check(all(vapply(byRole("end"), function(cn) wv1(tb[[cn]]) > 1e-6, logical(1))) && wv1(tb[[ct[role == "logSl"]$covariate]]) > 0.5, "end columns and logSl vary between the steps of a stratum")
gm <- wf$geometry; d <- sqrt((gm$x2_ - tb$x1_)^2 + (gm$y2_ - tb$y1_)^2); okSl <- mean(abs(d - gm$sl_) < 1e-6 * pmax(1, gm$sl_))   # clamping at the box edge is the only exception
check(okSl > 0.999 && all(gm$sl_ > 0) && all(abs(gm$ta_) <= pi + 1e-9) && max(abs(tb[[ct[role == "logSl"]$covariate]] - log(gm$sl_))) < 1e-9, sprintf("geometry is consistent: step length = distance start-end (%.3f%% of steps), logSl = log(sl), turn angles in [-pi, pi]", 100 * okSl))
nx <- tb[case_ == TRUE]; gx <- gm[tb$case_]; i2 <- which(nx$id[-1] == nx$id[-nrow(nx)] & nx$year[-1] == nx$year[-nrow(nx)])
check(max(abs(nx$x1_[i2 + 1L] - gx$x2_[i2])) < 1e-6 && max(abs(nx$y1_[i2 + 1L] - gx$y2_[i2])) < 1e-6, "the animal really moves: the start of each stratum is the end of its previous observed step")
check(max(tb$x1_) <= 1.4e6 && min(tb$x1_) >= 1.0e6 && max(tb$y1_) <= 2.4e6 && min(tb$y1_) >= 2.0e6, "positions stay inside the 400 x 400 km box")
engineIdx <- file.path(repoRoot, "modules", "preValNN", "R", "strataLayout.R"); prepF <- file.path(repoRoot, "modules", "preValPrep_Caribou", "R", "prepCaribou.R")
if (file.exists(engineIdx) && file.exists(prepF)) { e1 <- new.env(); sys.source(engineIdx, envir = e1); e2 <- new.env(); sys.source(prepF, envir = e2); `%||%` <- function(a, b) if (is.null(a)) b else a
  ok1 <- tryCatch({ ix <- e1$buildStrataCaseIndex(tb); nrow(ix) == nS }, error = function(e) FALSE); ok2 <- tryCatch({ e2$checkCaribouFinal(tb[, c(meta, cv), with = FALSE], cv); TRUE }, error = function(e) FALSE)
  check(ok1 && ok2, "the engine's own buildStrataCaseIndex() and the prep module's checkCaribouFinal() accept the table") } else check(TRUE, "engine files not found next to the tests: engine acceptance of the table skipped")
d1 <- tempfile(); writeMovementWorld(wf, d1, repoRoot = repoRoot)
check(all(file.exists(file.path(d1, c("final_caribou.rds", "truth_caribou.rds", "featureTable.csv", "networkSettings.rds")))) && isTRUE(all.equal(readRDS(file.path(d1, "final_caribou.rds")), tb)) && nrow(fread(file.path(d1, "featureTable.csv"))) == 30 &&
        isTRUE(all.equal(readRDS(file.path(d1, "networkSettings.rds"))$lr, 0.001)), "writeMovementWorld writes final_caribou.rds, truth, featureTable.csv (30 candidates) and the caribou networkSettings.rds (lr 0.001)")

# ---------------- determinism and common random numbers
w3 <- function(extra = list(), seed = 3L) simulateMovement(tiny(utils::modifyList(list(seed = seed), extra)))
w20 <- w3(list(nCandidates = 20L)); t20 <- w20$table
check(nrow(t20) %% 11 == 0 && sum(t20$case_) == nrow(t20) / 11 && all(t20$case_[seq(1L, nrow(t20), 11L)]) && !anyNA(t20) && !isTRUE(all.equal(t20$mv_cov_001, w3()$table$mv_cov_001)), "nCandidates 20 (observed step chosen among 20 candidates, 10 fresh available steps) gives valid strata of 11 rows")
a <- w3(); b <- w3(); c4 <- w3(seed = 4L)
check(identical(a$table, b$table) && identical(a$trueBeta, b$trueBeta) && !identical(a$table$mv_cov_001, c4$table$mv_cov_001) && !identical(a$trueBeta, c4$trueBeta), "deterministic by seed; a different seed gives a different world")
f0 <- w3(list(flipStrength = 0, moveFlipStrength = 0)); f1 <- w3(list(flipStrength = 1, moveFlipStrength = 1))
stb <- a$covariates[family %in% c("stable", "drift") & block == "habitat"]$covariate
check(identical(f0$animals[, .(id, herd, hx, hy)], f1$animals[, .(id, herd, hx, hy)]) && identical(f0$deviations, f1$deviations) && identical(f0$active, f1$active) && identical(f0$covariates$internal, f1$covariates$internal) &&
        isTRUE(all.equal(f0$trueBeta[stb, ], f1$trueBeta[stb, ])) && !isTRUE(all.equal(f0$trueBeta, f1$trueBeta)) && identical(f0$table[case_ == TRUE, .N, by = .(id, year)], f1$table[case_ == TRUE, .N, by = .(id, year)]),
      "worlds that differ only in flipStrength / moveFlipStrength share animals, herds, homes, individual deviations, composition and the stable and drifting coefficients (matched control)")

# ---------------- flips are what they are said to be
fz <- w3(list(flipStrength = 1, moveFlipStrength = 1, moveMode = "random", nFlip = 40L, nStable = 2L, nDrift = 1L, nNoise = 1L, flipProb = 0.3), seed = 5L)
fl <- fz$covariates[role == "end" & layerFamily == "flip"]$covariate; B <- fz$trueBeta[fl, ]
fs <- fz$flipSigns[fl, ]; fr <- worldDiagnostics(fz)$flipFrequency
check(all(sign(B) == sign(B[, 1]) * sign(fs) * sign(fs[, 1])) && all(apply(B, 1, function(r) all(r != 0))), "flipStrength 1: the yearly sign of every flipping layer equals its stored sign chain (coefficient = c x s(t))")
check(abs(fr - 0.3) < 0.08, sprintf("flipProb 0.3: realised yearly sign-change frequency %.3f (%d flippers x 5 transitions)", fr, length(fl)))
f85 <- w3(list(flipStrength = 0.85, nFlip = 40L), seed = 5L); B85 <- f85$trueBeta[f85$covariates[role == "end" & layerFamily == "flip"]$covariate, ]
fs85 <- f85$flipSigns[rownames(B85), ]
check(all(sign(B85) == sign(B85[, 1]) * sign(fs85) * sign(fs85[, 1])), "flipStrength 0.85 still changes sign with the chain (persistent part sqrt(1 - phi^2) = 0.53 < 0.85)")
f40 <- w3(list(flipStrength = 0.4, nFlip = 40L), seed = 5L); B40 <- f40$trueBeta[f40$covariates[role == "end" & layerFamily == "flip"]$covariate, ]
check(all(apply(B40, 1, function(r) all(sign(r) == sign(r[1])))) && any(apply(B40, 1, function(r) diff(range(r)) > 0)), "flipStrength 0.4: the effect changes size but never sign")
f00 <- f0$trueBeta[f0$covariates[role == "end" & layerFamily == "flip"]$covariate, , drop = FALSE]
check(all(apply(f00, 1, function(r) diff(range(r)) < 1e-12)), "flipStrength 0: the flip family is stable (matched control)")
rg <- w3(list(flipMode = "regime", regimeLength = 2, flipStrength = 1, nFlip = 20L), seed = 6L); Br <- rg$trueBeta[rg$covariates[role == "end" & layerFamily == "flip"]$covariate, ]
sr <- sign(Br) * sign(Br[, 1])
check(all(abs(sr - rep(sr[1, ], each = nrow(sr))) < 1e-9) && any(sr[1, ] != 1), "flipMode regime: all flippers follow ONE shared sign chain (each tied to it with a fixed random sign)")
stbv <- a$covariates[family == "stable" & role == "end"]$covariate; drv <- a$covariates[layerFamily == "drift" & role == "end"]$covariate
check(all(apply(a$trueBeta[stbv, , drop = FALSE], 1, function(r) diff(range(r)) < 1e-12)) && all(apply(a$trueBeta[drv, , drop = FALSE], 1, function(r) all(r * r[length(r)] >= 0))) && all(apply(a$trueBeta[drv, , drop = FALSE], 1, function(r) diff(range(abs(r))) > 0.1 * max(abs(r)))),
      "stable layers have a constant effect; drifting layers change strength without changing sign")
nzc <- a$covariates[layerFamily == "noise" | role %in% c("start", "sinTa")]$covariate
check(all(a$trueBeta[nzc, ] == 0) && all(a$covariates[covariate %in% nzc]$family == "noise"), "noise layers, the `_start` columns and sinTa have no true effect")
mvS <- sign(fz$trueBeta[fz$covariates[block == "movement" & peakEffect > 0]$covariate, ]); mvS <- mvS * mvS[, 1]
check(all(abs(mvS - rep(mvS[1, ], each = nrow(mvS))) < 1e-9), "movement block (moveMode random): logSl, cosTa and all interactions share ONE year pattern (one flipping group, as in the real caribou screen)")
ws2 <- w3(list(moveMode = "schedule", moveFlipStrength = 1, years = 2013:2022, animalsPerYear = rep(3L, 10), newPerYear = c(3L, rep(1L, 9))))
lgn <- ws2$covariates[role == "logSl"]$covariate; idx <- defaultMovementParams()$scheduleIndex
check(suppressWarnings(cor(ws2$trueBeta[lgn, ], idx, method = "spearman")) > 0.999 && any(ws2$trueBeta[lgn, ] > 0) && any(ws2$trueBeta[lgn, ] < 0), "moveMode schedule, moveFlipStrength 1: the yearly logSl coefficient is the real mean observed log step length by year (Spearman 1) and changes sign")
nm <- w3(list(moveMode = "none")); lgN <- nm$covariates[role == "logSl"]$covariate
check(diff(range(nm$trueBeta[lgN, ])) < 1e-12 && all(nm$covariates[block == "movement" & peakEffect > 0]$family == "stable"), "moveMode none: the movement block is stable")
yw <- w3(list(yearShare = 0.5, years = 2013:2018, animalsPerYear = rep(40L, 6), newPerYear = c(40L, rep(8L, 5)), stepsPerAnimalYear = 2, minSteps = 1L), seed = 7L); y0 <- w3(list(yearShare = 0, years = 2013:2018, animalsPerYear = rep(40L, 6), newPerYear = c(40L, rep(8L, 5)), stepsPerAnimalYear = 2, minSteps = 1L), seed = 7L)
vyr <- function(w) { cn <- w$covariates[role == "start" & layerFamily == "flip"]$covariate; mean(sapply(cn, function(c1) var(w$table[case_ == TRUE, mean(get(c1)), by = year]$V1))) }
check(vyr(yw) > 5 * vyr(y0), sprintf("yearShare creates a year-wide shift of the layers (variance of yearly means of a flip layer's start value %.3f vs %.3f without)", vyr(yw), vyr(y0)))
tr <- w3(list(trend = 3, years = 2013:2018, animalsPerYear = rep(40L, 6), newPerYear = c(40L, rep(8L, 5)), stepsPerAnimalYear = 2, minSteps = 1L), seed = 7L); t0 <- w3(list(trend = 0, years = 2013:2018, animalsPerYear = rep(40L, 6), newPerYear = c(40L, rep(8L, 5)), stepsPerAnimalYear = 2, minSteps = 1L), seed = 7L)
shf <- function(w) mean(sapply(w$covariates[role == "start" & layerFamily %in% c("flip", "drift", "noise")]$covariate, function(c1) { d <- w$table[case_ == TRUE, mean(get(c1)), by = year]; abs(d[year == 2018]$V1 - d[year == 2013]$V1) }))
check(shf(tr) > shf(t0) + 1.5, sprintf("trend = 3 moves the dynamic layers by about %.1f SD over the series (%.1f without); stable layers are not moved", shf(tr), shf(t0)))
# individual deviations: herds share part of them
bigP <- list(seed = 8L, years = 2013:2018, animalsPerYear = rep(150L, 6), newPerYear = c(150L, rep(0L, 5)), stepsPerAnimalYear = 1, minSteps = 1L, nHerds = 8L, herdSdKm = 15, animalRangeKm = 120)
big <- simulateMovement(tiny(utils::modifyList(bigP, list(animalDriftShare = 0))))
dv <- big$deviations[, which(big$layerFamily != "noise"), 1]; hs <- big$animals$herd; same <- outer(hs, hs, "=="); dd <- as.matrix(dist(dv))^2; diag(same) <- NA
check(mean(dd[which(same)], na.rm = TRUE) < 0.85 * mean(dd[which(!same)], na.rm = TRUE) && all(big$deviations[, big$layerFamily == "noise", ] == 0), "individual deviations are spatially shared (animals of the same herd are more alike) and absent on noise layers")
check(all(abs(big$deviations[, , 1] - big$deviations[, , 6]) < 1e-12), "animalDriftShare 0: the individual preferences are fixed over the years")
# MEMORY factor: drifting individual preferences. corr(year 1, year t) = (1 - share) + share * rho^(t - 1); the variance does not change with the drift
cr <- function(w, t) { e <- which(w$layerFamily != "noise"); cor(as.vector(w$deviations[, e, 1]), as.vector(w$deviations[, e, t])) }
dr <- simulateMovement(tiny(utils::modifyList(bigP, list(animalDriftShare = 0.5, animalDriftRho = 0.6)))); d1 <- simulateMovement(tiny(utils::modifyList(bigP, list(animalDriftShare = 1, animalDriftRho = 0))))
okDr <- abs(cr(dr, 2) - (0.5 + 0.5 * 0.6)) < 0.08 && abs(cr(dr, 6) - (0.5 + 0.5 * 0.6^5)) < 0.08 && abs(cr(d1, 4)) < 0.08
check(okDr, sprintf("drifting individual preferences: correlation between year 1 and year 2 / 6 is %.2f / %.2f (expected %.2f / %.2f); with share 1 and rho 0 years are independent (%.2f)", cr(dr, 2), cr(dr, 6), 0.8, 0.5 + 0.5 * 0.6^5, cr(d1, 4)))
vv <- function(w, t) { e <- which(w$layerFamily != "noise"); var(as.vector(w$deviations[, e, t])) }
check(abs(vv(dr, 1) / vv(dr, 6) - 1) < 0.2 && abs(vv(dr, 3) / vv(big, 3) - 1) < 0.2, "the variance of the individual preferences does not depend on the drift (same animal share in every year)")

# memory value with a known truth: fixed preferences keep their value at long lags, drifting ones lose it
mp <- list(seed = 9L, years = 2013:2018, animalsPerYear = rep(22L, 6), newPerYear = c(22L, rep(0L, 5)), stepsPerAnimalYear = 120, stepsSdLog = 0.2, shares = c(stable = 0.2, drift = 0.05, flip = 0.05, move = 0.1, animal = 0.6), etaSd = 0.7, homePull = 0)
mFix <- memoryValue(simulateMovement(utils::modifyList(mp, list(animalDriftShare = 0))), lags = c(0, 2, 4)); mDr <- memoryValue(simulateMovement(utils::modifyList(mp, list(animalDriftShare = 1, animalDriftRho = 0))), lags = c(0, 2, 4))
check(mFix$memoryGain[1] > 0.02 && mFix$memoryGain[3] > 0.8 * mFix$memoryGain[1] && mDr$memoryGain[1] > 0.02 && mDr$memoryGain[3] < 0.3 * mDr$memoryGain[1],
      sprintf("memory value (true model with the animal's preferences of `lag` years earlier minus without them, cross-entropy): fixed preferences %s at lags 0/2/4; drifting preferences %s", paste(sprintf("%.3f", mFix$memoryGain), collapse = "/"), paste(sprintf("%.3f", mDr$memoryGain), collapse = "/")))

# ---------------- recovery of the true coefficients by a conditional logit on a stable-only world
sp <- list(seed = 11L, years = 2013:2015, animalsPerYear = c(24L, 24L, 30L), newPerYear = c(24L, 4L, 8L), stepsPerAnimalYear = 700, stepsSdLog = 0.3, flipStrength = 0, moveMode = "none", moveFlipStrength = 0, driftRate = 0,
           homePull = 0, etaSd = 0.7, shares = c(stable = 0.45, drift = 0.1, flip = 0.1, move = 0.35, animal = 0))
t0 <- Sys.time(); ws <- simulateMovement(sp); cat(sprintf("   (stable-only world: %d strata, %.0f s)\n", nrow(ws$table) / 11, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
idc <- ws$covariates[role %in% c("end", "inter", "logSl", "cosTa", "sinTa")]; X <- idc$covariate
fit <- clogit(as.formula(paste("case_ ~", paste(X, collapse = " + "), "+ strata(indiv_step_id)")), data = as.data.frame(ws$table[, c("case_", "indiv_step_id", X), with = FALSE]))
est <- coef(fit); se <- sqrt(diag(vcov(fit))); tru <- rowMeans(ws$trueBeta[X, ]); z <- (est - tru) / se
check(max(abs(est - tru)) < 0.06 && mean(abs(z) < 3) >= 0.9 && cor(est, tru) > 0.99, sprintf("stable-only world: a conditional logit recovers the true coefficients (21 identifiable columns; max |error| %.3f; correlation %.4f; %.0f%% within 3 SE)", max(abs(est - tru)), cor(est, tru), 100 * mean(abs(z) < 3)))
check(abs(est[ws$covariates[role == "logSl"]$covariate] - tru[ws$covariates[role == "logSl"]$covariate]) < 0.03 && all(abs(est[ws$covariates[layerFamily == "noise" & role == "end"]$covariate]) < 0.05), "the logSl coefficient is recovered and the noise layers get coefficients near 0")
dgs <- worldDiagnostics(ws); ol <- oracleLoss(ws, withHome = FALSE)
tg <- c(stable = 0.45, drift = 0.1, flip = 0.1, move = 0.35, animal = 0)
check(abs(dgs$withinSdEta / 0.7 - 1) < 0.1 && max(abs(dgs$realisedShares - tg)) < 0.13, sprintf("realised variance shares of eta over the 11 steps of a stratum match the targets within the scatter between worlds (up to about 0.12: the shares are calibrated before the animals move; stable tends to come out high and movement low) (realised %s; max deviation %.3f; within-stratum sd of eta %.3f, target 0.7)",
      paste(sprintf("%s %.2f", names(tg), dgs$realisedShares), collapse = ", "), max(abs(dgs$realisedShares - tg)), dgs$withinSdEta))
check(mean(ol$ce) < log(11) - 0.05 && all(ol[, mean(ce), by = year]$V1 < log(11)), sprintf("the true selection function beats chance on the observed steps: oracle cross-entropy %.3f against log 11 = %.3f", mean(ol$ce), log(11)))
nz2 <- ws$covariates[family == "noise" & role == "end" & layerFamily == "noise"]$covariate; pv <- 2 * pnorm(-abs(z[nz2]))
check(TRUE, sprintf("(info) noise-layer p-values of the conditional logit: %s", paste(sprintf("%.2f", pv), collapse = ", ")))

# ---------------- a world with strong flips: yearly conditional logits recover the yearly signs
fp <- list(seed = 12L, years = 2013:2018, animalsPerYear = rep(22L, 6), newPerYear = c(22L, rep(5L, 5)), stepsPerAnimalYear = 420, stepsSdLog = 0.3, flipStrength = 1, flipMode = "random", flipProb = 0.5, nFlip = 3L, nStable = 3L, nDrift = 1L, nNoise = 2L,
           moveMode = "schedule", moveFlipStrength = 1, etaSd = 0.7, shares = c(stable = 0.2, drift = 0.05, flip = 0.35, move = 0.3, animal = 0.1), homePull = 2)
t0 <- Sys.time(); wp <- simulateMovement(fp); cat(sprintf("   (flip world: %d strata, %.0f s)\n", nrow(wp$table) / 11, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
top <- c(wp$covariates[role == "end" & layerFamily == "flip"]$covariate, wp$covariates[role == "logSl"]$covariate); idc <- wp$covariates[role %in% c("end", "inter", "logSl", "cosTa") & layerFamily %in% c("stable", "drift", "flip", NA)]$covariate
agree <- unlist(lapply(2013:2018, function(yy) { dt <- as.data.frame(wp$table[year == yy, c("case_", "indiv_step_id", idc), with = FALSE])
  cf <- coef(clogit(as.formula(paste("case_ ~", paste(idc, collapse = " + "), "+ strata(indiv_step_id)")), data = dt)); sapply(top, function(cn) sign(cf[[cn]]) == sign(wp$trueBeta[cn, as.character(yy)])) }))
check(mean(agree) >= 0.9, sprintf("yearly conditional logits recover the sign of the 3 flipping layers and of logSl in %.0f%% of the %d year-covariate pairs", 100 * mean(agree), length(agree)))
sg <- apply(wp$flipSigns, 1, function(r) length(unique(r))); check(all(sg == 2), "with flipStrength 1 every flipping layer really takes both signs in this world")
dgp <- worldDiagnostics(wp); check(max(abs(dgp$realisedShares - c(stable = 0.2, drift = 0.05, flip = 0.35, move = 0.3, animal = 0.1) / 1)) < 0.09, sprintf("realised shares in the flip world: %s", paste(sprintf("%s %.2f", names(dgp$realisedShares), dgp$realisedShares), collapse = ", ")))
# movement flips follow the schedule: the yearly mean of observed minus available log step length tracks the year pattern
lg <- wp$covariates[role == "logSl"]$covariate; dl <- wp$table[, .(used = mean(get(lg)[case_]), avail = mean(get(lg)[!case_])), by = year][order(year)]; dl[, tilt := used - avail]
ints <- wp$covariates[role == "inter"]; sts <- wp$covariates[role == "start"]       # the effective logSl coefficient of a year also contains the interactions evaluated at the start habitat of that year
beff <- sapply(2013:2018, function(yy) { r <- wp$table[year == yy & case_ == TRUE]; wp$trueBeta[lg, as.character(yy)] + sum(sapply(1:9, function(l) wp$trueBeta[ints[layer == l]$covariate, as.character(yy)] * mean(r[[sts[layer == l]$covariate]]))) })
check(cor(dl$tilt, beff, method = "spearman") > 0.8, sprintf("the movement pattern is visible in the data: yearly observed-minus-available mean log step length follows the effective logSl coefficient (logSl + interactions at the start habitat; Spearman %.2f)", cor(dl$tilt, beff, method = "spearman")))

# ---------------- ladders (the simulation-only orders), in the engine's ranking-file format
orc <- oracleRanking(wp); ctp <- wp$covariates[match(orc$Feature, covariate)]
check(all(c("Feature", "group", "Importance", "ImportanceSD", "marginalAssoc", "rank") %in% names(orc)) && nrow(orc) == 30 && setequal(orc$Feature, wp$covariates$covariate) && all(ctp$peakEffect[1:sum(ctp$peakEffect > 0)] > 0) && !is.unsorted(rev(orc$Importance[ctp$peakEffect > 0])),
      "oracle ladder: columns with a true effect by peak effect first, zero-effect columns last, engine ranking format")
ff <- ladderRanking(wp, "flippersFirst"); fl2 <- ladderRanking(wp, "flippersLast"); rn <- ladderRanking(wp, "random", seed = 5L); fam <- function(r) wp$covariates[match(r$Feature, covariate)]
nF <- sum(wp$covariates$family == "flip" & wp$covariates$peakEffect > 0); nSD <- sum(wp$covariates$family %in% c("stable", "drift") & wp$covariates$peakEffect > 0)
check(all(fam(ff)$family[1:nF] == "flip") && all(fam(fl2)$family[1:nSD] %in% c("stable", "drift")) && all(fam(fl2)$family[nSD + 1:nF] == "flip") && setequal(rn$Feature, wp$covariates$covariate) && identical(rn$Feature, ladderRanking(wp, "random", seed = 5L)$Feature) && !identical(rn$Feature, ladderRanking(wp, "random", seed = 6L)$Feature),
      sprintf("flippers-first puts the %d flipping columns first, flippers-last puts them after the %d stable and drifting ones, random is a seeded permutation of all 30", nF, nSD))

cat(sprintf("\n%d passed, %d failed\n", nPass, nFail)); if (nFail > 0L) quit(status = 1L)
