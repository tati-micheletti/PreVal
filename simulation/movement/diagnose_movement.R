# Realism-anchor statistics for a movement table in the caribou engine format. Works on a simulated world AND on the real final_caribou.rds, so the two can be printed next to each other.
#   Rscript simulation/movement/diagnose_movement.R                         simulate the default world (seed 1) and describe it (about 3 min)
#   Rscript simulation/movement/diagnose_movement.R <dir>                   describe the world written by writeMovementWorld() in <dir> (final_caribou.rds + truth_caribou.rds)
#   Rscript simulation/movement/diagnose_movement.R --table <final.rds> [--logSl logSl] [--cosTa cosTa]   describe ANY table in the engine format (e.g. the real final_caribou.rds): the table-only statistics
# Statistics (all computable without the truth, so they can be matched against the real data): strata per year, animals per year and new share, strata per animal-year, extent and block occupancy,
# observed minus available log step length by year (the collar-schedule pattern), turn angles, per-year single-covariate conditional logits (sign changes among the 21 identifiable columns, as in
# caribou/transferability_screen.R), and a pooled conditional logit forecast (train 2013-2017, test later years) against chance log 11.
# With the truth of a simulated world it also prints the oracle loss, the realised variance shares and which columns truly flip.
if (nzchar(Sys.getenv("LOCALAPPDATA"))) .libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))
suppressMessages({ library(data.table); library(survival) })
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(here, "simulate_movement.R")); options(width = 220)
args <- commandArgs(trailingOnly = TRUE); opt <- function(k, d = NULL) { i <- match(k, args); if (is.na(i)) d else args[i + 1L] }
meta <- c("indiv_step_id", "id", "year", "x1_", "y1_", "case_", "burst_")

# real caribou values read from existing result files (results/covariate_transferability_caribou_check_fixrate.csv, _check_composition.csv, _topk.csv; ANALYSIS_PLAN_CARIBOU_NN.md): NOT blind, they are the anchor
real <- list(
  strataByYear = c(`2013` = 16701, `2014` = 15224, `2015` = 18643, `2016` = 19837, `2017` = 20629, `2018` = 33611, `2019` = 37985, `2020` = 48748, `2021` = 42208, `2022` = 38993),
  animalsByYear = c(`2013` = 26, `2014` = 26, `2015` = 34, `2016` = 33, `2017` = 36, `2018` = 48, `2019` = 55, `2020` = 64, `2021` = 61, `2022` = 58),
  newByYear = c(`2013` = 26, `2014` = 6, `2015` = 10, `2016` = 3, `2017` = 14, `2018` = 16, `2019` = 8, `2020` = 19, `2021` = 10, `2022` = 14),
  meanLogSl = c(`2013` = 5.429, `2014` = 5.767, `2015` = 5.684, `2016` = 5.765, `2017` = 5.956, `2018` = 5.837, `2019` = 6.086, `2020` = 5.811, `2021` = 5.755, `2022` = 5.567),
  meanAbsTurn = c(`2013` = 1.524, `2014` = 1.454, `2015` = 1.492, `2016` = 1.472, `2017` = 1.478, `2018` = 1.497, `2019` = 1.443, `2020` = 1.457, `2021` = 1.437, `2022` = 1.501),
  medianStepM = c(`2013` = 287, `2014` = 410, `2015` = 390, `2016` = 421, `2017` = 511, `2018` = 436, `2019` = 554, `2020` = 428, `2021` = 399, `2022` = 346),
  flipTopK = data.table(k = c(2, 5, 10, 20), nFlipPrimary = c(1, 4, 7, 10), nIdentifiable = c(2, 5, 10, 20)), flipTotal = 10, identifiable = 21, flipMovement = 9, chance = log(11),
  statusQuoExcessOverChance = c(0.019, 0.046))

# single-covariate conditional logit by Newton steps (one coefficient, model-based SE); m = [11 steps, strata] with the observed step in row 1
singleClogit <- function(m) { b <- 0; for (it in 1:30) { e <- exp(b * m - rep(apply(b * m, 2, max), each = 11L)); p <- e / rep(colSums(e), each = 11L); mu <- colSums(p * m); sc <- sum(m[1, ] - mu); info <- sum(colSums(p * m^2) - mu^2)
  if (info <= 0) return(c(NA_real_, NA_real_)); step <- sc / info; b <- b + step; if (abs(step) < 1e-9) break }
  c(b, 1 / sqrt(info)) }
ceOf <- function(eta11) { m <- matrix(eta11, 11L); log(colSums(exp(m - rep(m[1, ], each = 11L)))) }      # cross-entropy of the observed step (row 1)

anchorStats <- function(tab, logSl, cosTa, truth = NULL, label = "table", nFit = 60000L) {
  out <- list(); tab <- as.data.table(tab); cand <- setdiff(names(tab), meta); first <- seq(1L, nrow(tab), 11L); st <- tab[first, .(id, year, x1_, y1_, burst_)]; yrs <- sort(unique(st$year)); nS <- nrow(st)
  cat(sprintf("\n===== %s =====\n", label))
  cat(sprintf("strata %s, rows %s, animals %d, animal-years %d, candidate columns %d, years %d-%d\n", format(nS, big.mark = ","), format(nrow(tab), big.mark = ","), uniqueN(st$id), nrow(unique(st[, .(id, year)])), length(cand), min(yrs), max(yrs)))
  ay <- st[, .(strata = .N), by = .(id, year)]; cat(sprintf("strata per animal-year: mean %.0f, median %.0f, quartiles %.0f-%.0f, min %d, max %d   (real: mean 663)\n", mean(ay$strata), median(ay$strata), quantile(ay$strata, .25), quantile(ay$strata, .75), min(ay$strata), max(ay$strata)))
  comp <- rbindlist(lapply(yrs, function(y) { ids <- unique(st[year == y]$id); prev <- unique(st[year < y]$id); data.table(year = y, strata = sum(st$year == y), animals = length(ids), newAnimals = length(setdiff(ids, prev)), shareNew = length(setdiff(ids, prev)) / length(ids)) }))
  cat("by year:\n"); print(comp, row.names = FALSE); if (identical(as.integer(yrs), 2013:2022)) cat("real strata:", paste(real$strataByYear, collapse = " "), "\nreal animals:", paste(real$animalsByYear, collapse = " "), "\nreal new animals:", paste(real$newByYear, collapse = " "), "\n")
  blk <- st[, .(bx = floor((x1_ - min(x1_)) / 1e5), byy = floor((y1_ - min(y1_)) / 1e5))][, .N, by = .(bx, byy)][order(-N)]
  cat(sprintf("extent %.0f x %.0f km; 100 km blocks occupied %d; share of strata in the 3 fullest blocks %.2f; animals' range (median over animals of the 95%% span of start positions) %.0f km\n", diff(range(st$x1_)) / 1e3, diff(range(st$y1_)) / 1e3, nrow(blk), sum(head(blk$N, 3)) / nS,
              median(st[, .(sp = max(diff(quantile(x1_, c(.025, .975))), diff(quantile(y1_, c(.025, .975))))), by = id]$sp) / 1e3))
  retained <- mean(sapply(yrs[-1], function(y) mean(unique(st[year == y]$id) %in% unique(st[year == y - 1L]$id)))); cat(sprintf("share of a year's animals that were also present the year before: %.2f (real, from the table above: %.2f)\n", retained, mean((real$animalsByYear[-1] - real$newByYear[-1]) / real$animalsByYear[-1])))
  # movement by year
  mv <- rbindlist(lapply(yrs, function(y) { r <- which(tab$year == y); ls <- tab[[logSl]][r]; ca <- tab[[cosTa]][r]; cs <- tab$case_[r]
    data.table(year = y, meanLogSlUsed = mean(ls[cs]), meanLogSlAvail = mean(ls[!cs]), tilt = mean(ls[cs]) - mean(ls[!cs]), medianStepUsedM = median(exp(ls[cs])), meanCosTaUsed = mean(ca[cs]), meanCosTaAvail = mean(ca[!cs])) }))
  cat("movement by year (used = observed steps, avail = the 10 available steps of the strata):\n"); print(mv[, lapply(.SD, function(v) round(v, 3))], row.names = FALSE)
  if (identical(as.integer(yrs), 2013:2022)) cat(sprintf("real mean observed log step length 2013-22: %s (mean %.3f, sd %.3f); real median step (m): %s\nthis table: sd of the yearly mean observed log step length %.3f; range %.2f-%.2f\n", paste(real$meanLogSl, collapse = " "), mean(real$meanLogSl), sd(real$meanLogSl), paste(real$medianStepM, collapse = " "), sd(mv$meanLogSlUsed), min(mv$meanLogSlUsed), max(mv$meanLogSlUsed)))
  # per-year single-covariate conditional logits on the identifiable columns (those that vary inside a stratum)
  wv <- vapply(cand, function(cn) { m <- matrix(tab[[cn]], 11L); mean((colSums(m^2) - colSums(m)^2 / 11) / 10) }, numeric(1)); ident <- cand[wv > 1e-10]
  cat(sprintf("\nidentifiable columns (vary inside a stratum): %d of %d   (real: 21 of 30)\n", length(ident), length(cand)))
  srByYear <- lapply(yrs, function(y) which(st$year == y))
  fits <- rbindlist(lapply(ident, function(cn) { Mall <- matrix(tab[[cn]], 11L); rbindlist(lapply(seq_along(yrs), function(i) { f <- singleClogit(Mall[, srByYear[[i]], drop = FALSE]); data.table(covariate = cn, year = yrs[i], beta = f[1], se = f[2]) })) }))
  fits[, z := beta / se]; flipTab <- fits[, { mb <- mean(beta, na.rm = TRUE); .(meanBeta = mb, nPositive = sum(beta > 0, na.rm = TRUE), nNegative = sum(beta < 0, na.rm = TRUE), oppositeSignYears = sum(sign(beta) != sign(mb), na.rm = TRUE), sigPos = sum(z > 1.96, na.rm = TRUE), sigNeg = sum(z < -1.96, na.rm = TRUE), pooledAbsZ = abs(sum(beta / se^2) / sqrt(sum(1 / se^2))) )}, by = covariate]
  flipTab[, flipPrimary := oppositeSignYears >= 3L][, flipStrict := sigPos >= 1L & sigNeg >= 1L]; fam <- if (!is.null(truth)) truth$covariates[match(flipTab$covariate, covariate), .(role, block, trueFamily = family, truePeak = peakEffect)] else data.table(role = NA_character_, block = NA_character_, trueFamily = NA_character_, truePeak = NA_real_)
  flipTab <- cbind(flipTab, fam)[order(-pooledAbsZ)]; flipTab[, rankByAssoc := seq_len(.N)]
  cat(sprintf("single-covariate yearly conditional logits: %d of %d identifiable columns have >= 3 years with the opposite sign (flipPrimary), %d have a significant positive AND a significant negative year (flipStrict)   (real: %d of %d; strict 9)\n", sum(flipTab$flipPrimary), nrow(flipTab), sum(flipTab$flipStrict), real$flipTotal, real$identifiable))
  if (!is.null(truth)) cat(sprintf("  of the %d flipPrimary columns, %d are movement-block columns (logSl, cosTa, interactions) and %d are habitat end columns   (real: 9 and 1)\n", sum(flipTab$flipPrimary), sum(flipTab$flipPrimary & flipTab$block == "movement", na.rm = TRUE), sum(flipTab$flipPrimary & flipTab$block == "habitat", na.rm = TRUE)))
  tk <- rbindlist(lapply(c(2, 5, 10, 20), function(k) { k2 <- min(k, nrow(flipTab)); data.table(k = k, nFlipPrimary = sum(flipTab$flipPrimary[1:k2]), share = mean(flipTab$flipPrimary[1:k2])) }))
  cat("flipPrimary among the top-k columns, ordered by the pooled association strength |z| (a stand-in for the engine's importance order):\n"); print(tk, row.names = FALSE); cat("real (first-pass importance order): top 2: 1, top 5: 4, top 10: 7, top 20: 10\n")
  cat("columns, strongest pooled association first:\n"); print(flipTab[1:min(21, .N), .(rankByAssoc, covariate, role, trueFamily, meanBeta = round(meanBeta, 3), nPositive, nNegative, oppositeSignYears, sigPos, sigNeg, flipPrimary, flipStrict)], row.names = FALSE)
  # pooled conditional-logit forecast against chance
  set.seed(1L); tr <- which(st$year <= 2017L); te <- yrs[yrs > 2017L]; if (length(tr) > nFit) tr <- sort(sample(tr, nFit)); X <- ident
  trRows <- as.vector(outer(1:11, (tr - 1L) * 11L, "+")); dtr <- as.data.frame(tab[trRows, X, with = FALSE]); dtr$case_ <- tab$case_[trRows]; dtr$s <- rep(seq_along(tr), each = 11L)
  fit <- tryCatch(clogit(as.formula(paste("case_ ~", paste(X, collapse = " + "), "+ strata(s)")), data = dtr), error = function(e) NULL)
  if (!is.null(fit)) { cf <- coef(fit); cf[is.na(cf)] <- 0; fc <- rbindlist(lapply(c(2013L:2017L, te), function(y) { r <- which(tab$year == y); data.table(year = y, set = if (y <= 2017L) "training years" else "forecast", ce = mean(ceOf(as.matrix(tab[r, X, with = FALSE]) %*% cf[X])), chance = log(11)) }))
    fc[, excessOverChance := ce - chance]; cat(sprintf("\npooled conditional logit (%d identifiable columns, %s training strata of 2013-2017), cross-entropy by year:\n", length(X), format(length(tr), big.mark = ","))); print(fc[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 4) else v)], row.names = FALSE)
    cat(sprintf("mean over forecast years: %.4f above chance (a flexible NN overfits more; the real status quo was %+.3f to %+.3f above chance, PreVal about at chance)\n", mean(fc[set == "forecast"]$excessOverChance), real$statusQuoExcessOverChance[1], real$statusQuoExcessOverChance[2])) }
  if (!is.null(truth)) {
    ol <- oracleLoss(truth, withAnimal = TRUE, withHome = TRUE); ol2 <- oracleLoss(truth, withAnimal = FALSE, withHome = FALSE)
    cat(sprintf("\nORACLE (the true selection function; chance = log 11 = %.4f): cross-entropy %.4f (gain %.4f); without the animal deviations and the home pull %.4f (gain %.4f)\n", log(11), mean(ol$ce), log(11) - mean(ol$ce), mean(ol2$ce), log(11) - mean(ol2$ce)))
    cat("oracle cross-entropy by year:", paste(sprintf("%d %.3f", ol[, mean(ce), by = year]$year, ol[, mean(ce), by = year]$V1), collapse = " | "), "\n")
    mv <- memoryValue(truth, 0:6); cat(sprintf("MEMORY value with a known truth (animal drift share %.2f, persistence %.2f): gain in cross-entropy of knowing the animal's preferences from `lag` years earlier: %s\n", truth$params$animalDriftShare, truth$params$animalDriftRho, paste(sprintf("lag %d %.4f", mv$lag, mv$memoryGain), collapse = " | ")))
    dg <- worldDiagnostics(truth); cat(sprintf("realised shares of the within-stratum variance of eta over the 11 steps of a stratum: %s; within-stratum sd of eta %.3f (target %.2f); sd of the omitted home pull term %.3f\n", paste(sprintf("%s %.2f", names(dg$realisedShares), dg$realisedShares), collapse = ", "), dg$withinSdEta, truth$params$etaSd, dg$homeSdEta))
    cat(sprintf("true yearly pattern of the movement block (shape = sqrt(1-phi^2) + phi s(t)): %s\n", paste(sprintf("%.2f", dg$movePattern), collapse = " ")))
    d2h <- { an <- truth$animals[match(st$id, id)]; sqrt((st$x1_ - an$hx)^2 + (st$y1_ - an$hy)^2) / 1e3 }; cat(sprintf("distance of the animals from their home centre (km): median %.1f, 90%% quantile %.1f (home range parameter %.0f km)\n", median(d2h), quantile(d2h, .9), truth$params$homeRangeKm))
    cat("true flip structure: habitat layers that flip: ", paste(rownames(truth$flipSigns), collapse = ", "), "; movement block:", truth$params$moveMode, "with flip strength", truth$params$moveFlipStrength, "\n")
  }
  invisible(list(comp = comp, move = mv, flips = flipTab, forecast = if (exists("fc")) fc else NULL))
}

tp <- opt("--table")
if (!is.null(tp)) { tab <- readRDS(tp); invisible(anchorStats(tab, opt("--logSl", "logSl"), opt("--cosTa", "cosTa"), label = paste("table", tp))) } else {
  wd <- if (length(args) && !startsWith(args[1], "--")) args[1] else NULL
  if (!is.null(wd)) { w <- readRDS(file.path(wd, "truth_caribou.rds")); w$table <- readRDS(file.path(wd, "final_caribou.rds")) } else {
    t0 <- Sys.time(); w <- simulateMovement(list(seed = 1L)); cat(sprintf("default world simulated in %.0f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs")))); sv <- opt("--save"); if (!is.null(sv)) writeMovementWorld(w, sv) }
  cv <- w$covariates; invisible(anchorStats(w$table, cv[role == "logSl"]$covariate, cv[role == "cosTa"]$covariate, truth = w, label = if (is.null(wd)) "default simulated world (seed 1)" else paste("world in", wd)))
}
