# Unit tests of simulateWorld(): format, determinism, and that the realised flips, importances, route variance, spatial structure and noise match the parameters
suppressMessages(library(data.table))
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(here, "simulate_world.R"))
nFail <- 0L
check <- function(cond, msg) { ok <- isTRUE(cond); cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) nFail <<- nFail + 1L }

w <- simulateWorld(list(seed = 3L, nRoutes = 800L)); tb <- w$table; dg <- worldDiagnostics(w)
# ---- format: the same columns and classes as data_birds/v2/final_<species>.rds
check(is.data.table(tb) && identical(names(tb)[1:5], c("SITE_ID", "year", "Reviere", "x", "y")) && is.character(tb$SITE_ID) && is.integer(tb$year) && is.numeric(tb$Reviere) && is.numeric(tb$x),
      "table has the engine's format: SITE_ID (character), year (integer), Reviere, x, y, then numeric covariates")
cv <- setdiff(names(tb), c("SITE_ID", "year", "Reviere", "x", "y"))
check(length(cv) == 60 && all(grepl("^sim_cov_", cv)) && !anyNA(tb) && all(tb$Reviere >= 0 & tb$Reviere == round(tb$Reviere)) && !anyDuplicated(tb[, .(SITE_ID, year)]) && setequal(tb$year, 2005:2024),
      "60 covariates with neutral names, no NA, integer counts, one row per route and year, all 20 years present")
check(setequal(w$covariates$covariate, cv) && identical(sort(w$covariates$family), sort(rep(c("stable", "drift", "flip", "noise"), c(12, 6, 12, 30)))), "the truth table lists every covariate with its family")
check(abs(mean(tb$Reviere) / 3.9 - 1) < 0.1 && max(tb$x) <= 4.67e6 && min(tb$y) >= 2.70e6, sprintf("mean count %.2f near the target 3.9; coordinates in the birds' EPSG:3035 box", mean(tb$Reviere)))
check(identical(simulateWorld(list(seed = 3L, nRoutes = 800L))$table, tb) && !identical(simulateWorld(list(seed = 4L, nRoutes = 800L))$table$Reviere, tb$Reviere), "deterministic by seed; a different seed gives a different world")
# ---- common random numbers: flipStrength changes only the flip effect, routes and covariates stay the same
w0 <- simulateWorld(list(seed = 3L, nRoutes = 800L, flipStrength = 0))
check(identical(w0$table[, !"Reviere"], tb[, !"Reviere"]) && !identical(w0$table$Reviere, tb$Reviere), "worlds that differ only in flipStrength share routes and covariates (matched control)")

# ---- variance shares: realised = target (route, stable, drift, flip, year)
tgt <- c(route = 0.20, stable = 0.30, drift = 0.10, flip = 0.25, year = 0.05); tgt <- tgt / sum(tgt)
check(max(abs(dg$realisedShares - tgt)) < 1e-6 && abs(dg$varEta - 1) < 0.15, sprintf("family contributions have the target variance shares (max deviation %.1e); total variance of eta %.2f (target 1)", max(abs(dg$realisedShares - tgt)), dg$varEta))
K <- w$contributions
check(abs(var(K$route) / var(K$eta) - 0.2) < 0.06, sprintf("route effect carries %.2f of the variance of eta (target 0.20)", var(K$route) / var(K$eta)))
wr <- simulateWorld(list(seed = 3L, nRoutes = 800L, shares = c(route = 0.6, stable = 0.2, drift = 0.05, flip = 0.1, year = 0.05)))
check(abs(var(wr$contributions$route) / var(wr$contributions$eta) - 0.6) < 0.08, "a larger route share is realised")
# ---- spatial structure: neighbouring routes have more similar route effects than distant ones
rt <- merge(unique(tb[, .(SITE_ID, x, y)]), K[, .(route = mean(route)), by = SITE_ID], by = "SITE_ID")
set.seed(1); pr <- data.table(a = sample(nrow(rt), 20000, TRUE), b = sample(nrow(rt), 20000, TRUE)); pr[, d := sqrt((rt$x[a] - rt$x[b])^2 + (rt$y[a] - rt$y[b])^2)]; pr[, dv := (rt$route[a] - rt$route[b])^2]
check(pr[d < 50000, mean(dv)] < 0.8 * pr[d > 300000, mean(dv)], sprintf("route effects are spatially autocorrelated (squared difference %.2f within 50 km vs %.2f beyond 300 km)", pr[d < 50000, mean(dv)], pr[d > 300000, mean(dv)]))
# ---- flips: realised yearly sign-change frequency equals flipProb
for (fp in c(0.1, 0.5)) { wf <- simulateWorld(list(seed = 5L, nRoutes = 300L, nFlip = 40L, flipProb = fp)); fr <- worldDiagnostics(wf)$flipFrequency
  check(abs(fr - fp) < 0.05, sprintf("flipProb %.1f: realised yearly sign-change frequency %.3f", fp, fr)) }
# ---- the data really contain the yearly signs: per-year regression of the noise-free log-mean recovers the sign of the strongest flipping covariates
wp <- simulateWorld(list(seed = 6L, nRoutes = 1500L, flipStrength = 1, flipProb = 0.4, shares = c(route = 0.05, stable = 0.25, drift = 0.05, flip = 0.55, year = 0.02), nNoise = 10L))
Kp <- wp$contributions; X <- as.matrix(wp$table[, wp$covariates$covariate, with = FALSE]); fl <- wp$covariates[family == "flip"][order(rankInFamily)]$covariate[1:3]
agree <- unlist(lapply(unique(wp$table$year), function(yy) { ii <- which(wp$table$year == yy); cf <- coef(lm(Kp$eta[ii] ~ X[ii, ]))[-1]; names(cf) <- colnames(X)
  sapply(fl, function(cn) sign(cf[[cn]]) == sign(wp$trueBeta[cn, as.character(yy)])) }))
check(mean(agree) > 0.9, sprintf("per-year regression recovers the sign of the 3 strongest flipping covariates in %.0f%% of year-covariate pairs", 100 * mean(agree)))
sgn <- apply(wp$flipSigns, 1, function(r) length(unique(r))); check(all(sgn == 2), "with flipStrength 1 every flipping covariate really takes both signs")
# ---- stable covariates keep their coefficient; drifting ones change strength but keep their sign (driftRate 1)
st <- w$covariates[family == "stable"]$covariate; check(all(apply(w$trueBeta[st, , drop = FALSE], 1, function(r) diff(range(r)) < 1e-12)), "stable covariates have a constant effect")
dr <- w$covariates[family == "drift"]$covariate; check(all(apply(w$trueBeta[dr, , drop = FALSE], 1, function(r) all(r * r[length(r)] >= 0)) & apply(w$trueBeta[dr, , drop = FALSE], 1, function(r) abs(diff(range(abs(r)))) > 0.1 * max(abs(r)))), "drifting covariates change in strength without changing sign")
# ---- noise covariates carry no effect; flipStrength 0 turns the flip family into a stable one
nz <- w$covariates[family == "noise"]$covariate; cn <- abs(cor(as.matrix(tb[, nz, with = FALSE]), K$eta)); check(all(w$trueBeta[nz, ] == 0) && median(cn) < 0.08, sprintf("noise covariates have no true effect; their association with the log-mean is only chance (median |r| %.3f, max %.2f: spatially structured noise can correlate with the spatial route effect by chance)", median(cn), max(cn)))
fl0 <- w0$covariates[family == "flip"]$covariate; check(all(apply(w0$trueBeta[fl0, , drop = FALSE], 1, function(r) diff(range(r)) < 1e-12)), "flipStrength 0: the flip family is stable (matched control)")
# ---- year fingerprint and novelty trend
wy <- simulateWorld(list(seed = 3L, nRoutes = 800L, yearShare = 0)); vy <- function(world) { f <- world$covariates[family == "flip"]$covariate[1:6]; mean(sapply(f, function(cn) var(world$table[, mean(get(cn)), by = year]$V1))) }
check(vy(w) > 3 * vy(wy), sprintf("yearShare creates a year-wide shift in dynamic covariates (variance of yearly means %.3f vs %.3f without)", vy(w), vy(wy)))
wt <- simulateWorld(list(seed = 3L, nRoutes = 800L, trend = 2)); fT <- wt$covariates[family == "flip"]$covariate
sh <- function(tt) mean(sapply(fT, function(cn) { d <- tt[, mean(get(cn)), by = year]; abs(d[year == 2024]$V1 - d[year == 2005]$V1) }))
check(sh(wt$table) > sh(tb) + 1.2, sprintf("trend = 2 moves the dynamic covariates by about %.1f SD over the series (%.1f without)", sh(wt$table), sh(tb)))
# ---- overdispersion follows theta
wd <- simulateWorld(list(seed = 3L, nRoutes = 800L, theta = 0.5)); check(var(wd$table$Reviere) / mean(wd$table$Reviere) > 1.5 * var(tb$Reviere) / mean(tb$Reviere), "a smaller theta gives more overdispersed counts")
# ---- the simulation-only oracle ladder: ordered by peak true effect, noise last, in the engine's ranking-file format
orc <- oracleRanking(w); fo <- w$covariates[match(orc$Feature, covariate)]$family
check(all(c("Feature", "group", "Importance", "ImportanceSD", "marginalAssoc", "rank") %in% names(orc)) && nrow(orc) == 60 && all(fo[1:30] != "noise") && all(fo[31:60] == "noise") && !is.unsorted(rev(orc$Importance[1:30])),
      "oracle ladder: true covariates by peak effect first, noise last, engine ranking format")
# ---- ladder orderings (natural = the engine's own ranking, no file) and regime flips
fam <- function(r) w$covariates[match(r$Feature, covariate)]$family
lf <- ladderRanking(w, "flippersFirst"); ll <- ladderRanking(w, "flippersLast"); lr <- ladderRanking(w, "random", seed = 5L)
check(all(fam(lf)[1:12] == "flip") && all(fam(lf)[13:30] %in% c("stable", "drift")) && all(fam(lf)[31:60] == "noise"), "flippers-first ladder: 12 flippers, then stable and drifting, then noise")
check(all(fam(ll)[1:18] %in% c("stable", "drift")) && all(fam(ll)[19:30] == "flip") && all(fam(ll)[31:60] == "noise"), "flippers-last ladder: stable and drifting first, flippers next, noise last")
check(nrow(lr) == 60 && !anyDuplicated(lr$Feature) && identical(lr$Feature, ladderRanking(w, "random", seed = 5L)$Feature) && !identical(lr$Feature, ladderRanking(w, "random", seed = 6L)$Feature) && !all(fam(lr)[31:60] == "noise"), "random ladder: all covariates, deterministic by seed, noise mixed in")
check(all(c("Feature", "group", "Importance", "ImportanceSD", "marginalAssoc", "rank") %in% names(lf)) && identical(lf$rank, 1:60), "ladders are in the engine's ranking-file format")
wr <- simulateWorld(list(seed = 3L, nRoutes = 800L, flipMode = "regime", regimeLength = 8, flipStrength = 1))
sg <- wr$flipSigns
check(all(abs(sg) == 1) && length(unique(apply(sg, 1, function(v) paste(v * v[1], collapse = "")))) == 1, "regime flips: all flippers follow ONE regime chain (up to a fixed sign each)")
wl <- sapply(1:40, function(sd) { z <- simulateWorld(list(seed = sd, nRoutes = 100L, nNoise = 2L, nStable = 2L, nDrift = 2L, nFlip = 2L, flipMode = "regime", regimeLength = 8)); mean(z$flipSigns[1, -1] != z$flipSigns[1, -ncol(z$flipSigns)]) })
check(abs(mean(wl) - 1 / 8) < 0.04, sprintf("regime length: realised change frequency %.3f near 1/8", mean(wl)))
check(identical(wr$table[, !"Reviere"], simulateWorld(list(seed = 3L, nRoutes = 800L, flipMode = "random", flipStrength = 1))$table[, !"Reviere"]), "flip mode changes only the counts, not the routes or covariates (matched worlds)")
cat(sprintf("\n%d failure(s)\n", nFail)); if (nFail) quit(status = 1)
