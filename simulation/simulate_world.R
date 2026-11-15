# Controlled simulated "worlds" for the PreVal engine (route counts, same table format as data_birds/v2/final_<species>.rds).
# One world = routes with spatial structure and a route effect, surveyed in some of the years 2005..2024, with negative-binomial counts
# whose log-mean is a sum of FIVE contributions with TARGET shares of its variance:
#   route   a route effect (partly spatial), the source of "route memory" (the leak of random validation, and of PreVal's seen routes)
#   stable  covariates with a constant effect
#   drift   covariates whose effect changes in strength (same sign unless driftRate > 1)
#   flip    covariates whose effect is a persistent part plus a yearly SIGN (Markov chain, flipProb per year), controlled by flipStrength
#   year    a year random effect
# plus pure NOISE covariates with no effect. All covariates mix a static (route-level, spatial) part, a year-wide shift (the "year
# fingerprint": it lets a model recognise a year, which is what lets random validation learn year-specific signs) and a route-year part.
# Everything is deterministic by `seed`; every component has its own seeded stream, so worlds that differ in ONE parameter share
# their routes, covariates and noise (common random numbers): the matched control of the design (flipStrength = 0 is the same world without flips).
# Nothing here uses the engine; the output only needs SITE_ID, year, x, y, Reviere and numeric covariates.

defaultWorldParams <- function() list(
  seed = 1L, nRoutes = 1200L, years = 2005:2024, surveyProb = 0.5,
  box = c(xmin = 4.05e6, xmax = 4.67e6, ymin = 2.70e6, ymax = 3.55e6), rangeKm = 150,
  nStable = 12L, nDrift = 6L, nFlip = 12L, nNoise = 30L,
  shares = c(route = 0.20, stable = 0.30, drift = 0.10, flip = 0.25, year = 0.05),      # normalised to sum 1
  etaSd = 1.0, theta = 3, meanCount = 3.9,
  decay = 0.12,                                       # coefficient size inside a family: exp(-decay * (rank - 1)), random sign, jitter 0.8-1.2
  staticShare = c(stable = 0.8, drift = 0.5, flip = 0.2, noise = 0.5), yearShare = 0.25, routeSpatialShare = 0.6,
  driftRate = 1.0,                                    # beta_j(t) = c_j * (1 + driftRate * u(t)), u from -1 (first year) to +1 (last year)
  flipMode = "random", regimeLength = 6,             # flipMode "random": every flipper has its own sign chain (flipProb per year); "regime" (added 2026-10-29): ONE regime chain shared by all flippers, mean regime length `regimeLength` years (warm / cold spells), each flipper tied to it with a random sign
  flipStrength = 1.0, flipProb = 0.3, flipShared = FALSE,   # beta_j(t) = c_j * (sqrt(1 - phi^2) + phi * s_j(t)); phi = flipStrength; 0 = the family is stable
  trend = 0, trendFamilies = c("drift", "flip", "noise"),    # shift of the dynamic covariates over the whole series, in SD units (novelty of the later years)
  # CHANGED 2026-11-05: lighter-tailed route effect. routeDist = "lognormal" is the original behaviour (route effect = a Gaussian share of var(eta), so exp() gives a log-normal multiplier on the mean).
  # routeDist = "beta": the route multiplier on the MEAN is Beta(routeBeta[["a"]], routeBeta[["b"]]) rescaled to mean 1 (bounded above at 1 / mean(Beta) times the mean; the real greenfinch counts have no
  # heavy upper tail, the log-normal one gave a variance over mean of 29 against 8). The multiplier is the rank transform of the same spatial-plus-independent Gaussian route field as before
  # (rangeKm, routeSpatialShare), so the spatial structure is kept up to the (weaker) effect of the rank transform. In this mode `shares["route"]` is ignored, the other shares are
  # renormalised among themselves, and etaSd is the sd of the NON-route part of eta (covariates + year). The route share of var(eta) is then an outcome (see worldDiagnostics).
  routeDist = "lognormal", routeBeta = c(a = 0.35, b = 0.70),
  # CHANGED 2026-11-05: route-level drift (default off). The stable level of a route is no longer constant: log-mean of route i in year t = route effect + d[i, t], d an AR(1) process per route with stationary sd `routeDrift`
  # (log scale; 0 = off: the world is then identical to the one without this option) and year-to-year autocorrelation `routeDriftRho` (exp(-1/5) = 0.82: the autocorrelation decays to 1/e in about 5 years). Covariates cannot explain it,
  # so a model that memorises the route level goes stale with the horizon: the test of the "stale memory" explanation with known truth. Its own seeded stream: switching it on changes only the counts (matched worlds).
  routeDrift = 0, routeDriftRho = exp(-1 / 5),
  # CHANGED 2026-11-15 (stage 0n, plan section 9g): three generator options, all OFF by default (the world is then identical to the one without them).
  #   routeWalkSd   NON-stationary route level: log-mean of route i in year t gets a random walk W[i, t] with step sd `routeWalkSd` per year (log scale). Unlike routeDrift (AR(1), mean-reverting: the long-run mean of the route always stays
  #                 recoverable) the walk never returns, so a route's old level keeps getting less informative as the years pass. W is 0 in the year `routeWalkPin` (default NA = the middle year, which keeps the pooled marginals of the
  #                 anchor; 1 = start of the series) and extends in both directions from there.
  #   noiseScale    scale of the count noise around the mean: count = round(mu + noiseScale * (NB count - mu)), 1 = off. Real counts are less noisy from one year to the next than negative-binomial (theta 15) counts (plan 9f).
  #   surveyRho     persistence of the survey: a route is surveyed or not in a Markov chain over the years with the same stationary probability p as before and lag-1 autocorrelation surveyRho (0 = independent years, as before).
  routeWalkSd = 0, routeWalkPin = NA_integer_, noiseScale = 1, surveyRho = 0
)

# CHANGED 2026-11-05: the calibrated birds-type realism anchor (parameter set found in simulation/sim_calibrate_anchor.R, section 9 of the analysis plan); defaultWorldParams() stays the generic world of the unit tests.
anchorWorldParams <- function() utils::modifyList(defaultWorldParams(), list(
  nRoutes = 2139L, surveyProb = 0.49, meanCount = 3.87, theta = 15, routeDist = "beta", routeBeta = c(a = 0.35, b = 0.60),
  etaSd = 0.50, rangeKm = 18, routeSpatialShare = 0.20))

# run `expr` with its own seed and restore the caller's RNG state
.seeded <- function(seed, expr) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had) old <- get(".Random.seed", envir = globalenv())
  on.exit(if (had) assign(".Random.seed", old, envir = globalenv()) else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv()))
  set.seed(as.integer(seed %% .Machine$integer.max)); force(expr)
}

# smooth random field (random Fourier features), standardised to mean 0 and sd 1 over the given points; correlation length about `ell` metres
.field <- function(x, y, ell, seed, nf = 48L) {
  .seeded(seed, {
    w <- matrix(stats::rnorm(2L * nf, 0, 1 / ell), ncol = 2L); ph <- stats::runif(nf, 0, 2 * pi)
    f <- rowSums(cos(outer(x, w[, 1]) + outer(y, w[, 2]) + rep(ph, each = length(x))))
    (f - mean(f)) / stats::sd(f)
  })
}
.std <- function(v) { s <- stats::sd(v); if (!is.finite(s) || s == 0) rep(0, length(v)) else (v - mean(v)) / s }

simulateWorld <- function(params = list()) {
  P <- utils::modifyList(defaultWorldParams(), params)
  stopifnot(P$flipMode %in% c("random", "regime"), P$regimeLength >= 1, P$flipStrength >= 0, P$flipStrength <= 1, P$flipProb >= 0, P$flipProb <= 1, P$yearShare >= 0, P$yearShare < 1, all(P$shares >= 0),
            P$routeDist %in% c("lognormal", "beta"), P$routeDrift >= 0, P$routeDriftRho >= 0, P$routeDriftRho < 1, P$routeWalkSd >= 0, P$noiseScale > 0, P$noiseScale <= 1, P$surveyRho >= 0, P$surveyRho < 1, all(P$routeBeta > 0), all(c("a", "b") %in% names(P$routeBeta)))
  shr <- P$shares; if (identical(P$routeDist, "beta")) shr[["route"]] <- 0      # CHANGED 2026-11-05: lighter-tailed route effect: no route share, the route multiplier is Beta (see defaultWorldParams)
  sh <- shr / sum(shr); V <- P$etaSd^2; yrs <- as.integer(P$years); nY <- length(yrs); sd0 <- P$seed * 1000L
  # --- routes and surveys
  rt <- .seeded(sd0 + 1L, data.table::data.table(route = sprintf("s%04d", seq_len(P$nRoutes)), x = stats::runif(P$nRoutes, P$box[["xmin"]], P$box[["xmax"]]), y = stats::runif(P$nRoutes, P$box[["ymin"]], P$box[["ymax"]]),
                                                 p = stats::rbeta(P$nRoutes, 4 * P$surveyProb, 4 * (1 - P$surveyProb))))
  ell <- P$rangeKm * 1000
  tab <- if (P$surveyRho > 0) .seeded(sd0 + 2L, {      # CHANGED 2026-11-15: persistent survey (Markov chain per route, stationary probability p, lag-1 autocorrelation surveyRho)
    nr <- nrow(rt); uu <- matrix(stats::runif(nr * nY), nr, nY); S <- matrix(FALSE, nr, nY); S[, 1] <- uu[, 1] < rt$p
    for (t in seq_len(nY)[-1]) S[, t] <- uu[, t] < ifelse(S[, t - 1], rt$p + P$surveyRho * (1 - rt$p), rt$p * (1 - P$surveyRho))
    data.table::data.table(route = rt$route[row(S)[S]], year = yrs[col(S)[S]])
  }) else .seeded(sd0 + 2L, { g <- data.table::CJ(route = rt$route, year = yrs, sorted = FALSE); g[rt, on = "route", p := i.p]; g[stats::runif(.N) < p][, p := NULL] })
  rt <- rt[route %in% tab$route]; rt[, ri := seq_len(.N)]; tab[rt, on = "route", `:=`(ri = i.ri, x = i.x, y = i.y)]
  data.table::setorder(tab, route, year); n <- nrow(tab); yi <- match(tab$year, yrs); u01 <- (yi - 1) / max(1, nY - 1); u <- 2 * u01 - 1
  # --- covariates
  fam <- rep(c("stable", "drift", "flip", "noise"), c(P$nStable, P$nDrift, P$nFlip, P$nNoise)); p <- length(fam)
  Z <- matrix(0, n, p); trendSign <- rep(1, p)
  for (j in seq_len(p)) {
    s <- P$staticShare[[fam[j]]]; yw <- P$yearShare * (1 - s); ew <- max(0, 1 - s - yw)
    stat <- .seeded(sd0 + 100L + j, sqrt(P$routeSpatialShare) * .field(rt$x, rt$y, ell, sd0 + 100L + j) + sqrt(1 - P$routeSpatialShare) * stats::rnorm(nrow(rt)))
    g <- .seeded(sd0 + 400L + j, stats::rnorm(nY)); e <- .seeded(sd0 + 700L + j, stats::rnorm(n))
    z <- .std(sqrt(s) * .std(stat)[tab$ri] + sqrt(yw) * .std(g)[yi] + sqrt(ew) * e)
    if (P$trend != 0 && fam[j] %in% P$trendFamilies) { trendSign[j] <- .seeded(sd0 + 1000L + j, sample(c(-1, 1), 1L)); z <- z + P$trend * (u01 - 0.5) * trendSign[j] }
    Z[, j] <- z
  }
  # --- effects: coefficient sizes, signs, yearly flip signs
  coefs <- numeric(p); rk <- integer(p)
  for (f in unique(fam)) { ix <- which(fam == f); rk[ix] <- seq_along(ix)
    coefs[ix] <- .seeded(sd0 + 2000L + match(f, c("stable", "drift", "flip", "noise")), sample(c(-1, 1), length(ix), TRUE) * exp(-P$decay * (seq_along(ix) - 1)) * stats::runif(length(ix), 0.8, 1.2)) }
  flipIx <- which(fam == "flip")
  S <- matrix(1, length(flipIx), nY)                                      # yearly sign of each flipping covariate
  if (length(flipIx)) .seeded(sd0 + 3000L, {
    chain <- function() { s <- numeric(nY); s[1] <- sample(c(-1, 1), 1L); for (t in seq_len(nY)[-1]) s[t] <- if (stats::runif(1) < P$flipProb) -s[t - 1] else s[t - 1]; s }
    if (identical(P$flipMode, "regime")) {
      c1 <- numeric(nY); c1[1] <- sample(c(-1, 1), 1L); for (t in seq_len(nY)[-1]) c1[t] <- if (stats::runif(1) < 1 / P$regimeLength) -c1[t - 1] else c1[t - 1]
      for (k in seq_along(flipIx)) S[k, ] <- sample(c(-1, 1), 1L) * c1
    } else if (isTRUE(P$flipShared)) { c1 <- chain(); S[] <- rep(c1, each = length(flipIx)) } else for (k in seq_along(flipIx)) S[k, ] <- chain() })
  B <- matrix(0, p, nY); phi <- P$flipStrength                             # true effect of covariate j in year t (before the family scale)
  B[fam == "stable", ] <- coefs[fam == "stable"]
  for (j in which(fam == "drift")) B[j, ] <- coefs[j] * (1 + P$driftRate * (2 * (seq_len(nY) - 1) / max(1, nY - 1) - 1))
  for (k in seq_along(flipIx)) B[flipIx[k], ] <- coefs[flipIx[k]] * (sqrt(1 - phi^2) + phi * S[k, ])
  raw <- function(f) { ix <- which(fam == f); if (!length(ix)) return(rep(0, n)); rowSums(Z[, ix, drop = FALSE] * t(B[ix, yi, drop = FALSE])) }
  scaleTo <- function(v, target) { sdv <- stats::sd(v); if (target <= 0 || sdv == 0) list(v = rep(0, n), k = 0) else list(v = v * sqrt(target * V) / sdv, k = sqrt(target * V) / sdv) }
  cs <- lapply(c("stable", "drift", "flip"), function(f) scaleTo(raw(f), sh[[f]])); names(cs) <- c("stable", "drift", "flip")
  rr <- .seeded(sd0 + 4000L, .std(sqrt(P$routeSpatialShare) * .field(rt$x, rt$y, ell, sd0 + 4001L) + sqrt(1 - P$routeSpatialShare) * stats::rnorm(nrow(rt))))
  route <- if (identical(P$routeDist, "beta")) {      # CHANGED 2026-11-05: lighter-tailed route effect: rank-transform the spatial Gaussian route field to a Beta marginal (multiplier on the mean, mean 1), log-scale in eta
    mB <- stats::qbeta(rank(rr, ties.method = "first") / (length(rr) + 1), P$routeBeta[["a"]], P$routeBeta[["b"]]); mB <- mB / mean(mB); log(mB)[tab$ri]
  } else scaleTo(rr[tab$ri], sh[["route"]])$v
  yEff <- scaleTo(.seeded(sd0 + 5000L, .std(stats::rnorm(nY)))[yi], sh[["year"]])$v
  routeDriftV <- if (P$routeDrift > 0) {      # CHANGED 2026-11-05: route-level drift (AR(1) per route, stationary sd routeDrift, stationary start); the first year is the first year of the series
    dm <- .seeded(sd0 + 4500L, { z <- matrix(stats::rnorm(nrow(rt) * nY), nrow(rt), nY); d <- z; d[, 1] <- z[, 1]; for (t in seq_len(nY)[-1]) d[, t] <- P$routeDriftRho * d[, t - 1] + sqrt(1 - P$routeDriftRho^2) * z[, t]; P$routeDrift * d })
    dm[cbind(tab$ri, yi)]
  } else rep(0, n)
  routeWalkV <- if (P$routeWalkSd > 0) {      # CHANGED 2026-11-15: non-stationary route level (random walk per route, 0 in the pinned year, own seeded stream)
    pin <- if (is.na(P$routeWalkPin)) ceiling((nY + 1) / 2) else as.integer(P$routeWalkPin)
    wm <- .seeded(sd0 + 4600L, { z <- P$routeWalkSd * matrix(stats::rnorm(nrow(rt) * nY), nrow(rt), nY); W <- matrix(0, nrow(rt), nY)
      for (t in seq_len(nY)[seq_len(nY) > pin]) W[, t] <- W[, t - 1] + z[, t]
      for (t in rev(seq_len(nY)[seq_len(nY) < pin])) W[, t] <- W[, t + 1] + z[, t]
      W })
    wm[cbind(tab$ri, yi)]
  } else rep(0, n)
  eta <- cs$stable$v + cs$drift$v + cs$flip$v + route + yEff + routeDriftV + routeWalkV
  b0 <- log(P$meanCount) - log(mean(exp(eta))); mu <- exp(b0 + eta)
  cnt <- .seeded(sd0 + 6000L, stats::rnbinom(n, mu = mu, size = P$theta))
  if (P$noiseScale < 1) cnt <- pmax(0, round(mu + P$noiseScale * (cnt - mu)))      # CHANGED 2026-11-15: less count noise than negative binomial (see noiseScale)
  # --- table in the engine's format: neutral names, shuffled column order
  ord <- .seeded(sd0 + 7000L, sample(p))                                  # position j of the family order gets the neutral name sim_cov_<ord[j]>
  colnames(Z) <- sprintf("sim_cov_%03d", ord)
  final <- data.table::data.table(SITE_ID = tab$route, year = as.integer(tab$year), Reviere = as.numeric(cnt), x = tab$x, y = tab$y)
  Zs <- data.table::as.data.table(Z[, order(ord), drop = FALSE]); final <- cbind(final, Zs)
  famTab <- data.table::data.table(covariate = sprintf("sim_cov_%03d", ord), family = fam, rankInFamily = rk)
  scl <- c(stable = cs$stable$k, drift = cs$drift$k, flip = cs$flip$k, noise = 0)
  famTab[, coefficient := coefs * scl[fam]]
  trueBeta <- B * scl[fam]; dimnames(trueBeta) <- list(famTab$covariate, yrs)
  list(table = final[], params = P, covariates = famTab[], trueBeta = trueBeta, flipSigns = `dimnames<-`(S, list(famTab$covariate[flipIx], yrs)),
       contributions = data.table::data.table(SITE_ID = tab$route, year = tab$year, stable = cs$stable$v, drift = cs$drift$v, flip = cs$flip$v, route = route, year_effect = yEff, route_drift = routeDriftV, route_walk = routeWalkV, eta = eta, mu = mu),
       intercept = b0)
}

# what the world really contains (used by the tests and logged by the smoke run)
worldDiagnostics <- function(w) {
  K <- w$contributions; vv <- c(route = stats::var(K$route), stable = stats::var(K$stable), drift = stats::var(K$drift), flip = stats::var(K$flip), year = stats::var(K$year_effect))
  fs <- w$flipSigns; flipFreq <- if (nrow(fs) && ncol(fs) > 1) mean(fs[, -1, drop = FALSE] != fs[, -ncol(fs), drop = FALSE]) else NA_real_
  list(nCases = nrow(w$table), nRoutes = data.table::uniqueN(w$table$SITE_ID), meanCount = mean(w$table$Reviere), varEta = stats::var(K$eta),
       realisedShares = vv / sum(vv), flipFrequency = flipFreq, share = vv / stats::var(K$eta))
}

writeWorld <- function(w, dir, name) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(w$table, file.path(dir, sprintf("final_%s.rds", name))); saveRDS(w[setdiff(names(w), "table")], file.path(dir, sprintf("truth_%s.rds", name)))
  invisible(file.path(dir, sprintf("final_%s.rds", name)))
}

# SIMULATION-ONLY "oracle" ladder: covariates ordered by their PEAK true effect over the years (noise last, random order), written in the format of the engine's
# per-window ranking files (run<tag>/rankings/s<start>_e<end>.csv). It puts flipping covariates in the ladder as early as their strength deserves, which the
# engine's own ranking (importance in the training years) does not do when the sign changes from year to year. Used only by the oracle arm of the simulation.
oracleRanking <- function(w) {
  pk <- apply(abs(w$trueBeta), 1, max); ct <- w$covariates[match(rownames(w$trueBeta), covariate)]
  key <- ifelse(ct$family == "noise", -stats::runif(nrow(ct)) * 1e-6, pk)
  o <- order(-key); r <- data.table::data.table(Feature = ct$covariate[o], group = ct$covariate[o], Importance = key[o], ImportanceSD = 0, marginalAssoc = NA_real_)
  r[, rank := seq_len(.N)]; r[]
}

# SIMULATION-ONLY ladders (orders of the covariates), written in the format of the engine's per-window ranking files. Together with the engine's own ranking ("natural",
# no override) they test whether the ORDER of the covariates matters (added 2026-10-29):
#   flippersFirst  flipping covariates first (by peak true effect), then stable and drifting ones (by peak effect), then noise
#   flippersLast   stable and drifting covariates first (by peak effect), then the flipping ones, then noise
#   random         all covariates, including noise, in a random order (seeded by `seed`)
#   oracle         all covariates ordered by peak true effect (the oracle ladder of oracleRanking(), made deterministic by `seed`)
#   oracleSwap     CHANGED 2026-11-05: the oracle ladder with every flipping covariate swapped with the stable covariate of the SAME RANK WITHIN ITS FAMILY (strongest flipper <-> strongest stable, ...), each in the other's ladder POSITION.
#                  The causal swap test: the two ladders occupy identical positions and differ only in which covariates (flipping instead of stable, of matched strength rank) sit there.
ladderRanking <- function(w, order = c("flippersFirst", "flippersLast", "random", "oracle", "oracleSwap"), seed = 1L) {
  order <- match.arg(order); pk <- apply(abs(w$trueBeta), 1, max); ct <- w$covariates[match(rownames(w$trueBeta), covariate)]
  if (order %in% c("oracle", "oracleSwap")) {
    key <- ifelse(ct$family == "noise", -.seeded(seed, stats::runif(nrow(ct))) * 1e-6, pk); o <- base::order(-key); feat <- ct$covariate[o]
    if (order == "oracleSwap") {
      fl <- ct[family == "flip"][base::order(rankInFamily)]$covariate; st <- ct[family == "stable"][base::order(rankInFamily)]$covariate; k <- min(length(fl), length(st))
      mp <- c(stats::setNames(st[seq_len(k)], fl[seq_len(k)]), stats::setNames(fl[seq_len(k)], st[seq_len(k)])); hit <- feat %in% names(mp); feat[hit] <- unname(mp[feat[hit]])
    }
    r <- data.table::data.table(Feature = feat, group = feat, Importance = key[o], ImportanceSD = 0, marginalAssoc = NA_real_); r[, rank := seq_len(.N)]; return(r[])
  }
  tier <- switch(order, flippersFirst = c(flip = 3, stable = 2, drift = 2, noise = 0)[ct$family], flippersLast = c(flip = 2, stable = 3, drift = 3, noise = 0)[ct$family], random = rep(0, nrow(ct)))
  key <- if (order == "random") -.seeded(seed, stats::runif(nrow(ct))) else as.numeric(tier) * 1000 + ifelse(ct$family == "noise", -.seeded(seed, stats::runif(nrow(ct))) * 1e-3, pk)
  o <- base::order(-key); r <- data.table::data.table(Feature = ct$covariate[o], group = ct$covariate[o], Importance = key[o], ImportanceSD = 0, marginalAssoc = NA_real_)
  r[, rank := seq_len(.N)]; r[]
}
