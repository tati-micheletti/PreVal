# "SMURFS": controlled simulated movement worlds for the PreVal engine in its caribou layout (dataLayout = "strata"): the movement twin of simulation/simulate_world.R ("DODOS", birds-type).
# Design: simulation/movement/DESIGN_MOVEMENT_SIMULATION.md. Nothing here uses the engine; the output only needs the columns that
# modules/preValPrep_Caribou writes: indiv_step_id, id (animal), year, x1_, y1_ (start of the observed step), case_, burst_ and numeric candidate covariates, in blocks of 11 rows
# per stratum (the observed step first, then 10 available steps).
#
# One world = animals (grouped in herds, with a home centre) that move through a landscape of smooth habitat layers for several years. At every step the animal looks at
# `nCandidates` possible steps drawn from a FIXED availability kernel (gamma step length, von Mises turn angle, "the kernel fitted on the pooled data") and chooses one with probability
# proportional to exp(eta). eta is a step-selection function with KNOWN coefficients:
#   habitat   layer values at the END of the step; layers belong to families stable (constant), drift (same sign, changing size), flip (sign changes between years), noise (no effect)
#   movement  log step length, cos(turn angle) and the interactions log step length x habitat at the START of the step. Their coefficients are the "tilt" of the true movement kernel
#             against the fixed availability kernel; they change from year to year when the collar schedule (step duration) changes. ONE shared year pattern drives the whole block:
#             in the real caribou data the movement terms are the flipping group, and the interactions are tied to the step length (one group, not ten)
#   animal    individual deviations of the habitat coefficients (random slopes, partly shared by neighbouring animals of a herd): the source of "animal memory" that the 8-d embedding can use
#   year      a year fingerprint: habitat layers carry a year-wide shift (constant within a stratum, so it cancels in the conditional logit), which lets a network recognise a year
#   home      a weak pull back towards the home centre (the old smurf "village attraction"): omitted from the candidate set, like range fidelity in the real data
# The 30 candidate columns mirror the real caribou table: for each of 9 layers `<layer>_start`, `<layer>_end` and `inter_logSl_x_<layer>_start`, plus logSl, cosTa, sinTa. The names are
# neutral (mv_cov_001 ...) and shuffled, as in the birds worlds. The coefficient of a column is the coefficient on the column AS SUPPLIED, so a conditional logit on the identifiable
# columns (the `_start` columns are constant within a stratum and cancel out) estimates exactly the truth stored in `trueBeta`.
# Everything is deterministic by `seed`; every component has its own seeded stream, and the random numbers of the movement loop are drawn in lock-step by (year, step number, animal), so worlds that
# differ in ONE coefficient parameter share landscape, animals, herds, home centres, composition, individual deviations and the innovations of every step (common random numbers).
# Trajectories separate after the first different choice, so the matched control is exact for the world components and statistical for the paths.

defaultMovementParams <- function() list(
  seed = 1L, years = 2013:2022,
  animalsPerYear = c(26L, 26L, 34L, 33L, 36L, 48L, 55L, 64L, 61L, 58L),                      # real caribou table, animals per year (results/covariate_transferability_caribou_check_composition.csv)
  newPerYear     = c(26L, 6L, 10L, 3L, 14L, 16L, 8L, 19L, 10L, 14L),                        # animals appearing for the first time (the rest are kept from the previous year)
  stepsPerAnimalYear = 663, stepsSdLog = 0.5, minSteps = 30L, warmup = 30L,                  # real: 292,579 strata over 441 animal-years = 663; lognormal spread between animal-years (assumed)
  box = c(xmin = 1.00e6, xmax = 1.40e6, ymin = 2.00e6, ymax = 2.40e6),                       # projected metres; 400 x 400 km, room for several 100 km blocks
  nHerds = 6L, herdSdKm = 25, homeRangeKm = 15, homePull = 1,                                  # herds of animals; pull towards the home centre (0 = none); homePull 1 gives a median distance from the home centre of about 15 km
  nStable = 4L, nDrift = 2L, nFlip = 1L, nNoise = 2L,                                          # habitat layers by family (9 layers -> 30 candidate columns, as in the real table). Real caribou screen: stable = wetland, mixed forest, vegetation, distance to paved roads; drift (same sign, changing size) = the two disturbance clocks; flip = needleleaf cover; ~no effect = distance to unpaved roads and to polygons
  fineKm = 1.5, broadKm = 40, fineShare = 0.6,                                                 # patch scale and regional scale of the layers; share of the layer variance at the patch scale
  kernelShape = 0.9, kernelScaleM = 650, kernelKappa = 0.3, nCandidates = 11L,                # availability kernel (gamma step length in metres, von Mises turn angle); candidates per step. 11 = the animal chooses among the 11 steps of its stratum (exact conditional-logit truth); > 11 = chooses among that many candidates and the 10 available steps are fresh kernel draws (approaches the continuous step-selection model; slower, coefficients slightly attenuated)
  shares = c(stable = 0.40, drift = 0.05, flip = 0.10, move = 0.15, animal = 0.30),           # shares of the within-stratum variance of eta over the candidate steps (before the choice); normalised to sum 1. The movement share is set by the yearly swing of the mean observed log step length (real sd 0.18): with 0.35 the swing was 0.31, with 0.15 it is about 0.2; the real importance of the movement terms (about 60%) is NOT matched at this share (known deviation, stage 0)
  etaSd = 0.6,                                                                                 # sd of eta within a stratum (candidate steps); the oracle then gains about 0.1 below chance (log 11), see diagnose_movement.R
  decay = 0.12, driftRate = 1.0,
  flipMode = "random", regimeLength = 3, flipStrength = 0.85, flipProb = 0.3,                 # habitat flips; as in the birds worlds: beta_j(t) = c_j (sqrt(1 - phi^2) + phi s_j(t)); phi = flipStrength; 0 = stable (matched control)
  moveMode = "schedule", moveFlipStrength = 1.0, moveFlipProb = 0.3,                         # movement block: "none" (stable), "random", "regime" or "schedule" (the year pattern is the standardised scheduleIndex)
  scheduleIndex = c(5.4287777, 5.7673164, 5.6838715, 5.7646501, 5.9559958, 5.8373823, 6.0859957, 5.8114349, 5.7552628, 5.5670361),   # real mean observed log step length by year 2013-2022 (check_fixrate.csv)
  yearShare = 0.25, animalSpatialShare = 0.6, animalRangeKm = 60,
  animalDriftShare = 0.5, animalDriftRho = 0.6,                                                # MEMORY factor: share of the individual preference variance that drifts over the years (0 = all fixed), and its year-to-year persistence (AR(1)); corr(preference in year s, year t) = (1 - share) + share * rho^|t - s|
  trend = 0, trendFamilies = c("drift", "flip", "noise"),                                       # shift of the dynamic layers over the series in SD units (novelty of the later years)
  keepGeometry = TRUE
)

.mvSeeded <- function(seed, expr) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had) old <- get(".Random.seed", envir = globalenv())
  on.exit(if (had) assign(".Random.seed", old, envir = globalenv()) else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv()))
  set.seed(as.integer(seed %% .Machine$integer.max)); force(expr)
}
.mvStd <- function(v) { s <- stats::sd(v); if (!is.finite(s) || s == 0) rep(0, length(v)) else (v - mean(v)) / s }

# random-Fourier-feature layers: a smooth field per layer at two scales (patches and region), variance 1; evaluated at any point, so available steps need no raster
.mvLayerSet <- function(P, L, seed, nF = 16L, nB = 16L) .mvSeeded(seed, {
  fine <- P$fineKm * 1000; broad <- P$broadKm * 1000; Fn <- nF + nB
  Wext <- matrix(0, 3L, L * Fn); A <- matrix(0, L * Fn, L)
  for (l in seq_len(L)) {
    cols <- (l - 1L) * Fn + seq_len(Fn)
    Wext[1:2, cols] <- cbind(matrix(stats::rnorm(2L * nF, 0, 1 / fine), 2L), matrix(stats::rnorm(2L * nB, 0, 1 / broad), 2L)); Wext[3L, cols] <- stats::runif(Fn, 0, 2 * pi)
    A[cols, l] <- c(rep(sqrt(P$fineShare * 2 / nF), nF), rep(sqrt((1 - P$fineShare) * 2 / nB), nB))
  }
  list(Wext = Wext, A = A, L = L)
})
.mvBase <- function(LS, xr, yr) cos(cbind(xr, yr, 1) %*% LS$Wext) %*% LS$A          # [n, L], coordinates relative to the box corner

# von Mises turn angle through a tabulated inverse CDF, so that the random numbers are plain uniforms (needed for lock-step draws)
.mvVonMisesQuantile <- function(kappa) {
  th <- seq(-pi, pi, length.out = 4001L); d <- exp(kappa * (cos(th) - 1)); cdf <- cumsum(d); cdf <- (cdf - cdf[1]) / (cdf[length(cdf)] - cdf[1])
  function(u) { r <- stats::approx(cdf, th, xout = as.vector(u), ties = "ordered", rule = 2)$y; if (!is.null(dim(u))) matrix(r, nrow(u), ncol(u)) else r }
}

.mvComposition <- function(P, seed) .mvSeeded(seed, {
  nY <- length(P$years); n <- as.integer(P$animalsPerYear); nw <- as.integer(P$newPerYear); nw[1] <- n[1]
  active <- vector("list", nY); ids <- 0L; cur <- integer(0)
  for (t in seq_len(nY)) {
    keepN <- min(max(0L, n[t] - nw[t]), length(cur)); kept <- if (keepN > 0L) sort(cur[sample.int(length(cur), keepN)]) else integer(0)
    newN <- n[t] - length(kept); newIds <- if (newN > 0L) ids + seq_len(newN) else integer(0); ids <- ids + newN
    cur <- c(kept, newIds); active[[t]] <- cur
  }
  A <- matrix(FALSE, ids, nY); for (t in seq_len(nY)) A[active[[t]], t] <- TRUE
  A
})

simulateMovement <- function(params = list()) {
  P <- utils::modifyList(defaultMovementParams(), params)
  yrs <- as.integer(P$years); nY <- length(yrs); sd0 <- P$seed * 1000L
  stopifnot(P$flipMode %in% c("random", "regime"), P$moveMode %in% c("none", "random", "regime", "schedule"), P$regimeLength >= 1, P$flipStrength >= 0, P$flipStrength <= 1,
            P$moveFlipStrength >= 0, P$moveFlipStrength <= 1, P$flipProb >= 0, P$flipProb <= 1, P$yearShare >= 0, P$yearShare < 1, all(P$shares >= 0), length(P$animalsPerYear) == nY, length(P$newPerYear) == nY,
            P$nCandidates >= 11L, P$etaSd > 0, P$animalDriftShare >= 0, P$animalDriftShare <= 1, P$animalDriftRho >= 0, P$animalDriftRho < 1)
  sh <- P$shares / sum(P$shares); V <- P$etaSd^2; M <- as.integer(P$nCandidates); ys <- P$yearShare
  lfam <- rep(c("stable", "drift", "flip", "noise"), c(P$nStable, P$nDrift, P$nFlip, P$nNoise)); L <- length(lfam); effL <- which(lfam != "noise")
  xlo <- P$box[["xmin"]]; xhi <- P$box[["xmax"]]; ylo <- P$box[["ymin"]]; yhi <- P$box[["ymax"]]
  # --- layers, year fingerprint, trend
  LS <- .mvLayerSet(P, L, sd0 + 1L)
  g <- .mvSeeded(sd0 + 400L, apply(matrix(stats::rnorm(nY * L), nY, L), 2, .mvStd)); u01 <- (seq_len(nY) - 1) / max(1, nY - 1)
  tsign <- .mvSeeded(sd0 + 1000L, sample(c(-1, 1), L, TRUE)); trendShift <- outer(P$trend * (u01 - 0.5), tsign * (lfam %in% P$trendFamilies))
  zYear <- sqrt(ys) * g + trendShift                                                              # [nY, L]
  zAt <- function(x, y, ti) { b <- .mvBase(LS, x - xlo, y - ylo); sqrt(1 - ys) * b + rep(zYear[ti, ], each = nrow(b)) }
  # --- animals, herds, homes, composition, individual deviations (all independent of the coefficients)
  act <- .mvComposition(P, sd0 + 5L); nAn <- nrow(act); aid <- sprintf("A%03d", seq_len(nAn)); firstYr <- apply(act, 1, function(r) which(r)[1])
  home <- .mvSeeded(sd0 + 10L, {
    mg <- 60e3; hc <- cbind(stats::runif(P$nHerds, xlo + mg, xhi - mg), stats::runif(P$nHerds, ylo + mg, yhi - mg)); herd <- sample.int(P$nHerds, nAn, TRUE)
    hx <- pmin(pmax(hc[herd, 1] + stats::rnorm(nAn, 0, P$herdSdKm * 1000), xlo + 20e3), xhi - 20e3); hy <- pmin(pmax(hc[herd, 2] + stats::rnorm(nAn, 0, P$herdSdKm * 1000), ylo + 20e3), yhi - 20e3)
    data.table::data.table(id = aid, herd = herd, hx = hx, hy = hy) })
  uDev <- .mvSeeded(sd0 + 20L, { nf <- 24L; ell <- P$animalRangeKm * 1000; out <- matrix(0, nAn, L)
    for (l in seq_len(L)) { w <- matrix(stats::rnorm(2L * nf, 0, 1 / ell), 2L); ph <- stats::runif(nf, 0, 2 * pi)
      f <- sqrt(2 / nf) * rowSums(cos(cbind(home$hx - xlo, home$hy - ylo) %*% w + rep(ph, each = nAn))); out[, l] <- sqrt(P$animalSpatialShare) * f + sqrt(1 - P$animalSpatialShare) * stats::rnorm(nAn) }
    out })
  nSteps <- .mvSeeded(sd0 + 30L, { m <- matrix(pmax(P$minSteps, round(P$stepsPerAnimalYear * exp(stats::rnorm(nAn * nY, 0, P$stepsSdLog) - P$stepsSdLog^2 / 2))), nAn, nY); m[!act] <- 0L; m })
  # --- coefficients (before the block scales): habitat by family, yearly flip signs, movement block with ONE year pattern
  cHab <- numeric(L)
  for (f in c("stable", "drift", "flip")) { ix <- which(lfam == f); if (length(ix)) cHab[ix] <- .mvSeeded(sd0 + 2000L + match(f, c("stable", "drift", "flip")), sample(c(-1, 1), length(ix), TRUE) * exp(-P$decay * (seq_along(ix) - 1)) * stats::runif(length(ix), 0.8, 1.2)) }
  flipIx <- which(lfam == "flip"); S <- matrix(1, length(flipIx), nY)
  signChain <- function(prob) { s <- numeric(nY); s[1] <- sample(c(-1, 1), 1L); for (t in seq_len(nY)[-1]) s[t] <- if (stats::runif(1) < prob) -s[t - 1] else s[t - 1]; s }
  if (length(flipIx)) .mvSeeded(sd0 + 3000L, {
    if (identical(P$flipMode, "regime")) { c1 <- signChain(1 / P$regimeLength); for (k in seq_along(flipIx)) S[k, ] <- sample(c(-1, 1), 1L) * c1 } else for (k in seq_along(flipIx)) S[k, ] <- signChain(P$flipProb) })
  Bh <- matrix(0, L, nY); phi <- P$flipStrength
  for (l in which(lfam == "stable")) Bh[l, ] <- cHab[l]
  for (l in which(lfam == "drift")) Bh[l, ] <- cHab[l] * (1 + P$driftRate * (2 * (seq_len(nY) - 1) / max(1, nY - 1) - 1))
  for (k in seq_along(flipIx)) Bh[flipIx[k], ] <- cHab[flipIx[k]] * (sqrt(1 - phi^2) + phi * S[k, ])
  phiM <- if (identical(P$moveMode, "none")) 0 else P$moveFlipStrength
  sm <- .mvSeeded(sd0 + 3500L, switch(P$moveMode, none = rep(1, nY), random = signChain(P$moveFlipProb), regime = signChain(1 / P$regimeLength),
                                      schedule = { v <- stats::approx(seq_along(P$scheduleIndex), P$scheduleIndex, n = nY)$y; .mvStd(v) }))
  shapeM <- sqrt(1 - phiM^2) + phiM * sm
  mvc <- .mvSeeded(sd0 + 3600L, { gl <- numeric(L); gl[effL] <- sample(c(-1, 1), length(effL), TRUE) * 0.5 * exp(-P$decay * (seq_along(effL) - 1)) * stats::runif(length(effL), 0.8, 1.2); list(sgnCos = sample(c(-1, 1), 1L), gl = gl) })
  bL0 <- 0.35; bC0 <- 0.4 * mvc$sgnCos                                                           # relative sizes inside the movement block (logSl : cosTa : interactions of the 7 effect layers = 0.35 : 0.4 : 0.5 x decay)
  # --- calibration of the block scales on candidate steps drawn from the kernel (before the animals move with their habitat selection): shares of the within-stratum variance of eta, at start points taken from a pilot walk
  # of the same animals (kernel and home pull only); the year shifts cancel inside a stratum. The variance of the interactions depends on the squared start value, so the realised shares of a world
  # still vary with where its herds happen to live and with how habitat selection moves the animals; worldDiagnostics() reports the realised shares.
  nc <- 2L * L + 2L; kern <- .mvVonMisesQuantile(P$kernelKappa); n0 <- 1500L; nu <- 10L; R2 <- (P$homeRangeKm)^2
  px <- pmin(pmax(home$hx + .mvSeeded(sd0 + 40L, stats::rnorm(nAn, 0, P$homeRangeKm * 500)), xlo), xhi); py <- pmin(pmax(home$hy + .mvSeeded(sd0 + 41L, stats::rnorm(nAn, 0, P$homeRangeKm * 500)), ylo), yhi); hd <- .mvSeeded(sd0 + 42L, stats::runif(nAn, -pi, pi))
  # pilot walk with the kernel and the home pull only (no habitat selection, no layers): where the animals of each year will be, so that the calibration is made at realistic start points
  pilotPts <- .mvSeeded(sd0 + 50L, { qx <- px; qy <- py; qh <- hd
    lapply(seq_len(nY), function(ti) { aa <- which(act[, ti]); tot <- nSteps[aa, ti] + ifelse(firstYr[aa] == ti, P$warmup, 0L); pts <- matrix(0, 0L, 2L); hx <- home$hx[aa]; hy <- home$hy[aa]
      for (n in seq_len(max(tot))) { ra <- which(n <= tot); nr <- length(ra); sx <- qx[aa][ra]; sy <- qy[aa][ra]
        sl <- matrix(stats::qgamma(stats::runif(nr * 11L), P$kernelShape, scale = P$kernelScaleM), nr, 11L); br <- qh[aa][ra] + matrix(kern(stats::runif(nr * 11L)), nr, 11L)
        ex <- pmin(pmax(sx + sl * cos(br), xlo), xhi); ey <- pmin(pmax(sy + sl * sin(br), ylo), yhi)
        eta <- if (P$homePull != 0) -P$homePull * (((ex - hx[ra])^2 + (ey - hy[ra])^2) / 1e6 - ((sx - hx[ra])^2 + (sy - hy[ra])^2) / 1e6) / (2 * R2) else matrix(0, nr, 11L)
        ch <- max.col(eta - log(-log(matrix(stats::runif(nr * 11L), nr, 11L))), ties.method = "first"); ii <- cbind(seq_len(nr), ch)
        qx[aa[ra]] <- ex[ii]; qy[aa[ra]] <- ey[ii]; qh[aa[ra]] <- br[ii]; if (n %% 5L == 0L || n == max(tot)) pts <- rbind(pts, cbind(qx[aa[ra]], qy[aa[ra]])) }
      pts }) })
  Sig <- lapply(seq_len(nY), function(ti) .mvSeeded(sd0 + 5000L + ti, {
    pk <- sample.int(nrow(pilotPts[[ti]]), n0, TRUE); sx <- pilotPts[[ti]][pk, 1]; sy <- pilotPts[[ti]][pk, 2]; h <- stats::runif(n0, -pi, pi)
    sl <- matrix(stats::qgamma(stats::runif(n0 * 11L), P$kernelShape, scale = P$kernelScaleM), n0, 11L); ta <- matrix(kern(stats::runif(n0 * 11L)), n0, 11L); br <- h + ta
    ze <- zAt(as.vector(sx + sl * cos(br)), as.vector(sy + sl * sin(br)), ti); zs <- zAt(sx, sy, ti); lsl <- log(sl)
    X <- cbind(ze, matrix(as.vector(lsl) * zs[rep(seq_len(n0), 11L), ], ncol = L), as.vector(lsl), as.vector(cos(ta)))     # [n0*11, 2L+2]; row i + n0*(c-1) = stratum i candidate c
    Xc <- apply(X, 2, function(v) { m <- matrix(v, n0, 11L); as.vector(m - rowMeans(m)) }); crossprod(Xc) / (n0 * 10) }))
  blockVec <- function(fam, ti) { b <- numeric(nc)
    if (fam == "move") { b[L + seq_len(L)] <- mvc$gl * shapeM[ti]; b[2L * L + 1L] <- bL0 * shapeM[ti]; b[2L * L + 2L] <- bC0 * shapeM[ti] } else b[which(lfam == fam)] <- Bh[lfam == fam, ti]
    b }
  kScale <- vapply(c("stable", "drift", "flip", "move"), function(fam) { if (sh[[fam]] <= 0 || (fam != "move" && !any(lfam == fam))) return(0)
    v <- mean(vapply(seq_len(nY), function(ti) { b <- blockVec(fam, ti); sum(b * (Sig[[ti]] %*% b)) }, numeric(1))); if (v <= 0) 0 else sqrt(sh[[fam]] * V / v) }, numeric(1))
  tau <- if (sh[["animal"]] <= 0 || !length(effL)) 0 else sqrt(sh[["animal"]] * V / mean(vapply(seq_len(nY), function(ti) sum(diag(Sig[[ti]])[effL]), numeric(1))))
  Bpop <- matrix(0, L, nY)                                                                          # [L, nY] population coefficients of the end columns (noise layers stay 0)
  for (f in c("stable", "drift", "flip")) Bpop[lfam == f, ] <- Bh[lfam == f, , drop = FALSE] * kScale[[f]]
  bLs <- bL0 * shapeM * kScale[["move"]]; bCs <- bC0 * shapeM * kScale[["move"]]; Gm <- outer(mvc$gl, shapeM) * kScale[["move"]]   # [nY], [nY], [L, nY]
  # individual deviations of the habitat coefficients (effect layers only), per animal, layer and YEAR: a fixed part (shared partly by the herd) plus a DRIFTING part (AR(1) over the years with persistence
  # animalDriftRho; added 2026-11-05 for the memory factor). The variance is the same in every year, so the animal share of eta does not depend on the drift. A fixed preference is what the animal embedding can
  # learn and keep using (memory helps at short and long horizons); a drifting one is memory that goes stale: its value in the training years predicts the test year less well as the horizon grows.
  wDrift <- .mvSeeded(sd0 + 25L, { a <- array(stats::rnorm(nAn * L * nY), c(nAn, L, nY)); for (t in seq_len(nY)[-1]) a[, , t] <- P$animalDriftRho * a[, , t - 1L] + sqrt(1 - P$animalDriftRho^2) * a[, , t]; a })
  devArr <- array(0, c(nAn, L, nY)); for (t in seq_len(nY)) devArr[, , t] <- tau * (sqrt(1 - P$animalDriftShare) * uDev + sqrt(P$animalDriftShare) * wDrift[, , t])
  devArr[, lfam == "noise", ] <- 0
  # --- the movement loop: all animals of a year move together, one step per iteration; random numbers are drawn in lock-step for all animals of the year
  yearPieces <- vector("list", nY)
  for (ti in seq_len(nY)) {
    aa <- which(act[, ti]); nA <- length(aa); ns <- nSteps[aa, ti]; wm <- ifelse(firstYr[aa] == ti, P$warmup, 0L); tot <- ns + wm; nStr <- sum(ns)
    E <- matrix(0, 11L * nStr, L); G3 <- matrix(0, 11L * nStr, 7L); Zs <- matrix(0, nStr, L); SXY <- matrix(0, nStr, 2L); Aid <- integer(nStr); Stp <- integer(nStr); ptr <- 0L
    Bp <- matrix(Bpop[, ti], nA, L, byrow = TRUE) + matrix(devArr[aa, , ti], nA, L); gamT <- Gm[, ti]; hxA <- home$hx[aa]; hyA <- home$hy[aa]
    .mvSeeded(sd0 + 10000L + ti, for (n in seq_len(max(tot))) {
      U1 <- matrix(stats::runif(nA * M), nA, M); U2 <- matrix(stats::runif(nA * M), nA, M); U3 <- matrix(stats::runif(nA * M), nA, M)
      if (M > 11L) { V1 <- matrix(stats::runif(nA * nu), nA, nu); V2 <- matrix(stats::runif(nA * nu), nA, nu) }
      ra <- which(n <= tot); nr <- length(ra); sx <- px[aa][ra]; sy <- py[aa][ra]; h <- hd[aa][ra]
      sl <- matrix(stats::qgamma(U1[ra, , drop = FALSE], P$kernelShape, scale = P$kernelScaleM), nr, M); ta <- matrix(kern(U2[ra, , drop = FALSE]), nr, M); br <- h + ta
      ex <- pmin(pmax(sx + sl * cos(br), xlo), xhi); ey <- pmin(pmax(sy + sl * sin(br), ylo), yhi)
      Ze <- zAt(as.vector(ex), as.vector(ey), ti); zs <- zAt(sx, sy, ti); lsl <- log(sl)
      eta <- matrix(rowSums(Ze * Bp[ra, , drop = FALSE][rep(seq_len(nr), M), , drop = FALSE]), nr, M) + bLs[ti] * lsl + bCs[ti] * cos(ta) + lsl * as.vector(zs %*% gamT)
      if (P$homePull != 0) eta <- eta - P$homePull * (((ex - hxA[ra])^2 + (ey - hyA[ra])^2) / 1e6 - ((sx - hxA[ra])^2 + (sy - hyA[ra])^2) / 1e6) / (2 * R2)
      ch <- max.col(eta - log(-log(U3[ra, , drop = FALSE])), ties.method = "first"); ii <- cbind(seq_len(nr), ch); rr <- which(n > wm[ra]); nrec <- length(rr)
      if (nrec > 0L) {
        used <- ii[rr, , drop = FALSE]; ui <- used[, 1] + nr * (used[, 2] - 1L)
        if (M == 11L) {                                   # choice-set mode: the 11 candidates ARE the stratum; the 10 not chosen are the available steps (exact conditional-logit likelihood)
          idm <- matrix(1:11, 11L, nrec); oth <- t(matrix(idm[idm != rep(ch[rr], each = 11L)], 10L, nrec)); ai <- cbind(rep(rr, nu), as.vector(oth)); aRow <- rep(rr, nu) + nr * (as.vector(oth) - 1L)
          sa <- matrix(sl[ai], nrec, nu); tb <- matrix(ta[ai], nrec, nu); eax <- matrix(ex[ai], nrec, nu); eay <- matrix(ey[ai], nrec, nu); Za <- Ze[aRow, , drop = FALSE]
        } else {                                          # large choice set: the observed step is chosen among M candidates (approximates "kernel x exp(eta)"), the 10 available steps are fresh draws from the kernel
          sa <- matrix(stats::qgamma(V1[ra[rr], , drop = FALSE], P$kernelShape, scale = P$kernelScaleM), nrec, nu); tb <- matrix(kern(V2[ra[rr], , drop = FALSE]), nrec, nu); bra <- h[rr] + tb
          eax <- pmin(pmax(sx[rr] + sa * cos(bra), xlo), xhi); eay <- pmin(pmax(sy[rr] + sa * sin(bra), ylo), yhi); Za <- zAt(as.vector(eax), as.vector(eay), ti)
        }
        s <- ptr + seq_len(nrec); rows <- as.vector(outer(1:11, (s - 1L) * 11L, "+"))
        for (l in seq_len(L)) E[rows, l] <- as.vector(t(cbind(Ze[ui, l], matrix(Za[, l], nrec, nu))))
        slU <- sl[used]; taU <- ta[used]; slAll <- cbind(slU, sa); taAll <- cbind(taU, tb); exAll <- cbind(ex[used], eax); eyAll <- cbind(ey[used], eay)
        G3[rows, 1] <- as.vector(t(log(slAll))); G3[rows, 2] <- as.vector(t(cos(taAll))); G3[rows, 3] <- as.vector(t(sin(taAll))); G3[rows, 4] <- as.vector(t(slAll)); G3[rows, 5] <- as.vector(t(taAll)); G3[rows, 6] <- as.vector(t(exAll)); G3[rows, 7] <- as.vector(t(eyAll))
        Zs[s, ] <- zs[rr, , drop = FALSE]; SXY[s, 1] <- sx[rr]; SXY[s, 2] <- sy[rr]; Aid[s] <- aa[ra[rr]]; Stp[s] <- n - wm[ra[rr]]; ptr <- ptr + nrec
      }
      px[aa[ra]] <- ex[ii]; py[aa[ra]] <- ey[ii]; hd[aa[ra]] <- br[ii]
    })
    ord <- order(Aid, Stp); rows <- as.vector(outer(1:11, (ord - 1L) * 11L, "+")); Aid <- Aid[ord]; Stp <- Stp[ord]; Zs <- Zs[ord, , drop = FALSE]; SXY <- SXY[ord, , drop = FALSE]
    yearPieces[[ti]] <- list(E = E[rows, , drop = FALSE], G3 = G3[rows, , drop = FALSE], Zs = Zs, SXY = SXY, Aid = Aid, Stp = Stp)
  }
  # --- table in the engine's format: neutral names, shuffled column order
  Ecat <- do.call(rbind, lapply(yearPieces, `[[`, "E")); G3 <- do.call(rbind, lapply(yearPieces, `[[`, "G3")); Zs <- do.call(rbind, lapply(yearPieces, `[[`, "Zs")); SXY <- do.call(rbind, lapply(yearPieces, `[[`, "SXY"))
  Aid <- unlist(lapply(yearPieces, `[[`, "Aid")); Stp <- unlist(lapply(yearPieces, `[[`, "Stp")); Yr <- rep(yrs, vapply(yearPieces, function(p) length(p$Aid), integer(1)))
  nStr <- length(Aid); rep11 <- function(v) rep(v, each = 11L); startRows <- rep(seq_len(nStr), each = 11L)
  intern <- c(sprintf("end_%d", seq_len(L)), sprintf("start_%d", seq_len(L)), sprintf("inter_%d", seq_len(L)), "logSl", "cosTa", "sinTa")
  Xall <- cbind(Ecat, Zs[startRows, , drop = FALSE], G3[, 1] * Zs[startRows, , drop = FALSE], G3[, 1], G3[, 2], G3[, 3]); colnames(Xall) <- intern
  ordn <- .mvSeeded(sd0 + 7000L, sample(length(intern))); neutral <- sprintf("mv_cov_%03d", ordn)                       # internal column j gets the neutral name mv_cov_<ordn[j]>
  firstYearOfAnimal <- yrs[firstYr]; burst <- as.integer(Yr - firstYearOfAnimal[Aid] + 1L)
  final <- data.table::data.table(indiv_step_id = rep11(sprintf("%s_%d_%05d", aid[Aid], Yr, Stp)), id = rep11(aid[Aid]), year = rep11(as.integer(Yr)), x1_ = rep11(SXY[, 1]), y1_ = rep11(SXY[, 2]),
                                  case_ = rep(c(TRUE, rep(FALSE, 10L)), nStr), burst_ = rep11(burst))
  Xn <- Xall[, order(ordn), drop = FALSE]; colnames(Xn) <- sprintf("mv_cov_%03d", seq_along(ordn)); final <- cbind(final, data.table::as.data.table(Xn))
  # --- truth
  role <- c(rep("end", L), rep("start", L), rep("inter", L), "logSl", "cosTa", "sinTa"); layer <- c(seq_len(L), seq_len(L), seq_len(L), NA, NA, NA)
  layerFam <- c(lfam, lfam, lfam, NA, NA, NA); movesFlip <- !identical(P$moveMode, "none")
  family <- ifelse(role == "end", lfam[layer], ifelse(role == "start", "noise", ifelse(role == "sinTa", "noise", ifelse(role == "inter", ifelse(lfam[layer] == "noise", "noise", if (movesFlip) "flip" else "stable"), if (movesFlip) "flip" else "stable"))))
  block <- ifelse(role == "end", "habitat", ifelse(role == "start", "nuisance", "movement"))
  Tb <- rbind(Bpop, matrix(0, L, nY), Gm, bLs, bCs, rep(0, nY)); dimnames(Tb) <- list(intern, yrs)
  sdW <- vapply(intern, function(cn) { m <- matrix(Xall[, cn], 11L); sqrt(max(0, mean((colSums(m^2) - colSums(m)^2 / 11) / 10))) }, numeric(1))      # within-stratum sd of the column (what a model can use)
  famTab <- data.table::data.table(covariate = neutral, internal = intern, role = role, layer = layer, layerFamily = layerFam, family = family, block = block, withinSd = sdW,
                                   coefficientMean = rowMeans(Tb), peakEffect = apply(abs(Tb), 1, max) * sdW)
  trueBeta <- Tb[order(ordn), , drop = FALSE]; rownames(trueBeta) <- sprintf("mv_cov_%03d", seq_along(ordn)); famTab <- famTab[match(rownames(trueBeta), covariate)]
  flipSigns <- S; dimnames(flipSigns) <- list(famTab[role == "end" & layerFamily == "flip"][order(layer)]$covariate, yrs)
  geometry <- if (isTRUE(P$keepGeometry)) data.table::data.table(x2_ = G3[, 6], y2_ = G3[, 7], sl_ = G3[, 4], ta_ = G3[, 5]) else NULL
  animals <- cbind(home, data.table::data.table(firstYear = firstYearOfAnimal, nYears = rowSums(act), nStrata = as.integer(tabulate(Aid, nAn))))
  list(table = final[], params = P, covariates = famTab[], trueBeta = trueBeta, flipSigns = flipSigns, moveSigns = `names<-`(sm, yrs), moveShape = `names<-`(shapeM, yrs),
       animals = animals[], deviations = `dimnames<-`(devArr, list(aid, sprintf("layer%d", seq_len(L)), yrs)), layerFamily = lfam, scales = c(kScale, animalTau = tau), geometry = geometry, active = `dimnames<-`(act, list(aid, yrs)))
}

# within-stratum contributions of every block to eta, for the rows given (default: the 10 available steps of every stratum), computed from the table and the stored truth
movementContributions <- function(w, rows = NULL, lag = 0L) {      # lag > 0: the animal term uses the animal's preferences of `lag` years earlier (stale memory; the first year is kept as the earliest)
  tb <- w$table; if (is.null(rows)) rows <- which(!tb$case_); cv <- w$covariates; yi <- match(tb$year[rows], as.integer(colnames(w$trueBeta))); yiDev <- pmax(1L, yi - as.integer(lag))
  contribOf <- function(sel) { cn <- cv$covariate[sel]; if (!length(cn)) return(rep(0, length(rows)))
    Reduce(`+`, lapply(cn, function(c1) tb[[c1]][rows] * w$trueBeta[c1, yi])) }
  endSel <- function(f) which(cv$role == "end" & cv$layerFamily == f)
  ai <- match(tb$id[rows], rownames(w$deviations)); an <- numeric(length(rows))
  for (l in seq_len(dim(w$deviations)[2])) { cn <- cv$covariate[cv$role == "end" & cv$layer == l]; an <- an + w$deviations[cbind(ai, l, yiDev)] * tb[[cn]][rows] }
  out <- data.table::data.table(stable = contribOf(endSel("stable")), drift = contribOf(endSel("drift")), flip = contribOf(endSel("flip")), move = contribOf(which(cv$block == "movement")), animal = an)
  if (!is.null(w$geometry) && w$params$homePull != 0) { an2 <- w$animals[match(tb$id[rows], id)]; R2 <- w$params$homeRangeKm^2
    out[, home := -w$params$homePull * (((w$geometry$x2_[rows] - an2$hx)^2 + (w$geometry$y2_[rows] - an2$hy)^2) - ((tb$x1_[rows] - an2$hx)^2 + (tb$y1_[rows] - an2$hy)^2)) / 1e6 / (2 * R2)] } else out[, home := 0]
  out[, eta := stable + drift + flip + move + animal + home]; out[]
}

# cross-entropy of the TRUE selection function on the observed step of every stratum (chance = log 11); the "oracle" loss. With the home pull (omitted from the candidate set) the oracle is exact up to the choice approximation.
oracleLoss <- function(w, withAnimal = TRUE, withHome = TRUE) {
  K <- movementContributions(w, rows = seq_len(nrow(w$table))); eta <- K$stable + K$drift + K$flip + K$move + (if (withAnimal) K$animal else 0) + (if (withHome) K$home else 0)
  m <- matrix(eta, 11L); data.table::data.table(year = w$table$year[seq(1L, nrow(w$table), 11L)], ce = log(colSums(exp(m - rep(m[1, ], each = 11L)))))   # log sum_j exp(eta_j - eta_used): the observed step is row 1 of every stratum
}

# MEMORY with a known truth: the cross-entropy of the true model when the animal term is (a) absent, (b) the animal's CURRENT preferences, (c) its preferences `lag` years earlier. The gain of (c) over (a) is the most
# that a memory of the animal (the embedding learned `lag` years ago) can be worth at horizon `lag`: constant in lag for fixed preferences, falling to 0 (and below) for drifting ones. The home pull is left out.
memoryValue <- function(w, lags = 0:5) {
  K0 <- movementContributions(w, rows = seq_len(nrow(w$table))); base <- K0$stable + K0$drift + K0$flip + K0$move; ce <- function(eta) { m <- matrix(eta, 11L); log(colSums(exp(m - rep(m[1, ], each = 11L)))) }
  ce0 <- mean(ce(base)); data.table::rbindlist(lapply(c(lags), function(g) { K <- movementContributions(w, rows = seq_len(nrow(w$table)), lag = g); cg <- mean(ce(base + K$animal)); data.table::data.table(lag = g, ceNoMemory = ce0, ceWithMemory = cg, memoryGain = ce0 - cg) }))
}

# what the world really contains (used by the tests and by the diagnostic script)
worldDiagnostics <- function(w) {
  tb <- w$table; K <- movementContributions(w, rows = seq_len(nrow(tb)))
  # within-stratum variances over the 11 steps of a stratum (in choice-set mode they are 11 iid draws of the kernel before one is labelled "observed"; dropping the observed step would bias the variances, because the observed step is the one with a high eta)
  wv <- function(v) { m <- matrix(v, 11L); mean((colSums(m^2) - colSums(m)^2 / 11) / 10) }; vw <- c(stable = wv(K$stable), drift = wv(K$drift), flip = wv(K$flip), move = wv(K$move), animal = wv(K$animal)); vEta <- wv(K$eta - K$home)
  fs <- w$flipSigns; flipFreq <- if (nrow(fs) && ncol(fs) > 1) mean(fs[, -1, drop = FALSE] != fs[, -ncol(fs), drop = FALSE]) else NA_real_
  list(nStrata = nrow(tb) / 11, nRows = nrow(tb), nAnimals = data.table::uniqueN(tb$id), nStrataByYear = tb[case_ == TRUE, .N, by = year][order(year)], realisedShares = vw / sum(vw), withinSdEta = sqrt(vEta), homeSdEta = sqrt(wv(K$home)),
       flipFrequency = flipFreq, movePattern = w$moveShape)
}

# write the world for the engine: final_<name>.rds (the table), truth_<name>.rds, and -- with engine = TRUE -- the files the caribou engine reads next to it:
# featureTable.csv (the candidate list; no ranking: Importance NA) and networkSettings.rds (the original caribou network, as caribouNetworkSettings() writes it).
writeMovementWorld <- function(w, dir, name = "caribou", engine = TRUE, repoRoot = NULL) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(w$table, file.path(dir, sprintf("final_%s.rds", name))); saveRDS(w[setdiff(names(w), "table")], file.path(dir, sprintf("truth_%s.rds", name)))
  if (isTRUE(engine)) {
    cand <- setdiff(names(w$table), c("indiv_step_id", "id", "year", "x1_", "y1_", "case_", "burst_")); data.table::fwrite(data.table::data.table(Feature = cand, Importance = NA_real_), file.path(dir, "featureTable.csv"))
    pf <- if (!is.null(repoRoot)) file.path(repoRoot, "modules", "preValPrep_Caribou", "R", "prepCaribou.R") else NA_character_
    ns <- if (!is.na(pf) && file.exists(pf)) { e <- new.env(); sys.source(pf, envir = e); e$caribouNetworkSettings() } else list(embDim = 8L, hidden = c(128, 64), lr = 0.001, batchSize = 128L, epochs = 300L, weightDecay = 0, idIntercept = FALSE, dropout = 0,
                                                                                                                                   lastPilotYear = NA_real_, source = "original caribou network (modules/caribouNN/R/stratumNet.R); not tuned")
    saveRDS(ns, file.path(dir, "networkSettings.rds"))
  }
  invisible(file.path(dir, sprintf("final_%s.rds", name)))
}

# SIMULATION-ONLY "oracle" ladder: columns ordered by their PEAK true effect (max over years of |beta| x within-stratum sd), zero-effect columns last in random order, in the format of the engine's per-window ranking files
oracleRanking <- function(w) {
  ct <- w$covariates; key <- ifelse(ct$peakEffect > 0, ct$peakEffect, -.mvSeeded(1L, stats::runif(nrow(ct))) * 1e-6)
  o <- order(-key); r <- data.table::data.table(Feature = ct$covariate[o], group = ct$covariate[o], Importance = key[o], ImportanceSD = 0, marginalAssoc = NA_real_); r[, rank := seq_len(.N)]; r[]
}

# SIMULATION-ONLY ladders (orders of the columns), as in simulation/simulate_world.R; together with the engine's own ranking ("natural", no override) they test whether the ORDER matters:
#   flippersFirst  flipping columns first (habitat flip layers and, when the movement block flips, the movement terms; by peak effect), then stable and drifting ones, then zero-effect columns
#   flippersLast   stable and drifting columns first, then the flipping ones, then zero-effect columns
#   random         all 30 columns in a random order (seeded by `seed`)
ladderRanking <- function(w, order = c("flippersFirst", "flippersLast", "random"), seed = 1L) {
  order <- match.arg(order); ct <- w$covariates; fam <- ct$family; noEff <- ct$peakEffect <= 0
  tier <- switch(order, flippersFirst = c(flip = 3, stable = 2, drift = 2, noise = 0)[fam], flippersLast = c(flip = 2, stable = 3, drift = 3, noise = 0)[fam], random = rep(0, nrow(ct)))
  tier[noEff] <- 0
  key <- if (order == "random") -.mvSeeded(seed, stats::runif(nrow(ct))) else as.numeric(tier) * 1000 + ifelse(noEff, -.mvSeeded(seed, stats::runif(nrow(ct))) * 1e-3, ct$peakEffect)
  o <- base::order(-key); r <- data.table::data.table(Feature = ct$covariate[o], group = ct$covariate[o], Importance = key[o], ImportanceSD = 0, marginalAssoc = NA_real_); r[, rank := seq_len(.N)]; r[]
}
