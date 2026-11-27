# Reader and scorer of stage 1y (ANALYSIS_PLAN_SIMULATION.md section 9z), written 2026-11-27 BEFORE any stage-1y model exists, so the scoring rules cannot be shaped by the result.
# Usage: Rscript simulation/read_stage1y.R <stage1y outputs folder (holds one folder per world)> [<stage1x outputs folder, for the reproduction check P-Y0>] [<output folder>]
# Reads every unit's analysis/allModels.csv (columns reported, realized, optimism = calibrated loss; typeValidation; numberOfCovariates; horizon; testYear), builds seed-level quantities
# (mean over the forecast years with equal weight, then over the stated levels; the replication unit is the SEED) and scores P-Y0 .. P-Y7 exactly as written in 9z. Writes stage1y_scores.csv and stage1y_seed_values.csv.
suppressMessages(library(data.table)); options(width = 220)
a <- commandArgs(TRUE); root <- if (length(a) >= 1) a[1] else stop("give the stage-1y outputs folder"); root1x <- if (length(a) >= 2 && nzchar(a[2])) a[2] else NA_character_; outDir <- if (length(a) >= 3) a[3] else getwd()
H <- c(1, 2, 4, 9)
# ---- read -------------------------------------------------------------------------------------------------------------------------------------------------------------------
readUnits <- function(root, worldPattern) {
  fs <- list.files(root, pattern = "^allModels[.]csv$", recursive = TRUE, full.names = TRUE); fs <- fs[grepl("/analysis/", fs)]
  rbindlist(lapply(fs, function(f) {
    p <- strsplit(sub(paste0("^", gsub("\\\\", "/", normalizePath(root, winslash = "/", mustWork = FALSE)), "/?"), "", gsub("\\\\", "/", normalizePath(f, winslash = "/", mustWork = FALSE))), "/")[[1]]
    w <- p[1]; run <- p[2]; if (!grepl(worldPattern, w)) return(NULL)
    x <- fread(f, select = c("splitId", "typeValidation", "numberOfCovariates", "testYear", "horizon", "bestEpoch", "epochsRun", "nTrain", "reported", "realized", "optimism"))
    x[, `:=`(world = w, run = run)]
  }), fill = TRUE)
}
D <- readUnits(root, "^s1y_"); stopifnot(nrow(D) > 0)
D[, `:=`(cell = sub("^s1y_([a-z]+)_w[0-9]+$", "\\1", world), seed = 100L + as.integer(sub(".*_w([0-9]+)$", "\\1", world)),
         lad = fifelse(grepl("oracle", run), "oracle", "natural"), des = fifelse(grepl("new", run), "new", "same"))]
setnames(D, "numberOfCovariates", "lvl"); D[, lvlMax := max(lvl), by = world]; D[lvl == lvlMax & lvl > 20, lvl := Inf]      # "all covariates" = the largest level of the world
# seed-level value: mean over testYear of the mean within the year, then over the requested levels
agg <- function(cell_, lad_, des_, reg_, lv, h_, meas = "realized") {
  x <- D[cell == cell_ & lad == lad_ & des == des_ & typeValidation == reg_ & lvl %in% lv & horizon %in% h_]
  if (!nrow(x)) return(data.table(seed = integer(), v = numeric()))
  x[, .(v = mean(get(meas))), by = .(seed, lvl, horizon, testYear)][, .(v = mean(v)), by = .(seed, lvl, horizon)][, .(v = mean(v)), by = seed][order(seed)]
}
PV <- "FutureUnseenGrouped"; SQ <- "FutureTainted"; PVP <- "FutureUnseenGroupedPoolRefit"; SQR <- "FutureTaintedRefit"; LN <- c(10, Inf); LO <- c(5, 10)
gap <- function(cell_, des_, h_, lad_ = "natural", lv = LN) { p <- agg(cell_, lad_, des_, PV, lv, h_); s <- agg(cell_, lad_, des_, SQ, lv, h_); merge(p, s, by = "seed", suffixes = c("_pv", "_sq"))[, .(seed, v = v_pv - v_sq)] }
chg <- function(cell_, lad_, reg_, lv, meas = "realized") { x <- agg(cell_, lad_, "same", reg_, lv, H, meas); y <- agg(cell_, lad_, "same", reg_, 2, H, meas); merge(x, y, by = "seed", suffixes = c("_l", "_2"))[, .(seed, v = v_l - v_2)] }
signflipP <- function(d) { d <- d[is.finite(d)]; n <- length(d); if (n < 2) return(NA_real_); g <- as.matrix(expand.grid(rep(list(c(-1, 1)), n))); mean(rowSums(g * rep(d, each = nrow(g))) >= sum(d) - 1e-12) }
SC <- list(); SV <- list()
# score(): d = per-seed values, eff = per-seed signed margin (>= 0 means the seed meets the prediction); the exact one-sided sign-flip p is computed on the margins (reported, not a gate)
score <- function(id, what, d, eff, gate = 4L) {
  n <- length(eff); k <- sum(eff >= 0, na.rm = TRUE); SV[[length(SV) + 1L]] <<- data.table(id = id, what = what, seeds = paste(d$seed, collapse = " "), values = paste(round(d$v, 3), collapse = " "))
  SC[[length(SC) + 1L]] <<- data.table(id = id, what = what, seedsMet = k, seeds = n, gate = gate, met = n >= gate && k >= gate, pSignFlip_oneSided = round(signflipP(eff), 4)); invisible()
}
# sc(): the common case "v must lie above (dir = +1) or below (dir = -1) a threshold"
sc <- function(id, what, d, dir, thr = 0, gate = 4L) { if (!nrow(d)) { SC[[length(SC) + 1L]] <<- data.table(id = id, what = what, seedsMet = NA_integer_, seeds = 0L, gate = gate, met = NA, pSignFlip_oneSided = NA_real_); return(invisible()) }; score(id, what, d, dir * (d$v - thr), gate) }
# ---- P-Y0: reproduction of stage 1x -----------------------------------------------------------------------------------------------------------------------------------------
if (!is.na(root1x) && dir.exists(root1x)) {
  X <- readUnits(root1x, "^s1x_"); X <- X[grepl("oracle", run) & !grepl("Swap", run)]
  X[, `:=`(cell = fifelse(grepl("phi0", world), "anchor", "flip"), seed = 100L + as.integer(sub(".*_w([0-9]+)$", "\\1", world)))]; setnames(X, "numberOfCovariates", "lvl"); X[, lvlMax := max(lvl), by = world]; X[lvl == lvlMax & lvl > 20, lvl := Inf]
  Y <- D[lad == "oracle" & typeValidation %in% c(PV, SQ)]; j <- merge(Y[, .(cell, seed, splitId, lvl, horizon, typeValidation, r1y = realized)], X[horizon %in% H, .(cell, seed, splitId, lvl, horizon, typeValidation, r1x = realized)], by = c("cell", "seed", "splitId", "lvl", "horizon", "typeValidation"))
  cellD <- j[, .(dif = abs(mean(r1y - r1x))), by = .(cell, seed, lvl, horizon, typeValidation)]
  SC[[length(SC) + 1L]] <- data.table(id = "P-Y0", what = sprintf("share of (cell, seed, level, horizon, regime) cells within 0.005 of stage 1x (%d cells; need >= 0.95)", nrow(cellD)), seedsMet = sum(cellD$dif <= 0.005), seeds = nrow(cellD), gate = ceiling(0.95 * nrow(cellD)), met = mean(cellD$dif <= 0.005) >= 0.95, pSignFlip_oneSided = NA_real_)
} else SC[[length(SC) + 1L]] <- data.table(id = "P-Y0", what = "stage-1x folder not given: reproduction check NOT done", seedsMet = NA_integer_, seeds = 0L, gate = NA_integer_, met = NA, pSignFlip_oneSided = NA_real_)
# ---- P-Y1: drift test ------------------------------------------------------------------------------------------------------------------------------------------------------
for (h in H) { dd <- merge(gap("nodrift", "same", h), gap("anchor", "same", h), by = "seed", suffixes = c("_nd", "_an"))[, .(seed, v = v_nd - v_an)]; sc("P-Y1a", sprintf("same places, G(nodrift) - G(anchor) > 0, horizon %d", h), dd, 1, 0) }
dd <- merge(gap("nodrift", "same", 9), gap("anchor", "same", 9), by = "seed", suffixes = c("_nd", "_an"))[, .(seed, v = v_nd - v_an)]; sc("P-Y1b", "same places, G(nodrift) - G(anchor) >= +0.10 at horizon 9", dd, 1, 0.10)
for (h in H) sc("P-Y1c", sprintf("same places, G(nodrift) > 0, horizon %d", h), gap("nodrift", "same", h), 1, 0)
for (cl in c("anchor", "nodrift")) for (h in H) sc("P-Y1d", sprintf("NEW places, G(%s) < 0, horizon %d", cl, h), gap(cl, "new", h), -1, 0)
# ---- P-Y2: new places with flips ---------------------------------------------------------------------------------------------------------------------------------------------
for (h in H) sc("P-Y2a", sprintf("NEW places, G(flip) < 0, horizon %d", h), gap("flip", "new", h), -1, 0)
sc("P-Y2b", "NEW places, G(flip) <= -0.10 pooled over horizons", gap("flip", "new", H), -1, -0.10)
for (h in c(1, 2)) sc("P-Y2c", sprintf("same places with flips, G(flip) > 0, horizon %d", h), gap("flip", "same", h), 1, 0)
# ---- P-Y3: the hump ---------------------------------------------------------------------------------------------------------------------------------------------------------
sc("P-Y3a", "oracle ladder, flip: status quo realized change level 2 -> {5,10} > +0.01", chg("flip", "oracle", SQ, LO), 1, 0.01)
sc("P-Y3b", "oracle ladder, anchor: status quo realized change level 2 -> {5,10} < 0", chg("anchor", "oracle", SQ, LO), -1, 0)
sc("P-Y3c", "oracle ladder, flip: PreVal realized change level 2 -> {5,10} <= +0.01", chg("flip", "oracle", PV, LO), -1, 0.01)
sc("P-Y3d", "natural ladder, flip: status quo realized change level 2 -> 10 < 0 (no hump)", chg("flip", "natural", SQ, 10), -1, 0)
# ---- P-Y4: the validation cannot see it ---------------------------------------------------------------------------------------------------------------------------------------
rs <- merge(chg("flip", "oracle", SQ, LO, "reported"), chg("flip", "oracle", SQ, LO, "realized"), by = "seed", suffixes = c("_rep", "_real")); score("P-Y4a", "oracle, flip, status quo: reported change < 0 AND realized change > 0 (value = realized - reported change)", rs[, .(seed, v = v_real - v_rep)], pmin(-rs$v_rep, rs$v_real))
rp <- merge(chg("flip", "oracle", PV, LO, "reported"), chg("flip", "oracle", PV, LO, "realized"), by = "seed", suffixes = c("_rep", "_real")); score("P-Y4b", "oracle, flip, PreVal: same sign or both within +-0.01 (value = realized - reported change)", rp[, .(seed, v = v_real - v_rep)], ifelse(sign(rp$v_rep) == sign(rp$v_real) | (abs(rp$v_rep) <= 0.01 & abs(rp$v_real) <= 0.01), 1, -1))
# ---- P-Y5: epochs -----------------------------------------------------------------------------------------------------------------------------------------------------------
cs <- chg("flip", "oracle", SQR, LO); cp <- chg("flip", "oracle", PVP, LO); e <- merge(cs, cp, by = "seed", suffixes = c("_sqr", "_pool"))
if (nrow(e)) { e[, D := v_sqr - v_pool]; score("P-Y5", "oracle, flip: D_ep = SQ-refit change - pool-refit change >= +0.02 AND pool-refit change <= +0.01", e[, .(seed, v = D)], pmin(e$D - 0.02, 0.01 - e$v_pool))
  SV[[length(SV) + 1L]] <- data.table(id = "P-Y5", what = "case per seed: (i) epoch carries the harm, (ii) harm independent of epoch, (iii) no hump in the refit", seeds = paste(e$seed, collapse = " "), values = paste(ifelse(e$D >= 0.02 & e$v_pool <= 0.01, "i", ifelse(e$v_sqr <= 0, "iii", ifelse(e$v_sqr > 0.01 & e$v_pool > 0.01 & abs(e$D) < 0.02, "ii", "other"))), collapse = " ")) } else sc("P-Y5", "oracle, flip: refit changes missing", data.table(seed = integer(), v = numeric()), 1)
# ---- P-Y6: the recipe ---------------------------------------------------------------------------------------------------------------------------------------------------------
g1 <- gap("anchor", "same", 1); K1 <- merge(agg("anchor", "natural", "same", PVP, LN, 1), agg("anchor", "natural", "same", SQR, LN, 1), by = "seed", suffixes = c("_pool", "_sqr"))[, .(seed, v = v_pool - v_sqr)]
k <- merge(K1, g1, by = "seed", suffixes = c("_K", "_G")); if (nrow(k)) { score("P-Y6i", "anchor same: K(h=1) < G(h=1) - 0.05 (value = K - G)", k[, .(seed, v = v_K - v_G)], -(k$v_K - k$v_G + 0.05)); SV[[length(SV) + 1L]] <- data.table(id = "P-Y6-reported", what = "K(h=1) per seed (not scored; <= 0 means the pool refit is ahead of the status-quo refit)", seeds = paste(k$seed, collapse = " "), values = paste(round(k$v_K, 3), collapse = " ")) } else sc("P-Y6i", "anchor same: K or G missing", data.table(seed = integer(), v = numeric()), 1)
im <- merge(agg("anchor", "natural", "same", PV, LN, 1), agg("anchor", "natural", "same", PVP, LN, 1), by = "seed", suffixes = c("_pv", "_pool"))[, .(seed, v = v_pv - v_pool)]; sc("P-Y6ii", "anchor same, h=1: pool refit improves on PreVal plain by >= 0.05", im, 1, 0.05)
is_ <- merge(agg("anchor", "natural", "same", SQ, LN, H), agg("anchor", "natural", "same", SQR, LN, H), by = "seed", suffixes = c("_sq", "_sqr"))[, .(seed, v = v_sq - v_sqr)]; sc("P-Y6iii", "anchor same: status-quo refit improves on its plain loss by >= 0.005", is_, 1, 0.005)
rec <- rbindlist(lapply(H, function(h) { x <- Reduce(function(p, q) merge(p, q, by = "seed"), list(agg("anchor", "natural", "same", PVP, LN, h)[, .(seed, pool = v)], agg("anchor", "natural", "same", PV, LN, h)[, .(seed, pv = v)], agg("anchor", "natural", "same", SQ, LN, h)[, .(seed, sq = v)])); x[, .(seed, h = h, ok = pool <= pmin(pv, sq) + 0.01)] }))
if (nrow(rec)) { r4 <- rec[, .(ok = all(ok)), by = seed]; SC[[length(SC) + 1L]] <- data.table(id = "P-Y6-reported", what = "recipe: pool refit <= min(status quo, PreVal) + 0.01 at ALL four horizons (reported, counts as shown only if >= 4 of 5)", seedsMet = sum(r4$ok), seeds = nrow(r4), gate = 4L, met = sum(r4$ok) >= 4, pSignFlip_oneSided = NA_real_)
  SV[[length(SV) + 1L]] <- data.table(id = "P-Y6-reported", what = "recipe by horizon: number of seeds with pool <= min(SQ, PreVal) + 0.01", seeds = paste(H, collapse = " "), values = paste(rec[, sum(ok), by = h]$V1, collapse = " ")) }
# ---- P-Y7: optimism -------------------------------------------------------------------------------------------------------------------------------------------------------------
ri <- function(cl) merge(agg(cl, "natural", "same", SQ, LN, 9, "optimism"), agg(cl, "natural", "same", SQ, LN, 1, "optimism"), by = "seed", suffixes = c("_9", "_1"))[, .(seed, v = v_9 - v_1)]
sc("P-Y7a", "anchor: status-quo optimism rise h=1 -> h=9 >= +0.20", ri("anchor"), 1, 0.20)
rr <- merge(ri("nodrift"), ri("anchor"), by = "seed", suffixes = c("_nd", "_an")); if (nrow(rr)) score("P-Y7b", "nodrift rise <= half of the anchor rise (value = nodrift rise - anchor rise / 2)", rr[, .(seed, v = v_nd - v_an / 2)], -(rr$v_nd - rr$v_an / 2)) else sc("P-Y7b", "missing", data.table(seed = integer(), v = numeric()), 1)
po <- rbindlist(lapply(c("anchor", "flip", "nodrift"), function(cl) rbindlist(lapply(H, function(h) agg(cl, "natural", "same", PV, LN, h, "optimism")[, .(seed, cl = cl, h = h, ok = abs(v) <= 0.10, v = v)]))))
if (nrow(po)) { s7 <- po[, .(ok = all(ok), v = max(abs(v))), by = seed]; score("P-Y7c", "PreVal |optimism| <= 0.10 at every horizon and cell (value = largest |O|)", s7[, .(seed, v = v)], 0.10 - s7$v) }
# ---- exploratory: stopping epochs -----------------------------------------------------------------------------------------------------------------------------------------------
ep <- D[lad == "natural" & des == "same" & lvl %in% LN, .(medianBestEpoch = median(bestEpoch)), by = .(cell, typeValidation)][order(cell, typeValidation)]
cat("\n=== SCORES (replication unit = seed; gate as written in 9z) ===\n"); S <- rbindlist(SC, fill = TRUE); print(S[, .(id, what, seedsMet, seeds, gate, met, pSignFlip_oneSided)], row.names = FALSE)
cat("\n=== seed values ===\n"); V <- rbindlist(SV, fill = TRUE); print(V, row.names = FALSE)
cat("\n=== median best epoch (natural ladder, same places, levels 10 and all; exploratory) ===\n"); print(ep, row.names = FALSE)
fwrite(S, file.path(outDir, "stage1y_scores.csv")); fwrite(V, file.path(outDir, "stage1y_seed_values.csv")); fwrite(ep, file.path(outDir, "stage1y_best_epochs.csv"))
cat("\nwritten: stage1y_scores.csv, stage1y_seed_values.csv, stage1y_best_epochs.csv in", outDir, "\n")
