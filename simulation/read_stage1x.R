suppressMessages(library(data.table)); options(width = 230)
# Reads the swap test (stage 1x, plan 9x). Usage: Rscript read_stage1x.R <folder with the unpacked s1x_* worlds>
# Quantities (plan 9x): G = PreVal - status quo realized loss; S = G(oracle) - G(oracleSwap); D = S(phi 1) - S(phi 0); level 60 ("all") = negative control.
b <- commandArgs(TRUE)[1]
X <- rbindlist(lapply(list.dirs(b, recursive = FALSE, full.names = FALSE), function(w) rbindlist(lapply(c("run_oracle_same", "run_oracleSwap_same"), function(r) {
  f <- file.path(b, w, r, "analysis", "pairedContrasts_perSplit.csv"); if (!file.exists(f)) return(NULL)
  x <- fread(f, select = c("complexity", "testYear", "horizon", "realized_FutureTainted", "realized_FutureUnseenGrouped", "reported_FutureTainted", "reported_FutureUnseenGrouped"))
  x[, `:=`(world = w, ladder = ifelse(grepl("Swap", r), "oracleSwap", "oracle"))] }))))
p <- strsplit(X$world, "_"); X[, `:=`(phi = as.numeric(substring(vapply(p, `[`, "", 2), 4)), seed = 100L + as.integer(substring(vapply(p, `[`, "", 4), 2)))]
stopifnot(uniqueN(X$world) == 8, uniqueN(X$ladder) == 2)
X[, `:=`(SQ = realized_FutureTainted, PV = realized_FutureUnseenGrouped, SQo = realized_FutureTainted - reported_FutureTainted, PVo = realized_FutureUnseenGrouped - reported_FutureUnseenGrouped)]
cat("worlds", uniqueN(X$world), " rows per world x ladder:", paste(range(X[, .N, by = .(world, ladder)]$N), collapse = "-"), " years:", uniqueN(X$testYear), " levels:", paste(sort(unique(X$complexity)), collapse = ","), "\n")
r3 <- function(d) { n <- names(d)[sapply(d, is.numeric)]; n <- setdiff(n, c("phi", "complexity", "seed", "testYear")); d[, (n) := lapply(.SD, round, 3), .SDcols = n]; d }
# ---- HEADLINE: raw numbers side by side (row-weighted over splits, years, seeds), per phi x ladder x level
raw <- X[, .(SQ = mean(SQ), PV = mean(PV), SQ_optim = mean(SQo), PV_optim = mean(PVo)), by = .(phi, ladder, complexity)][, `:=`(gap = PV - SQ, OG = SQ_optim - PV_optim)]
cat("\n=== raw numbers: realized loss of status quo (SQ) and PreVal (PV), optimism, gap = PV - SQ, OG = SQ optimism - PV optimism ===\n"); print(r3(copy(raw))[order(complexity, ladder, phi)])
# ---- per (phi, seed, ladder, level, year), then seeds averaged inside a year
Y <- X[, .(SQ = mean(SQ), PV = mean(PV), SQo = mean(SQo), PVo = mean(PVo)), by = .(phi, seed, ladder, complexity, testYear)][, G := PV - SQ]
Yr <- Y[, .(SQ = mean(SQ), PV = mean(PV), SQo = mean(SQo), PVo = mean(PVo), G = mean(G)), by = .(phi, ladder, complexity, testYear)]
sw <- function(d, by) { w <- dcast(d, as.formula(paste(paste(by, collapse = " + "), "+ phi + complexity ~ ladder")), value.var = "G"); w[, S := oracle - oracleSwap]; w }
Sy <- sw(Yr, "testYear"); Dy <- dcast(Sy, testYear + complexity ~ phi, value.var = "S"); setnames(Dy, c("0", "1"), c("S0", "S1")); Dy[, D := S1 - S0]
sf <- function(v) { n <- length(v); neg <- sum(v < 0); pos <- sum(v > 0); sprintf("%d neg / %d pos, sign-flip p=%.4f", neg, pos, min(1, 2 * pbinom(min(neg, pos), n, 0.5))) }
cat("\n=== S (oracle - oracleSwap gap) per phi and level, mean over 13 years; D = S(phi 1) - S(phi 0), years negative / positive ===\n")
print(r3(Dy[, .(S_phi0 = mean(S0), S_phi1 = mean(S1), D = mean(D), D_years = sf(D)), by = complexity][order(complexity)]))
pl <- Dy[complexity %in% c(5, 10, 20), .(D = mean(D), S0 = mean(S0), S1 = mean(S1)), by = testYear]
cat("\n=== D pooled over levels 5, 10, 20 (reading-rule quantity) ===\nmean D =", round(mean(pl$D), 4), " |", sf(pl$D), " | years D <= -0.03:", sum(pl$D <= -0.03), " | years |D| < 0.02:", sum(abs(pl$D) < 0.02), "of", nrow(pl), "\n")
cat("S at phi 0 (swap alone), pooled levels 5-20:", round(mean(pl$S0), 4), "; at phi 1:", round(mean(pl$S1), 4), "\n")
# per seed
Ys <- dcast(sw(Y, c("seed", "testYear")), seed + testYear + complexity ~ phi, value.var = "S"); setnames(Ys, c("0", "1"), c("S0", "S1")); Ys[, D := S1 - S0]
cat("\n=== D per seed, pooled levels 5, 10, 20 and the 13 years (and level 'all' as control) ===\n")
print(r3(merge(Ys[complexity %in% c(5, 10, 20), .(D_levels5_10_20 = mean(D), S_phi0 = mean(S0), S_phi1 = mean(S1)), by = seed], Ys[complexity == 60, .(D_all = mean(D)), by = seed], by = "seed")))
# negative control
ctl <- Dy[complexity == 60, .(S0 = mean(S0), S1 = mean(S1), D = mean(D))]
cat("\nNEGATIVE CONTROL level all: S phi0 =", round(ctl$S0, 4), " S phi1 =", round(ctl$S1, 4), " D =", round(ctl$D, 4), "\n")
# P-X3 and P-X4
LL <- Yr[complexity %in% c(5, 10, 20), .(SQ = mean(SQ), PV = mean(PV), SQo = mean(SQo), PVo = mean(PVo)), by = .(phi, ladder)]
chg <- function(col) { w <- dcast(LL, phi ~ ladder, value.var = col); w[, d := oracle - oracleSwap]; w[phi == 1]$d - w[phi == 0]$d }
cat("\nP-X3: change (phi 0 -> 1) in [loss(oracle) - loss(oracleSwap)]: status quo", round(chg("SQ"), 4), " PreVal", round(chg("PV"), 4), "\n")
cat("P-X4: change (phi 0 -> 1) in [SQ optimism(oracle) - SQ optimism(oracleSwap)]:", round(chg("SQo"), 4), "  (PreVal's optimism:", round(chg("PVo"), 4), ")\n")
fwrite(r3(copy(raw)), file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])), "stage1x_raw_by_cell.csv"))
fwrite(Dy, file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])), "stage1x_D_by_year_level.csv"))
