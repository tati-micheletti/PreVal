suppressMessages(library(data.table)); options(width = 230)
b <- commandArgs(TRUE)[1]      # folder with the unpacked s1w_* worlds (each holds run_same and run_flippersFirst_same/analysis)
X <- rbindlist(lapply(list.dirs(b, recursive = FALSE, full.names = FALSE), function(w) rbindlist(lapply(c("run_same", "run_flippersFirst_same"), function(r) {
  f <- file.path(b, w, r, "analysis", "pairedContrasts_perSplit.csv"); if (!file.exists(f)) return(NULL)
  x <- fread(f, select = c("complexity", "testYear", "horizon", "realized_FutureTainted", "realized_FutureUnseenGrouped", "reported_FutureTainted", "reported_FutureUnseenGrouped"))
  x[, `:=`(world = w, ladder = ifelse(grepl("flippers", r), "flippersFirst", "natural"))] }))))
p <- strsplit(X$world, "_"); X[, `:=`(phi = as.numeric(substring(vapply(p, `[`, "", 2), 4)), fsd = as.numeric(substring(vapply(p, `[`, "", 3), 3)), seed = as.integer(substring(vapply(p, `[`, "", 4), 2)))]
X[, bin := fifelse(horizon == 1, "1", fifelse(horizon == 2, "2", fifelse(horizon <= 4, "3-4", "5-9")))]
X[, `:=`(SQ = realized_FutureTainted, PV = realized_FutureUnseenGrouped, SQo = realized_FutureTainted - reported_FutureTainted, PVo = realized_FutureUnseenGrouped - reported_FutureUnseenGrouped)]
cat("units/years/splits per world:\n"); print(X[, .(rows = .N, years = uniqueN(testYear), levels = paste(sort(unique(complexity)), collapse = ",")), by = .(ladder)][1])
r3 <- function(d) { n <- setdiff(names(d), c("fsd", "ladder", "complexity", "phi", "bin", "lev")); d[, (n) := lapply(.SD, round, 3), .SDcols = n]; d }
agg <- function(by) X[, .(SQ = mean(SQ), PV = mean(PV), SQ_optim = mean(SQo), PV_optim = mean(PVo)), by = by][, `:=`(gap = PV - SQ, OG = SQ_optim - PV_optim)]
for (lad in c("natural", "flippersFirst")) for (lv in c(60, 10)) { cat(sprintf("\n=== %s, level %d, horizons pooled: realized loss, optimism (realized - reported), gap = PreVal - SQ, OG = SQ optimism - PreVal optimism ===\n", lad, lv))
  print(r3(agg(c("fsd", "ladder", "complexity", "phi"))[ladder == lad & complexity == lv][order(fsd, phi)])[, !c("ladder", "complexity")]) }
cat("\n=== natural, level 60 by horizon bin ===\n"); print(r3(agg(c("fsd", "ladder", "complexity", "phi", "bin"))[ladder == "natural" & complexity == 60][order(fsd, bin, phi)])[, !c("ladder", "complexity")])
cat("\n=== flippersFirst, level 10 by horizon bin ===\n"); print(r3(agg(c("fsd", "ladder", "complexity", "phi", "bin"))[ladder == "flippersFirst" & complexity == 10][order(fsd, bin, phi)])[, !c("ladder", "complexity")])
# change from the matched phi 0 world, per forecast year (2 seeds averaged inside a year), pooled over horizons; level 10 and all pooled
sf <- function(v) { n <- length(v); neg <- sum(v < 0); pos <- sum(v > 0); sprintf("%d/%d p=%.3f", neg, pos, min(1, 2 * pbinom(min(neg, pos), n, 0.5))) }
Y <- X[complexity %in% c(10, 60) | TRUE, .(SQ = mean(SQ), PV = mean(PV), SQo = mean(SQo), PVo = mean(PVo)), by = .(fsd, ladder, lev = complexity, phi, seed, testYear)][, .(SQ = mean(SQ), PV = mean(PV), SQo = mean(SQo), PVo = mean(PVo)), by = .(fsd, ladder, lev, phi, testYear)]
C <- merge(Y[phi > 0], Y[phi == 0, .(fsd, ladder, lev, testYear, SQ0 = SQ, PV0 = PV, SQo0 = SQo, PVo0 = PVo)], by = c("fsd", "ladder", "lev", "testYear"))
C[, `:=`(dSQ = SQ - SQ0, dPV = PV - PV0, dG = (PV - SQ) - (PV0 - SQ0), dSQo = SQo - SQo0, dPVo = PVo - PVo0, dOG = (SQo - PVo) - (SQo0 - PVo0))]
for (lv in c(60, 10)) { cat(sprintf("\n=== change from matched phi 0, level %d, pooled over horizons: mean over years (years negative / positive; sign-flip p) ===\n", lv))
  r <- C[lev == lv, .(dSQ = round(mean(dSQ), 3), dPV = round(mean(dPV), 3), dG = round(mean(dG), 3), dG_yrs = sf(dG), dSQo = round(mean(dSQo), 3), dPVo = round(mean(dPVo), 3), dOG = round(mean(dOG), 3), dOG_yrs = sf(dOG)), by = .(ladder, fsd, phi)][order(ladder, fsd, phi)]; print(r) }
cat("\n=== change from matched phi 0, levels 10 and all pooled (headline rule of plan 9v) ===\n")
print(C[lev %in% c(10, 60), .(dG = mean(dG), dOG = mean(dOG), dSQ = mean(dSQ), dPV = mean(dPV), dSQo = mean(dSQo)), by = .(ladder, fsd, phi, testYear)][, .(dSQ = round(mean(dSQ), 3), dPV = round(mean(dPV), 3), dG = round(mean(dG), 3), dG_yrs = sf(dG), dSQo = round(mean(dSQo), 3), dOG = round(mean(dOG), 3), dOG_yrs = sf(dOG)), by = .(ladder, fsd, phi)][order(ladder, fsd, phi)])
cat("\n=== by level (pooled over horizons), phi 1 vs phi 0: dG and dOG ===\n")
print(dcast(C[phi == 1, .(dG = round(mean(dG), 3), dOG = round(mean(dOG), 3)), by = .(ladder, fsd, lev)], ladder + fsd ~ lev, value.var = c("dG", "dOG")))
