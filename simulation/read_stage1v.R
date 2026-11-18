# Stage 1v read (added 2026-11-18, plan section 9u): DiD = (PreVal minus status quo in the flip world) minus (same in the matched phi 0 world), per forecast year, by horizon bin and level. Level "all" = 60 covariates.
# Usage: Rscript simulation/read_stage1v.R <folder with the unpacked <world>/run_same|run_flippersFirst_same/analysis folders>
suppressMessages(library(data.table)); options(width = 220)
b <- if (length(commandArgs(TRUE)) && commandArgs(TRUE)[1] != "check0v") commandArgs(TRUE)[1] else stop("give the folder of the unpacked analysis")
a0 <- commandArgs(TRUE)
binOf <- function(h) fifelse(h == 1, "1", fifelse(h == 2, "2", fifelse(h <= 4, "3-4", "5-9")))
rdOne <- function(dirp, world, ladder) { f <- file.path(dirp, "analysis", "pairedContrasts_perSplit.csv"); if (!file.exists(f)) return(NULL)
  x <- fread(f); x[, `:=`(world = world, ladder = ladder, d = realized_FutureUnseenGrouped - realized_FutureTainted, bin = binOf(horizon), lev = as.character(complexity))]; x[, .(world, ladder, lev, testYear, horizon, bin, d)] }
if (length(a0) && a0[1] == "check0v") {
  X <- rbindlist(lapply(c("s0v_eta30_w1", "s0v_eta30_w2"), function(w) rdOne(file.path(b, w, "run_same"), w, "natural")))
  y <- X[, .(d = mean(d)), by = .(world, bin, testYear)][, .(est = round(mean(d), 3)), by = .(world, bin)]; print(dcast(y, bin ~ world, value.var = "est")); quit() }
worlds <- fread("C:/Users/michelet/Documents/GitHub/PreVal_refit/simulation/manifests/sim_stage1v_worlds.csv")
us <- fread("C:/Users/michelet/Documents/GitHub/PreVal_refit/simulation/manifests/sim_stage1v_units.csv", colClasses = list(character = c("ladderTag", "rankFrom")))
X <- rbindlist(lapply(seq_len(nrow(us)), function(i) { w <- us$world[i]; l <- us$ladder[i]; d <- list.files(file.path(b, w), pattern = "^run", full.names = TRUE); if (!length(d)) return(NULL)
  # one run folder per unit: natural -> run_same, flippersFirst -> run_flippersFirst_same (as the engine names it); take all and tag by folder
  rbindlist(lapply(d, function(dd) { tg <- basename(dd); lad <- if (grepl("flippersFirst", tg)) "flippersFirst" else "natural"; rdOne(dd, w, lad) })) }))
X <- unique(X); X[, c("phi", "fs", "seed") := .(as.numeric(sub("^s1v_phi([0-9.]+)_fs.*", "\\1", world)), as.numeric(sub("^.*_fs([0-9.]+)_w.*", "\\1", world)), as.integer(sub("^.*_w", "", world)))]
cat("worlds/ladders present:\n"); print(X[, .N, by = .(world, ladder)][order(world, ladder)])
# per year, per (phi, fs, ladder, level, bin): contrast = mean over splits, then average of the two seeds inside a year
Cy <- X[, .(d = mean(d)), by = .(phi, fs, ladder, lev, bin, seed, testYear)][, .(d = mean(d)), by = .(phi, fs, ladder, lev, bin, testYear)]
ctl <- Cy[phi == 0, .(fs, ladder, lev, bin, testYear, d0 = d)]; D <- merge(Cy[phi > 0], ctl, by = c("fs", "ladder", "lev", "bin", "testYear")); D[, did := d - d0]
sf <- function(v) { n <- length(v); neg <- sum(v < 0); pos <- sum(v > 0); k <- min(neg, pos); p <- min(1, 2 * pbinom(k, n, 0.5)); sprintf("%d/%d p=%.3f", neg, pos, p) }
cat("\nCONTROL: phi 0 contrast (PreVal minus status quo) by bin, same-places, per ladder / flip share (mean of years):\n")
print(dcast(Cy[phi == 0 & lev == "60", .(est = round(mean(d), 3)), by = .(fs, ladder, bin)], fs + ladder ~ bin, value.var = "est"))
print(dcast(Cy[phi == 0, .(est = round(mean(d), 3)), by = .(fs, ladder, bin)], fs + ladder ~ bin, value.var = "est"))
cat("\nDiD by horizon bin, pooled over levels 10, 20, all (mean over years; years neg/pos, sign-flip p)\n")
r <- D[lev %in% c("10", "20", "60"), .(d = mean(did)), by = .(phi, fs, ladder, bin, testYear)][, .(est = round(mean(d), 3), yrs = sf(d)), by = .(phi, fs, ladder, bin)][order(fs, ladder, phi, bin)]; print(r)
cat("\nDiD pooled over bins, by level\n")
r2 <- D[, .(d = mean(did)), by = .(phi, fs, ladder, lev, testYear)][, .(est = round(mean(d), 3), yrs = sf(d)), by = .(phi, fs, ladder, lev)]
r2[, lev := factor(lev, c("2", "5", "10", "20", "60"))]; print(dcast(r2, fs + ladder + phi ~ lev, value.var = "est")); print(dcast(r2, fs + ladder + phi ~ lev, value.var = "yrs"))
cat("\nDiD pooled over bins and levels 10, 20, all\n")
print(D[lev %in% c("10", "20", "60"), .(d = mean(did)), by = .(phi, fs, ladder, testYear)][, .(est = round(mean(d), 3), yrs = sf(d)), by = .(fs, ladder, phi)][order(fs, ladder, phi)])
cat("\nRaw contrast (PreVal minus status quo) pooled over bins, by phi, level all\n")
print(dcast(Cy[lev == "60", .(est = round(mean(d), 3)), by = .(phi, fs, ladder, bin)], fs + ladder + phi ~ bin, value.var = "est"))
