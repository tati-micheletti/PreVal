# LOCAL CHECK (no engine, no cluster): does a flip with a given size make a simple model, trained on early years and tested on later years, worse?
# World = stage-1v anchor settings (flip share 0.30 for stable/drift scaling), only the flip contribution sd (flipSd) and flip strength phi vary; matched seeds.
suppressMessages(library(data.table)); options(width = 220)
source("C:/Users/michelet/Documents/GitHub/PreVal_refit/simulation/simulate_world.R")
args <- commandArgs(TRUE); seeds <- if (length(args)) as.integer(strsplit(args[1], ",")[[1]]) else 101:104
sds <- if (length(args) > 1) as.numeric(strsplit(args[2], ",")[[1]]) else c(0.16, 0.23, 0.5, 0.7)
flipShares <- function(fs) { rest <- 1 - 0.0714 - fs; c(route = 0.20, stable = 0.75 * rest, drift = 0.25 * rest, flip = fs, year = 0.0714) }
mk <- function(seed, phi, fsd) { p <- modifyList(anchorWorldParams(), list(seed = seed, flipStrength = phi, nRoutes = 2139L, nStable = 12L, nDrift = 6L, nFlip = 12L, nNoise = 30L, routeDrift = 0,
  routeFromCov = 0.90, routeWalkSd = 0.23, noiseScale = 0.7, etaSd = 0.30, surveyRho = 0.70, flipSd = fsd)); p$shares <- flipShares(0.30); p }
dev <- function(y, mu) { mu <- pmax(mu, 1e-8); 2 * mean(ifelse(y > 0, y * log(y / mu), 0) - (y - mu)) }
bin <- function(h) ifelse(h == 1, "1", ifelse(h == 2, "2", ifelse(h <= 4, "3-4", ifelse(h <= 9, "5-9", "10-12"))))
one <- function(seed, phi, fsd) {
  w <- simulateWorld(mk(seed, phi, fsd)); tb <- w$table; fm <- w$covariates; cv <- function(f) fm[family %in% f]$covariate
  tr <- tb[year <= 2012]; te <- tb[year >= 2013]
  rl <- tr[, .(rl = log(mean(Reviere) + 0.5)), by = SITE_ID]; tr <- merge(tr, rl, by = "SITE_ID"); te <- merge(te, rl, by = "SITE_ID")   # routes seen in training only ("same places")
  fit <- function(vars) { f <- as.formula(paste("Reviere ~ rl", if (length(vars)) paste("+", paste(vars, collapse = "+")) else "")); m <- glm(f, family = poisson(), data = tr, control = glm.control(maxit = 50)); list(tr = dev(tr$Reviere, fitted(m)), te = predict(m, te, type = "response")) }
  mods <- list(R = character(0), A = cv(c("stable", "drift")), B = cv(c("stable", "drift", "flip")), N = cv(c("stable", "drift", "noise")))
  te[, h := year - 2012L]; out <- rbindlist(lapply(names(mods), function(mn) { f <- fit(mods[[mn]]); te[, .(dev = dev(Reviere, f$te[.I]), n = .N), by = .(bin = bin(h))][, `:=`(model = mn, trainDev = f$tr)] }))
  K <- w$contributions; out[, `:=`(seed = seed, phi = phi, flipSd = fsd, sdFlipRealised = sd(K$flip), zeros = mean(tb$Reviere == 0), vmr = var(tb$Reviere) / mean(tb$Reviere), sdEta = sd(K$eta))]; out
}
X <- rbindlist(lapply(sds, function(s) rbindlist(lapply(seeds, function(sd) rbindlist(lapply(c(0, 1), function(ph) { cat(sprintf("seed %d flipSd %.2f phi %d\n", sd, s, ph)); one(sd, ph, s) }))))))
fwrite(X, "C:/Users/michelet/AppData/Local/Temp/claude/99d62fbcc8f70fb6/d5cc68df-910c-4547-a0c2-4feecbab7f6b/scratchpad/flip_visible_raw.csv")
W <- X[, .(dev = mean(dev)), by = .(flipSd, phi, bin, model)]; W <- dcast(W, flipSd + phi + bin ~ model, value.var = "dev")
W[, `:=`(gainFlipCov = A - B, gainNoiseCov = A - N)]; W[, bin := factor(bin, c("1", "2", "3-4", "5-9", "10-12"))]; setorder(W, flipSd, bin, phi)
num <- c("R", "A", "B", "N", "gainFlipCov", "gainNoiseCov"); W[, (num) := lapply(.SD, round, 4), .SDcols = num]
cat("\nOut-of-time Poisson deviance, mean over seeds. R = route level only; A = + stable & drift covariates; B = A + flip covariates; N = A + noise covariates\n")
cat("gainFlipCov = A - B: how much adding the flip covariates helps (positive = helps; negative = hurts). gainNoiseCov = A - N: the same for pure noise covariates (reference for 'hurts').\n")
print(W)
cat("\nWorld checks (mean over seeds): realised sd of the flip contribution, sd of eta, share of zeros, variance/mean of counts\n")
print(X[model == "R" & bin == "1", .(sdFlip = round(mean(sdFlipRealised), 3), sdEta = round(mean(sdEta), 3), zeros = round(mean(zeros), 3), vmr = round(mean(vmr), 2)), by = .(flipSd, phi)][order(flipSd, phi)])
cat("\nIn-sample (training-years) deviance of A and B: does B fit training better?\n")
print(dcast(X[bin == "1" & model %in% c("A", "B"), .(tr = round(mean(trainDev), 4)), by = .(flipSd, phi, model)], flipSd + phi ~ model, value.var = "tr"))
