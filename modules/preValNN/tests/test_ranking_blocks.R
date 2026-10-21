# Tests: covariate ranking (grouping of correlated covariates, informative first) and block size from residual autocorrelation
suppressMessages({library(data.table); library(torch)})
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
for (f in c("seeds.R", "caseIndex.R", "adapters.R", "fitNet.R", "rankCovariates.R", "blockSize.R", "pilotNetwork.R")) source(file.path(here, "..", "R", f))
nFail <- 0L
check <- function(cond, msg) { ok <- isTRUE(cond); cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) nFail <<- nFail + 1L }

set.seed(11)
n <- 4000
tab <- data.table(SITE_ID = sprintf("u%04d", 1:n), year = sample(2005:2012, n, TRUE), x = runif(n, 4e6, 4.5e6), y = runif(n, 2.7e6, 3.2e6))
tab[, `:=`(a = rnorm(n), b = rnorm(n), n1 = rnorm(n), n2 = rnorm(n), n3 = rnorm(n), n4 = rnorm(n))]
tab[, a_twin := a + rnorm(n, sd = 0.05)]                    # near-copy of a: must share a's group
tab[, count := rnbinom(n, mu = exp(0.5 + 0.8 * a - 0.5 * b), size = 2)]
feats <- c("n1", "a_twin", "n2", "a", "b", "n3", "n4")
idx <- buildCaseIndex(tab)
st <- buildCaseStore(tab, idx, feats, "count")
hyper <- list(hidden = c(16, 8), embDim = 0L, lr = 0.01, batchSize = 128L, epochs = 40L)
rk <- rankCovariates(st, feats, adapterCount(), hyper, seed = 5L, mode = "random", nRep = 3L)
check(setequal(rk$Feature[1:3], c("a", "a_twin", "b")), sprintf("informative covariates rank first (top 3: %s)", paste(rk$Feature[1:3], collapse = ", ")))
check(rk[Feature == "a", group] == rk[Feature == "a_twin", group] && rk[Feature == "n1", group] != rk[Feature == "a", group], "a correlated twin shares the group, an unrelated covariate does not")
check(rk[Feature == "a", Importance] == rk[Feature == "a_twin", Importance], "group members share the group importance")
check(rk[Feature == "a", Importance] > 5 * max(rk[grepl("^n", Feature), Importance]), "importance of a real signal far above the noise covariates")
rk2 <- rankCovariates(st, feats, adapterCount(), hyper, seed = 5L, mode = "forward", nRep = 3L)
check(all(rk2$Feature[1:3] %in% c("a", "a_twin", "b")), "forward-mode ranking also puts the informative covariates first")
check(identical(featuresForLevel(rk, 2), rk$Feature[1:2]) && length(featuresForLevel(rk, Inf)) == length(feats), "ladder levels: k first, Inf = all")
check(effectiveNumber(tab[, ..feats]) == 6L, sprintf("effective number counts a and its twin once (%d of 7)", effectiveNumber(tab[, ..feats])))

# block size: residual field with a known correlation range (~80 km exponential) should give a block of the same order
set.seed(3)
m <- 900; pts <- data.table(SITE_ID = sprintf("s%04d", 1:m), x = runif(m, 4e6, 4.5e6), y = runif(m, 2.7e6, 3.2e6))
D <- as.matrix(dist(pts[, .(x, y)])); Sg <- exp(-D / 30000)              # exp(-d/30km): practical range ~ 90 km
fld <- as.numeric(t(chol(Sg + diag(1e-6, m))) %*% rnorm(m))
dat <- rbindlist(lapply(2005:2008, function(yy) copy(pts)[, `:=`(year = yy, z = 1 + fld + rnorm(m, sd = 0.3))]))
bs <- blockSizeFromAutocorrelation(dat, "z", baselineFeatures = character(0), family = "gaussian", crs = 3035, minKm = 10)
check(bs$rangeKm > 40 && bs$rangeKm < 200, sprintf("block size from residual autocorrelation is the right order (range %.0f km; truth about 90 km)", bs$rangeKm))
# pure noise: no autocorrelation -> a short range, the minimum block size applies
noise <- copy(dat)[, z := rnorm(.N)]
bn <- blockSizeFromAutocorrelation(noise, "z", character(0), family = "gaussian", crs = 3035, minKm = 25)
check(bn$blockKm >= 25 && bn$rangeKm < bs$rangeKm, sprintf("white-noise residuals give a much shorter range (%.0f km) and the minimum block size", bn$rangeKm))
# network pilot: random mode (all years) and strict forward mode, tiny grid
g <- list(list(hidden = c(8, 4), embDim = 0L, idIntercept = FALSE, idDecay = 0, weightDecay = 0, dropout = 0, lr = 0.01, batchSize = 128L),
          list(hidden = c(16, 8), embDim = 0L, idIntercept = TRUE, idDecay = 1e-3, weightDecay = 0, dropout = 0, lr = 0.01, batchSize = 128L))
pr <- pilotNetwork(st, feats, adapterCount(), lastPilotYear = NA, grid = g, seed = 1L, epochs = 15L, mode = "random")
check(is.na(pr$validationYear) && pr$nVal == floor(0.2 * nrow(tab)) && is.finite(pr$table$valLoss[1]), "pilot (random mode) uses a random 20% of all years and returns a finite validation loss")
pf <- pilotNetwork(st, feats, adapterCount(), lastPilotYear = 2009, grid = g, seed = 1L, epochs = 15L, mode = "forward")
check(pf$validationYear == 2009 && pf$nTrain == sum(tab$year < 2009), "pilot (forward mode) trains before and validates on the last pre-test year")
# per-window ranking (2026-10-21): only the years given to rankYears are used; a covariate informative only LATER does not rank on earlier years
set.seed(21); n2 <- 6000
t2 <- data.table(SITE_ID = sprintf("w%04d", 1:n2), year = sample(2005:2012, n2, TRUE), x = runif(n2, 4e6, 4.5e6), y = runif(n2, 2.7e6, 3.2e6))
t2[, `:=`(early = rnorm(n2), late = rnorm(n2), n1 = rnorm(n2), n2 = rnorm(n2))]
t2[, count := rnbinom(n2, mu = exp(0.5 + ifelse(year <= 2008, 1.0 * early, 1.0 * late)), size = 3)]
st2 <- buildCaseStore(t2, buildCaseIndex(t2), c("n1", "early", "late", "n2"), "count")
rkE <- rankCovariates(st2, c("n1", "early", "late", "n2"), adapterCount(), hyper, seed = 5L, mode = "random", rankYears = 2005:2008, nRep = 3L)
rkL <- rankCovariates(st2, c("n1", "early", "late", "n2"), adapterCount(), hyper, seed = 5L, mode = "random", rankYears = 2009:2012, nRep = 3L)
check(rkE$Feature[1] == "early" && rkL$Feature[1] == "late", "ranking on the years given only: the covariate informative in those years comes first (early on 2005-2008, late on 2009-2012)")
check(attr(rkE, "nTrain") + attr(rkE, "nHeldOut") <= sum(t2$year <= 2008) && attr(rkL, "nTrain") + attr(rkL, "nHeldOut") <= sum(t2$year >= 2009), "no case outside the ranking years is used (training + held-out cases fit inside those years)")
# balanced pilot (2026-10-21): the SAME configurations judged under the random hold-out and two forward geometries; the mean decides; a failing configuration is reported, not hidden
g3 <- c(g, list(list(hidden = c(8, 4), embDim = 0L, idIntercept = FALSE, idDecay = 0, weightDecay = 0, dropout = 0.1, lr = 0.01, batchSize = 128L)))   # dropout > 0 must now work
pb <- pilotNetwork(st, feats, adapterCount(), lastPilotYear = NA, grid = g3, seed = 1L, epochs = 15L, mode = "balanced", gaps = c(1L, 3L))
check(setequal(pb$geometries, c("random", "forward_gap1", "forward_gap3")) && all(c("valLoss_random", "valLoss_forward_gap1", "valLoss_forward_gap3") %in% names(pb$table)),
      "balanced pilot: configurations are judged under the random and the two forward geometries")
check(all(is.finite(pb$table$valLoss)) && all(abs(pb$table$valLoss - rowMeans(pb$table[, .(valLoss_random, valLoss_forward_gap1, valLoss_forward_gap3)])) < 1e-9),
      "balanced pilot: the decisive loss is the mean over the geometries, and the dropout configuration produced a finite loss")
check(pb$validationYear == max(tab$year), "balanced pilot: the forward geometries validate on the last year")
pp <- pilotNetwork(st, feats, adapterCount(), lastPilotYear = 2009, grid = g, seed = 1L, epochs = 12L, mode = "balanced")
check(pp$validationYear == 2009 && setequal(pp$geometries, c("random", "forward_gap1", "forward_gap3")) && pp$nTrain + pp$nVal == sum(tab$year <= 2009),
      "balanced pilot with lastPilotYear: every geometry stays inside the years up to it (validation on that year, gaps 1 and 3)")
cat(sprintf("\n%d failure(s)\n", nFail)); if (nFail) quit(status = 1)
