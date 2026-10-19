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
cat(sprintf("\n%d failure(s)\n", nFail)); if (nFail) quit(status = 1)
