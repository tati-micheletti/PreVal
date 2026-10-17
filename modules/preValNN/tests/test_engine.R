# End-to-end test of the engine on synthetic data: plan -> manifests -> models for both adapters (count NB and continuous Gaussian)
suppressMessages({library(data.table); library(torch)})
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
for (f in c("seeds.R", "caseIndex.R", "regimeSplits.R", "verifyRegimes.R", "adapters.R", "fitNet.R", "runModel.R", "generatePlan.R"))
  source(file.path(here, "..", "R", f))
nFail <- 0L
check <- function(cond, msg) { ok <- isTRUE(cond); cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) nFail <<- nFail + 1L }

set.seed(7)
nUnit <- 250; years <- 2005:2016
units <- data.table(SITE_ID = sprintf("r%03d", 1:nUnit), x = runif(nUnit, 4e6, 4.6e6), y = runif(nUnit, 2.7e6, 3.3e6), a = rnorm(nUnit), b = rnorm(nUnit))
tab <- rbindlist(lapply(years, function(yy) { u <- units[runif(nUnit) < 0.6]; u[, year := yy]; u }))
tab[, `:=`(n1 = rnorm(.N), n2 = rnorm(.N), n3 = rnorm(.N))]
tab[, mu := exp(0.4 + 0.6 * a - 0.4 * b)]
tab[, count := rnbinom(.N, mu = mu, size = 2)]
tab[, cont := 1 + 2 * a - b + rnorm(.N, sd = 0.5)]
feats <- c("a", "b", "n1", "n2", "n3")
idx <- buildCaseIndex(tab)

pl <- generatePlan(idx, startYear = 2005, endYear = 2016, minWindow = 5, horizons = 1, covariateLevels = c(2, Inf), blockKm = 100, bufferKm = 10,
                   minTrain = 100, minVal = 50, minTest = 50)
check(nrow(pl$plan) > 0 && all(pl$checks$ok), sprintf("plan built, %d models, all %d gate checks pass", nrow(pl$plan), nrow(pl$checks)))
check(uniqueN(pl$plan$typeValidation) == 5, "plan has all five regimes")
md <- file.path(tempdir(), "manifests"); writeManifests(pl$bundles, md); writeManifests(pl$bundles, md)
a <- auditManifests(idx, pl$plan, md, pl$spatial)
check(all(a$ok), "audit of the manifests on disk passes")

runOne <- function(responseType, response, hyper, planRow, store, outDir, k = 5) {
  ad <- getAdapter(responseType)
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  runModel(store, readManifest(md, planRow$splitId, planRow$typeValidation), planRow, feats[seq_len(k)], ad, hyper, outDir, reRun = TRUE)
}
hyper <- list(hidden = c(16, 8), embDim = 0L, lr = 0.01, batchSize = 64L, epochs = 40L, weightDecay = 0)
pr <- pl$plan[typeValidation == "FutureUnseen" & numberOfCovariates == Inf][1]

st <- buildCaseStore(tab, idx, feats, "count")
r1 <- runOne("count", "count", hyper, pr, st, file.path(tempdir(), "o1"))
r2 <- runOne("count", "count", hyper, pr, st, file.path(tempdir(), "o2"))
check(is.finite(r1$testLossMean) && r1$epochsRun >= 1, sprintf("count adapter: model fits (test NLL %.3f, %d epochs)", r1$testLossMean, r1$epochsRun))
check(isTRUE(all.equal(r1$testLossMean, r2$testLossMean)), "same seed gives the same test loss (deterministic)")
# the signal is learned: a model with the covariates beats the same model fed noise only (a, b hidden)
st0 <- buildCaseStore(tab, idx, c("n1", "n2", "n3"), "count")
ad <- getAdapter("count"); od <- file.path(tempdir(), "o3"); dir.create(od, showWarnings = FALSE)
rN <- runModel(st0, readManifest(md, pr$splitId, pr$typeValidation), pr, c("n1", "n2", "n3"), ad, hyper, od, reRun = TRUE)
check(r1$testLossMean < rN$testLossMean - 0.1, sprintf("informative covariates beat noise covariates (%.3f < %.3f)", r1$testLossMean, rN$testLossMean))

stc <- buildCaseStore(tab, idx, feats, "cont")
r3 <- runOne("continuous", "cont", hyper, pr, stc, file.path(tempdir(), "o4"))
check(is.finite(r3$testLossMean) && r3$testRMSE < 1.2, sprintf("continuous adapter: same engine, other head (RMSE %.2f)", r3$testRMSE))
# a status-quo manifest presented as PreVal is refused before any model is trained
o5 <- file.path(tempdir(), "o5"); dir.create(o5, showWarnings = FALSE)
msg <- tryCatch(runModel(st, readManifest(md, pr$splitId, "FutureTainted"), pr, feats, ad, hyper, o5, reRun = TRUE), error = function(e) conditionMessage(e))
check(is.character(msg) && grepl("FutureUnseen year rules violated", msg), "runModel refuses a status-quo manifest presented as PreVal")
cat(sprintf("\n%d failure(s)\n", nFail)); if (nFail) quit(status = 1)
