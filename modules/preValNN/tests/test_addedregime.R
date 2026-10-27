# compareAddedRegime: a regime trained in a separate run folder is compared with the base run's regimes on identical test cases (synthetic allModels.csv files)
suppressMessages(library(data.table))
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(here, "..", "R", "analyzeRegimes.R"))
nFail <- 0L
check <- function(cond, msg) { ok <- isTRUE(cond); cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) nFail <<- nFail + 1L }
set.seed(2)
g <- CJ(splitId = sprintf("s%02d", 1:40), complexity = c(2, 5), replicate = 1L); g[, `:=`(testYear = 2010L + as.integer(substr(splitId, 2, 3)) %% 14L, horizon = 1L + as.integer(substr(splitId, 2, 3)) %% 6L)]
mk <- function(reg, shift) { x <- copy(g); x[, `:=`(typeValidation = reg, realized = 1 + shift + rnorm(.N, 0, 0.01) + 0.5 * (splitId == "s01"), reported = 1 + shift - 0.1)]; x }
base <- rbindlist(list(mk("FutureUnseen", 0.1), mk("FutureTainted", 0.2))); add <- mk("FutureUnseenGrouped", 0)
dA <- file.path(tempdir(), "base"); dB <- file.path(tempdir(), "add"); dir.create(dA); dir.create(dB); fwrite(base, file.path(dA, "allModels.csv")); fwrite(add, file.path(dB, "allModels.csv"))
r <- compareAddedRegime(dA, dB, file.path(tempdir(), "out"), against = c("FutureUnseen", "FutureTainted"))
a <- r[contrast == "FutureUnseenGrouped minus FutureUnseen" & scope == "all"]
check(nrow(a) == 1 && abs(a$estimate + 0.1) < 0.02 && a$nNegative == a$nYears, "added regime is 0.1 better than PreVal on identical cases; every forecast year agrees")
check(all(c("all", "horizon 1", "covariates 2", "covariates 5") %in% r[contrast == "FutureUnseenGrouped minus FutureTainted"]$scope), "overall, per-horizon and per-complexity rows are reported")
check(abs(r[grepl("optimism", contrast)]$estimate - 0.1) < 0.02 && file.exists(file.path(tempdir(), "out", "added_regime_FutureUnseenGrouped.csv")), "optimism of the added regime and the csv output")
cat(sprintf("\n%d failure(s)\n", nFail)); if (nFail) quit(status = 1)
