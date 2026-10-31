# preValPrep_Caribou: the final table (long, 11 rows per stratum), its checks, the start-year filter, the recode check and the files written for the engine. Synthetic data only.
suppressMessages(library(data.table))
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(here, "..", "..", "preValNN", "R", "seeds.R")); source(file.path(here, "..", "R", "prepCaribou.R"))
nFail <- 0L
check <- function(cond, msg) { ok <- isTRUE(cond); cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) nFail <<- nFail + 1L }
set.seed(5); steps <- 11L; n <- 60L
st <- data.table(indiv_step_id = sprintf("s%03d", seq_len(n)), id = rep(c("a1", "a2", "a3"), each = n / 3), year = rep(c(2011L, 2013L, 2014L), times = n / 3), x1_ = runif(n), y1_ = runif(n), burst_ = "b1")
long <- st[rep(seq_len(.N), each = steps)]; long[, case_ := rep(c(TRUE, rep(FALSE, steps - 1L)), n)]
fc <- c("logSl", "timeSinceFire_endLog"); long[, logSl := rnorm(.N)]; long[, timeSinceFire_endLog := log(1 + sample(0:40, .N, TRUE))]
d <- tempfile(); dir.create(d); csv <- file.path(d, "preparedModelInputs.csv"); fwrite(long[, c(names(long)), with = FALSE], csv); saveRDS(fc, file.path(d, "featureCandidates.rds"))
r <- buildCaribouFinal(preparedCsv = csv, startYear = 2013L)
check(r$nStrata == sum(st$year >= 2013) && min(r$years) == 2013 && identical(names(r$finalTable)[1:6], c("indiv_step_id", "id", "year", "x1_", "y1_", "case_")) && all(fc %in% names(r$finalTable)), "final table: start year filter, strata kept whole, engine columns and candidate covariates")
check(nrow(r$finalTable) == steps * r$nStrata && all(r$finalTable$case_[seq(1, nrow(r$finalTable), by = steps)]), "11 rows per stratum, the observed step first")
check(inherits(try(checkCaribouFinal(long[-3], fc), silent = TRUE), "try-error"), "a table with an incomplete stratum is refused")
b <- copy(long); b[1, case_ := FALSE]; check(inherits(try(checkCaribouFinal(b, fc), silent = TRUE), "try-error"), "a stratum whose first row is not the observed step is refused")
b <- copy(long); b[5, logSl := NA]; check(inherits(try(checkCaribouFinal(b, fc), silent = TRUE), "try-error"), "NA in a candidate covariate is refused (no silent imputation)")
b <- copy(long); b[7, timeSinceFire_endLog := log(2000 - 1900 + 1)]; b[8, timeSinceFire_endLog := log(1 + 123)]
check(inherits(try(checkCaribouFinal(b, fc), silent = TRUE), "try-error"), "a calendar-year counter that was not recoded (above the 40-year cap) is refused")
o <- file.path(d, "out"); rk <- data.table(Feature = fc, Importance = c(0.2, 0.1)); fwrite(rk, file.path(d, "featureTable.csv"))
writeCaribouInputs(r, o, rankingCsv = file.path(d, "featureTable.csv"))
ns <- readRDS(file.path(o, "networkSettings.rds"))
check(all(file.exists(file.path(o, c("final_caribou.rds", "featureTable.csv", "networkSettings.rds")))) && ns$lr == 0.001 && ns$embDim == 8L && identical(ns$hidden, c(128, 64)) && is.na(ns$lastPilotYear), "engine inputs written: final table, ranking, and the original network settings (lr 0.001, 128-64, 8-d embedding, no pilot)")
fwrite(data.table(Feature = "logSl", Importance = 1), file.path(d, "bad.csv"))
check(inherits(try(writeCaribouInputs(r, o, rankingCsv = file.path(d, "bad.csv")), silent = TRUE), "try-error"), "a ranking file that does not list exactly the candidate covariates is refused")
cat(sprintf("\n%d failure(s)\n", nFail)); if (nFail) quit(status = 1)
