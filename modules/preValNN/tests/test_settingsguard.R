# Guard against stale / mixed network settings (2026-11-04). The first full caribou EVE runs trained with a stale networkSettings.rds (lr 0.01 instead of 0.001) and
# the engine had no way to notice: it reused any existing result file. This tests (1) the file guard, (2) the settings stamp of results folders, (3) the ranking stamp.
suppressMessages(library(data.table))
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(here, "..", "R", "fitNet.R")); source(file.path(here, "..", "R", "settingsStamp.R"))
source(file.path(here, "..", "..", "preValPrep_Caribou", "R", "prepCaribou.R"))
nFail <- 0L
check <- function(cond, msg) { ok <- isTRUE(cond); cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) nFail <<- nFail + 1L }
fails <- function(expr, pattern) { m <- tryCatch({ force(expr); NA_character_ }, error = function(e) conditionMessage(e)); !is.na(m) && grepl(pattern, m) }
Sys.setenv(PREVAL_ADOPT_STAMP = "")

# ---- 1. the networkSettings.rds guard ----
ref <- caribouNetworkSettings()
check(isTRUE(checkCaribouNetworkSettings(ref, ref)), "the file the prep code writes passes the guard")
stale <- ref; stale$lr <- 0.01; stale$epochs <- 50L                                    # exactly the file that went to EVE on 31 Oct 2026
check(fails(checkCaribouNetworkSettings(stale, ref), "lr, epochs"), "the stale file of 31 Oct (lr 0.01, epochs 50) is refused and the differing settings are named")
e50 <- ref; e50$epochs <- 50L
check(fails(checkCaribouNetworkSettings(e50, ref), "epochs"), "a file that differs only in epochs is refused too")
for (k in c("embDim", "hidden", "batchSize", "weightDecay", "dropout", "idIntercept")) {
  x <- ref; x[[k]] <- if (is.logical(ref[[k]])) !ref[[k]] else if (k == "hidden") c(64, 32) else ref[[k]] + 1
  check(fails(checkCaribouNetworkSettings(x, ref), k), sprintf("a file that differs only in %s is refused", k))
}
old <- ref; old$idIntercept <- NULL
check(fails(checkCaribouNetworkSettings(old, ref), "idIntercept"), "a file without idIntercept (older layout) is refused")
tf <- tempfile(fileext = ".rds"); saveRDS(ref, tf); check(isTRUE(checkCaribouNetworkSettings(readRDS(tf), ref)), "a file written with saveRDS and read back passes (no round-trip artefacts)")

# ---- 2. the stamp of a results folder ----
hyper <- list(embDim = 8L, hidden = c(128, 64), lr = 0.001, batchSize = 128L, epochs = 300L, weightDecay = 0, idIntercept = FALSE, dropout = 0, source = "ignored", lastPilotYear = NA)
stp <- settingsStamp(hyper, 10)
check(identical(sort(names(stp)), sort(c(intersect(SETTINGS_STAMP_KEYS, names(hyper)), "earlyStopPatience", "plateau"))) && !any(c("source", "lastPilotYear") %in% names(stp)),
      "the stamp holds the training settings that are set, the early-stopping patience and the plateau schedule, nothing else (not the file's source text)")
check(isTRUE(all.equal(stp$plateau$factor, 0.5)) && isTRUE(all.equal(stp$plateau$patience, 2)), "the plateau schedule is read from fitNet() (halve after 3 epochs without improvement)")
d <- file.path(tempfile("run"), "testedModels")
check(identical(checkSettingsStamp(d, stp, "_finalDT\\.csv$"), "written") && file.exists(file.path(d, "settingsStamp.rds")), "an empty results folder gets a stamp on the first call")
check(identical(checkSettingsStamp(d, stp, "_finalDT\\.csv$"), "match"), "the same settings match the stamp")
check(length(list.files(d, all.files = TRUE, pattern = "^\\.stamp_")) == 0L, "no temporary stamp file is left behind")
writeLines("x", file.path(d, "m1_finalDT.csv"))                                         # a finished model of this run
check(identical(checkSettingsStamp(d, stp, "_finalDT\\.csv$"), "match"), "with results present, the same settings still match")
h2 <- hyper; h2$lr <- 0.01
check(fails(checkSettingsStamp(d, settingsStamp(h2, 10), "_finalDT\\.csv$"), "differs in: lr"), "a run with another learning rate is refused (the 3 Nov mistake) and the message names lr")
h3 <- hyper; h3$epochs <- 50L
check(fails(checkSettingsStamp(d, settingsStamp(h3, 10), "_finalDT\\.csv$"), "epochs"), "another epoch cap is refused")
check(fails(checkSettingsStamp(d, settingsStamp(hyper, 15), "_finalDT\\.csv$"), "earlyStopPatience"), "another early-stopping patience is refused")
h4 <- hyper; h4$hidden <- c(64, 32)
check(fails(checkSettingsStamp(d, settingsStamp(h4, 10), "_finalDT\\.csv$"), "hidden"), "another network size is refused")
h5 <- hyper; h5$source <- "other text"; h5$lastPilotYear <- 2010
check(identical(checkSettingsStamp(d, settingsStamp(h5, 10), "_finalDT\\.csv$"), "match"), "settings that do not train anything (source text, lastPilotYear) do not trigger a refusal")
check(identical(checkSettingsStamp(d, settingsStamp(modifyList(hyper, list(hidden = c(128L, 64L), embDim = 8)), 10), "_finalDT\\.csv$"), "match"), "integer versus double storage of the same numbers is not a difference")

# results of an older run without a stamp
d2 <- file.path(tempfile("run"), "testedModels"); dir.create(d2, recursive = TRUE); writeLines("x", file.path(d2, "old_finalDT.csv"))
check(fails(checkSettingsStamp(d2, stp, "_finalDT\\.csv$"), "no settingsStamp"), "results without a stamp (older run) are refused: their settings are unknown")
check(identical(checkSettingsStamp(d2, stp, "_finalDT\\.csv$", writeIfMissing = FALSE), "missing") && !file.exists(file.path(d2, "settingsStamp.rds")), "a read-only stage (analysis) reports a missing stamp and writes nothing")
check(identical(checkSettingsStamp(d2, stp, "_finalDT\\.csv$", adopt = TRUE), "adopted") && identical(checkSettingsStamp(d2, stp, "_finalDT\\.csv$"), "match"), "adopting is possible only on request, and the folder is then stamped")
Sys.setenv(PREVAL_ADOPT_STAMP = "1"); d3 <- file.path(tempfile("run"), "testedModels"); dir.create(d3, recursive = TRUE); writeLines("x", file.path(d3, "old_finalDT.csv"))
check(identical(checkSettingsStamp(d3, stp, "_finalDT\\.csv$"), "adopted"), "PREVAL_ADOPT_STAMP=1 is the explicit way to adopt unstamped results"); Sys.setenv(PREVAL_ADOPT_STAMP = "")
d4 <- tempfile("emptyrun")                                                                # a folder that does not exist yet
check(identical(checkSettingsStamp(d4, stp, "_finalDT\\.csv$"), "written") && dir.exists(d4), "a missing folder is created for the stamp")
# two array tasks that start together: the second one must read a complete stamp
d5 <- tempfile("race"); a <- checkSettingsStamp(d5, stp, "_finalDT\\.csv$"); b <- checkSettingsStamp(d5, stp, "_finalDT\\.csv$"); check(identical(c(a, b), c("written", "match")), "a second task starting after the first finds a readable stamp")

# ---- 3. the rankings must come from the settings of the training ----
rk <- file.path(tempfile("run"), "rankings"); dir.create(rk, recursive = TRUE); writeLines("x", file.path(rk, "s2013_e2014.csv"))
check(fails(checkRankingStamp(rk, stp), "no settingsStamp"), "rankings without a stamp (made before 2026-11-04, possibly with the stale file) stop the training")
check(identical(checkRankingStamp(rk, stp, adopt = TRUE), "adopted-unchecked"), "they can be accepted only on request")
check(fails(checkSettingsStamp(rk, stp, "^s[0-9]+_e[0-9]+\\.csv$"), "no settingsStamp"), "the rank stage itself refuses to continue on unstamped rankings")
rk2 <- file.path(tempfile("run"), "rankings"); checkSettingsStamp(rk2, settingsStamp(h2, 10), "^s[0-9]+_e[0-9]+\\.csv$")      # rankings made at lr 0.01
check(fails(checkRankingStamp(rk2, stp), "differs in: lr"), "rankings made at lr 0.01 stop a training at lr 0.001")
rk3 <- file.path(tempfile("run"), "rankings"); checkSettingsStamp(rk3, stp, "^s[0-9]+_e[0-9]+\\.csv$")
check(identical(checkRankingStamp(rk3, stp), "match"), "rankings made with the same settings pass")
check(identical(settingsStamp(hyper, 10), settingsStamp(rev(hyper), 10)), "the stamp does not depend on the order in which settings were listed")

cat(sprintf("\n%d check(s) failed.\n", nFail)); if (nFail > 0L) quit(status = 1L)
