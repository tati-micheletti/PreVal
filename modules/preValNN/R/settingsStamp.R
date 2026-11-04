#' SETTINGS STAMP (added 2026-11-04, after the first full caribou runs trained with a stale networkSettings.rds: lr 0.01 instead of 0.001).
#'
#' The engine skips every model whose result file already exists, and it never knew WHICH settings had produced that file. A stamp (settingsStamp.rds) now sits in every folder that
#' holds results (testedModels, rankings) and records the EFFECTIVE training settings (after hyperOverride and after the run parameter `epoch` has replaced the file's epochs).
#' A later run with other settings stops instead of silently mixing old results with new ones; the rankings stamp is also compared with the training settings
#' (the covariate ladder must come from a network trained like the models that use it).
#' Results that exist WITHOUT a stamp (made before 2026-11-04) are refused unless PREVAL_ADOPT_STAMP=1 says "I know these were made with the current settings".

SETTINGS_STAMP_KEYS <- c("embDim", "hidden", "lr", "batchSize", "weightDecay", "dropout", "idIntercept", "idDecay", "epochs")

#' The stamp of one run: the effective hyper-parameters, the early-stopping patience and the plateau schedule of fitNet().
settingsStamp <- function(hyper, earlyStopPatience, plateau = NULL) {
  if (is.null(plateau)) {
    fm <- if (exists("fitNet", mode = "function")) formals(get("fitNet", mode = "function")) else list(patience = 2L, factor = 0.5, minLr = 1e-6, threshold = 1e-4)
    plateau <- lapply(fm[c("patience", "factor", "minLr", "threshold")], eval)
  }
  s <- hyper[intersect(SETTINGS_STAMP_KEYS, names(hyper))]
  s$earlyStopPatience <- as.numeric(earlyStopPatience)
  s$plateau <- plateau
  s[order(names(s))]
}

#' Names of the settings that differ between two stamps (character(0) when they agree).
stampDifferences <- function(a, b) {
  k <- union(names(a), names(b))
  k[!vapply(k, function(n) isTRUE(all.equal(a[[n]], b[[n]], check.attributes = FALSE)), logical(1))]
}

#' Check (and, the first time, write) the settings stamp of a results folder.
#' @param dir folder of the results (testedModels or rankings); created if missing.
#' @param stamp settingsStamp() of the current run.
#' @param resultPattern regular expression of the result files that would be reused (to recognise results of an unstamped, older run).
#' @param adopt TRUE stamps existing unstamped results with the current settings (explicit decision of the user).
#' @param writeIfMissing FALSE for read-only stages (analysis): then a missing stamp is reported, nothing is written.
#' @return "match", "written", "adopted" or "missing" (read-only stage, no stamp); stops when the stamp differs or when unstamped results exist and `adopt` is FALSE.
checkSettingsStamp <- function(dir, stamp, resultPattern, adopt = identical(Sys.getenv("PREVAL_ADOPT_STAMP"), "1"), writeIfMissing = TRUE) {
  f <- file.path(dir, "settingsStamp.rds")
  if (file.exists(f)) {
    old <- readRDS(f); d <- stampDifferences(old, stamp)
    if (length(d)) stop("The results in ", dir, " were made with other settings than this run (differs in: ", paste(d, collapse = ", "), "; stamped lr ", old$lr, ", this run lr ", stamp$lr,
                        "). Move the old results aside (e.g. rename the folder with a suffix) or use the settings of the stamp; they must not be mixed.", call. = FALSE)
    return("match")
  }
  have <- dir.exists(dir) && length(list.files(dir, pattern = resultPattern)) > 0L
  if (have && !adopt) {
    if (!writeIfMissing) return("missing")
    stop("Results exist in ", dir, " but there is no settingsStamp.rds, so the settings that made them are unknown. Move them aside, or set PREVAL_ADOPT_STAMP=1 to declare that they were made with the settings of this run.", call. = FALSE)
  }
  if (!writeIfMissing) return("missing")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = ".stamp_", tmpdir = dir); saveRDS(stamp, tmp)            # written under another name and renamed: array tasks start together and must never read a half-written stamp
  if (!isTRUE(file.rename(tmp, f))) { unlink(tmp); if (file.exists(f)) return(checkSettingsStamp(dir, stamp, resultPattern, adopt, writeIfMissing)); stop("could not write ", f) }
  if (have) "adopted" else "written"
}

#' The rankings of a run (the covariate ladder) must come from a network trained like the models that use it: compare the rankings stamp with the training stamp.
checkRankingStamp <- function(rankDir, stamp, adopt = identical(Sys.getenv("PREVAL_ADOPT_STAMP"), "1")) {
  f <- file.path(rankDir, "settingsStamp.rds")
  if (!file.exists(f)) {
    if (adopt) return("adopted-unchecked")
    stop("The rankings in ", rankDir, " have no settingsStamp.rds, so the network settings that made the covariate ladder are unknown (rankings made before 2026-11-04 may come from a stale networkSettings.rds). ",
         "Run the rank stage again, or set PREVAL_ADOPT_STAMP=1 to accept them.", call. = FALSE)
  }
  d <- stampDifferences(readRDS(f), stamp)
  if (length(d)) stop("The rankings in ", rankDir, " were made with other network settings than the training of this run (differs in: ", paste(d, collapse = ", "), "). Run the rank stage again with the current settings.", call. = FALSE)
  "match"
}
