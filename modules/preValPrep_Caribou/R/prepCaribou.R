#' preValPrep_Caribou: the FINAL TABLE of the caribou data for the standardised engine (preValNN, dataLayout = "strata").
#'
#' One case = one STRATUM (the observed step + its 10 available steps, 11 rows, observed step first). The final table is the LONG table (11 rows per stratum) with the columns
#' the engine needs: indiv_step_id, id (animal), year, x1_, y1_ (start of the observed step, projected metres), case_, burst_ and the candidate covariates. It is produced by the
#' SAME preparation as the old caribou experiment (prepareNNdata of caribouNN_Global: strata with a missing step length / turn angle or without 11 rows and one observed step
#' are removed whole, which includes the first step of a burst; the disturbance-year counters timeSince* are recoded to the published cap of 40 years; start year 2013; engineered
#' covariates and step-length interactions), either by calling it on the extracted-features table or by reading its saved result (results_firstpass/preparedModelInputs.csv).
#' The recode report is kept and a check refuses a table in which a calendar-year counter is still present.

#' @param extracted extracted-features table (11 rows per stratum, as read by caribouNN_Global); run through prepareNNdata. NULL = use `preparedCsv`.
#' @param preparedCsv path of the saved prepared table (preparedModelInputs.csv), read with fread(select = ...)
#' @param featureCandidates the candidate covariates (default: featureCandidates.rds next to the csv)
#' @param startYear first year used
#' @param nStrataMax smoke tests only: read only the first nStrataMax strata of the csv
#' @return list(finalTable, featureCandidates, recodeReport, nStrata, nAnimals, years)
buildCaribouFinal <- function(extracted = NULL, preparedCsv = NULL, featureCandidates = NULL, startYear = 2013L, nStrataMax = NULL, steps = 11L,
                              prepareFun = NULL, pathTmp = tempdir()) {
  keep <- c("indiv_step_id", "id", "year", "x1_", "y1_", "case_", "burst_")
  if (!is.null(extracted)) {
    if (is.null(prepareFun)) stop("buildCaribouFinal: pass prepareFun = prepareNNdata (source modules/caribouNN_Global/R/prepareNNdata.R first)")
    pr <- prepareFun(extracted, pathX = file.path(pathTmp, "tensor_x.pt"), pathID = file.path(pathTmp, "tensor_id.pt"), startYear = startYear)
    dt <- pr$preparedDataFinal; featureCandidates <- featureCandidates %||% pr$modelPrep$featureCandidates; recode <- pr$recodeReport
  } else {
    if (is.null(preparedCsv)) stop("buildCaribouFinal: give `extracted` or `preparedCsv`")
    fcFile <- file.path(dirname(preparedCsv), "featureCandidates.rds")
    featureCandidates <- featureCandidates %||% readRDS(fcFile)
    dt <- data.table::fread(preparedCsv, select = c(keep, featureCandidates), nrows = if (is.null(nStrataMax)) Inf else nStrataMax * steps)
    rf <- file.path(dirname(preparedCsv), "disturbanceRecodeReport.csv"); recode <- if (file.exists(rf)) data.table::fread(rf) else NULL
    dt <- dt[year >= startYear]
  }
  miss <- setdiff(c(keep[keep != "burst_"], featureCandidates), names(dt)); if (length(miss)) stop("buildCaribouFinal: columns missing: ", paste(miss, collapse = ", "))
  dt <- dt[, c(intersect(keep, names(dt)), featureCandidates), with = FALSE]
  chk <- checkCaribouFinal(dt, featureCandidates, steps)
  list(finalTable = dt, featureCandidates = featureCandidates, recodeReport = recode, nStrata = chk$nStrata, nAnimals = chk$nAnimals, years = chk$years)
}

#' Checks that the engine relies on: complete strata (11 rows, the observed step first and unique), strata contiguous, no NA in a candidate covariate, no calendar-year counter.
checkCaribouFinal <- function(dt, featureCandidates, steps = 11L) {
  n <- nrow(dt); if (n %% steps != 0) stop("checkCaribouFinal: number of rows is not a multiple of ", steps)
  first <- seq.int(1L, n, by = steps); last <- first + steps - 1L
  if (!all(dt$indiv_step_id[first] == dt$indiv_step_id[last])) stop("checkCaribouFinal: strata are not contiguous blocks of ", steps, " rows")
  if (!all(dt$case_[first] == TRUE) || sum(dt$case_) != length(first)) stop("checkCaribouFinal: the observed step must be the first and only observed row of every stratum")
  na <- vapply(featureCandidates, function(f) sum(is.na(dt[[f]])), numeric(1)); if (any(na > 0)) stop("checkCaribouFinal: NA in covariate(s): ", paste(names(na)[na > 0], collapse = ", "))
  yc <- grep("^timeSince", featureCandidates, value = TRUE)
  for (f in yc) if (max(dt[[f]]) > log(41) + 1e-6) stop("checkCaribouFinal: ", f, " still holds a calendar-year counter (max ", max(dt[[f]]), " > log(41)); the disturbance recode (cap 40 years) was not applied")
  list(nStrata = length(first), nAnimals = data.table::uniqueN(dt$id), years = range(dt$year))
}

#' Network settings of the caribou experiment: the ORIGINAL network, never tuned (Tati, 2026-10-29), with the settings the old runs ACTUALLY used (results_firstpass/designParams.rds):
#' animal embedding 8-d, 128-64, Adam learning rate 0.001 halved on plateau, batch 128, early stop after 10 epochs without a new best validation loss, epoch cap 50 in the first pass and 300 in
#' the second (the module defaults, lr 0.01 and no early stop, were NOT what was run). Best validation epoch kept. No pilot is run, so the engine's lastPilotYear is NA.
caribouNetworkSettings <- function() list(embDim = 8L, hidden = c(128, 64), lr = 0.001, batchSize = 128L, epochs = 300L, weightDecay = 0, idIntercept = FALSE, dropout = 0,
                                          lastPilotYear = NA_real_, source = "original caribou network (modules/caribouNN/R/stratumNet.R); not tuned")

#' Write everything the engine expects next to each other: <outDir>/final_caribou.rds, featureTable.csv (the global ranking used for the covariate list and for the replication arm),
#' networkSettings.rds, disturbanceRecodeReport.csv.
writeCaribouInputs <- function(res, outDir, rankingCsv = NULL) {
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(res$finalTable, file.path(outDir, "final_caribou.rds"))
  if (!is.null(res$recodeReport)) data.table::fwrite(res$recodeReport, file.path(outDir, "disturbanceRecodeReport.csv"))
  saveRDS(caribouNetworkSettings(), file.path(outDir, "networkSettings.rds"))
  if (!is.null(rankingCsv)) {
    rk <- data.table::fread(rankingCsv); if (!all(res$featureCandidates %in% rk$Feature) || nrow(rk) != length(res$featureCandidates))
      stop("writeCaribouInputs: the ranking file must list exactly the candidate covariates")
    data.table::fwrite(rk, file.path(outDir, "featureTable.csv"))
  } else data.table::fwrite(data.table::data.table(Feature = res$featureCandidates, Importance = NA_real_), file.path(outDir, "featureTable.csv"))
  invisible(outDir)
}
