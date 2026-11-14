# Per-case decomposition of "new PreVal minus status quo" (checks M3 and M4 of the route-specific-memory hypothesis; the same script handles caribou).
# READ-ONLY on the result folders: it reads <run>/experimentPlan.csv, caseIndex.rds, splits/*.rds and testedModels/*_perCase.rds and writes ONLY into --outDir.
#
# WHAT IT DOES
#   For every model of the status quo (regime FutureTainted) and the matching model of new PreVal (regime FutureUnseenGrouped) it joins the TEST cases of the two
#   per-case files (same splitId, numberOfCovariates, replicate, same case row) and forms d = loss(new PreVal) - loss(status quo) per case (positive = new PreVal worse).
#   Cases are then split by
#     poolCat        pool membership of the case's unit, from the seenUnit flags of the two per-case files (= unit has >= 1 training row of THAT model):
#                      inBoth   seen by the status quo and by new PreVal
#                      sqOnly   seen by the status quo, NOT by new PreVal (the units new PreVal set aside for its validation)   <- M4
#                      pvOnly   seen by new PreVal but not by the status quo
#                      inNeither seen by neither (new units)
#     yearsBin       number of distinct years of the unit inside the window's TRAINING years (s .. e-valYears, counted in caseIndex.rds): <=3 | 4-6 | >=7   <- M3
#                    (units absent from the window count as 0 and fall in <=3). For caribou the "years" are the animal's years; its number of training STRATA is used for expTerc.
#     expTerc        terciles (low / mid / high, ranks within the split's test cases, ties share a bin) of the exposure: route = number of training years, animal = number of training strata
#     poolCat:yearsBin  the cross of the two
#     lastGap        ADDED 2026-11-13 (recency cut): e - (last year the unit is observed inside the window s..e, validation year(s) included): g0 | g1 | g2-3 | g4+ | none (never observed).
#                    g0 = the unit was observed in the window's final year, which new PreVal keeps out of training (validation) and the status quo trains on. Dimensions lastGap and
#                    poolCat:lastGap; contrasts R1/R2 (g0 minus g2-3, g4+; within inBoth) predict POSITIVE at horizon 1 if the leftover deficit among units both models know is recency.
#   Everything is first averaged within split, then within FORECAST YEAR (the unit of inference, as in analyzeRegimes.R: every split x level x replicate row counts equally), and the
#   across-year test is .yearTest() of modules/preValNN/R/analyzeRegimes.R (mean, t interval, exact sign-flip p, nPositive/nNegative years). Interval and p are reported only
#   where >= 3 forecast years contribute. Horizon bins as in analyzeRegimes: 1, 2, 3-4, 5-9, 10+ (and "all").
#   Within-year differences between cells (e.g. sqOnly minus inBoth; >=7 years minus <=3 years) are tested the same way (percase_cell_contrasts.csv): they are the direct tests of
#   M4 (predicted sign: positive at h1) and M3 (predicted sign: positive at h1).
#
# USAGE (arguments --key=value; any of them can also come from an environment variable, shown in brackets; arguments win)
#   Birds, two run folders:
#     Rscript birds/post_memory_percase.R --species=Chloris_chloris --base=/data/birds/PreVal/birds/outputs/v4 --outDir=/work/$USER/preval/percase_Chloris_chloris
#       status quo = <base>/<species>/run, new PreVal = <base>/<species>/run_routes   (--runSQ=run --runPV=run_routes; names are relative to <base>/<species>, absolute paths also work)
#   Caribou, both regimes in one folder:
#     Rscript birds/post_memory_percase.R --unit=animal --base=/data/birds/PreVal/caribou/outputs/engine --outDir=...    (species caribou, run_main for both)
#   --unit=route|animal [PREVAL_UNIT, default route]      --species [PREVAL_SPECIES; default caribou for animal]      --base [PREVAL_OUT]      --outDir [PREVAL_PERCASE_OUT, required]
#   --runSQ [PREVAL_SQ_RUN]  --runPV [PREVAL_PV_RUN]  --sqRegime=FutureTainted  --pvRegime=FutureUnseenGrouped
#   --loss=lossCal|loss|both [PREVAL_PERCASE_LOSS; default lossCal for route (the birds' primary), loss for animal (the caribou's primary; raw cross-entropy)]
#   --levels=2,10,Inf  (subset of numberOfCovariates)   --testYears=2015:2024 or 2015,2016   --maxSplits=N (first N splits, for tests)   [PREVAL_LEVELS]
#   --exposure=window|sqTrain|pvTrain (how the years/strata count is taken: from caseIndex in PreVal's training years (default; same for both regimes) or from the train rows of
#                  one regime's manifest)      --cores=N [SLURM_CPUS_PER_TASK; forked, not on Windows]      --chunkSplits=40      --resume=1      --checkManifests=1
#   Resume: finished chunks are kept in <outDir>/work; rerunning the same command continues. If the arguments change, the script refuses and asks for an empty work folder.
# OUTPUT (in --outDir): percase_summary.txt (read this first), percase_cells_by_horizon.csv, percase_cell_contrasts.csv, percase_by_year.csv, percase_join_report.csv,
#   percase_manifest_check.csv, percase_run_info.txt.
# Memory: one pair of per-case files at a time per process (birds: a few hundred KB; caribou: a few MB) plus the case index and the small accumulator table; well under 2 GB per core.
if (nzchar(Sys.getenv("LOCALAPPDATA"))) .libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))
if (nzchar(Sys.getenv("SLURM_JOB_ID"))) {          # cluster job: the project library made by the one-time install (as runMe_birds.R does)
  libs <- Sys.glob(file.path(path.expand("~"), ".local", "share", "R", "PreVal", "packages", "*", "*")); if (length(libs)) .libPaths(c(libs, .libPaths()))
  birdLibs <- Sys.glob(file.path(path.expand("~"), ".local", "share", "R", "birdMonitor", "packages", "*", "*")); if (length(birdLibs)) .libPaths(c(.libPaths(), birdLibs))
}
suppressMessages(library(data.table))
t0 <- Sys.time()
logf <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n", sep = " ")

# ---- arguments --------------------------------------------------------------------------------------------------------------------------------------------------------------
known <- c("unit", "species", "base", "outDir", "runSQ", "runPV", "sqRegime", "pvRegime", "loss", "levels", "testYears", "maxSplits", "exposure", "cores", "chunkSplits", "resume", "checkManifests")
argv <- list()
for (a in commandArgs(trailingOnly = TRUE)) {
  if (!grepl("^--[A-Za-z]+=", a)) stop("Unrecognised argument '", a, "'; use --key=value. Known keys: ", paste(known, collapse = ", "))
  k <- sub("^--([A-Za-z]+)=.*$", "\\1", a); if (!k %in% known) stop("Unknown argument --", k, ". Known keys: ", paste(known, collapse = ", "))
  argv[[k]] <- sub("^--[A-Za-z]+=", "", a)
}
opt <- function(name, env = NULL, default = NULL) {
  v <- argv[[name]]
  if (is.null(v) && !is.null(env)) { e <- Sys.getenv(env, ""); if (nzchar(e)) v <- e }
  if (is.null(v)) default else v
}
unitKind <- opt("unit", "PREVAL_UNIT", "route"); if (!unitKind %in% c("route", "animal")) stop("--unit must be route or animal")
species <- opt("species", "PREVAL_SPECIES", if (unitKind == "animal") "caribou" else NULL); if (is.null(species)) stop("set --species (or PREVAL_SPECIES), e.g. Chloris_chloris")
base <- opt("base", "PREVAL_OUT", if (unitKind == "animal") "outputs/caribou_engine" else "outputs/birds_v4")
resolveRun <- function(x) if (grepl("^(/|[A-Za-z]:|~)", x)) x else file.path(base, species, x)
runSQ <- resolveRun(opt("runSQ", "PREVAL_SQ_RUN", if (unitKind == "animal") "run_main" else "run"))
runPV <- resolveRun(opt("runPV", "PREVAL_PV_RUN", if (unitKind == "animal") "run_main" else "run_routes"))
sqRegime <- opt("sqRegime", NULL, "FutureTainted"); pvRegime <- opt("pvRegime", NULL, "FutureUnseenGrouped")
outDir <- opt("outDir", "PREVAL_PERCASE_OUT"); if (is.null(outDir)) stop("set --outDir (or PREVAL_PERCASE_OUT): the only place this script writes to")
lossArg <- opt("loss", "PREVAL_PERCASE_LOSS", if (unitKind == "animal") "loss" else "lossCal")
lossTypes <- if (lossArg == "both") c("lossCal", "loss") else strsplit(lossArg, ",")[[1]]; if (!all(lossTypes %in% c("lossCal", "loss"))) stop("--loss must be lossCal, loss or both")
levelsArg <- opt("levels", "PREVAL_LEVELS", NULL); keepLevels <- if (is.null(levelsArg)) NULL else as.numeric(strsplit(levelsArg, ",")[[1]])
yearsArg <- opt("testYears", NULL, NULL)
keepYears <- if (is.null(yearsArg)) NULL else if (grepl(":", yearsArg)) { r <- as.integer(strsplit(yearsArg, ":")[[1]]); r[1]:r[2] } else as.integer(strsplit(yearsArg, ",")[[1]])
maxSplits <- as.integer(opt("maxSplits", NULL, NA))
exposure <- opt("exposure", NULL, "window"); if (!exposure %in% c("window", "sqTrain", "pvTrain")) stop("--exposure must be window, sqTrain or pvTrain")
cores <- as.integer(opt("cores", "SLURM_CPUS_PER_TASK", 1)); if (is.na(cores) || cores < 1 || .Platform$OS.type == "windows") cores <- 1L
chunkSplits <- as.integer(opt("chunkSplits", NULL, 40)); resume <- opt("resume", NULL, "1") %in% c("1", "TRUE", "true"); checkManifests <- opt("checkManifests", NULL, "1") %in% c("1", "TRUE", "true")
unitWord <- if (unitKind == "animal") "animal" else "route"; expWord <- if (unitKind == "animal") "training strata" else "training years"
minYears <- 3L                                      # interval and sign-flip p only where at least this many forecast years contribute

# ---- guard: read-only on the result folders ----------------------------------------------------------------------------------------------------------------------------------
nrm <- function(p) gsub("\\\\", "/", normalizePath(p, winslash = "/", mustWork = FALSE))
inside <- function(child, parent) { c1 <- nrm(child); p1 <- sub("/+$", "", nrm(parent)); c1 == p1 || startsWith(c1, paste0(p1, "/")) }
for (rd in unique(c(runSQ, runPV))) if (inside(outDir, rd)) stop("--outDir must not be inside a result folder (", rd, "): this script never writes there")
for (rd in unique(c(runSQ, runPV))) if (!file.exists(file.path(rd, "experimentPlan.csv")) || !dir.exists(file.path(rd, "testedModels"))) stop("not a run folder (no experimentPlan.csv / testedModels): ", rd)
dir.create(outDir, recursive = TRUE, showWarnings = FALSE); workDir <- file.path(outDir, "work"); dir.create(workDir, showWarnings = FALSE)

# the across-year test of the project, taken from the engine file without modifying it
srcDir <- NULL
for (cand in c(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), ".."), getwd(), file.path(getwd(), ".."))) {
  f <- file.path(cand, "modules", "preValNN", "R", "analyzeRegimes.R"); if (file.exists(f)) { srcDir <- f; break } }
if (is.null(srcDir)) stop("cannot find modules/preValNN/R/analyzeRegimes.R (run from the repository root)")
env <- new.env(); sys.source(srcDir, envir = env); yearTest <- env$.yearTest; if (!is.function(yearTest)) stop(".yearTest not found in ", srcDir)
logf("per-case decomposition | unit:", unitKind, "| status quo:", runSQ, "| new PreVal:", runPV, "| loss:", paste(lossTypes, collapse = ","), "| exposure:", exposure, "| cores:", cores)

# ---- plans and the join of the two regimes ----------------------------------------------------------------------------------------------------------------------------------------
planOf <- function(run, regime) {
  p <- fread(file.path(run, "experimentPlan.csv")); need <- c("splitId", "typeValidation", "numberOfCovariates", "replicate", "modelName", "windowStart", "historyEnd", "testYear", "horizon")
  if (length(miss <- setdiff(need, names(p)))) stop(run, "/experimentPlan.csv lacks columns: ", paste(miss, collapse = ", "))
  if (!"valYears" %in% names(p)) p[, valYears := 1L]
  p[typeValidation == regime, c(need, "valYears"), with = FALSE]
}
pSQ <- planOf(runSQ, sqRegime); pPV <- planOf(runPV, pvRegime)
if (!nrow(pSQ)) stop("no model of regime ", sqRegime, " in ", runSQ, "/experimentPlan.csv"); if (!nrow(pPV)) stop("no model of regime ", pvRegime, " in ", runPV, "/experimentPlan.csv")
key <- c("splitId", "numberOfCovariates", "replicate")
pairs <- merge(pSQ, pPV, by = key, suffixes = c(".sq", ".pv"))
report <- list(); addRep <- function(what, n) report[[length(report) + 1L]] <<- data.table(item = what, n = as.integer(n))
addRep("status-quo models in plan", nrow(pSQ)); addRep("new-PreVal models in plan", nrow(pPV)); addRep("matched on splitId + numberOfCovariates + replicate", nrow(pairs))
addRep("status-quo models without a new-PreVal partner", nrow(pSQ) - nrow(pairs)); addRep("new-PreVal models without a status-quo partner", nrow(pPV) - nrow(pairs))
same <- pairs[, testYear.sq == testYear.pv & horizon.sq == horizon.pv & windowStart.sq == windowStart.pv & historyEnd.sq == historyEnd.pv & valYears.sq == valYears.pv]
addRep("matched pairs whose window / test year / horizon / valYears differ (dropped)", sum(!same)); pairs <- pairs[same]
pairs[, `:=`(testYear = testYear.sq, horizon = horizon.sq, windowStart = windowStart.sq, historyEnd = historyEnd.sq, valYears = valYears.sq)]
if (!is.null(keepLevels)) pairs <- pairs[numberOfCovariates %in% keepLevels]; if (!is.null(keepYears)) pairs <- pairs[testYear %in% keepYears]
setorder(pairs, splitId, numberOfCovariates, replicate)
if (!is.na(maxSplits)) pairs <- pairs[splitId %in% head(unique(splitId), maxSplits)]
addRep("pairs after the --levels / --testYears / --maxSplits subset", nrow(pairs))
dSQ <- file.path(runSQ, "testedModels"); dPV <- file.path(runPV, "testedModels")
pairs[, `:=`(fileSQ = file.path(dSQ, paste0(modelName.sq, "_perCase.rds")), filePV = file.path(dPV, paste0(modelName.pv, "_perCase.rds")))]
okFiles <- file.exists(pairs$fileSQ) & file.exists(pairs$filePV)
addRep("pairs with a missing per-case file (dropped)", sum(!okFiles)); pairs <- pairs[okFiles]
if (!nrow(pairs)) stop("no pair of per-case files to analyse (see the counts above)")
splitIds <- unique(pairs$splitId); addRep("pairs analysed", nrow(pairs)); addRep("splits analysed", length(splitIds))

ciOf <- function(run) { x <- readRDS(file.path(run, "caseIndex.rds")); setDT(x); x[, .(row, unit = as.character(unit), year = as.integer(year))] }
ci <- ciOf(runSQ)
if (!identical(runSQ, runPV)) { ci2 <- ciOf(runPV); if (!identical(ci, ci2)) stop("the case indexes of the two run folders differ (rows, units or years): the cases cannot be matched") }
ciRowPos <- if (all(ci$row == seq_len(nrow(ci)))) NULL else match(seq_len(max(ci$row)), ci$row)
logf(sprintf("%d pairs in %d splits; case index %d rows, %d %ss", nrow(pairs), length(splitIds), nrow(ci), uniqueN(ci$unit), unitWord))
needManifests <- checkManifests || exposure != "window"
readMan <- function(run, sid, regime) { f <- file.path(run, "splits", paste0(sid, "__", regime, ".rds")); if (!file.exists(f)) return(NULL); m <- readRDS(f); setDT(m); m[, .(row, set)] }
stampNote <- tryCatch({ a <- file.path(dSQ, "settingsStamp.rds"); b <- file.path(dPV, "settingsStamp.rds")
  if (identical(runSQ, runPV)) "same run folder" else if (file.exists(a) && file.exists(b)) { if (isTRUE(all.equal(readRDS(a), readRDS(b)))) "settingsStamp.rds of the two run folders is identical" else "settingsStamp.rds of the two run folders DIFFERS (network settings were not the same)" } else "settingsStamp.rds missing in at least one run folder (settings cannot be compared)" },
  error = function(e) paste("settingsStamp comparison failed:", conditionMessage(e)))

# ---- per-split worker ------------------------------------------------------------------------------------------------------------------------------------------------------------
cutBin <- function(n) as.character(cut(n, c(-Inf, 3, 6, Inf), labels = c("<=3", "4-6", ">=7")))
tercile <- function(x) { r <- frank(x, ties.method = "average") / length(x); fifelse(r <= 1 / 3, "low", fifelse(r <= 2 / 3, "mid", "high")) }
aggCells <- function(dd) rbindlist(list(
  dd[, .(sumD = sum(d), sumSQ = sum(sq), sumPV = sum(pv), n = .N)][, `:=`(dimension = "all", level = "all")],
  dd[, .(sumD = sum(d), sumSQ = sum(sq), sumPV = sum(pv), n = .N), by = .(level = poolCat)][, dimension := "poolCat"],
  dd[, .(sumD = sum(d), sumSQ = sum(sq), sumPV = sum(pv), n = .N), by = .(level = yearsBin)][, dimension := "yearsBin"],
  dd[, .(sumD = sum(d), sumSQ = sum(sq), sumPV = sum(pv), n = .N), by = .(level = expTerc)][, dimension := "expTerc"],
  dd[, .(sumD = sum(d), sumSQ = sum(sq), sumPV = sum(pv), n = .N), by = .(level = paste(poolCat, yearsBin, sep = "|"))][, dimension := "poolCat:yearsBin"],
  dd[, .(sumD = sum(d), sumSQ = sum(sq), sumPV = sum(pv), n = .N), by = .(level = lastGap)][, dimension := "lastGap"],                                         # ADDED 2026-11-13 (recency cut)
  dd[, .(sumD = sum(d), sumSQ = sum(sq), sumPV = sum(pv), n = .N), by = .(level = paste(poolCat, lastGap, sep = "|"))][, dimension := "poolCat:lastGap"]), use.names = TRUE)   # ADDED 2026-11-13
readTest <- function(f, cols) {
  x <- readRDS(f); setDT(x); if ("set" %in% names(x)) x <- x[set == "test"]
  miss <- setdiff(cols, names(x)); if (length(miss)) return(list(x = NULL, why = paste("column(s) missing:", paste(miss, collapse = ", "))))
  list(x = x[, cols, with = FALSE], why = NA_character_)
}
processSplit <- function(sid) {
  pr <- pairs[splitId == sid]; s0 <- pr$windowStart[1]; e0 <- pr$historyEnd[1]; v0 <- pr$valYears[1]
  acc <- list(); iss <- list(); chk <- list(); addIss <- function(what, n = 1L) iss[[length(iss) + 1L]] <<- data.table(splitId = sid, item = what, n = as.integer(n))
  mSQ <- if (needManifests) readMan(runSQ, sid, sqRegime) else NULL; mPV <- if (needManifests) readMan(runPV, sid, pvRegime) else NULL
  if (needManifests && (is.null(mSQ) || is.null(mPV))) addIss("manifest missing (seenUnit check skipped)")
  sameTestRows <- NA; sameSQAcrossRuns <- NA
  if (!is.null(mSQ) && !is.null(mPV)) sameTestRows <- setequal(mSQ$row[mSQ$set == "test"], mPV$row[mPV$set == "test"])
  if (!is.null(mSQ) && !identical(runSQ, runPV)) { mX <- readMan(runPV, sid, sqRegime)
    if (!is.null(mX)) sameSQAcrossRuns <- isTRUE(all.equal(mSQ[order(set, row)], mX[order(set, row)], check.attributes = FALSE)) }
  posOf <- function(r) if (is.null(ciRowPos)) r else ciRowPos[r]
  trRows <- switch(exposure, window = NULL, sqTrain = mSQ$row[mSQ$set == "train"], pvTrain = mPV$row[mPV$set == "train"])
  if (exposure != "window" && is.null(trRows)) { addIss("exposure needs a manifest that is missing: split skipped"); return(list(acc = NULL, iss = rbindlist(iss), chk = NULL)) }
  cw <- if (is.null(trRows)) ci[year >= s0 & year <= e0 - v0] else ci[posOf(trRows)]
  et <- cw[, .(nYears = uniqueN(year), nStrata = .N), by = unit]
  lastObs <- ci[year >= s0 & year <= e0, .(lastYr = max(year)), by = unit]        # ADDED 2026-11-13 (recency cut): last year the unit is observed inside the window (up to e0, i.e. incl. the validation year(s))
  trUnitsSQ <- if (!is.null(mSQ)) unique(ci$unit[posOf(mSQ$row[mSQ$set == "train"])]); trUnitsPV <- if (!is.null(mPV)) unique(ci$unit[posOf(mPV$row[mPV$set == "train"])])
  cols <- c("row", "unit", "seenUnit", lossTypes)
  for (i in seq_len(nrow(pr))) {
    a <- readTest(pr$fileSQ[i], cols); b <- readTest(pr$filePV[i], cols)
    if (is.null(a$x) || is.null(b$x)) { addIss(paste("per-case file unusable:", if (is.null(a$x)) a$why else b$why)); next }
    sq <- a$x; pv <- b$x
    if (anyDuplicated(sq$row) || anyDuplicated(pv$row)) { addIss("duplicated case rows in a per-case file (pair dropped)"); next }
    ix <- match(sq$row, pv$row)
    if (nrow(sq) != nrow(pv) || anyNA(ix)) { addIss("the two models do not have the same test case rows (pair dropped)"); next }
    pv <- pv[ix]; if (!identical(as.character(sq$unit), as.character(pv$unit))) { addIss("same rows but different units (pair dropped)"); next }
    k <- match(sq$unit, et$unit); nY <- fifelse(is.na(k), 0L, et$nYears[k]); nS <- fifelse(is.na(k), 0L, et$nStrata[k])
    cs <- data.table(poolCat = fifelse(sq$seenUnit & pv$seenUnit, "inBoth", fifelse(sq$seenUnit, "sqOnly", fifelse(pv$seenUnit, "pvOnly", "inNeither"))),
                     yearsBin = cutBin(nY), expTerc = tercile(if (unitKind == "animal") nS else nY),
                     lastGap = { kL <- match(sq$unit, lastObs$unit); gp <- e0 - lastObs$lastYr[kL]                       # ADDED 2026-11-13 (recency cut): years between the unit's last observation in the window and the window end e0
                                 fifelse(is.na(gp), "none", fifelse(gp <= 0, "g0", fifelse(gp == 1, "g1", fifelse(gp <= 3, "g2-3", "g4+")))) })
    chk[[length(chk) + 1L]] <- data.table(nTest = nrow(sq), sqSeenVsManifest = if (!is.null(trUnitsSQ)) sum(sq$seenUnit != (sq$unit %in% trUnitsSQ)) else NA_integer_,
                                          pvSeenVsManifest = if (!is.null(trUnitsPV)) sum(pv$seenUnit != (pv$unit %in% trUnitsPV)) else NA_integer_,
                                          sqTestRowsInManifest = if (!is.null(mSQ)) setequal(sq$row, mSQ$row[mSQ$set == "test"]) else NA, pvTestRowsInManifest = if (!is.null(mPV)) setequal(pv$row, mPV$row[mPV$set == "test"]) else NA)
    for (L in lossTypes) {
      x <- sq[[L]]; y <- pv[[L]]; ok <- is.finite(x) & is.finite(y)
      if (!any(ok)) { addIss(paste("no finite", L, "values (pair skipped for this loss)")); next }
      if (any(!ok)) addIss(paste("cases with a non-finite", L, "dropped"), sum(!ok))
      dd <- cs[ok]; dd[, `:=`(sq = x[ok], pv = y[ok])]; dd[, d := pv - sq]
      r <- aggCells(dd); r[, `:=`(splitId = sid, numberOfCovariates = pr$numberOfCovariates[i], replicate = pr$replicate[i], testYear = pr$testYear[i], horizon = pr$horizon[i], lossType = L)]
      acc[[length(acc) + 1L]] <- r
    }
  }
  cs0 <- if (length(chk)) rbindlist(chk) else NULL
  chkS <- if (!is.null(cs0)) cs0[, .(splitId = sid, pairsChecked = .N, nTestCases = sum(nTest), sqSeenMismatchCases = sum(sqSeenVsManifest), pvSeenMismatchCases = sum(pvSeenVsManifest),
                                      pairsTestRowsNotInManifest = sum(!sqTestRowsInManifest | !pvTestRowsInManifest, na.rm = TRUE), manifestTestRowsSameInBothRuns = sameTestRows, statusQuoManifestSameInBothRuns = sameSQAcrossRuns)]
  list(acc = if (length(acc)) rbindlist(acc) else NULL, iss = if (length(iss)) rbindlist(iss) else NULL, chk = chkS)
}

# ---- run in chunks of splits (resumable) -----------------------------------------------------------------------------------------------------------------------------------------
argsFile <- file.path(workDir, "args.rds"); argsNow <- list(runSQ = nrm(runSQ), runPV = nrm(runPV), sqRegime = sqRegime, pvRegime = pvRegime, lossTypes = lossTypes, exposure = exposure, unit = unitKind,
                                                         chunkSplits = chunkSplits, splitIds = splitIds, nPairs = nrow(pairs), checkManifests = checkManifests, codeVersion = "recency1")   # codeVersion ADDED 2026-11-13: chunks made before the recency cut are refused
if (file.exists(argsFile)) { if (!identical(readRDS(argsFile), argsNow)) stop("--outDir/work holds chunks made with other arguments; use an empty --outDir or delete ", workDir) } else saveRDS(argsNow, argsFile)
chunks <- split(splitIds, ceiling(seq_along(splitIds) / chunkSplits)); nCh <- length(chunks); tStart <- Sys.time(); pairsDone <- 0L
for (j in seq_len(nCh)) {
  f <- file.path(workDir, sprintf("chunk_%04d.rds", j)); nP <- pairs[splitId %in% chunks[[j]], .N]
  if (resume && file.exists(f)) { logf(sprintf("chunk %d/%d: found, skipped", j, nCh)); pairsDone <- pairsDone + nP; next }
  res <- if (cores > 1L) parallel::mclapply(chunks[[j]], processSplit, mc.cores = cores, mc.preschedule = FALSE) else lapply(chunks[[j]], processSplit)
  bad <- vapply(res, function(r) inherits(r, "try-error") || !is.list(r), logical(1)); if (any(bad)) stop("worker error in chunk ", j, ": ", paste(unlist(res[bad]), collapse = " | "))
  saveRDS(list(acc = rbindlist(lapply(res, `[[`, "acc")), iss = rbindlist(lapply(res, `[[`, "iss")), chk = rbindlist(lapply(res, `[[`, "chk"))), f)
  pairsDone <- pairsDone + nP; el <- as.numeric(difftime(Sys.time(), tStart, units = "mins"))
  logf(sprintf("chunk %d/%d done: %d of %d pairs, %.1f min elapsed, about %.1f min left", j, nCh, pairsDone, nrow(pairs), el, el / pairsDone * (nrow(pairs) - pairsDone)))
}
parts <- lapply(seq_len(nCh), function(j) readRDS(file.path(workDir, sprintf("chunk_%04d.rds", j))))
acc <- rbindlist(lapply(parts, `[[`, "acc")); iss <- rbindlist(lapply(parts, `[[`, "iss")); chkAll <- rbindlist(lapply(parts, `[[`, "chk")); rm(parts)
if (!nrow(acc)) stop("no usable pair of per-case files (see percase_join_report.csv)")
logf(sprintf("accumulator: %d rows; aggregating", nrow(acc)))

# ---- aggregate: split -> forecast year -> across years ------------------------------------------------------------------------------------------------------------------------------
HB <- c("1", "2", "3-4", "5-9", "10+", "all")
acc[, hBin := as.character(cut(horizon, c(0, 1, 2, 4, 9, Inf), labels = HB[1:5]))]
acc[, isRef := numberOfCovariates == min(numberOfCovariates) & replicate == min(replicate), by = splitId]      # cases are counted once per split (every level and replicate reuses the same test cases)
A <- rbindlist(list(acc, copy(acc)[, hBin := "all"]))
byK <- c("lossType", "dimension", "level", "hBin", "testYear")
Yeq <- A[n > 0, .(v = mean(sumD / n), sq = mean(sumSQ / n), pv = mean(sumPV / n), nCases = sum(n[isRef]), nSplitRows = .N), by = byK][, weighting := "splitEqual"]
Ycw <- A[n > 0, .(v = sum(sumD) / sum(n), sq = sum(sumSQ) / sum(n), pv = sum(sumPV) / sum(n), nCases = sum(n[isRef]), nSplitRows = .N), by = byK][, weighting := "caseWeighted"]
Y <- rbindlist(list(Yeq, Ycw)); rm(A)
stat <- function(dt, by) { r <- dt[, yearTest(v), by = by]; r[, equivalent := NULL]; r[nYears < minYears, `:=`(lo = NA_real_, hi = NA_real_, pSignFlip = NA_real_)]; r }
cells <- stat(Y, c("weighting", "lossType", "dimension", "level", "hBin"))
ex <- Y[, .(meanSQ = mean(sq), meanPV = mean(pv), nCases = sum(nCases), meanCasesPerYear = mean(nCases)), by = .(weighting, lossType, dimension, level, hBin)]
cells <- merge(cells, ex, by = c("weighting", "lossType", "dimension", "level", "hBin"))
cells[, shareOfCases := nCases / sum(nCases), by = .(weighting, lossType, dimension, hBin)]
cells[, inferenceNote := fifelse(nYears < minYears, paste0("fewer than ", minYears, " forecast years: no interval or p"), "")]
lvlOrder <- c("all", "inBoth", "sqOnly", "pvOnly", "inNeither", "<=3", "4-6", ">=7", "low", "mid", "high", "g0", "g1", "g2-3", "g4+", "none")      # ADDED 2026-11-13: g0..none
dimOrder <- c("all", "poolCat", "yearsBin", "expTerc", "poolCat:yearsBin", "lastGap", "poolCat:lastGap")                                           # ADDED 2026-11-13: lastGap, poolCat:lastGap
cells[, `:=`(dimension = factor(dimension, dimOrder), hBin = factor(hBin, HB))]
cells <- cells[order(weighting, lossType, dimension, hBin, match(sub("\\|.*$", "", level), lvlOrder), match(sub("^.*\\|", "", level), lvlOrder))][, `:=`(dimension = as.character(dimension), hBin = as.character(hBin))]
setnames(cells, c("hBin", "estimate", "lo", "hi"), c("horizonBin", "meanDiff", "ciLo", "ciHi"))

# within-year differences between cells (the direct tests of M3 / M4)
cdef <- rbindlist(list(
  data.table(dimension = "poolCat", a = "sqOnly", b = "inBoth", label = "M4: sqOnly minus inBoth"),
  data.table(dimension = "yearsBin", a = ">=7", b = "<=3", label = "M3: >=7 years minus <=3 years"),
  data.table(dimension = "yearsBin", a = ">=7", b = "4-6", label = ">=7 years minus 4-6 years"),
  data.table(dimension = "yearsBin", a = "4-6", b = "<=3", label = "4-6 years minus <=3 years"),
  data.table(dimension = "expTerc", a = "high", b = "low", label = paste0("M3 (tercile): high minus low ", expWord)),
  data.table(dimension = "poolCat:yearsBin", a = paste0("sqOnly|", c("<=3", "4-6", ">=7")), b = paste0("inBoth|", c("<=3", "4-6", ">=7")), label = paste0("sqOnly minus inBoth within ", c("<=3", "4-6", ">=7"), " years")),
  data.table(dimension = "poolCat:yearsBin", a = c("inBoth|>=7", "sqOnly|>=7"), b = c("inBoth|<=3", "sqOnly|<=3"), label = c(">=7 minus <=3 years within inBoth", ">=7 minus <=3 years within sqOnly")),
  # ADDED 2026-11-13 (recency cut): R1/R2 predict POSITIVE at horizon 1 if the leftover deficit is recency (the status quo saw the unit's newest year, new PreVal kept it for validation)
  data.table(dimension = "lastGap", a = c("g0", "g0"), b = c("g2-3", "g4+"), label = c("R1: last seen in the final window year (g0) minus 2-3 years before (g2-3), all cases", "R1b: g0 minus g4+, all cases")),
  data.table(dimension = "poolCat:lastGap", a = c("inBoth|g0", "inBoth|g0", "inBoth|g1"), b = c("inBoth|g2-3", "inBoth|g4+", "inBoth|g2-3"),
             label = c("R2: within inBoth, g0 minus g2-3", "R2b: within inBoth, g0 minus g4+", "R2c: within inBoth, g1 minus g2-3"))))
cc <- rbindlist(lapply(seq_len(nrow(cdef)), function(i) {
  w <- dcast(Y[dimension == cdef$dimension[i] & level %in% c(cdef$a[i], cdef$b[i])], weighting + lossType + hBin + testYear ~ level, value.var = "v")
  if (!all(c(cdef$a[i], cdef$b[i]) %in% names(w))) return(NULL)
  w[, v := get(cdef$a[i]) - get(cdef$b[i])]; w <- w[is.finite(v)]; if (!nrow(w)) return(NULL)
  r <- stat(w, c("weighting", "lossType", "hBin")); r[, `:=`(contrast = cdef$label[i], dimension = cdef$dimension[i], cellA = cdef$a[i], cellB = cdef$b[i])]
}))
if (nrow(cc)) { cc[, hBin := factor(hBin, HB)]; setorder(cc, weighting, lossType, contrast, hBin); cc[, hBin := as.character(hBin)]
  setnames(cc, c("hBin", "estimate", "lo", "hi"), c("horizonBin", "meanDiffInDiff", "ciLo", "ciHi")); setcolorder(cc, c("weighting", "lossType", "contrast", "dimension", "cellA", "cellB", "horizonBin")) }
byYear <- Y[weighting == "splitEqual" & hBin != "all", .(weighting, lossType, dimension, level, horizonBin = hBin, testYear, meanDiff = v, meanSQ = sq, meanPV = pv, nCases, nSplitRows)]
setorder(byYear, lossType, dimension, level, horizonBin, testYear)

# ---- write -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fwrite(cells, file.path(outDir, "percase_cells_by_horizon.csv")); fwrite(cc, file.path(outDir, "percase_cell_contrasts.csv")); fwrite(byYear, file.path(outDir, "percase_by_year.csv"))
rep2 <- rbindlist(list(rbindlist(report)[, source := "design"], if (nrow(iss)) iss[, .(n = sum(n), splits = uniqueN(splitId)), by = item][, source := "per-pair problems"]), fill = TRUE)
fwrite(rep2, file.path(outDir, "percase_join_report.csv")); fwrite(chkAll, file.path(outDir, "percase_manifest_check.csv"))
writeLines(c(paste("started:", format(t0, "%Y-%m-%d %H:%M:%S")), paste("finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), paste("script: birds/post_memory_percase.R"),
             paste("host:", Sys.info()[["nodename"]]), paste("R:", R.version.string), paste("data.table:", as.character(packageVersion("data.table"))),
             paste("status-quo run folder:", runSQ), paste("new-PreVal run folder:", runPV), paste("regimes:", sqRegime, "(status quo) vs", pvRegime, "(new PreVal)"), paste("loss columns:", paste(lossTypes, collapse = ", ")),
             paste("exposure definition:", exposure), paste("levels subset:", if (is.null(keepLevels)) "all" else paste(keepLevels, collapse = ",")), paste("test-year subset:", if (is.null(keepYears)) "all" else paste(range(keepYears), collapse = "-")),
             paste("cores:", cores), paste("chunkSplits:", chunkSplits), paste("settings of the two runs:", stampNote), paste("analyzeRegimes.R used for .yearTest:", normalizePath(srcDir, winslash = "/"))), file.path(outDir, "percase_run_info.txt"))

# ---- plain-text summary -----------------------------------------------------------------------------------------------------------------------------------------------------------------
fmt <- function(x, d = 3) ifelse(is.na(x), "   NA", formatC(x, format = "f", digits = d, flag = " "))
line <- function(r) sprintf("  %-14s h=%-4s share %5.1f%%  cases %7d  d %s  [%s, %s]  p %s  years %2d (%d worse / %d better)", r$level, r$horizonBin, 100 * r$shareOfCases, as.integer(r$nCases), fmt(r$meanDiff),
                            fmt(r$ciLo), fmt(r$ciHi), ifelse(is.na(r$pSignFlip), "   NA", formatC(r$pSignFlip, format = "f", digits = 4)), r$nYears, r$nPositive, r$nNegative)
S <- c("PER-CASE DECOMPOSITION OF NEW PREVAL MINUS STATUS QUO", strrep("=", 60),
       sprintf("d = loss(new PreVal, %s) - loss(status quo, %s), paired per test case. POSITIVE = new PreVal worse; negative = new PreVal better.", pvRegime, sqRegime),
       sprintf("Units: %ss. Cases are averaged within split, then within forecast year (every split x level x replicate row equal); the forecast year is the unit of inference.", unitWord),
       sprintf("Interval = t interval over forecast years, p = exact sign-flip over forecast years (.yearTest of analyzeRegimes.R); both only where >= %d forecast years contribute.", minYears),
       "share = this cell's share of the (split-summed) test cases of the horizon bin within its split dimension; cases = test cases summed over splits, counted once per split (not per covariate level).",
       "", "SETUP", sprintf("  status quo run folder : %s", runSQ), sprintf("  new PreVal run folder : %s", runPV),
       sprintf("  pairs analysed        : %d in %d splits (see percase_join_report.csv for what was dropped)", nrow(pairs), length(splitIds)),
       sprintf("  covariate levels      : %s", paste(sort(unique(pairs$numberOfCovariates)), collapse = ", ")), sprintf("  forecast years        : %s", paste(range(pairs$testYear), collapse = " to ")),
       sprintf("  loss column(s)        : %s", paste(lossTypes, collapse = ", ")),
       sprintf("  pool membership       : seenUnit flag of each model's per-case file (unit has >= 1 training row of that model); checked against the manifests (below)"),
       sprintf("  yearsBin rule         : number of distinct years of the unit in %s; bins <=3 | 4-6 | >=7 (absent unit = 0, in <=3); expTerc = within-split terciles of the %s",
               if (exposure == "window") "the window's training years s..e-valYears (caseIndex.rds)" else if (exposure == "sqTrain") "the status quo's training rows (manifest)" else "new PreVal's training rows (manifest)", expWord),
       sprintf("  settings of the runs  : %s", stampNote))
if (nrow(chkAll)) {
  S <- c(S, sprintf("  manifest check        : %d pairs checked; seenUnit disagrees with the manifest for %d (status quo) and %d (new PreVal) test cases; %d pairs have test rows not equal to the manifest's test set; manifests share the test rows in %s of %d splits",
                    sum(chkAll$pairsChecked), sum(chkAll$sqSeenMismatchCases, na.rm = TRUE), sum(chkAll$pvSeenMismatchCases, na.rm = TRUE), sum(chkAll$pairsTestRowsNotInManifest, na.rm = TRUE),
                    sum(chkAll$manifestTestRowsSameInBothRuns, na.rm = TRUE), nrow(chkAll)))
  if (any(!is.na(chkAll$statusQuoManifestSameInBothRuns))) S <- c(S, sprintf("                          the status quo's manifest is identical in both run folders in %d of %d splits that have it in both", sum(chkAll$statusQuoManifestSameInBothRuns, na.rm = TRUE), sum(!is.na(chkAll$statusQuoManifestSameInBothRuns))))
} else S <- c(S, "  manifest check        : not done (--checkManifests=0 or manifests missing)")
if (nrow(iss)) { S <- c(S, "  problems while reading:"); S <- c(S, sprintf("    %s: %d", iss[, .(n = sum(n)), by = item]$item, iss[, .(n = sum(n)), by = item]$n)) }
mainCells <- cells[weighting == "splitEqual"]
for (L in unique(mainCells$lossType)) {
  S <- c(S, "", strrep("-", 60), sprintf("LOSS: %s   (weighting: every split row equal within a forecast year; case-weighted version in percase_cells_by_horizon.csv)", L))
  sec <- function(title, dim, pick = NULL) { r <- mainCells[lossType == L & dimension == dim]; if (!is.null(pick)) r <- r[level %in% pick]
    if (!nrow(r)) return(NULL); c("", title, unlist(lapply(HB, function(h) { x <- r[horizonBin == h]; if (nrow(x)) c(sprintf(" horizon %s", h), vapply(seq_len(nrow(x)), function(i) line(x[i]), character(1))) }))) }
  S <- c(S, sec("A. ALL CASES (anchor: compare with the headline by horizon in analyzeRegimes output; the headline is the unweighted-split mean per forecast year)", "all"),
         sec("B. POOL MEMBERSHIP (M4: sqOnly = trained by the status quo, set aside by new PreVal)", "poolCat"),
         sec("C. TRAINING YEARS OF THE UNIT IN THE WINDOW (M3)", "yearsBin"),
         sec(sprintf("D. TERCILES OF %s (within split)", toupper(expWord)), "expTerc"),
         sec("F. RECENCY (ADDED 2026-11-13): years between the unit's last observation in the window and the window end (g0 = observed in the final window year, which new PreVal keeps out of training for validation; none = never observed in the window)", "lastGap"),
         sec("G. RECENCY inside inBoth (both models were trained on the unit)", "poolCat:lastGap", pick = paste0("inBoth|", c("g0", "g1", "g2-3", "g4+"))))
  cx <- cc[lossType == L & weighting == "splitEqual"]
  if (nrow(cx)) { S <- c(S, "", "E. WITHIN-YEAR DIFFERENCES BETWEEN CELLS (M3 and M4 predict POSITIVE at horizon 1: the deficit is larger in the first-named cell)")
    for (cn in unique(cx$contrast)) { S <- c(S, paste0(" ", cn)); x <- cx[contrast == cn]
      S <- c(S, vapply(seq_len(nrow(x)), function(i) sprintf("   h=%-4s diff %s  [%s, %s]  p %s  years %2d (%d positive / %d negative)", x$horizonBin[i], fmt(x$meanDiffInDiff[i]), fmt(x$ciLo[i]), fmt(x$ciHi[i]),
                                                              ifelse(is.na(x$pSignFlip[i]), "   NA", formatC(x$pSignFlip[i], format = "f", digits = 4)), x$nYears[i], x$nPositive[i], x$nNegative[i]), character(1))) } }
}
S <- c(S, "", strrep("-", 60), "Not in this file: the cross dimension poolCat:yearsBin (percase_cells_by_horizon.csv), year-by-year values (percase_by_year.csv), the case-weighted weighting, the other loss column.")
writeLines(S, file.path(outDir, "percase_summary.txt"))
unlink(workDir, recursive = TRUE)
logf(sprintf("finished in %.1f min; wrote %s", as.numeric(difftime(Sys.time(), t0, units = "mins")), outDir))
