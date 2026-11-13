# Smoke test: can the caribou engine (caribou/runMe_caribou.R, preset main, dataLayout "strata") read and train on a SIMULATED movement world? No engine code is changed and no adapter is needed:
# writeMovementWorld() writes final_caribou.rds, featureTable.csv and networkSettings.rds exactly where the engine expects them, and the engine is called with its own environment variables
# (PREVAL_OUT, PREVAL_FINAL, PREVAL_START/END, PREVAL_HORIZONS, PREVAL_COMPLEXITY, PREVAL_EPOCHS ...). The prep stage is skipped (it reads the real prepared table, which must not be used here).
# Nothing in the repository is written: everything goes to the output folder given below. The result is a plumbing check ONLY; the tiny network and the 3 training epochs say nothing about validation.
#   Rscript simulation/movement/smoke_engine_movement.R <outDir> [preset = main] [scale = small|tiny] [memoryOff]
if (nzchar(Sys.getenv("LOCALAPPDATA"))) .libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))
suppressMessages(library(data.table))
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]))); repoRoot <- normalizePath(file.path(here, "..", ".."))
source(file.path(here, "simulate_movement.R"))
args <- commandArgs(trailingOnly = TRUE); outDir <- if (length(args) >= 1) args[1] else file.path(tempdir(), "smoke_movement"); preset <- if (length(args) >= 2) args[2] else "main"; scale <- if (length(args) >= 3) args[3] else "small"
dir.create(outDir, recursive = TRUE, showWarnings = FALSE); outDir <- normalizePath(outDir)
sz <- switch(scale, tiny = list(years = 2013:2015, animalsPerYear = c(24L, 24L, 28L), newPerYear = c(24L, 4L, 8L), stepsPerAnimalYear = 150), small = list(years = 2013:2016, animalsPerYear = c(30L, 30L, 36L, 36L), newPerYear = c(30L, 6L, 10L, 4L), stepsPerAnimalYear = 250))
t0 <- Sys.time(); w <- simulateMovement(utils::modifyList(list(seed = 21L, stepsSdLog = 0.3), sz)); cat(sprintf("world: %d strata, %d animals, years %d-%d, simulated in %.0f s\n", nrow(w$table) / 11, uniqueN(w$table$id), min(w$table$year), max(w$table$year), as.numeric(difftime(Sys.time(), t0, units = "secs"))))
worldDir <- file.path(outDir, "caribou"); writeMovementWorld(w, worldDir, repoRoot = repoRoot); cat("written to", worldDir, "\n")
rscript <- file.path(R.home("bin"), "Rscript.exe"); if (!file.exists(rscript)) rscript <- file.path(R.home("bin"), "Rscript")
envv <- c(PREVAL_PRESET = preset, PREVAL_OUT = outDir, PREVAL_FINAL = worldDir, PREVAL_START = as.character(min(sz$years)), PREVAL_END = as.character(max(sz$years)), PREVAL_HORIZONS = "1", PREVAL_COMPLEXITY = "2,5", PREVAL_EPOCHS = "3", PREVAL_PATIENCE = "2", PREVAL_MINTRAIN = "1")
if (nzchar(Sys.getenv("PREVAL_EXTRA"))) { kv <- strsplit(strsplit(Sys.getenv("PREVAL_EXTRA"), ";")[[1]], "="); envv <- c(envv, setNames(vapply(kv, `[`, "", 2), vapply(kv, `[`, "", 1))) }
# MEMORY OFF (animal embedding off, embDim = 0): the engine supports it (module parameter hyperOverride "embDim=0"), but caribou/runMe_caribou.R does not pass that parameter on. The original script is NOT edited:
# a COPY with one added argument (hyperOverride = envChr("PREVAL_HYPER", "")) is written next to the outputs and run instead. A permanent switch in runMe_caribou.R would be a one-line change for Tati / the EVE agent.
runScript <- "caribou/runMe_caribou.R"
if (length(args) >= 4 && args[4] == "memoryOff") {
  src <- readLines(file.path(repoRoot, "caribou", "runMe_caribou.R")); key <- 'rankingFile = "featureTable.csv", runTag = runTag,'; stopifnot(any(grepl(key, src, fixed = TRUE)))
  src <- sub(key, paste0('rankingFile = "featureTable.csv", hyperOverride = envChr("PREVAL_HYPER", ""), runTag = runTag,'), src, fixed = TRUE)
  runScript <- file.path(outDir, "runMe_caribou_memoryOff_COPY.R"); writeLines(src, runScript); envv <- c(envv, PREVAL_HYPER = "embDim=0", PREVAL_TAG = "memoryOff"); preset <- "memoryOff"
}
old <- setwd(repoRoot); on.exit(setwd(old), add = TRUE); do.call(Sys.setenv, as.list(envv)); if (preset == "memoryOff") Sys.setenv(PREVAL_PRESET = "main")
for (st in c("design", "rank", "train", "analyze")) {
  Sys.setenv(PREVAL_STAGE = st); t1 <- Sys.time(); lg <- file.path(outDir, sprintf("log_%s.txt", st))
  status <- system2(rscript, c(shQuote(runScript)), stdout = lg, stderr = lg); cat(sprintf("stage %-8s exit status %d, %.0f s  (log: %s)\n", st, status, as.numeric(difftime(Sys.time(), t1, units = "secs")), lg))
  if (status != 0) { cat(tail(readLines(lg, warn = FALSE), 25), sep = "\n"); quit(status = 1L) }
}
runDir <- file.path(worldDir, paste0("run_", preset)); cat("\nengine output folder:", runDir, "\n"); print(list.files(runDir)); cat("models trained:", length(list.files(file.path(runDir, "testedModels"), pattern = "_finalDT\\.csv$")), "\n")
pl <- fread(file.path(runDir, "experimentPlan.csv")); cat(sprintf("plan: %d models, splits %d, regimes %s, levels %s, test years %s\n", nrow(pl), uniqueN(pl$splitId), paste(unique(pl$typeValidation), collapse = ", "), paste(sort(unique(pl$numberOfCovariates)), collapse = ", "), paste(sort(unique(pl$testYear)), collapse = ", ")))
ap <- file.path(runDir, "analysis", "contrasts_pooled.csv"); if (file.exists(ap)) { cat("analysis/contrasts_pooled.csv (plumbing only, not interpreted):\n"); print(fread(ap)) }
