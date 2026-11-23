# World runner of the controlled simulation (added 2026-11-05). Generates simulated birds-type worlds and runs the UNCHANGED birds engine (birds/runMe_birds.R) on them, the same way locally and on EVE.
#
# A STAGE (smoke | stage0 | stage1a) is a table of WORLDS (generator parameters) and a table of UNITS (one engine run = world x ladder order x design), plus the engine settings (PREVAL_* values).
# The three tables are written to simulation/manifests/sim_<stage>_{worlds,units,engine}.csv by `manifest`; EVE and local runs read those files, so what runs is what is checked in.
#   stage0   realism anchor (section 3a): 5 worlds without flips (phi = 0), natural ladder, same-places and new-places designs, regimes PreVal (FutureUnseenGrouped), status quo, spatial CV, PreVal + spatial.
#   stage0d  (added 2026-11-14) anchor with route drift: stage-0 worlds 1 and 2 (seeds 101, 102) with route drift sd 0.2 / 0.4 (rho 0.82; sd 0.6 dropped 2026-11-14, outside the marginal anchor), same and new places, 4 regimes: 4 worlds, 8 units, 25,440 models.
#   stage0r  (added 2026-11-14) fade rate of the route drift: worlds 1 and 2 at drift sd 0.4 with rho 0.5 (fast) and 0.95 (slow), same and new places, 4 regimes: 4 worlds, 8 units, 25,440 models.
#   stage0n  (added 2026-11-15) non-stationary route level + less count noise + persistent surveys (plan section 9g): worlds 1 and 2 (seeds 101, 102) with all three changes, plus two ablations of world 1 (walk only; less noise and persistent surveys without the walk), same and new places, 4 regimes: 4 worlds, 8 units, 25,440 models.
#   stage0c  (added 2026-11-16) route level partly explained by the covariates (plan sections 9i, 9j): worlds 1 and 2 (seeds 101, 102) with route field from covariates 0.5 + random-walk route level 0.23 + count noise 0.7 + etaSd 0.4, plus two ablations of world 1 (without the covariate part; without the walk), same and new places, 4 regimes: 4 worlds, 8 units, 25,440 models.
#   stage0s  (added 2026-11-16, night run step 1, plan section 9l) the stage-0c worlds plus persistent surveys (surveyRho 0.70): A seeds 101 and 102, B (covariate share 0.75) seed 101, same and new places, 4 regimes: 3 worlds, 6 units, 19,080 models.
#   stage0t  (added 2026-11-17, night run step 2, plan section 9n) stage-0s settings with the covariate share of the route level 0.90, seeds 101 and 102, same and new places, 4 regimes: 2 worlds, 4 units, 12,720 models.
#   stage0u  (added 2026-11-17, night run step 3, plan section 9p) lever probes: etaSd 0.30 (seeds 101, 102), covariate share 0.99 with etaSd 0.36, survey persistence 0.80, all at walk 0.23 and noise 0.7; same and new places, 4 regimes: 4 worlds, 8 units, 25,440 models.
#   stage0v  (added 2026-11-17, night run step 4, plan section 9r) the real horizon bins: engine horizons 1,2,4,6,9; worlds eta30 (seeds 101, 102) and rho80 (seed 101); same and new places, 4 regimes: 3 worlds, 6 units, 5 horizons.
#   stage1v  (added 2026-11-17, plan section 9t) FIRST FLIP STAGE on the anchor (s0v_eta30 settings): phi 0 / 0.85 / 1 x flip share 0.30 / 0.60 x seeds 101, 102, ladders natural and flippersFirst, same places, PreVal vs status quo, horizons 1,2,4,6,9: 12 worlds, 24 units.
#   stage1w  (added 2026-11-23, plan section 9v) STRONG-FLIP STAGE: the stage-1v anchor with the flip contribution set directly (flipSd 0.35 / 0.50 on the log scale): phi 0 / 0.85 / 1 x flipSd x seeds 101, 102, ladders natural and flippersFirst, same places, PreVal vs status quo, horizons 1,2,4,6,9; 12 worlds, 24 units
#   stage1a  F1 x F2 x F3: phi 0 / 0.85 / 1 x flip share 0.10 / 0.30 x ladder order natural / flippersFirst / flippersLast / random, 5 replicate worlds per (phi, share), same-places design, PreVal vs status quo.
#   stage1d  (Tweety, added 2026-11-05) route-level drift: drift off / moderate (AR(1), sd 0.4, 1/e in 5 years) x phi 0 / 0.85 x flip share 0.30, 5 replicates, natural ladder, same places, PreVal vs status quo, horizons 1, 3, 5, 10.
#   swap     (Tweety, added 2026-11-05) causal swap test: phi 0 / 0.85 / 1 at flip share 0.30, 5 replicates, ladders oracle and oracleSwap (flippers and stable covariates exchanged at the same ladder positions), same places.
#   smoke    one tiny world, a few models, a few epochs: only to see that the chain runs.
#
# Usage (from the repo root; SIM_ROOT = folder of the stage, default /data/birds/PreVal/simulation/<stage> on EVE, outputs/simulation/<stage> elsewhere):
#   Rscript simulation/run_worlds.R manifest <stage>                       write the three manifest CSVs and print the sizes
#   Rscript simulation/run_worlds.R run <stage> <step> <taskId> [nSlices]   one SLURM array task (see below); on EVE called by eve/eve_sim_task.sbatch
#   Rscript simulation/run_worlds.R generate <stage> <worldIndex>           only the world and its ladder files
#   Rscript simulation/run_worlds.R local <stage> [nSlices]                 all steps one after the other in this process (small stages, local tests)
# STEPS (array index -> work):  prep      taskId = world: generate the world, ladder files, engine stage `prep` (network pilot, global ranking, block size, design of the first run), `design` of the other runs
#                               rank      taskId = (unit, slice) over the units that rank their own covariates (natural ladder, same-places design); slice = share of the windows
#                               rankcopy  taskId = unit over the units that reuse those rankings (natural ladder, new-places design: PREVAL_RANKFROM = same-places run)
#                               train     taskId = (unit, slice) over all units; slice = round-robin share of the models, largest first (engine PREVAL_SLICE_ID / PREVAL_SLICE_COUNT)
#                               analyze   taskId = unit
# Environment variables already set (PREVAL_*, SIM_ROOT) win over the manifest's engine settings, so a test can shorten anything without editing a file.
if (nzchar(Sys.getenv("LOCALAPPDATA"))) .libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))
if (nzchar(Sys.getenv("SLURM_JOB_ID"))) {          # cluster job: the project library made by the one-time install (as birds/runMe_birds.R does; this wrapper only needs data.table)
  libs <- Sys.glob(file.path(path.expand("~"), ".local", "share", "R", "PreVal", "packages", "*", "*")); if (length(libs)) .libPaths(c(libs, .libPaths()))
  birdLibs <- Sys.glob(file.path(path.expand("~"), ".local", "share", "R", "birdMonitor", "packages", "*", "*")); if (length(birdLibs)) .libPaths(c(.libPaths(), birdLibs))
}
suppressMessages(library(data.table))
.args <- commandArgs(FALSE); .fa <- sub("--file=", "", grep("--file=", .args, value = TRUE)[1])
.here <- dirname(normalizePath(.fa, winslash = "/")); .repo <- normalizePath(file.path(.here, ".."), winslash = "/")
source(file.path(.here, "simulate_world.R"))
manifestDir <- file.path(.here, "manifests")
LADDERS <- c("natural", "flippersFirst", "flippersLast", "random"); FILE_LADDERS <- c("flippersFirst", "flippersLast", "random", "oracle", "oracleSwap")      # ladder files written for every world (the last two: the swap test, CHANGED 2026-11-05)      # natural = the engine's own per-window ranking; the other three are written by ladderRanking()

# ---------------------------------------------------------------------------------------------------------------------------------- stage definitions
flipShares <- function(fs) { rest <- 1 - 0.0714 - fs; c(route = 0.20, stable = 0.75 * rest, drift = 0.25 * rest, flip = fs, year = 0.0714) }   # shares of the NON-route variance (the route share is ignored in the Beta mode); fs = 0.357 reproduces the default
FULL_ENGINE <- c(PREVAL_START = 2005, PREVAL_END = 2024, PREVAL_MINTRAIN = 5, PREVAL_VALYEARS = 1, PREVAL_HORIZONS = "1,5", PREVAL_COMPLEXITY = "2,5,10,20,Inf", PREVAL_PILOTMODE = "forward", PREVAL_LASTPILOT = 2011,
                 PREVAL_EPOCHS = 100, PREVAL_PILOT_EPOCHS = 100, PREVAL_PATIENCE = 15, PREVAL_REPLICATES = 1, PREVAL_THREADS = 1, PREVAL_DATASET = "birds")
makeUnits <- function(worlds, ladders, designs, regimesSame, regimesNew) {
  u <- CJ(world = worlds$world, ladder = ladders, design = designs, sorted = FALSE); u[, `:=`(
    arm = ifelse(design == "same", "main", "spatialTest"), tag = design, ladderTag = ifelse(ladder == "natural", "", ladder), rankPerWindow = as.integer(ladder == "natural"),
    rankFrom = ifelse(ladder == "natural" & design == "new", "same", ""), routeHeldOut = 1L, planRegimes = ifelse(design == "same", regimesSame, regimesNew))]
  u[, unit := seq_len(.N)]; setcolorder(u, "unit"); u[]
}
REG4 <- "FutureUnseenGrouped,FutureTainted,FutureTaintedSpatial,FutureUnseenSpatial"       # PreVal (unseen routes), status quo, spatial CV, PreVal + spatial (amendment 35 of the birds plan: "PreVal" = FutureUnseenGrouped)
REG2 <- "FutureUnseenGrouped,FutureTainted"                                                    # PreVal vs status quo (stage 1a)
simStage <- function(stage) {
  stopifnot(stage %in% c("smoke", "stage0", "stage0d", "stage0r", "stage0n", "stage0c", "stage0s", "stage0t", "stage0u", "stage0v", "stage1v", "stage1w", "stage1a", "stage1d", "swap"))
  base <- list(nRoutes = 2139L, nStable = 12L, nDrift = 6L, nFlip = 12L, nNoise = 30L, routeDrift = 0)
  if (stage == "stage1v") {      # ADDED 2026-11-17 (ANALYSIS_PLAN_SIMULATION.md section 9t): FIRST FLIP STAGE on the anchor (s0v_eta30 settings: covariate share 0.90, walk 0.23, noise 0.7, etaSd 0.30, surveyRho 0.70, no drift), horizons 1,2,4,6,9
    W <- CJ(phi = c(0, 0.85, 1), fs = c(0.30, 0.60), w = 1:2); W[, `:=`(world = sprintf("s1v_phi%.2f_fs%.2f_w%d", phi, fs, w), seed = 100L + w, flipStrength = phi, flipShare = fs)]; W[, c("phi", "fs", "w") := NULL]
    for (n in setdiff(names(base), "routeDrift")) W[[n]] <- base[[n]]; W[["routeDrift"]] <- 0
    W[, `:=`(routeFromCov = 0.90, routeWalkSd = 0.23, noiseScale = 0.7, etaSd = 0.30, surveyRho = 0.70)]; setcolorder(W, c("world", "seed", "flipStrength", "flipShare"))
    U <- makeUnits(W, c("natural", "flippersFirst"), "same", REG2, REG2); E <- replace(FULL_ENGINE, "PREVAL_HORIZONS", "1,2,4,6,9")
    return(list(worlds = W[], units = U[], engine = data.table(name = names(E), value = as.character(E))))
  }
  if (stage == "stage1w") {      # ADDED 2026-11-23 (ANALYSIS_PLAN_SIMULATION.md section 9v): STRONG-FLIP STAGE on the same anchor as stage 1v; only the size of the flip contribution changes (flipSd 0.35 / 0.50 on the log scale, set directly; stage 1v had 0.16 / 0.23)
    W <- CJ(phi = c(0, 0.85, 1), fsd = c(0.35, 0.50), w = 1:2); W[, `:=`(world = sprintf("s1w_phi%.2f_sd%.2f_w%d", phi, fsd, w), seed = 100L + w, flipStrength = phi, flipShare = 0.30, flipSd = fsd)]; W[, c("phi", "fsd", "w") := NULL]
    for (n in setdiff(names(base), "routeDrift")) W[[n]] <- base[[n]]; W[["routeDrift"]] <- 0
    W[, `:=`(routeFromCov = 0.90, routeWalkSd = 0.23, noiseScale = 0.7, etaSd = 0.30, surveyRho = 0.70)]; setcolorder(W, c("world", "seed", "flipStrength", "flipShare", "flipSd"))
    U <- makeUnits(W, c("natural", "flippersFirst"), "same", REG2, REG2); E <- replace(FULL_ENGINE, "PREVAL_HORIZONS", "1,2,4,6,9")
    return(list(worlds = W[], units = U[], engine = data.table(name = names(E), value = as.character(E))))
  }
  if (stage == "stage0v") {      # ADDED 2026-11-17 night, step 4 (ANALYSIS_PLAN_SIMULATION.md section 9r): the real horizon bins (1, 2, 3-4, 5-9) via engine horizons 1,2,4,6,9; worlds eta30 (seeds 101, 102: covariate share 0.90, etaSd 0.30, surveys 0.70) and rho80 (stage-0u world, paired)
    W <- data.table(world = c("s0v_eta30_w1", "s0v_eta30_w2", "s0v_rho80_w1"), seed = c(101L, 102L, 101L), flipStrength = 0, flipShare = 0.3571,
                    routeFromCov = 0.90, routeWalkSd = 0.23, noiseScale = 0.7, etaSd = c(0.30, 0.30, 0.40), surveyRho = c(0.70, 0.70, 0.80))
    for (n in setdiff(names(base), "routeDrift")) W[[n]] <- base[[n]]; W[["routeDrift"]] <- 0; setcolorder(W, c("world", "seed", "flipStrength", "flipShare"))
    U <- makeUnits(W, "natural", c("same", "new"), REG4, REG4); E <- replace(FULL_ENGINE, "PREVAL_HORIZONS", "1,2,4,6,9")
  } else if (stage == "stage0u") {      # ADDED 2026-11-17 night, step 3 (ANALYSIS_PLAN_SIMULATION.md section 9p): lever probes around the stage-0t worlds (covariate share 0.90): weaker covariate effects (etaSd 0.30, seeds 101 and 102), covariate share 0.99 with etaSd 0.36, survey persistence 0.80
    W <- data.table(world = c("s0u_eta30_w1", "s0u_eta30_w2", "s0u_g99_w1", "s0u_rho80_w1"), seed = c(101L, 102L, 101L, 101L), flipStrength = 0, flipShare = 0.3571,
                    routeFromCov = c(0.90, 0.90, 0.99, 0.90), routeWalkSd = 0.23, noiseScale = 0.7, etaSd = c(0.30, 0.30, 0.36, 0.40), surveyRho = c(0.70, 0.70, 0.70, 0.80))
    for (n in setdiff(names(base), "routeDrift")) W[[n]] <- base[[n]]; W[["routeDrift"]] <- 0; setcolorder(W, c("world", "seed", "flipStrength", "flipShare"))
    U <- makeUnits(W, "natural", c("same", "new"), REG4, REG4); E <- FULL_ENGINE
  } else if (stage == "stage0t") {      # ADDED 2026-11-17 night, step 2 (ANALYSIS_PLAN_SIMULATION.md section 9n): stage-0s settings with the covariate share of the route level raised to 0.90 (stage 0s: A 0.5, B 0.75); seeds 101, 102
    W <- data.table(world = c("s0t_c_w1", "s0t_c_w2"), seed = c(101L, 102L), flipStrength = 0, flipShare = 0.3571,
                    routeFromCov = 0.90, routeWalkSd = 0.23, noiseScale = 0.7, etaSd = 0.4, surveyRho = 0.70)
    for (n in setdiff(names(base), "routeDrift")) W[[n]] <- base[[n]]; W[["routeDrift"]] <- 0; setcolorder(W, c("world", "seed", "flipStrength", "flipShare"))
    U <- makeUnits(W, "natural", c("same", "new"), REG4, REG4); E <- FULL_ENGINE
  } else if (stage == "stage0s") {      # ADDED 2026-11-16 night, step 1 (ANALYSIS_PLAN_SIMULATION.md section 9l): the stage-0c worlds plus persistent surveys (surveyRho 0.70, real continuity 0.86); B also raises the covariate share of the route level to 0.75
    W <- data.table(world = c("s0s_a_w1", "s0s_a_w2", "s0s_b_w1"), seed = c(101L, 102L, 101L), flipStrength = 0, flipShare = 0.3571,
                    routeFromCov = c(0.5, 0.5, 0.75), routeWalkSd = 0.23, noiseScale = 0.7, etaSd = 0.4, surveyRho = 0.70)
    for (n in setdiff(names(base), "routeDrift")) W[[n]] <- base[[n]]; W[["routeDrift"]] <- 0; setcolorder(W, c("world", "seed", "flipStrength", "flipShare"))
    U <- makeUnits(W, "natural", c("same", "new"), REG4, REG4); E <- FULL_ENGINE
  } else if (stage == "stage0c") {      # ADDED 2026-11-16 (ANALYSIS_PLAN_SIMULATION.md section 9j): route field from covariates (cross-validated share about 0.5 as in the real data), walk 0.23, noise scale 0.7, etaSd 0.4 (the marginals need it, see 9j); ablations: no covariate part, no walk
    W <- data.table(world = c("s0c_full_w1", "s0c_full_w2", "s0c_nocov_w1", "s0c_nowalk_w1"), seed = c(101L, 102L, 101L, 101L), flipStrength = 0, flipShare = 0.3571,
                    routeFromCov = c(0.5, 0.5, 0, 0.5), routeWalkSd = c(0.23, 0.23, 0.23, 0), noiseScale = 0.7, etaSd = 0.4)
    for (n in setdiff(names(base), "routeDrift")) W[[n]] <- base[[n]]; W[["routeDrift"]] <- 0; setcolorder(W, c("world", "seed", "flipStrength", "flipShare"))
    U <- makeUnits(W, "natural", c("same", "new"), REG4, REG4); E <- FULL_ENGINE
  } else if (stage == "stage0n") {      # ADDED 2026-11-15 (ANALYSIS_PLAN_SIMULATION.md section 9g): random-walk route level (sd 0.23 per year, calibrated to the real greenfinch variogram), count noise scale 0.55, survey persistence 0.70; the two ablations isolate the walk and the other two changes
    W <- data.table(world = c("s0n_full_w1", "s0n_full_w2", "s0n_walk_w1", "s0n_noiseSurvey_w1"), seed = c(101L, 102L, 101L, 101L), flipStrength = 0, flipShare = 0.3571,
                    routeWalkSd = c(0.23, 0.23, 0.23, 0), noiseScale = c(0.55, 0.55, 1, 0.55), surveyRho = c(0.70, 0.70, 0, 0.70))
    for (n in setdiff(names(base), "routeDrift")) W[[n]] <- base[[n]]; W[["routeDrift"]] <- 0; setcolorder(W, c("world", "seed", "flipStrength", "flipShare"))
    U <- makeUnits(W, "natural", c("same", "new"), REG4, REG4); E <- FULL_ENGINE
  } else if (stage == "stage0r") {      # ADDED 2026-11-14 (fade rate of the route drift, ANALYSIS_PLAN_SIMULATION.md section 9e): worlds 1 and 2 (seeds 101, 102) at route drift sd 0.4 with year-to-year autocorrelation rho 0.5 (fast) and 0.95 (slow); rho 0.82 is stage0d's dr0.4
    W <- CJ(rho = c(0.5, 0.95), w = 1:2); W[, `:=`(world = sprintf("s0r_rho%.2f_w%d", rho, w), seed = 100L + w, flipStrength = 0, flipShare = 0.3571, routeDrift = 0.4, routeDriftRho = rho)]; W[, c("rho", "w") := NULL]
    for (n in setdiff(names(base), "routeDrift")) W[[n]] <- base[[n]]; setcolorder(W, c("world", "seed", "flipStrength", "flipShare"))
    U <- makeUnits(W, "natural", c("same", "new"), REG4, REG4); E <- FULL_ENGINE
  } else if (stage == "stage0d") {      # ADDED 2026-11-14 (anchor change after stage 0, ANALYSIS_PLAN_SIMULATION.md section 9d): the stage-0 worlds 1 and 2 (same seeds 101, 102, so paired with the drift-off result) with route drift sd 0.2 / 0.4 (0.6 dropped, see 9d), both designs, 4 regimes
    W <- CJ(dr = c(0.2, 0.4), w = 1:2); W[, `:=`(world = sprintf("s0d_dr%.1f_w%d", dr, w), seed = 100L + w, flipStrength = 0, flipShare = 0.3571, routeDrift = dr)]; W[, c("dr", "w") := NULL]
    for (n in setdiff(names(base), "routeDrift")) W[[n]] <- base[[n]]; setcolorder(W, c("world", "seed", "flipStrength", "flipShare"))
    U <- makeUnits(W, "natural", c("same", "new"), REG4, REG4); E <- FULL_ENGINE
  } else if (stage == "stage0") {
    W <- data.table(world = sprintf("s0_w%d", 1:5), seed = 100L + 1:5, flipStrength = 0, flipShare = 0.3571); for (n in names(base)) W[[n]] <- base[[n]]
    U <- makeUnits(W, "natural", c("same", "new"), REG4, REG4); E <- FULL_ENGINE
  } else if (stage == "stage1a") {
    W <- CJ(phi = c(0, 0.85, 1), fs = c(0.10, 0.30), rep = 1:5); W[, `:=`(world = sprintf("s1a_phi%.2f_fs%.2f_r%d", phi, fs, rep), seed = 200L + rep, flipStrength = phi, flipShare = fs)]; W[, c("phi", "fs", "rep") := NULL]
    for (n in names(base)) W[[n]] <- base[[n]]; setcolorder(W, c("world", "seed", "flipStrength", "flipShare"))
    U <- makeUnits(W, LADDERS, "same", REG2, REG2); E <- FULL_ENGINE
  } else if (stage == "stage1d") {
    W <- CJ(dr = c(0, 0.4), phi = c(0, 0.85), rep = 1:5); W[, `:=`(world = sprintf("s1d_dr%.2f_phi%.2f_fs0.30_r%d", dr, phi, rep), seed = 200L + rep, flipStrength = phi, flipShare = 0.30, routeDrift = dr)]; W[, c("dr", "phi", "rep") := NULL]
    for (n in setdiff(names(base), "routeDrift")) W[[n]] <- base[[n]]; setcolorder(W, c("world", "seed", "flipStrength", "flipShare"))
    U <- makeUnits(W, "natural", "same", REG2, REG2); E <- replace(FULL_ENGINE, "PREVAL_HORIZONS", "1,3,5,10")
  } else if (stage == "swap") {
    W <- CJ(phi = c(0, 0.85, 1), rep = 1:5); W[, `:=`(world = sprintf("sw_phi%.2f_fs0.30_r%d", phi, rep), seed = 200L + rep, flipStrength = phi, flipShare = 0.30)]; W[, c("phi", "rep") := NULL]
    for (n in names(base)) W[[n]] <- base[[n]]; setcolorder(W, c("world", "seed", "flipStrength", "flipShare"))
    U <- makeUnits(W, c("oracle", "oracleSwap"), "same", REG2, REG2); E <- FULL_ENGINE
  } else {
    W <- data.table(world = "smoke_w1", seed = 7L, flipStrength = 0.85, flipShare = 0.30, nRoutes = 1200L, nStable = 3L, nDrift = 2L, nFlip = 3L, nNoise = 6L, routeDrift = 0)
    U <- makeUnits(W, c("natural", "flippersFirst"), c("same", "new"), REG4, REG4)
    E <- replace(FULL_ENGINE, c("PREVAL_START", "PREVAL_MINTRAIN", "PREVAL_HORIZONS", "PREVAL_COMPLEXITY", "PREVAL_LASTPILOT", "PREVAL_EPOCHS", "PREVAL_PILOT_EPOCHS", "PREVAL_PATIENCE"), c("2017", "2", "1", "2,5", "2019", "3", "3", "3"))
  }
  list(worlds = W[], units = U[], engine = data.table(name = names(E), value = as.character(E)))
}
writeManifest <- function(stage) {
  d <- simStage(stage); dir.create(manifestDir, showWarnings = FALSE)
  for (n in names(d)) fwrite(d[[n]], file.path(manifestDir, sprintf("sim_%s_%s.csv", stage, n)))
  cat(sprintf("%s: %d worlds, %d units (engine runs) written to %s\n", stage, nrow(d$worlds), nrow(d$units), manifestDir)); invisible(d)
}
readManifest <- function(stage) { f <- function(n) file.path(manifestDir, sprintf("sim_%s_%s.csv", stage, n)); stopifnot(file.exists(f("worlds")), file.exists(f("units")), file.exists(f("engine")))
  list(worlds = fread(f("worlds")), units = fread(f("units"), colClasses = list(character = c("ladderTag", "rankFrom"))), engine = fread(f("engine"), colClasses = "character")) }

# ---------------------------------------------------------------------------------------------------------------------------------- paths and the engine call
simRoot <- function(stage) { r <- Sys.getenv("SIM_ROOT", ""); if (nzchar(r)) r else if (dir.exists("/data/birds")) file.path("/data/birds/PreVal/simulation", stage) else file.path(.repo, "outputs", "simulation", stage) }
dataDir <- function(stage) { v <- Sys.getenv("PREVAL_FINAL", ""); if (nzchar(v)) v else file.path(simRoot(stage), "data") }
outDir <- function(stage) { v <- Sys.getenv("PREVAL_OUT", ""); if (nzchar(v)) v else file.path(simRoot(stage), "outputs") }
worldParams <- function(row) {
  p <- modifyList(anchorWorldParams(), list(seed = as.integer(row$seed), flipStrength = row$flipStrength, nRoutes = as.integer(row$nRoutes), nStable = as.integer(row$nStable), nDrift = as.integer(row$nDrift), nFlip = as.integer(row$nFlip), nNoise = as.integer(row$nNoise), routeDrift = if (!is.null(row$routeDrift)) row$routeDrift else 0))
  if (!is.null(row$routeDriftRho) && !is.na(row$routeDriftRho)) p$routeDriftRho <- row$routeDriftRho      # ADDED 2026-11-14: optional column of the worlds table (stage0r); without it the generator default exp(-1/5) is used, as before
  for (n in c("routeWalkSd", "noiseScale", "surveyRho", "routeFromCov", "etaSd", "flipSd")) if (!is.null(row[[n]]) && !is.na(row[[n]])) p[[n]] <- row[[n]]      # ADDED 2026-11-15: optional columns of the worlds table (stage0n); without them the generator defaults (all off) are used, as before
  p$shares <- flipShares(row$flipShare); p
}
# SIMULATION-ONLY ladder files in the folder of the world (the engine reads featureTable_<ladder>.csv with PREVAL_LADDER=<ladder>); natural needs no file (per-window rankings)
generateWorld <- function(stage, row) {
  dd <- dataDir(stage); od <- file.path(outDir(stage), row$world); f <- file.path(dd, sprintf("final_%s.rds", row$world))
  if (file.exists(f) && all(file.exists(file.path(od, sprintf("featureTable_%s.csv", FILE_LADDERS))))) { message("world ", row$world, " and ladder files exist; kept"); return(invisible(f)) }
  w <- simulateWorld(worldParams(row)); writeWorld(w, dd, row$world); dir.create(od, recursive = TRUE, showWarnings = FALSE)
  for (o in FILE_LADDERS) fwrite(ladderRanking(w, o, seed = row$seed), file.path(od, sprintf("featureTable_%s.csv", o)))        # oracle and oracleSwap are used by the swap stage only
  message(sprintf("world %s written: %d route-years, %d covariates, ladder files %s", row$world, nrow(w$table), ncol(w$table) - 5L, paste(FILE_LADDERS, collapse = ", "))); invisible(f)
}
runEngine <- function(stage, engineStage, u, eng, slice = NULL, nSlices = NULL) {
  ev <- setNames(eng$value, eng$name); ev <- ev[!nzchar(Sys.getenv(names(ev), ""))]            # an engine setting already in the environment wins over the manifest
  uv <- c(PREVAL_STAGE = engineStage, PREVAL_SPECIES = u$world, PREVAL_FINAL = dataDir(stage), PREVAL_OUT = outDir(stage), PREVAL_LADDER = u$ladderTag, PREVAL_RANKPERWINDOW = as.character(u$rankPerWindow), PREVAL_TAG = u$tag,
          PREVAL_ARM = u$arm, PREVAL_ROUTEHELDOUT = as.character(u$routeHeldOut), PREVAL_PLANREGIMES = u$planRegimes, PREVAL_RANKFROM = u$rankFrom)      # unit-specific variables are always set here
  if (!is.null(slice)) uv <- c(uv, PREVAL_SLICE_ID = as.character(slice), PREVAL_SLICE_COUNT = as.character(nSlices))
  v <- c(ev, uv); v[is.na(v)] <- ""
  old <- Sys.getenv(names(v), unset = NA); on.exit({ for (k in names(old)) if (is.na(old[[k]])) Sys.unsetenv(k) else do.call(Sys.setenv, setNames(list(old[[k]]), k)) }, add = TRUE)
  do.call(Sys.setenv, as.list(v)); owd <- setwd(.repo); on.exit(setwd(owd), add = TRUE)
  cat(sprintf("[%s] engine stage %s: unit %d  %s  ladder=%s design=%s%s\n", format(Sys.time(), "%H:%M:%S"), engineStage, u$unit, u$world, u$ladder, u$design, if (!is.null(slice)) sprintf("  slice %d/%d", slice, nSlices) else ""))
  rc <- system2(file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"), "birds/runMe_birds.R")
  if (rc != 0) stop(sprintf("engine stage %s failed (exit %d) for unit %d (%s, %s, %s)", engineStage, rc, u$unit, u$world, u$ladder, u$design)); invisible(rc)
}

# ---------------------------------------------------------------------------------------------------------------------------------- steps
stepUnits <- function(M, step) switch(step, prep = M$worlds, rank = M$units[rankPerWindow == 1L & !nzchar(rankFrom)], rankcopy = M$units[nzchar(rankFrom)], train = M$units, analyze = M$units, stop("unknown step ", step))
stepSlices <- function(step, nSlices) if (step %in% c("rank", "train")) as.integer(nSlices) else 1L
runStep <- function(stage, M, step, taskId, nSlices = 1L) {
  S <- stepSlices(step, nSlices); items <- stepUnits(M, step); k <- (as.integer(taskId) - 1L) %/% S + 1L; sl <- (as.integer(taskId) - 1L) %% S + 1L
  if (k > nrow(items)) { cat(sprintf("task %s is beyond the %d work items of step %s (%d x %d slices); nothing to do\n", taskId, nrow(items), step, nrow(items), S)); return(invisible()) }
  it <- items[k]
  if (step == "prep") {                                       # the world, then the engine's global preparation and design of the first run, then the design of every other run of this world
    generateWorld(stage, it); us <- M$units[world == it$world]; first <- us[1]
    runEngine(stage, "prep", first, M$engine)
    for (i in seq_len(nrow(us))[-1]) runEngine(stage, "design", us[i], M$engine)
  } else if (step %in% c("rank", "train")) runEngine(stage, step, it, M$engine, sl, S)
  else if (step == "rankcopy") runEngine(stage, "rank", it, M$engine, 1L, 1L)
  else runEngine(stage, "analyze", it, M$engine)
  invisible()
}
runLocal <- function(stage, nSlices = 1L) {
  M <- readManifest(stage); t0 <- Sys.time(); tm <- list(); tick <- function(nm, expr) { t1 <- Sys.time(); force(expr); tm[[nm]] <<- as.numeric(difftime(Sys.time(), t1, units = "secs")) }
  for (k in seq_len(nrow(M$worlds))) tick(sprintf("prep_%d", k), runStep(stage, M, "prep", k))
  nr <- nrow(stepUnits(M, "rank")); for (k in seq_len(nr * stepSlices("rank", nSlices))) tick(sprintf("rank_%d", k), runStep(stage, M, "rank", k, nSlices))
  for (k in seq_len(nrow(stepUnits(M, "rankcopy")))) tick(sprintf("rankcopy_%d", k), runStep(stage, M, "rankcopy", k))
  for (k in seq_len(nrow(M$units) * stepSlices("train", nSlices))) tick(sprintf("train_%d", k), runStep(stage, M, "train", k, nSlices))
  for (k in seq_len(nrow(M$units))) tick(sprintf("analyze_%d", k), runStep(stage, M, "analyze", k))
  tt <- data.table(step = names(tm), seconds = round(unlist(tm), 1)); print(tt, row.names = FALSE)
  cat(sprintf("local run of stage %s finished in %.1f minutes\n", stage, as.numeric(difftime(Sys.time(), t0, units = "mins")))); invisible(tt)
}

if (sys.nframe() == 0L && length(grep("--file=.*run_worlds[.]R", commandArgs(FALSE)))) {
  a <- commandArgs(TRUE); if (!length(a)) stop("usage: Rscript simulation/run_worlds.R manifest <stage> | run <stage> <step> <taskId> [nSlices] | local <stage> [nSlices]")
  if (a[1] == "manifest") writeManifest(a[2])
  else if (a[1] == "run") runStep(a[2], readManifest(a[2]), a[3], a[4], if (length(a) >= 5) as.integer(a[5]) else 1L)
  else if (a[1] == "generate") { M <- readManifest(a[2]); generateWorld(a[2], M$worlds[as.integer(a[3])]) }      # only the world and its ladder files (no engine)
  else if (a[1] == "local") runLocal(a[2], if (length(a) >= 3) as.integer(a[3]) else 1L)
  else stop("unknown command ", a[1])
}
