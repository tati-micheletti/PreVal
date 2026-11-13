# Calibration table of the realism anchor (phi = 0 world, anchorWorldParams()) against the real greenfinch, over several seeds. Run from the repo root:
#   Rscript simulation/sim_calibrate_anchor.R [nSeeds = 6]
# Prints the parameter set, one row per seed and the real target with the tolerance. No model is trained. Added 2026-11-05.
suppressMessages(library(data.table)); options(width = 220)
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(here, "simulate_world.R")); source(file.path(here, "sim_anchor_metrics.R"))
nSeeds <- { a <- suppressWarnings(as.integer(commandArgs(TRUE)[1])); if (is.na(a)) 6L else a }
ap <- anchorWorldParams(); cat("anchorWorldParams() (phi = 0 set by the call below):\n"); str(ap[c("nRoutes", "surveyProb", "meanCount", "theta", "routeDist", "routeBeta", "etaSd", "rangeKm", "routeSpatialShare", "shares")], give.attr = FALSE)
res <- rbindlist(lapply(seq_len(nSeeds), function(sd) { m <- anchorMetrics(simulateWorld(modifyList(ap, list(seed = sd, flipStrength = 0)))$table); ck <- anchorCheck(m)
  data.table(seed = sd, as.data.table(as.list(m)), allPass = all(ck), fails = paste(names(ck)[!ck], collapse = ",")) }))
print(res[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)], row.names = FALSE)
mm <- res[, lapply(.SD, mean), .SDcols = names(anchorTargets)]
tol <- c(mean = "+-5%", varOverMean = "+-30%", zeros = "+-0.03", routeYears = "", perYear = "+-10%", yearsPerRoute = "", ICC = "+-0.10", sp0_25 = "+-0.04", sp25_50 = "+-0.04", sp50_100 = "+-0.04", sp100_150 = "|x|<0.03", sp150_300 = "|x|<0.03")
cat("\nreal greenfinch vs mean of the simulated seeds\n"); print(data.table(metric = names(anchorTargets), real = anchorTargets, simulated = round(unlist(mm), 3), min = round(sapply(names(anchorTargets), function(n) min(res[[n]])), 3),
                                                                      max = round(sapply(names(anchorTargets), function(n) max(res[[n]])), 3), tolerance = tol[names(anchorTargets)]), row.names = FALSE)
