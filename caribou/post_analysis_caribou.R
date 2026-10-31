# Post-analysis of a caribou engine run: covariate novelty (DI; reference = PreVal's training strata of the split, i.e. the regime FutureUnseenGrouped; case vector = the mean over the
# 11 steps of a stratum, which does not use the outcome) and its statistics. Run after the analyze stage:  PREVAL_TAG=main Rscript caribou/post_analysis_caribou.R
# Settings: PREVAL_OUT (default outputs/caribou_engine), PREVAL_TAG (default main), PREVAL_NOV_LEVELS (default 10,20: levels of the plan)
suppressMessages({library(data.table); library(torch)})
for (f in list.files("modules/preValNN/R", "[.]R$", full.names = TRUE)) source(f)
out <- Sys.getenv("PREVAL_OUT", "outputs/caribou_engine"); tag <- Sys.getenv("PREVAL_TAG", "main"); runDir <- file.path(out, "caribou", paste0("run_", tag))
lv <- as.numeric(strsplit(Sys.getenv("PREVAL_NOV_LEVELS", "10,20"), ",")[[1]])
fin <- readRDS(file.path(out, "caribou", "final_caribou.rds")); rk <- fread(file.path(out, "caribou", "featureTable.csv"))
fin[, idIndex := as.numeric(as.factor(id))]; store <- buildCaseStore(fin, buildStrataCaseIndex(fin), rk$Feature, "case_", layout = "strata")
Xc <- as.matrix(as.array(store$X$mean(dim = 2L))); colnames(Xc) <- rk$Feature; rm(store, fin); gc()
cs <- noveltyCases(runDir, NULL, levels = lv, refRegime = "FutureUnseenGrouped", Xcases = Xc); od <- file.path(runDir, "analysis_novelty"); dir.create(od, showWarnings = FALSE)
st <- noveltyStats(cs); for (nm in names(st)) fwrite(st[[nm]], file.path(od, paste0("novelty_", nm, ".csv")))
cat("novelty done ->", od, "\n")
