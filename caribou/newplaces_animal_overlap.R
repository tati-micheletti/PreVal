# Animal-overlap diagnostic of the caribou NEW-PLACES designs (design stage only; reads the saved manifests, trains nothing).
# Question: in the new-places arm the test strata lie in spatial blocks that no regime trains or validates on. Caribou home ranges are tens of km, so an animal whose range
# is split across blocks can appear BOTH in the test blocks and in a regime's training set: "new places" would then partly be "the same animal in a new place".
# For every split of a design and every regime: the share of test STRATA whose animal also appears in that regime's training set, and the share of test ANIMALS that do.
# Designs: A = run_newPlaces (100 km blocks, 10 km buffer, minimum training share 0.4, the primary design), B = run_newPlaces25 (25 km blocks, 5 km buffer, sensitivity design).
# Run from the repo root:  Rscript caribou/newplaces_animal_overlap.R      (writes caribou/newplaces_animal_overlap.csv and prints the summaries)
suppressMessages(library(data.table))
root <- "outputs/caribou_engine/caribou"
designs <- c(A_100km_10km = "run_newPlaces", B_25km_5km = "run_newPlaces25", C_newAnimals = "run_newAnimals")      # C = the new-animals arm (2026-10-31): the share must be exactly 0 for every regime
res <- list()
for (dn in names(designs)) {
  d <- file.path(root, designs[[dn]]); if (!dir.exists(d)) { message("missing: ", d); next }
  idx <- readRDS(file.path(d, "caseIndex.rds")); plan <- fread(file.path(d, "experimentPlan.csv"))
  grid <- unique(plan[, .(splitId, testYear, horizon)])
  for (i in seq_len(nrow(grid))) {
    sid <- grid$splitId[i]; fs <- list.files(file.path(d, "splits"), pattern = paste0("^", sid, "__.*[.]rds$"), full.names = TRUE)
    for (f in fs) {
      rg <- sub(paste0("^", sid, "__(.*)[.]rds$"), "\\1", basename(f)); m <- as.data.table(readRDS(f))
      if (!rg %in% c("FutureUnseenGrouped", "FutureTainted", "FutureTaintedSpatial", "FutureUnseenSpatial", "FutureTaintedGrouped")) next
      te <- idx$unit[m[set == "test"]$row]; tr <- unique(idx$unit[m[set == "train"]$row]); va <- unique(idx$unit[m[set == "val"]$row])
      res[[length(res) + 1L]] <- data.table(design = dn, splitId = sid, testYear = grid$testYear[i], horizon = grid$horizon[i], regime = rg, nTestStrata = length(te), nTestAnimals = uniqueN(te),
                                            shareStrataInTraining = mean(te %in% tr), shareAnimalsInTraining = mean(unique(te) %in% tr), shareStrataInTrainOrVal = mean(te %in% c(tr, va)))
    }
  }
}
R <- rbindlist(res); fwrite(R, "caribou/newplaces_animal_overlap.csv")
label <- c(FutureUnseenGrouped = "PreVal", FutureTainted = "StatusQuo", FutureTaintedSpatial = "SpatialCV", FutureUnseenSpatial = "PreVal+Spatial", FutureTaintedGrouped = "AnimalCV"); R[, regime := label[regime]]
cat("\n== Share of test strata whose animal is also in the regime's TRAINING set (median and range over splits) ==\n")
print(R[, .(splits = .N, median = round(median(shareStrataInTraining), 3), min = round(min(shareStrataInTraining), 3), max = round(max(shareStrataInTraining), 3)), by = .(design, regime)])
cat("\n== Share of test ANIMALS that are also in the training set ==\n")
print(R[, .(median = round(median(shareAnimalsInTraining), 3), min = round(min(shareAnimalsInTraining), 3), max = round(max(shareAnimalsInTraining), 3)), by = .(design, regime)])
cat("\n== By forecast year (strata share, median over splits), PreVal and SpatialCV ==\n")
print(dcast(R[regime %in% c("PreVal", "SpatialCV"), .(m = round(median(shareStrataInTraining), 3)), by = .(design, regime, testYear)], design + regime ~ testYear, value.var = "m"))
