# Run the analysis on a DOWNLOADED copy of the results (e.g. on your PC), without SpaDES:
#   Rscript eve/analyze_local.R <folder with the *_finalDT.csv files> <output folder> [samestrata=TRUE|FALSE]
# The same-information contrast needs the *_perStratum.rds files next to the finalDT files; pass FALSE if they were not downloaded.
# Needs data.table (+ ggplot2 for the figures, lme4 for mixed_models.txt).
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("usage: Rscript eve/analyze_local.R <modelDir> <outDir> [TRUE|FALSE]")
suppressMessages(library(data.table))
fileArg <- grep("--file=", commandArgs(FALSE), value = TRUE)
root <- normalizePath(file.path(dirname(normalizePath(sub("--file=", "", fileArg[1]))), ".."))
source(file.path(root, "modules", "caribouNN_Global", "R", "stratumNet.R"))      # withSeed(), stringSeed()
source(file.path(root, "modules", "caribouNN", "R", "analyzeExperiment.R"))
analyzeExperiment(args[1], args[2], sameInfo = if (length(args) >= 3) as.logical(args[3]) else TRUE)
cat("Done. Tables and figures in ", args[2], "\n")
