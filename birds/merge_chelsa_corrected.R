# Replace the CHELSA temperature rows of 2022-2024 (built as MIN of daily minima / MAX of daily maxima, a ~4-5 K break found by birdMonitor on 2026-10-07)
# by the corrected monthly MEAN files (_dmean). The old table is kept under a new name; nothing is deleted.
#   Rscript birds/extract_chelsa_monthly.R <birdMonitor inputs> 2022:2024 chelsa_monthly_routes_dmean_2022-2024.csv     (first)
#   Rscript birds/merge_chelsa_corrected.R
.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))
suppressMessages(library(data.table))
old <- "data_birds/chelsa_monthly_routes.csv"; keep <- "data_birds/chelsa_monthly_routes_OLD_minmax_bug.csv"; new <- "data_birds/chelsa_monthly_routes_dmean_2022-2024.csv"
if (!file.exists(keep)) file.rename(old, keep) else stop("the old table is already archived; refusing to run twice (", keep, ")")
o <- fread(keep); n <- fread(new)
stopifnot(setequal(unique(n$year), 2022:2024), !anyNA(n$value), n[variable != "prec", all(value > 150 & value < 350)])
d <- rbind(o[year < 2022], n)
setorder(d, year, variable, month, SITE_ID); fwrite(d, old)
r <- dcast(d[variable %in% c("tasmax", "tasmin")], SITE_ID + year + month ~ variable, value.var = "value")[, .(range = mean(tasmax - tasmin), julTmax = mean(tasmax[month == 7])), by = year]
cat("mean (tasmax - tasmin) over route-months and July tasmax by year, corrected table:\n"); print(r[year >= 2018])
cat("rows old/new/merged:", nrow(o), nrow(n), nrow(d), "\n")
