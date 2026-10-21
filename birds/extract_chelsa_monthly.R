# Extract CHELSA monthly climate (prec, tasmin, tasmax; 2000-2024) at the centroid of every MhB route polygon (bilinear).
# Read-only on the inputs. Output (git-ignored): data_birds/chelsa_monthly_routes.csv  (long: SITE_ID, year, month, variable, value)
# Run:  Rscript birds/extract_chelsa_monthly.R [inputs folder of birdMonitor] [years] [output file name]
.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))
suppressMessages({library(terra); library(sf); library(data.table)})
args <- commandArgs(trailingOnly = TRUE)
inp <- if (length(args) >= 1) args[1] else "C:/Users/michelet/Documents/GitHub/birdMonitor/inputs"
years <- if (length(args) >= 2) eval(parse(text = args[2])) else 2000:2024
out <- "data_birds"; dir.create(out, showWarnings = FALSE)
s <- st_read(file.path(inp, "response/raw/MhB/MhB_Probeflaechen_DE_S2637_epsg25832.shp"), quiet = TRUE)
sites <- data.table(SITE_ID = tolower(as.character(s$SITE_ID)), x = s$x_wgs84, y = s$y_wgs84)
stopifnot(!anyDuplicated(sites$SITE_ID), all(is.finite(sites$x)))
fwrite(sites, file.path(out, "route_coordinates.csv"))
dir <- file.path(inp, "predictors/raw/chelsa_monthly/europe")
res <- list(); t0 <- Sys.time()
for (yr in years) for (v in c("prec", "tasmin", "tasmax")) for (m in 1:12) {
  f <- file.path(dir, sprintf("%s_%d_%02d.tif", v, yr, m))
  fd <- file.path(dir, sprintf("%s_%d_%02d_dmean.tif", v, yr, m))                # birdMonitor, 2026-10-07: from 2022 the plain files are MIN of daily minima / MAX of daily maxima (a ~4-5 K break); the _dmean files are the corrected monthly MEAN
  if (v != "prec" && file.exists(fd)) f <- fd else if (v != "prec" && yr >= 2022) { message("NO corrected file for ", basename(f), ": skipped (the plain 2022+ file is wrong)"); next }
  if (!file.exists(f)) { message("missing: ", basename(f)); next }
  r <- rast(f)
  val <- extract(r, as.matrix(sites[, .(x, y)]), method = "bilinear")[, 1]
  if (v != "prec") val[!is.na(val) & (val < 150 | val > 350)] <- NA_real_          # fill values
  res[[length(res) + 1L]] <- data.table(SITE_ID = sites$SITE_ID, year = yr, month = m, variable = v, value = val)
  if (m == 12 && v == "tasmax") message(sprintf("%d done (%.1f min)", yr, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
d <- rbindlist(res)
fwrite(d, file.path(out, if (length(args) >= 3) args[3] else "chelsa_monthly_routes.csv"))   # 3rd argument: output file name (to extract only the corrected years)
cat("rows:", nrow(d), " NA share:", round(mean(is.na(d$value)), 4), "\n")
print(d[, .(min = min(value, na.rm = TRUE), median = median(value, na.rm = TRUE), max = max(value, na.rm = TRUE)), by = variable])
