# The Copernicus Data Space API delivers every tile as a ZIP (tif + metadata + legend), saved by download_cdse_clms.R with a .tif name.
# This step renames those files to .zip (so the download script can still verify them) and unpacks the GeoTIFFs into <folder>/tif/.
#   Rscript birds/unpack_clms.R
.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))
root <- file.path("data_birds", "downloads")
for (d in c("hrl_tcd", "hrl_bcd", "hrl_ccd")) {
  dir <- file.path(root, d); dir.create(file.path(dir, "tif"), showWarnings = FALSE)
  old <- list.files(dir, pattern = "[.]tif$", full.names = TRUE)
  for (f in old) { sig <- readBin(f, "raw", 2); if (identical(sig, as.raw(c(0x50, 0x4b)))) file.rename(f, sub("[.]tif$", ".zip", f)) }   # PK = zip
  zips <- list.files(dir, pattern = "[.]zip$", full.names = TRUE); n <- 0L
  for (z in zips) {
    nm <- sub("[.]zip$", "", basename(z)); out <- file.path(dir, "tif", paste0(nm, ".tif"))
    if (!file.exists(out)) { utils::unzip(z, files = paste0(nm, "/", nm, ".tif"), exdir = file.path(dir, "tif"), junkpaths = TRUE); n <- n + 1L }
  }
  cat(d, ":", length(zips), "archives,", n, "newly unpacked,", length(list.files(file.path(dir, "tif"), pattern = "[.]tif$")), "tifs in total\n")
}
