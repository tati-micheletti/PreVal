# preValPrep_Birds

Final table for modeling (one row per route and year) for one bird species. Reuses birdMonitor's processed rasters and its route-year
tables READ-ONLY; adds CHELSA climate features, 200 m summaries (mean and SD inside each route polygon), nearest-epoch land cover,
harmonised land use (hedges break). Functions in `R/`; scripts in `birds/`. Tests in `tests/`.
