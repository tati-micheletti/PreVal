# Birds: analysis plan (written 2026-10-17, BEFORE any model is run)

Amendments are dated and labelled. Anything decided after seeing results is exploratory and says so.

## Scope
- **Two species, each its own experiment:** greenfinch (*Chloris chloris*, EURING 16490) and great spotted woodpecker (*Dendrocopos major*, 8760). Species are NOT a variable: each has its own covariate ranking, its own ladder, its own network settings and its own results. Species are only compared in the summary.
- **Response:** `Reviere` (territory count per route and year), 2005-2024, negative-binomial loss. Rows with `Reviere` = -1 are excluded (observer could not give an exact count; no territory count exists).
- **Unit:** route (fixed location, observed in many years). **Final table for modelling:** one row per route and year with the response, route id, coordinates, spatial block, and all covariates.
- **Covariates (no pruning of correlated ones in the main analysis, as in birdMonitor; ranking by permutation importance with highly correlated covariates, |r| > 0.9, permuted together; the effective number of covariates, from a principal-component count at 90% variance, is reported next to the nominal count):** the ones already prepared (1 km land use and cover, elevation, slope, solar radiation, distance to woodland; climate from CHELSA and the 50 km bioclim windows; 200 m summaries inside each route) plus the extra ones from the predictor review (forest and human layers). Nothing already prepared is dropped. Landscape heterogeneity is rebuilt for 2005-2020 from the land-use and land-cover layers. The beech/oak mast index is NOT used (not needed for a regime comparison).

## Design (same as the caribou refit)
- Split = history window s..e and forecast year T > e. Test set = a fixed random half of the routes of year T, identical in all regimes.
- **PreVal:** train s..e-1, validate on year e. **Status quo:** the same route-years re-allocated at random to train and validation. **Route-grouped CV (added 2026-10-17):** same pool, validation = whole routes (all their years) chosen at random, no buffer; it separates route-identity leakage from spatial autocorrelation. **Spatial-block CV:** same pool, validation = whole spatial blocks of routes (blockCV; block size from the residual-autocorrelation range of a baseline model), training excludes the validation blocks and a buffer. **Random CV reference:** random draw from s..T minus the test set (leaky; used only as the H1 reference, never compared with PreVal).
- **Balance (amended 2026-10-17):** equal pool, equal validation size (PreVal's), equal test set, shared model seeds per cell, and EQUAL TRAINING SIZE: every regime is randomly subsampled to the training size of the spatial-block regime (which loses its buffer). Splits where spatial blocking leaves less than 80% of the status-quo training set are skipped. Leak gates stop the run on any violation; a hard unit test checks zero overlap.
- **Network:** one scoring network per species, settings chosen by a pilot on pre-test history only (never on test years), then FIXED and identical for every regime and every ladder level of that species. Output head: negative binomial.
- **Ladder:** covariates ranked by permutation importance of a global model per species; levels 2, 5, 10, 15, 25, 35, 50 and beyond if enough covariates exist.
- **Inference:** the forecast year is the independent unit; exact sign-flip tests across years, t intervals, plus a mixed model on all splits. Shape of the complexity curve: PEAK and END contrasts and the mixed-model regime x count interaction (as in caribou amendment 4), and the per-year stability of each covariate's coefficient (negative-binomial model per year, heterogeneity I-squared and sign flips) as a pre-declared diagnostic.

## Predictions (Tati, 2026-10-17, and the engine's checks)
1. **H1:** the status quo's forecast loss is larger than the random-CV reference's, and the status quo's reported error understates its forecast error (optimism > 0).
2. **H2:** PreVal forecasts better than the status quo.
3. **Spatial-block CV** is not as good as PreVal but better than the status quo: forecast loss PreVal < spatial-block < status quo. Reason for the second half: in a panel the same route appears in many years, so random validation lets a route's other years sit in training; blocks remove that route-level leakage but still validate inside the training years.
4. **Route-grouped CV** (our addition): forecast loss status quo >= route-grouped >= spatial-block >= PreVal (weakly ordered). The gap from the status quo to route-grouped measures route-identity leakage, route-grouped to spatial-block the spatial neighbours, spatial-block to PreVal the forward step in time.
4b. **Optimism** (forecast loss minus the loss the validation reported): largest for the status quo, intermediate for spatial-block CV, smallest for PreVal.
5. **Covariate ladder:** similar to caribou. Under PreVal the loss does not rise with the number of covariates (flat or slightly improving). Under the status quo the loss rises and may form a hump at intermediate counts, driven by covariates whose relationship with the counts changes over time.
6. **Risk noted in advance:** because routes repeat across years, random CV may leak more than in caribou, so the status-quo optimism may be larger than in caribou.

## Sensitivity analyses
- S1: the same experiment with 2 km spatial thinning of the route-years.
- S2: block size x0.5 and x2.
- Optional, only if a reviewer asks (decided 2026-10-17 not to plan it): pruning of correlated covariates (|r| < 0.7, birdMonitor's threshold). The effective number of covariates is reported instead.
- S3 (potential improvement, later): other algorithms on the same splits (birdMonitor's `nnet`, BRT) to see whether the conclusions depend on the network type.

## Rules
Predictions are not changed after seeing results. Differences between this plan and what is run are listed in the results document.
