# preValPrep_Caribou

Final table of the caribou data for the standardised validation engine (`modules/preValNN`, `dataLayout = "strata"`).

* `R/prepCaribou.R`: `buildCaribouFinal()` (from the extracted-features table through `prepareNNdata`, or from the saved `preparedModelInputs.csv`), `checkCaribouFinal()`,
  `caribouNetworkSettings()` (the original network, never tuned), `writeCaribouInputs()`.
* One case = one stratum (11 rows, observed step first). The table keeps `indiv_step_id`, `id` (animal), `year`, `x1_`, `y1_` (start of the observed step), `case_`, `burst_` and
  the 30 candidate covariates. Start year 2013; strata without a turn angle (the first step of a burst) or without 11 rows are removed whole by `prepareNNdata`;
  the calendar-year counters `timeSinceFire/Harvest` are recoded to the published cap of 40 years (recode report kept; `checkCaribouFinal` refuses a table that still holds a counter).
* The engine (`preValNN`) builds the case index (`buildStrataCaseIndex`) and the 3-D tensor store (`buildStrataStoreEngine`) from this table; see `R/strataLayout.R` there.
* This is a plain function folder, not a SpaDES module (the birds prep module is one; the caribou prep is run by `caribou/runMe_caribou.R`, stage `prep`).
