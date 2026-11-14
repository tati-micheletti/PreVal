# Resource ledger: time and RAM of PreVal jobs on EVE

Created 2026-11-14. The full per-job table (every PreVal job since 1 Oct 2026, from EVE's accounting) is `simulation/ledger/ledger_by_job_day.md` (+ `.csv`);
per-task rows of the simulation jobs are in `simulation/ledger/ledger_simulation_tasks.csv`. Both are made by `simulation/make_resource_ledger.R` from a `sacct` dump (command in the script header).
Refresh after every stage; add the new row to the table below by hand.

## Simulation (Tweety), stage 0, first pass of 13 Nov 2026 (5 worlds, 10 units, 31,800 models)

| step | tasks | wall time per task (min / median / max) | CPU-hours (all tasks) | peak RAM GB (median / max) | requested GB | estimate before running |
|---|---|---|---|---|---|---|
| prep (make world, covariates, splits) | 5 | 16 / 21 / 23 min | 1.7 | 1.12 / 1.20 | 10 | 3 h, 5.9 GB peak |
| rank (covariate ranking per window) | 30 | 7 / 8 / 11 min | 4.3 | 0.98 / 1.22 | 3 | 28 s per window, 1 GB |
| rankcopy | 5 | 0 / 0 / 2 min | 0.0 | 0.80 / 1.16 | 3 | not estimated |
| train (3,180 models per unit in 8 slices) | 80 | 42 / 77 / 240 min (2 tasks hit the 4 h limit) | 133.2 | 0.78 / 0.87 | 3 | 14.5 s per model, 1.6 h per task, 0.8 GB |
| analyze | cancelled (training had not fully succeeded) | not measured yet | | | 4 | 0.5 GB |

- **Total first pass about 139 CPU-hours** (prep 1.7 + rank 4.3 + train 133.2), against the estimate of 135. Training ran at 15.2 s per model (133.2 CPU-h / about 31,580 models), against 14.5 s estimated.
- **Correction:** in my message of 14 Nov I wrote "about 100-110 CPU-hours". That was wrong; the measured figure is 139.
- **Time limit:** 2 of 80 training tasks ran out of the 4 h limit (unit 7, tasks 49 and 53, at 287/393 and 277/392 models), and two more needed 3 h 40 and 3 h 52. Models in the status quo regime (FutureTainted) are the slow ones: the same unit's other slices took about 1.7 h. Resubmitted on 14 Nov with `TRAIN_TIME=08:00:00` (chain 66690230 to 66690234); only the missing ~220 models are repeated.
- **RAM:** every step peaked around 1 GB or less. Requests were 10 / 3 / 3 / 3 GB. Proposed tighter requests for the next stages (not yet applied, apart from prep): prep 5 GB (done in `eve/submit_sim.sh`), rank 2 GB, rankcopy 2 GB, train 2 GB, analyze 4 GB until measured.
- **Time limits proposed:** train 8 h (median 1.3 h, max so far above 4 h), rank 1 h is fine (max 11 min), prep 1 h is enough (max 23 min).

## Real-data campaigns (from the generated table; day groups, caribou jobs share the name preval-*)

| day (campaign, as far as the notes say) | job | tasks | CPU-hours | median wall min | peak RAM GB (max) | requested GB | problems |
|---|---|---|---|---|---|---|---|
| 8-9 Oct (birds, first runs) | preval-train | 721 + 748 | 422 + 460 | 24 / 33 | 5.4 | 6-7 | 3 time-outs on 8 Oct |
| 19-23 Oct (birds, later runs) | preval-train | 224 + 518 + 847 | 166 + 280 + 365 | 47 / 32 / 18 | 0.9 | 2 | 14 time-outs, 3 failed |
| 24 Oct (birds arms none, refit, spatialTest, val2) | preval-train | 1654 | 345 | 11 | 0.9 | 2 | 90 cancelled by hand, 2 failed |
| 29-30 Oct (birds routes arms, full pool) | preval-train | 401 + 400 | 53 + 285 | 7 / 34 | 0.9 | 2 | none |
| 3 Nov (caribou, first full runs) | preval-train | 980 | 403 | 12 | 7.3 | 7 and 20 | **112 out-of-memory at 7 GB**, 3 time-outs, 11 failed |
| 4 Nov (caribou corrected rerun) | preval-rank + preval-train | 72 + 560 | 6.6 + 380 | 4 / 29 | 9.2 (rank), 7.1 (train) | 20 | none |
| 13 Nov | per-case decomposition (E) | 2 completed | 0.1 | 1-2 min | 0.26 | 12 | first job failed in 1 s: environment variable was not passed to the job |

- **Rules of thumb from the ledger:** birds training needs about 1 GB per core (the first October runs needed 5 GB and a different design); caribou training peaks at about 7 GB and caribou ranking at about 9 GB, so request 10 GB for caribou training and 12 GB for caribou ranking.
- **Total PreVal compute since 1 Oct 2026** is 3,348 CPU-hours (sum of the CPU-hours column of `ledger_by_job_day.csv`, all jobs named preval-* or sim-*, including time-outs and cancelled tasks, excluding the resubmission of 14 Nov).
