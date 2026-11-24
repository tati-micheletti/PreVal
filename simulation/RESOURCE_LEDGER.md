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

## Simulation (Tweety), all stages 13-17 Nov 2026 (refreshed 2026-11-17 from a new `sacct` dump; per-task rows in `ledger/ledger_simulation_tasks.csv`)

One row per stage; each stage is one submission of the chain prep, rank, rankcopy, train, analyze (`eve/submit_sim.sh`). Wall = first task start to last task end of the stage.

| stage | what it was | tasks | not completed | CPU-hours | train wall per task (median / max min) | peak RAM GB (max) | wall, whole stage |
|---|---|---|---|---|---|---|---|
| 0 | first pass 13 Nov + resubmission of the 2 time-outs 14 Nov | 250 | 2 (time-out at 4 h) | 144.9 | 42 / 240 | 1.22 | 14.1 h (13 Nov 19:21 to 14 Nov 09:29) |
| 0d | drift worlds | 120 | 0 | 117.5 | 78 / 183 | 1.16 | 3.7 h |
| 0r | rho worlds | 120 | 0 | 125.8 | 86 / 183 | 1.11 | 3.6 h |
| 0n | noise worlds | 120 | 0 | 127.7 | 77 / 231 | 1.16 | 4.8 h |
| 0c | covariate-driven route level | 120 | 0 | 81.4 | 50 / 153 | 1.15 | 3.2 h |
| 0s | 3 worlds, 20 km block size | 114 | 2 (prep out of memory, twice) | 102.3 | 74 / 133 | 12.00 (the failed prep at its 12 GB request) | 3.7 h |
| 0t | 2 worlds | 100 | 0 | 62.7 | 37 / 148 | 1.23 | 3.2 h |
| 0u | 4 worlds | 120 | 0 | 120.0 | 84 / 197 | 1.25 | 3.8 h |
| 0v | 3 worlds, 5 horizons (1, 2, 4, 6, 9) | 126 | 0 | 205.1 | 146 / 264 | 1.36 | 5.1 h |
| 1v | flip experiment 1: 12 worlds, flip effect 0.16 / 0.23, natural + flippers-first ladders (prep 4.0, rank 12.0, train 569.1, analyze 0.4) | 468 | 0 | 585.5 | 111 / 270 | 1.40 | 8.0 h (17 Nov 17:54 to 18 Nov 01:56) |
| 1w | flip experiment 2: 12 worlds, flip effect 0.35 / 0.50 (prep 3.9, rank 10.2, train 545.4, analyze 0.3) | 468 | 0 | 559.8 | 113 / 272 | 1.47 | 8.1 h (23 Nov 19:34 to 24 Nov 03:42) |
| 1x | swap test: 8 worlds, oracle + oracleSwap ladders, no ranking step (prep 2.9, train 383.0, analyze 0.2) | 312 | 0 | 386.1 | 79 / 181 | 1.35 | 6.6 h (24 Nov 10:01 to 16:37) |

*Added 24 Nov 2026 from a fresh `sacct` dump (job-array totals, `ledger_by_job_day.csv` regenerated). The CPU-hours of 1v and 1w differ slightly from the figures I quoted in chat on 23-24 Nov (593 and 570), which came from a different counting; the table uses the same method as all rows above. The 1w training array was split over two start days in `ledger_by_job_day.csv` (240 + 48 tasks) because the run crossed midnight.*

- **Total simulation compute so far: 2,619 CPU-hours** (stages 0 to 1x; 1,087 up to 0v, plus 586 + 560 + 386 for the three flip stages). All PreVal jobs since 1 Oct 2026, including real-data runs, time-outs and cancelled tasks: **5,828 CPU-hours**. Cost per world: 1v 49, 1w 47, 1x 48 CPU-hours, against 30-68 in stages 0 to 0v. Stages with 2.5 horizons cost 60-145 CPU-hours; stage 0v with 5 horizons cost 205 (68 CPU-hours per world against 30 for stage 0u, i.e. 2.3 times, with twice the horizons).
- **Wall time is about 3-4 h per stage whatever its size**, because the slowest training task (status quo regime, 2.5-4.4 h) sets the end. A 5-horizon stage takes 5 h. Plan overnight runs with this: about 5 stages per night in sequence is the limit, if each needs the previous result.
- **RAM:** every step peaked at 1.4 GB or less, except one case. Prep of world `s0s_a_w1` ran out of memory at 5 GB and again at 12 GB: the block-size step (`blockCV::cv_spatial_autocor`, variogram fit) never ends on a spatially flat residual field and grows without limit. Not a size problem, so more RAM does not help; fixed by copying the block size from the sibling world (plan 9l). Current requests in `eve/submit_sim.sh`: prep 12 GB (kept because of this incident), rank 2, rankcopy 2, train 2, analyze 4 GB; measured peaks would allow prep 2 GB, analyze 2 GB.
- **Time limits:** train 8 h (stage 0v maximum 4.4 h) is enough; rank 1 h (max 15 min); prep 1 h (max 37 min).
- **Training speed per model** was not recomputed; stage 0 was 15.2 s per model. The stage 0v median task (146 min) is twice that of 0u because it trains models for 5 instead of 2.5 horizons per window.
- **Whole project, all PreVal jobs since 1 Oct 2026: 4,296 CPU-hours** (sum of `ledger_by_job_day.csv`, including time-outs and cancelled tasks, now including the 14 Nov resubmission).

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
