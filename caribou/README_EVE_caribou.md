# Caribou through the engine on EVE: order of commands (one step at a time)

Arms (see `caribou/ANALYSIS_PLAN_CARIBOU_NN.md`, sections 11-14; SIX archives, ONE upload, ONE checksum step, ONE unpack): `replication` (acceptance arm, 1,800 models), `main` (same animals, 1,800 models), `newPlaces` (design A, primary: 1,140 models, 57 splits at the minimum training share 0.4, 100 km blocks, 10 km buffer, forecast years 2016-2022), `newPlaces25` (design B, sensitivity: 1,660 models, 83 splits, 25 km blocks, 5 km buffer, share 0.4, forecast years 2015-2022; added 2026-10-31), `newAnimals` (new-animals test: 1,560 models, 104 splits, 34 windows, forecast years 2015-2022 with few splits in 2015-2016; test strata of held-out animals only; PreVal, status quo and animal-grouped CV; added 2026-10-31, plan section 14). Decisions of 2026-10-31: PreVal validation = 30% of the last-year strata in the main arm (median 14 animals); new-places minimum training share 0.4. Expected md5: inputs 3544038c5418987e6f505cddf98f05b5, replication a19bed2f39d0d725c31f4ae8411bcce0, main 56d097a70613a1a30fe96081d385e8d7, newPlaces b5bd9a956b54baedf2849bd0c1e9b25d, newPlaces25 b9c7d8c0b6f2d1da0704458a2e9fb265, newAnimals e4668c40a2498cd8c4fee7dc4871f708.
Design and per-window rankings were made LOCALLY; EVE only trains and analyses. Caribou data cannot be disclosed: the archives contain the data table, keep the folders private.

Local files (PC): `outputs/caribou_engine/upload/` with `caribou_inputs.tar.gz`, `caribou_replication.tar.gz`, `caribou_main.tar.gz`, `caribou_newPlaces.tar.gz`, `caribou_newPlaces25.tar.gz`, `caribou_newAnimals.tar.gz` and `md5sums.txt`.
EVE target folder: `/data/birds/PreVal/caribou/outputs/engine` (the default of `eve/submit_caribou.sh`). The archives unpack into `caribou/` inside it.

## 1. Update the code (EVE terminal; no PreVal jobs running)
```bash
cd ~/projects/PreVal && git pull --ff-only && git log --oneline -1
```

## 2. Create the target folder (EVE terminal)
```bash
mkdir -p /data/birds/PreVal/caribou/outputs/engine && ls -ld /data/birds/PreVal/caribou/outputs/engine
```

## 3. Upload the six archives in ONE command (LOCAL PowerShell, VPN on)
```powershell
scp "C:\Users\michelet\Documents\GitHub\PreVal_refit\outputs\caribou_engine\upload\caribou_inputs.tar.gz" "C:\Users\michelet\Documents\GitHub\PreVal_refit\outputs\caribou_engine\upload\caribou_replication.tar.gz" "C:\Users\michelet\Documents\GitHub\PreVal_refit\outputs\caribou_engine\upload\caribou_main.tar.gz" "C:\Users\michelet\Documents\GitHub\PreVal_refit\outputs\caribou_engine\upload\caribou_newPlaces.tar.gz" "C:\Users\michelet\Documents\GitHub\PreVal_refit\outputs\caribou_engine\upload\caribou_newPlaces25.tar.gz" "C:\Users\michelet\Documents\GitHub\PreVal_refit\outputs\caribou_engine\upload\caribou_newAnimals.tar.gz" michelet@frontend1.eve.ufz.de:/data/birds/PreVal/caribou/outputs/engine/
```
`caribou_inputs.tar.gz` is large (about 350 MB); the six archives total about 630 MB; expect 10-20 minutes.

## 4. Check the archives (EVE terminal)
```bash
cd /data/birds/PreVal/caribou/outputs/engine && md5sum caribou_*.tar.gz
```
Expected: inputs 3544038c5418987e6f505cddf98f05b5, main 56d097a70613a1a30fe96081d385e8d7, newAnimals e4668c40a2498cd8c4fee7dc4871f708, newPlaces b5bd9a956b54baedf2849bd0c1e9b25d, newPlaces25 b9c7d8c0b6f2d1da0704458a2e9fb265, replication a19bed2f39d0d725c31f4ae8411bcce0 (also in `md5sums.txt`).

## 5. Unpack (EVE terminal; `-k` never overwrites)
```bash
cd /data/birds/PreVal/caribou/outputs/engine && for a in inputs replication main newPlaces newPlaces25 newAnimals; do tar -xzkf caribou_$a.tar.gz; done && ls caribou
```
Expected: `disturbanceRecodeReport.csv featureTable.csv final_caribou.rds networkSettings.rds run_main run_newAnimals run_newPlaces run_newPlaces25 run_replication`.

## 6. Small test of every arm (EVE terminal; about 1 model per arm; one command submits the six tests)
```bash
cd ~/projects/PreVal && unset PREVAL_ROUTEHELDOUT PREVAL_PLANREGIMES PREVAL_ARM PREVAL_VALYEARS PREVAL_BASETAG PREVAL_HYPER PREVAL_REGIMES PREVAL_MATCHUNITS PREVAL_TAG && for a in replication main newPlaces newPlaces25 newAnimals; do echo "== $a"; PREVAL_PRESET=$a bash eve/submit_caribou.sh test 2>&1 | grep -E "test:|MISSING|rankings found|rror"; done
```
Each arm prints a job id (five tests; the replication arm uses the old global ranking and has no per-window rankings). Check (a few minutes later):
```bash
sacct -j <jobid> -X -n --format=JobID,State,Elapsed; ls /data/birds/PreVal/caribou/outputs/engine/caribou/run_<tag>/testedModels | head
```
Expected: `COMPLETED`, and `..._finalDT.csv` files. The elapsed time of the test gives the time per model.

## 7. Full submissions (EVE terminal; one arm per command; wait for the job ids before the next)
Suggestion from the model counts (the old second pass needed about 7 min per model on average; check the test): about 12 models per task.
```bash
cd ~/projects/PreVal && PREVAL_PRESET=replication NTASKS=150 MAXPAR=25 bash eve/submit_caribou.sh all
```
then `PREVAL_PRESET=main NTASKS=150 MAXPAR=25 ...`, then `PREVAL_PRESET=newPlaces NTASKS=60 MAXPAR=20 ...`, then `PREVAL_PRESET=newPlaces25 NTASKS=80 MAXPAR=20 ...`, then `PREVAL_PRESET=newAnimals NTASKS=120 MAXPAR=20 ...` (1,560 models, about 13 per task; the settings file of every submission carries the preset; the tag of the run folder is the preset name).
As one command, after the six tests have COMPLETED (the settings files are unique per submission; the five submissions run at most 110 tasks at once, lower MAXPAR if the queue is long):
```bash
cd ~/projects/PreVal && unset PREVAL_ROUTEHELDOUT PREVAL_PLANREGIMES PREVAL_ARM PREVAL_VALYEARS PREVAL_BASETAG PREVAL_HYPER PREVAL_REGIMES PREVAL_MATCHUNITS PREVAL_TAG && for spec in replication:150:25 main:150:25 newPlaces:60:20 newPlaces25:80:20 newAnimals:120:20; do IFS=: read a n m <<< "$spec"; echo "== $a"; PREVAL_PRESET=$a NTASKS=$n MAXPAR=$m bash eve/submit_caribou.sh all 2>&1 | grep -E "train:|analyze:|MISSING|rankings found|rror"; done
```
The script writes one settings file per submission, checks the plan, the rankings and the input files, and submits the training array and the analysis (the analysis waits for the training).

## 8. Check that everything finished (EVE terminal)
```bash
sacct -u michelet -S 2026-10-31 -X -n --format=JobID%14,JobName%18,State%12 | grep -E "preval" | awk '{print $2, $3}' | sort | uniq -c
```
Expected: 5 `preval-analyze COMPLETED` (one per arm) and all `preval-train COMPLETED` (150 + 150 + 60 + 80 + 120 tasks). If `FAILED`, send the end of `/work/michelet/preval/logs/*<analyze job id>*.err`.

## 9. Then (my side)
Pack the five `analysis` folders for download, run the acceptance comparison of the replication arm with the old numbers (P1'), then read the main, new-places and new-animals arms (`caribou/post_analysis_caribou.R`; the overlap diagnostic `caribou/newplaces_animal_overlap.R` must give 0 for the new-animals arm, which it already does for the design). The sensitivity add-on (patience 20 on a subset) follows the main arm.
