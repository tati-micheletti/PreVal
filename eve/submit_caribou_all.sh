#!/bin/bash
# ONE command for the whole caribou experiment on EVE (2026-11-04): all five arms, each as rankings -> training -> analysis (eve/submit_caribou.sh all), with the dependencies between them.
#   main          ranks its 36 windows; newPlaces and newPlaces25 wait for that ranking and COPY it (same windows, same ladder); newAnimals ranks its own (held-out animals are left out of the ranking);
#   replication   has no per-window ranking.
# Run from the repo root on an EVE login node:   MOVE_OLD_RANKINGS=1 MEM_PER_CPU=20G bash eve/submit_caribou_all.sh
# MOVE_OLD_RANKINGS=1 renames rankings folders that have no settingsStamp.rds (made before 2026-11-04) to rankings_old_<date>. DRYRUN=1 prints the sbatch commands instead of submitting them.
# Finished models and rankings are skipped, so running it again after a failure never repeats work.
set -euo pipefail
cd "$(dirname "$0")/.."
export MEM_PER_CPU="${MEM_PER_CPU:-20G}"
sub() { local out; out=$("$@" 2>&1) || { echo "${out}"; echo "STOPPED: the submission of ${PREVAL_PRESET} failed (above); the arms before it are already submitted."; exit 1; }; echo "${out}"; LAST_RANK_JOB=$(echo "${out}" | sed -n 's/^RANK_JOB=//p' | tail -1); }
echo "=== main ==="
PREVAL_PRESET=main NTASKS=150 MAXPAR=25 RANKTASKS=36 sub bash eve/submit_caribou.sh all; MAIN_RANK="${LAST_RANK_JOB}"
echo "=== newPlaces ==="
PREVAL_PRESET=newPlaces NTASKS=60 MAXPAR=20 RANKTASKS=1 PREVAL_RANKFROM=main RANK_AFTER="${MAIN_RANK}" sub bash eve/submit_caribou.sh all
echo "=== newPlaces25 ==="
PREVAL_PRESET=newPlaces25 NTASKS=80 MAXPAR=20 RANKTASKS=1 PREVAL_RANKFROM=main RANK_AFTER="${MAIN_RANK}" sub bash eve/submit_caribou.sh all
echo "=== newAnimals ==="
PREVAL_PRESET=newAnimals NTASKS=120 MAXPAR=20 RANKTASKS=34 sub bash eve/submit_caribou.sh all
echo "=== replication ==="
PREVAL_PRESET=replication NTASKS=150 MAXPAR=25 TRAIN_TIME=08:00:00 sub bash eve/submit_caribou.sh all
echo "All five arms submitted. Monitor: squeue -u \$USER"
