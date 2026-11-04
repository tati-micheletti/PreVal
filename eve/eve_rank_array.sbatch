#!/bin/bash -l
# STAGE rank (SLURM array), added 2026-11-04: the per-window covariate ladders are made on EVE, in the same submission as the training (eve/submit_caribou.sh), so that the ladder and the
# models always come from the same network settings (the first full runs mixed a ladder made with a stale networkSettings.rds with a corrected training).
# Each task ranks its share of the windows (round-robin; SLURM_ARRAY_TASK_ID / SLURM_ARRAY_TASK_COUNT). Windows that already have a ranking file are skipped.
# An arm that shares the ladder of another run (PREVAL_RANKFROM=main) only copies it: one task is enough.
#SBATCH --job-name=preval-rank
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=7G
#SBATCH --time=02:00:00
#SBATCH --output=/work/%u/preval/logs/rank_%A_%a.out
#SBATCH --error=/work/%u/preval/logs/rank_%A_%a.err

set -euo pipefail
mkdir -p "/work/${USER}/preval/logs"
source "${SLURM_SUBMIT_DIR}/eve/eve_env.sh"
export PREVAL_STAGE=rank
export PREVAL_THREADS=1            # one torch thread per task; the parallelism comes from the array
cd "${PREVAL_ROOT}"
Rscript "${PREVAL_SCRIPT:-runMe.R}"
