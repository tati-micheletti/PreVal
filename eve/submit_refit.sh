#!/bin/bash
# Submit the whole refit as one dependency chain: prep -> train array -> analyze.
# Run from the repo root on an EVE login node, AFTER the smoke test passed:
#   [NTASKS=100] [MAXPAR=50] [TRAIN_TIME=02:00:00] [EVE_PARTITION=<from sinfo -s>] bash eve/submit_refit.sh
# Second pass (20 covariates + converge cap-bound models), only after the first pass finished:
#   PREVAL_NEW_PLAN=1 PREVAL_COMPLEXITY=2,5,10,20,Inf PREVAL_EPOCHS=300 PREVAL_EXTEND_FROM=50 TRAIN_TIME=04:00:00 bash eve/submit_refit.sh
# If prep fails, SLURM cancels the waiting jobs (--kill-on-invalid-dep=yes). Fix the cause, run this script again:
# finished models are skipped, so nothing already trained is redone.
set -euo pipefail
cd "$(dirname "$0")/.."
mkdir -p "/work/${USER}/preval/logs"
NTASKS="${NTASKS:-100}"; MAXPAR="${MAXPAR:-50}"
PART=(); if [ -n "${EVE_PARTITION:-}" ]; then PART=(--partition="${EVE_PARTITION}"); fi

prep=$(sbatch --parsable "${PART[@]+"${PART[@]}"}" eve/eve_prep.sbatch)
echo "prep:    ${prep}"
train=$(sbatch --parsable "${PART[@]+"${PART[@]}"}" --dependency=afterok:"${prep}" --kill-on-invalid-dep=yes \
          --time="${TRAIN_TIME:-02:00:00}" --array=1-"${NTASKS}"%"${MAXPAR}" eve/eve_train_array.sbatch)
echo "train:   ${train}  (${NTASKS} tasks, at most ${MAXPAR} at once)"
ana=$(sbatch --parsable "${PART[@]+"${PART[@]}"}" --dependency=afterok:"${train}" --kill-on-invalid-dep=yes eve/eve_analyze.sbatch)
echo "analyze: ${ana}"
echo "Monitor: squeue -u \$USER   |   logs in ./logs/"
