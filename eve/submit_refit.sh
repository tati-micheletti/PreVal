#!/bin/bash
# Submit the whole refit as one dependency chain: prep -> train array -> analyze.
# Run from the repo root on an EVE login node, AFTER the smoke test passed:
#   [NTASKS=100] [MAXPAR=50] [EVE_PARTITION=<from sinfo -s>] bash eve/submit_refit.sh
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
          --array=1-"${NTASKS}"%"${MAXPAR}" eve/eve_train_array.sbatch)
echo "train:   ${train}  (${NTASKS} tasks, at most ${MAXPAR} at once)"
ana=$(sbatch --parsable "${PART[@]+"${PART[@]}"}" --dependency=afterok:"${train}" --kill-on-invalid-dep=yes eve/eve_analyze.sbatch)
echo "analyze: ${ana}"
echo "Monitor: squeue -u \$USER   |   logs in ./logs/"
