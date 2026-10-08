#!/bin/bash -l
# STAGE train (SLURM array): each task trains an equal share of the models (largest first, round-robin).
# The array size is the number of tasks; each task reads SLURM_ARRAY_TASK_ID / SLURM_ARRAY_TASK_COUNT.
# Submit after eve_prep finished (use eve/submit_refit.sh, which sets the dependency):
#   sbatch --array=1-100%50 eve/eve_train_array.sbatch
# Safe to resubmit: models with a result file are skipped.
#
# RESOURCES (measured locally, one thread): largest model (30 covariates, 162k training strata) 8.5 s per epoch,
# peak memory 4.3 GB. Whole experiment ~50 core-hours (estimate) => ~30 min per task with 100 tasks.
# 1 cpu x 6G stays inside EVE's standard 6-8 GB per core. Tighten after the first tasks with sacct (MaxRSS, Elapsed).
#SBATCH --job-name=preval-train
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=7G
#SBATCH --time=02:00:00
#SBATCH --output=/work/%u/preval/logs/train_%A_%a.out
#SBATCH --error=/work/%u/preval/logs/train_%A_%a.err

set -euo pipefail
mkdir -p "/work/${USER}/preval/logs"
source "${SLURM_SUBMIT_DIR}/eve/eve_env.sh"
export PREVAL_STAGE=train
export PREVAL_THREADS=1            # one torch thread per task; the parallelism comes from the array
cd "${PREVAL_ROOT}"
Rscript runMe.R
