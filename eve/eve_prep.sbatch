#!/bin/bash -l
# STAGE prep (one job): data preparation, global ranking model, covariate ranking, experiment design,
# verification gate, tensor store. SpaDES: runMe_refit.R with PREVAL_STAGE=prep.
# Submit from the repo root on a login node:  sbatch eve/eve_prep.sbatch      (or use eve/submit_refit.sh)
#
# RESOURCES: measured locally with the same code: ~7 min and ~34 GB peak (reading and validating the 1.8 GB table).
# 8 cpus x 6G = 48G leaves headroom; tighten after the first EVE run with
#   sacct -j <jobid> --format=JobID,Elapsed,MaxRSS,State
#SBATCH --job-name=preval-prep
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=6G
#SBATCH --time=02:00:00
#SBATCH --output=/work/%u/preval/logs/prep_%j.out
#SBATCH --error=/work/%u/preval/logs/prep_%j.err

set -euo pipefail
mkdir -p "/work/${USER}/preval/logs"
source "${SLURM_SUBMIT_DIR}/eve/eve_env.sh"
export PREVAL_STAGE=prep
export PREVAL_THREADS="${SLURM_CPUS_PER_TASK}"
cd "${PREVAL_ROOT}"
Rscript runMe.R
