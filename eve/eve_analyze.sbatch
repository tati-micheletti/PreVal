#!/bin/bash -l
# STAGE analyze: completeness check (fails if any model has no result), re-audit of every split from the saved
# manifests, pre-specified analysis (see ANALYSIS_PLAN.md). Submit after all train tasks ended.
#SBATCH --job-name=preval-analyze
#SBATCH --cpus-per-task=2
#SBATCH --mem-per-cpu=4G
#SBATCH --time=01:00:00
#SBATCH --output=/work/%u/preval/logs/analyze_%j.out
#SBATCH --error=/work/%u/preval/logs/analyze_%j.err

set -euo pipefail
mkdir -p "/work/${USER}/preval/logs"
source "${SLURM_SUBMIT_DIR}/eve/eve_env.sh"
export PREVAL_STAGE=analyze
cd "${PREVAL_ROOT}"
Rscript runMe.R
