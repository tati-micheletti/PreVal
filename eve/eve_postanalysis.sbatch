#!/bin/bash -l
# Post-analysis of the birds results (cross-arm comparisons and the covariate-novelty analysis), one part of one species per job; see birds/post_analysis_birds.R.
#SBATCH --cpus-per-task=2
#SBATCH --mem-per-cpu=6G
#SBATCH --time=05:00:00
#SBATCH --output=/work/%u/preval/logs/post_%j.out
#SBATCH --error=/work/%u/preval/logs/post_%j.err

set -euo pipefail
mkdir -p "/work/${USER}/preval/logs"
source "${SLURM_SUBMIT_DIR}/eve/eve_env.sh"
cd "${PREVAL_ROOT}"
Rscript birds/post_analysis_birds.R
