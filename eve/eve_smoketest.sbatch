#!/bin/bash -l
# 10-minute smoke test on EVE's `testing` partition. Run it BEFORE the real pipeline, from the repo root on a login node:
#   cd ~/projects/PreVal && sbatch eve/eve_smoketest.sbatch
# Look for "PREVAL SMOKETEST: ALL OK" in logs/smoketest_<jobid>.out
#SBATCH --job-name=preval-smoke
#SBATCH --partition=testing
#SBATCH --time=00:10:00
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=4G
#SBATCH --output=/work/%u/preval/logs/smoketest_%j.out
#SBATCH --error=/work/%u/preval/logs/smoketest_%j.err

set -euo pipefail
mkdir -p "/work/${USER}/preval/logs"
source "${SLURM_SUBMIT_DIR}/eve/eve_env.sh"
cd "${PREVAL_ROOT}"
Rscript --vanilla eve/smoketest.R
