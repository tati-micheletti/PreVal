#!/bin/bash
# Shared settings for all PreVal jobs on EVE. Sourced by the sbatch files (edit HERE, nowhere else).
# EVE facts used (birdMonitor setup notes + EVE wiki pages): R is only available together with these modules;
# --mem is rejected (use --mem-per-cpu); jobs are killed when they exceed the requested memory; $TMPDIR is a RAM disk
# (so it is redirected to /work); software and scripts live in /home, DATA in /data or /work (never data in /home).
#
# Data layout (one project folder for all three datasets; the group directory /data/birds has no 60-day deletion):
#   /data/birds/PreVal/caribou/data      the extracted-features table (read only)
#   /data/birds/PreVal/caribou/outputs   everything the runs write
#   /data/birds/PreVal/birds/...         later
#   /data/birds/PreVal/trees/...         later (simulated forest growth)
# Caribou and bird data cannot be disclosed: keep the folders private (see eve/README_EVE.md, step 2).

# Settings of this run (written by submit_refit.sh, path passed with sbatch --export): sourced FIRST so they win over the defaults below.
if [ -n "${PREVAL_RUN_ENV:-}" ] && [ -f "${PREVAL_RUN_ENV}" ]; then source "${PREVAL_RUN_ENV}"; fi

export EVE_R_MODULE="${EVE_R_MODULE:-GCC/13.3.0 OpenMPI/5.0.5 R/4.5.1 GDAL/3.10.3 CMake ImageMagick/7.1.1-38 UDUNITS/2.2.28}"
module load ${EVE_R_MODULE}
if ! command -v Rscript >/dev/null 2>&1; then
  echo "ERROR on $(hostname): Rscript not found after: module load ${EVE_R_MODULE}" >&2; module list >&2; exit 1
fi

export PREVAL_DATASET="${PREVAL_DATASET:-caribou}"
export PREVAL_DATA_ROOT="${PREVAL_DATA_ROOT:-/data/birds/PreVal}"
export PREVAL_ROOT="${PREVAL_ROOT:-$HOME/projects/PreVal}"                    # code (git clone), in /home
export PREVAL_TABLE="${PREVAL_TABLE:-${PREVAL_DATA_ROOT}/${PREVAL_DATASET}/data/extractedFeatures_498a1edc8c19988e843def7542411d3e_2007_2022.csv}"
export PREVAL_OUT="${PREVAL_OUT:-${PREVAL_DATA_ROOT}/${PREVAL_DATASET}/outputs/refit}"
export PREVAL_WORK="${PREVAL_WORK:-/work/${USER}/preval}"                     # scratch and logs only
# RAM disk avoidance
export TMPDIR="${PREVAL_WORK}/tmp/${SLURM_JOB_ID:-interactive}"
mkdir -p "${TMPDIR}" "${PREVAL_OUT}" "${PREVAL_WORK}/logs"
echo "[preval settings] OUT=${PREVAL_OUT} COMPLEXITY=${PREVAL_COMPLEXITY:-default} REPLICATES=${PREVAL_REPLICATES:-default} EPOCHS=${PREVAL_EPOCHS:-default} NEW_PLAN=${PREVAL_NEW_PLAN:-0} RUN_ENV=${PREVAL_RUN_ENV:-none}" >&2
trap 'rm -rf "${TMPDIR}"' EXIT
