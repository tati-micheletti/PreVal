#!/bin/bash
# One-time setup of PreVal on EVE for ONE user (same idea as birdMonitor's cluster/setup_eve.sh).
# Safe to run again: it skips what is already done.
#
#   cd ~/projects/PreVal
#   bash --login eve/setup_eve.sh
#
# What it does:
#   1. loads the EVE modules (R 4.5.1 etc.) and checks the folders
#   2. installs every R package + libtorch (CPU) into the project library with
#      PREVAL_ON_EVE=1 PREVAL_INSTALL_ONLY=1 Rscript runMe.R   (login node: first time 30-60 min, keep the window open)
#   3. submits the 10-minute smoke test (eve/eve_smoketest.sbatch, testing partition)
set -euo pipefail
cd "$(dirname "$0")/.."
if ! type module >/dev/null 2>&1; then
  echo "The 'module' command is missing. Run this script as:  bash --login eve/setup_eve.sh" >&2; exit 1
fi
source eve/eve_env.sh
echo "== 1/3 modules loaded; folders:"
echo "   code    ${PREVAL_ROOT}"
echo "   data    ${PREVAL_TABLE}"
echo "   outputs ${PREVAL_OUT}"
echo "   scratch ${PREVAL_WORK}"
if [ ! -f "${PREVAL_TABLE}" ]; then
  echo "   NOTE: the data table is not there yet (step 4 of README_EVE.md). The install does not need it." >&2
fi
echo "== 2/3 Installing R packages and libtorch (first time: 30-60 minutes; keep this window open)"
PREVAL_ON_EVE=1 PREVAL_INSTALL_ONLY=1 Rscript runMe.R
echo "== 3/3 Submitting the smoke test"
mkdir -p "/work/${USER}/preval/logs"
sbatch eve/eve_smoketest.sbatch
echo
echo "Done. In about a minute:  cat /work/${USER}/preval/logs/smoketest_<jobnumber>.out   -- look for: PREVAL SMOKETEST: ALL OK"
