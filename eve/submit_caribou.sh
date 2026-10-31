#!/bin/bash
# Caribou through the standardised engine on EVE (caribou/runMe_caribou.R). The design (plan, manifests) and the per-window rankings are made first (design and rank stages, local or one
# EVE job), uploaded to ${PREVAL_OUT}/caribou/run<tag>, then this script submits the training array and the analysis. Run from the repo root on an EVE login node.
#   Test (a few models):   PREVAL_PRESET=replication bash eve/submit_caribou.sh test
#   Real run:              PREVAL_PRESET=main|replication|newPlaces|newPlaces25 [NTASKS=200] [MAXPAR=15] [TRAIN_TIME=04:00:00] bash eve/submit_caribou.sh all
# Finished models are skipped, so resubmitting after a failure never repeats work. One settings file per submission (species, tag, process id in its name).
set -euo pipefail
cd "$(dirname "$0")/.."
MODE="${1:-}"; [ "${MODE}" = "test" ] || [ "${MODE}" = "all" ] || { echo "usage: PREVAL_PRESET=<replication|main|newPlaces|newPlaces25> bash eve/submit_caribou.sh test|all"; exit 1; }
: "${PREVAL_PRESET:?set PREVAL_PRESET to replication, main, newPlaces or newPlaces25}"
export PREVAL_DATASET=caribouNN
OUTROOT="${PREVAL_OUT:-/data/birds/PreVal/caribou/outputs/engine}"
TAG="${PREVAL_TAG:-${PREVAL_PRESET}}"
RUN="${OUTROOT}/caribou/run_${TAG}"
for f in "${RUN}/experimentPlan.csv" "${OUTROOT}/caribou/networkSettings.rds" "${OUTROOT}/caribou/featureTable.csv" "${OUTROOT}/caribou/final_caribou.rds"; do
  [ -f "$f" ] || { echo "MISSING: $f  (run the prep and design stages first and upload them)"; exit 1; }
done
if [ "${PREVAL_PRESET}" != "replication" ]; then
  NRANK=$(ls "${RUN}/rankings"/s*_e*.csv 2>/dev/null | wc -l); [ "${NRANK}" -gt 0 ] || { echo "MISSING: per-window rankings in ${RUN}/rankings (run the rank stage and upload them)"; exit 1; }
  echo "per-window rankings found: ${NRANK}"
fi
mkdir -p "/work/${USER}/preval/logs"
PART=(); if [ -n "${EVE_PARTITION:-}" ]; then PART=(--partition="${EVE_PARTITION}"); fi
if [ "${MODE}" = "test" ]; then export PREVAL_SLICE_ID=1 PREVAL_SLICE_COUNT=2000; fi
RUNENV="/work/${USER}/preval/run_env_caribou_${TAG}_$(date +%Y%m%d_%H%M%S)_$$.sh"
[ -e "${RUNENV}" ] && { echo "settings file ${RUNENV} already exists; refusing to overwrite it"; exit 1; }
env | grep '^PREVAL_' | sed "s/^\([^=]*\)=\(.*\)\$/export \1='\2'/" > "${RUNENV}" || true
echo "Settings handed to every job (${RUNENV}):"; sed 's/^/  /' "${RUNENV}"
EXPORT="--export=ALL,PREVAL_RUN_ENV=${RUNENV}"
# caribou models are heavier than the bird models (up to ~160k training strata x 11 steps): 7 GB per core and 4 h per task (the largest model took 8.5 s per epoch locally)
if [ "${MODE}" = "test" ]; then
  id=$(sbatch --parsable "${EXPORT}" "${PART[@]+"${PART[@]}"}" --mem-per-cpu=7G --time=01:30:00 --array=1-1 eve/eve_train_array.sbatch)
  echo "test:    ${id}  Log: /work/${USER}/preval/logs/train_${id%%_*}_1.out"; exit 0
fi
NTASKS="${NTASKS:-200}"; MAXPAR="${MAXPAR:-15}"
train=$(sbatch --parsable "${EXPORT}" "${PART[@]+"${PART[@]}"}" --mem-per-cpu=7G --time="${TRAIN_TIME:-04:00:00}" --array=1-"${NTASKS}"%"${MAXPAR}" eve/eve_train_array.sbatch)
echo "train:   ${train}  (${NTASKS} tasks, at most ${MAXPAR} at once)"
ana=$(sbatch --parsable "${EXPORT}" "${PART[@]+"${PART[@]}"}" --dependency=afterok:"${train}" --kill-on-invalid-dep=yes eve/eve_analyze.sbatch)
echo "analyze: ${ana}"
echo "Monitor: squeue -u \$USER   |   logs in /work/${USER}/preval/logs/"
