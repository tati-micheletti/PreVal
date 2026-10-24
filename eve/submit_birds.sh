#!/bin/bash
# Birds on EVE. The preparation (network pilot, covariate ranking, block size, design) was done LOCALLY and uploaded, so this only
# submits the training array and then the analysis. Run from the repo root on an EVE login node.
#   First a small test (20 models = 1 in 2000, about 20 minutes, the SAME 20 models as the local timing test, so results can be compared):
#     PREVAL_SPECIES=Chloris_chloris bash eve/submit_birds.sh test
#   Then the real run (defaults: 300 tasks, at most 30 running at once, 4 h per task, 2 GB per task):
#     PREVAL_SPECIES=Chloris_chloris [NTASKS=300] [MAXPAR=30] [TRAIN_TIME=04:00:00] [EVE_PARTITION=<from sinfo -s>] bash eve/submit_birds.sh all
#   Second arm (two-year validation) later:  PREVAL_VALYEARS=2 PREVAL_TAG=val2 ... bash eve/submit_birds.sh all
# Finished models are skipped, so resubmitting after a failure never repeats work.
set -euo pipefail
cd "$(dirname "$0")/.."
MODE="${1:-}"; [ "${MODE}" = "test" ] || [ "${MODE}" = "all" ] || { echo "usage: PREVAL_SPECIES=<species> bash eve/submit_birds.sh test|all"; exit 1; }
export PREVAL_DATASET=birds
: "${PREVAL_SPECIES:?set PREVAL_SPECIES, e.g. Chloris_chloris}"
OUTROOT="${PREVAL_OUT:-/data/birds/PreVal/birds/outputs/refit}"
FINAL="${PREVAL_FINAL:-/data/birds/PreVal/birds/data}"
TAG=""; [ -n "${PREVAL_TAG:-}" ] && TAG="_${PREVAL_TAG}"
for f in "${OUTROOT}/${PREVAL_SPECIES}/run${TAG}/experimentPlan.csv" "${OUTROOT}/${PREVAL_SPECIES}/networkSettings.rds" \
         "${OUTROOT}/${PREVAL_SPECIES}/featureTable.csv" "${FINAL}/final_${PREVAL_SPECIES}.rds"; do
  [ -f "$f" ] || { echo "MISSING: $f  (upload the local outputs/birds/${PREVAL_SPECIES} folder and the final table first)"; exit 1; }
done
# per-window covariate rankings (made locally on PreVal's training years only; the train tasks stop without them)
NRANK=$(ls "${OUTROOT}/${PREVAL_SPECIES}/run${TAG}/rankings"/s*_e*.csv 2>/dev/null | wc -l)
[ "${NRANK}" -gt 0 ] || { echo "MISSING: no per-window rankings in ${OUTROOT}/${PREVAL_SPECIES}/run${TAG}/rankings (run the local rank stage and upload them)"; exit 1; }
echo "per-window rankings found: ${NRANK}"
mkdir -p "/work/${USER}/preval/logs"
PART=(); if [ -n "${EVE_PARTITION:-}" ]; then PART=(--partition="${EVE_PARTITION}"); fi

if [ "${MODE}" = "test" ]; then export PREVAL_SLICE_ID=1 PREVAL_SLICE_COUNT=2000; fi
# one settings file PER SUBMISSION: species, arm and the process id are part of the name (2026-10-24: two submissions in the same second used to share a file named only by the time,
# so the later one overwrote the earlier one and the jobs of both ran the later settings)
RUNENV="/work/${USER}/preval/run_env_${PREVAL_SPECIES}${TAG}_$(date +%Y%m%d_%H%M%S)_$$.sh"
[ -e "${RUNENV}" ] && { echo "settings file ${RUNENV} already exists; refusing to overwrite it"; exit 1; }
env | grep '^PREVAL_' | sed "s/^\([^=]*\)=\(.*\)\$/export \1='\2'/" > "${RUNENV}" || true
echo "Settings handed to every job (${RUNENV}):"; sed 's/^/  /' "${RUNENV}"
EXPORT="--export=ALL,PREVAL_RUN_ENV=${RUNENV}"

if [ "${MODE}" = "test" ]; then
  id=$(sbatch --parsable "${EXPORT}" "${PART[@]+"${PART[@]}"}" --mem-per-cpu=2G --time=01:30:00 --array=1-1 eve/eve_train_array.sbatch)
  echo "test:    ${id}  (20 models). Log: /work/${USER}/preval/logs/train_${id%%_*}_1.out"
  exit 0
fi
NTASKS="${NTASKS:-300}"; MAXPAR="${MAXPAR:-30}"
train=$(sbatch --parsable "${EXPORT}" "${PART[@]+"${PART[@]}"}" --mem-per-cpu=2G --time="${TRAIN_TIME:-04:00:00}" --array=1-"${NTASKS}"%"${MAXPAR}" eve/eve_train_array.sbatch)
echo "train:   ${train}  (${NTASKS} tasks, at most ${MAXPAR} at once)"
ana=$(sbatch --parsable "${EXPORT}" "${PART[@]+"${PART[@]}"}" --dependency=afterok:"${train}" --kill-on-invalid-dep=yes eve/eve_analyze.sbatch)
echo "analyze: ${ana}"
echo "Monitor: squeue -u \$USER   |   PREVAL_DATASET=birds PREVAL_SPECIES=${PREVAL_SPECIES} bash eve/status.sh   |   logs in /work/${USER}/preval/logs/"
