#!/bin/bash
# Caribou through the standardised engine on EVE (caribou/runMe_caribou.R). The design (plan, manifests; prep and design stages) must exist in ${PREVAL_OUT}/caribou/run<tag>. Since 2026-11-04
# ONE submission chains: per-window rankings (array) -> training (array, starts when ALL rankings are done) -> analysis (starts when ALL models are done). Run from the repo root on an EVE login node.
#   Test (a few models):   PREVAL_PRESET=replication bash eve/submit_caribou.sh test      (needs rankings that already exist; no ranking step)
#   Real run:              PREVAL_PRESET=main|replication|newPlaces|newPlaces25|newAnimals [NTASKS=200] [MAXPAR=15] [TRAIN_TIME=04:00:00] [RANKTASKS=36] bash eve/submit_caribou.sh all
#   replication            has no per-window ranking (it uses the global featureTable.csv): training -> analysis.
#   newPlaces/newPlaces25  share the ladder of main: PREVAL_RANKFROM=main RANKTASKS=1 RANK_AFTER=<job id of main's ranking array>  (eve/submit_caribou_all.sh does this for all five arms).
#   Rankings of an older run in the folder (no settingsStamp.rds, e.g. made before 2026-11-04): refused, unless MOVE_OLD_RANKINGS=1 renames the folder to rankings_old_<date> first.
#   DRYRUN=1               prints the sbatch commands instead of submitting.
# Finished models are skipped, so resubmitting after a failure never repeats work. One settings file per submission (species, tag, process id in its name).
set -euo pipefail
cd "$(dirname "$0")/.."
MODE="${1:-}"; [ "${MODE}" = "test" ] || [ "${MODE}" = "all" ] || { echo "usage: PREVAL_PRESET=<replication|main|newPlaces|newPlaces25|newAnimals> bash eve/submit_caribou.sh test|all"; exit 1; }
: "${PREVAL_PRESET:?set PREVAL_PRESET to replication, main, newPlaces, newPlaces25 or newAnimals}"
export PREVAL_DATASET=caribouNN
OUTROOT="${PREVAL_OUT:-/data/birds/PreVal/caribou/outputs/engine}"
export PREVAL_OUT="${OUTROOT}"      # handed to every job through the settings file, so the jobs read the same folder this script checked
TAG="${PREVAL_TAG:-${PREVAL_PRESET}}"
RUN="${OUTROOT}/caribou/run_${TAG}"
for f in "${RUN}/experimentPlan.csv" "${OUTROOT}/caribou/networkSettings.rds" "${OUTROOT}/caribou/featureTable.csv" "${OUTROOT}/caribou/final_caribou.rds"; do
  [ -f "$f" ] || { echo "MISSING: $f  (run the prep and design stages first and upload them)"; exit 1; }
done
WORKDIR="${PREVAL_WORK:-/work/${USER}/preval}"
if [ "${DRYRUN:-0}" = "1" ]; then sbatch() { echo "DRYRUN sbatch $*" >&2; echo "$((RANDOM + 10000))"; }; fi      # prints instead of submitting
NRANK=$( (ls "${RUN}/rankings"/s*_e*.csv 2>/dev/null || true) | wc -l)
if [ "${PREVAL_PRESET}" != "replication" ]; then
  if [ "${MODE}" = "test" ]; then
    [ "${NRANK}" -gt 0 ] || { echo "MISSING: per-window rankings in ${RUN}/rankings (a test needs existing rankings; run the real submission first)"; exit 1; }
    echo "per-window rankings found: ${NRANK}"
  elif [ "${NRANK}" -gt 0 ] && [ ! -f "${RUN}/rankings/settingsStamp.rds" ]; then      # rankings of an older run: their network settings are unknown (they may come from a stale settings file)
    if [ "${MOVE_OLD_RANKINGS:-0}" = "1" ]; then mv "${RUN}/rankings" "${RUN}/rankings_old_$(date +%Y%m%d_%H%M%S)"; echo "moved the unstamped rankings (${NRANK} windows) aside"
    else echo "REFUSED: ${RUN}/rankings holds ${NRANK} windows without settingsStamp.rds (made before 2026-11-04, settings unknown). Rerun with MOVE_OLD_RANKINGS=1 to rename that folder and make new rankings."; exit 1; fi
  fi
fi
mkdir -p "${WORKDIR}/logs"
PART=(); if [ -n "${EVE_PARTITION:-}" ]; then PART=(--partition="${EVE_PARTITION}"); fi
if [ "${MODE}" = "test" ]; then export PREVAL_SLICE_ID=1 PREVAL_SLICE_COUNT=2000; fi
RUNENV="${WORKDIR}/run_env_caribou_${TAG}_$(date +%Y%m%d_%H%M%S)_$$.sh"
[ -e "${RUNENV}" ] && { echo "settings file ${RUNENV} already exists; refusing to overwrite it"; exit 1; }
env | grep '^PREVAL_' | sed "s/^\([^=]*\)=\(.*\)\$/export \1='\2'/" > "${RUNENV}" || true
echo "Settings handed to every job (${RUNENV}):"; sed 's/^/  /' "${RUNENV}"
EXPORT="--export=ALL,PREVAL_RUN_ENV=${RUNENV}"
# MEM_PER_CPU (default 7G) raises the memory per task: on 3 Nov 2026 111 of 300 replication/main tasks were killed at 7 GB (OUT_OF_MEMORY).
# caribou models are heavier than the bird models (up to ~160k training strata x 11 steps): 7 GB per core and 4 h per task (the largest model took 8.5 s per epoch locally)
if [ "${MODE}" = "test" ]; then
  id=$(sbatch --parsable "${EXPORT}" "${PART[@]+"${PART[@]}"}" --mem-per-cpu="${MEM_PER_CPU:-7G}" --time=01:30:00 --array=1-1 eve/eve_train_array.sbatch)
  echo "test:    ${id}  Log: /work/${USER}/preval/logs/train_${id%%_*}_1.out"; exit 0
fi
NTASKS="${NTASKS:-200}"; MAXPAR="${MAXPAR:-15}"
DEP=()      # 2026-11-04: the training waits for the rankings of this arm
if [ "${PREVAL_PRESET}" != "replication" ]; then
  RANKTASKS="${RANKTASKS:-36}"; RDEP=(); if [ -n "${RANK_AFTER:-}" ]; then RDEP=(--dependency=afterok:"${RANK_AFTER%%_*}" --kill-on-invalid-dep=yes); fi
  rank=$(sbatch --parsable "${EXPORT}" "${PART[@]+"${PART[@]}"}" "${RDEP[@]+"${RDEP[@]}"}" --mem-per-cpu="${MEM_PER_CPU:-7G}" --time="${RANK_TIME:-02:00:00}" --array=1-"${RANKTASKS}"%"${RANK_MAXPAR:-20}" eve/eve_rank_array.sbatch)
  echo "rank:    ${rank}  (${RANKTASKS} tasks${RANK_AFTER:+, after ${RANK_AFTER}}${PREVAL_RANKFROM:+, ladder from ${PREVAL_RANKFROM}})"; echo "RANK_JOB=${rank}"
  DEP=(--dependency=afterok:"${rank}" --kill-on-invalid-dep=yes)
fi
train=$(sbatch --parsable "${EXPORT}" "${PART[@]+"${PART[@]}"}" "${DEP[@]+"${DEP[@]}"}" --mem-per-cpu="${MEM_PER_CPU:-7G}" --time="${TRAIN_TIME:-04:00:00}" --array=1-"${NTASKS}"%"${MAXPAR}" eve/eve_train_array.sbatch)
echo "train:   ${train}  (${NTASKS} tasks, at most ${MAXPAR} at once)"
ana=$(sbatch --parsable "${EXPORT}" "${PART[@]+"${PART[@]}"}" --dependency=afterok:"${train}" --kill-on-invalid-dep=yes eve/eve_analyze.sbatch)
echo "analyze: ${ana}"
echo "Monitor: squeue -u \$USER   |   logs in ${WORKDIR}/logs/"
