#!/bin/bash
# Progress of the PreVal refit on EVE (read only; changes nothing):
#   cd ~/projects/PreVal && bash eve/status.sh
OUT="${PREVAL_OUT:-/data/birds/PreVal/${PREVAL_DATASET:-caribou}/outputs/refit}"
LOG="/work/${USER}/preval/logs"
MOD="${OUT}/testedModels"

echo "=== PreVal status  $(date '+%Y-%m-%d %H:%M') ==="
echo
echo "--- Jobs (state: how many) ---"
squeue -u "$USER" -n preval-prep,preval-train,preval-analyze -h -o "%j %T" | sort | uniq -c | sed 's/^/  /'
[ -z "$(squeue -u "$USER" -n preval-prep,preval-train,preval-analyze -h)" ] && echo "  (no PreVal jobs in the queue)"

echo
echo "--- Models finished ---"
if [ -f "${OUT}/experimentPlan.csv" ]; then
  TOTAL=$(( $(wc -l < "${OUT}/experimentPlan.csv") - 1 ))
  DONE=$(ls "${MOD}"/*_finalDT.csv 2>/dev/null | wc -l)
  PCT=$(( 100 * DONE / TOTAL ))
  echo "  ${DONE} of ${TOTAL} models have a result (${PCT}%)"
  R10=$(find "${MOD}" -name '*_finalDT.csv' -mmin -10 2>/dev/null | wc -l)
  R60=$(find "${MOD}" -name '*_finalDT.csv' -mmin -60 2>/dev/null | wc -l)
  echo "  finished in the last 10 min: ${R10}   in the last 60 min: ${R60}"
  if [ "${R10}" -gt 0 ]; then
    LEFT=$(( TOTAL - DONE ))
    echo "  at the last-10-minute rate the remaining ${LEFT} models need about $(( LEFT * 10 / R10 )) minutes (rough)"
  fi
else
  echo "  no experimentPlan.csv yet (prep has not finished)"
fi

echo
echo "--- Failed models ---"
NERR=$(ls "${MOD}/errors"/*.txt 2>/dev/null | wc -l)
echo "  ${NERR} failed (files in ${MOD}/errors)"
[ "${NERR}" -gt 0 ] && ls -t "${MOD}/errors"/*.txt | head -3 | sed 's/^/  /'

echo
echo "--- What each running training task is doing now (latest model and epoch) ---"
for f in $(ls -t "${LOG}"/train_*_*.err 2>/dev/null | head -8); do
  M=$(grep -o '\[[0-9]*/[0-9]*\] Grp_[A-Za-z0-9_]*' "$f" | tail -1)
  E=$(grep -o 'epoch [0-9]*/[0-9]* train [0-9.]* val [0-9.]*' "$f" | tail -1)
  echo "  $(basename "$f" .err | sed 's/train_//'):  ${M:-starting}   ${E}"
done

echo
echo "--- Memory and time of finished tasks (tighten --mem-per-cpu / --time with this) ---"
JOB=$(squeue -u "$USER" -n preval-train -h -o "%A" | head -1)
[ -n "$JOB" ] && sacct -j "$JOB" -X --format=JobID,State,Elapsed,MaxRSS -n 2>/dev/null | head -5 | sed 's/^/  /'
sacct -u "$USER" -S today -n -X --format=JobName%14,JobID,State,Elapsed,MaxRSS 2>/dev/null | grep preval | tail -6 | sed 's/^/  /'
echo
echo "Logs: ${LOG}    Outputs: ${OUT}"
