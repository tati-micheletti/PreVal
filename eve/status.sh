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
echo "--- Time and memory of finished training tasks (tighten --mem-per-cpu / --time with this) ---"
JOB=$(sacct -u "$USER" -S today -n -X --format=JobID,JobName%14 2>/dev/null | grep preval-train | head -1 | awk '{print $1}' | sed 's/_.*//')
if [ -n "$JOB" ]; then
  sacct -j "$JOB" -n -P --format=JobID,State,Elapsed,MaxRSS 2>/dev/null | grep -E '[.]batch[|]COMPLETED' | \
    awk -F'|' '{v=$4; gsub("K","",v); if (v+0>m) m=v+0; n++} END{if (n>0) printf "  %d finished tasks; largest peak memory %.1f GB\n", n, m/1048576; else print "  (no finished task yet)"}'
  sacct -j "$JOB" -n -P -X --format=State,Elapsed 2>/dev/null | grep COMPLETED | awk -F'|' '{split($2,a,":"); s=a[1]*3600+a[2]*60+a[3]; if (s>m) m=s; t+=s; n++} END{if (n>0) printf "  longest task %d min, average %d min\n", m/60, t/n/60}'
fi
echo
echo "Logs: ${LOG}    Outputs: ${OUT}"
