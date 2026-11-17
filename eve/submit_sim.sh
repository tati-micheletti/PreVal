#!/bin/bash
# ONE submission for a whole simulation stage on EVE (added 2026-11-05), modelled on eve/submit_caribou_all.sh:
#   prep (array over worlds) -> rank (array over unit x slice) -> rankcopy (array over units) -> train (array over unit x slice) -> analyze (array over units)
# every step starts only when ALL tasks of the step before it have finished OK (--dependency=afterok, --kill-on-invalid-dep=yes), so a failure cancels the rest instead of leaving a half-finished chain.
# What the steps do is in simulation/run_worlds.R; the worlds, units and engine settings are the checked-in files simulation/manifests/sim_<stage>_{worlds,units,engine}.csv.
#   Stage 0 (realism anchor): 5 worlds without flips x natural ladder x 2 designs (same places, new places), 4 regimes. 10 units x 3,180 models = 31,800 models.
#   Stage 0d (anchor + drift): stage-0 worlds 1 and 2 with route drift sd 0.2 / 0.4 x 2 designs x 4 regimes (added 2026-11-14). 8 units x 3,180 models = 25,440 models.
#   Stage 0r (drift fade rate): worlds 1 and 2 at drift sd 0.4 with rho 0.5 and 0.95 x 2 designs x 4 regimes (added 2026-11-14). 8 units x 3,180 models = 25,440 models.
#   Stage 0n (non-stationary route level, less noise, persistent surveys): worlds 1 and 2 with all three changes + two ablations of world 1 x 2 designs x 4 regimes (added 2026-11-15). 8 units x 3,180 models = 25,440 models.
#   Stage 0c (route level partly explained by covariates + walk + less noise): worlds 1 and 2 with all changes + two ablations of world 1 x 2 designs x 4 regimes (added 2026-11-16). 8 units x 3,180 models = 25,440 models.
#   Stage 0s (night run, iterative steps, added 2026-11-16): stage-0c worlds plus persistent surveys, 3 worlds x 2 designs x 4 regimes. 6 units x 3,180 models = 19,080 models; use TRAIN_SLICES=13 (78 tasks, one wave).
#   Stage 0t (night run step 2, added 2026-11-17): stage-0s settings with covariate share 0.90, 2 worlds x 2 designs x 4 regimes. 4 units x 3,180 models = 12,720 models; use TRAIN_SLICES=20 (80 tasks, one wave) and MEM_PREP=12G.
#   Stage 0u (night run step 3, added 2026-11-17): lever probes (etaSd 0.30 x2 seeds, covariate share 0.99, survey persistence 0.80), 4 worlds x 2 designs x 4 regimes. 8 units x 3,180 models = 25,440 models; defaults (10 slices, 80 tasks), MEM_PREP=12G.
#   Stage 1a:                 30 worlds (phi 0/0.85/1 x flip share 0.10/0.30 x 5 replicates) x 4 ladder orders, same places, 2 regimes. 120 units x 1,590 models = 190,800 models.
#   Stage 1d (route drift):   20 worlds (drift off/moderate x phi 0/0.85 x 5 replicates) x natural ladder, horizons 1,3,5,10, 2 regimes. 20 units x 2,520 models = 50,400 models.
#   Swap (causal swap test):  15 worlds (phi 0/0.85/1 x 5 replicates) x ladders oracle and oracleSwap, 2 regimes. 30 units x 1,590 models = 47,700 models.
#   Memory arm: any stage again with PREVAL_IDMODE=none (route information off) and its own folder, e.g.  PREVAL_IDMODE=none SIM_ROOT=/data/birds/PreVal/simulation/stage1d_mem SIM_STAGE=stage1d bash eve/submit_sim.sh
# Run from the repo root on an EVE login node:
#   SIM_STAGE=stage0  bash eve/submit_sim.sh
#   SIM_STAGE=stage1a bash eve/submit_sim.sh        (also stage1d, swap, smoke)
#   DRYRUN=1 SIM_STAGE=stage0 bash eve/submit_sim.sh       prints the sbatch commands (and the settings file) instead of submitting
# Options (environment variables): MEM_PREP (default 5G), MEM_RANK (2G), MEM_TRAIN (2G), MEM_ANALYZE (4G), TRAIN_TIME, RANK_TIME, PREP_TIME, TRAIN_SLICES, RANK_SLICES, MAXPAR_TRAIN, EVE_PARTITION, SIM_ROOT (default /data/birds/PreVal/simulation/<stage>),
# plus any PREVAL_* engine setting (e.g. PREVAL_EPOCHS), which wins over the manifest. Memory, time and array sizes below are ESTIMATES from local timings (see simulation/ANALYSIS_PLAN_SIMULATION.md section 10) until measured on EVE
# (sacct -j <jobid> --format=JobID,Elapsed,MaxRSS,State, then tighten). Finished models, rankings and worlds are skipped, so running it again after a failure never repeats work.
set -euo pipefail
cd "$(dirname "$0")/.."
USER="${USER:-${USERNAME:-user}}"      # only needed for the local dry run (Git Bash has no USER)
: "${SIM_STAGE:?set SIM_STAGE to stage0, stage1a or smoke}"
case "${SIM_STAGE}" in stage0|stage0d|stage0r|stage0n|stage0c|stage0s|stage0t|stage0u|stage1a|stage1d|swap|smoke) ;; *) echo "SIM_STAGE must be stage0, stage0d, stage0r, stage0n, stage0c, stage0s, stage0t, stage0u, stage1a, stage1d, swap or smoke"; exit 1;; esac      # stage0d ADDED 2026-11-14
MAN="simulation/manifests/sim_${SIM_STAGE}"
for f in "${MAN}_worlds.csv" "${MAN}_units.csv" "${MAN}_engine.csv" simulation/run_worlds.R simulation/simulate_world.R birds/runMe_birds.R eve/eve_sim_task.sbatch eve/eve_env.sh; do
  [ -f "$f" ] || { echo "MISSING: $f  (the manifests are made by: Rscript simulation/run_worlds.R manifest ${SIM_STAGE}; commit and pull them)"; exit 1; }
done
NW=$(( $(wc -l < "${MAN}_worlds.csv") - 1 )); NU=$(( $(wc -l < "${MAN}_units.csv") - 1 ))
# units that rank their own covariates (natural ladder, rankPerWindow=1, no rankFrom) and units that copy a ranking (rankFrom set); columns of the units file: unit,world,ladder,design,arm,tag,ladderTag,rankPerWindow,rankFrom,...
NRANKU=$(awk -F, 'NR>1 && $8=="1" && $9=="\"\"" {n++} END{print n+0}' "${MAN}_units.csv")
NCOPY=$(awk -F, 'NR>1 && $9!="\"\"" && $9!="" {n++} END{print n+0}' "${MAN}_units.csv")
[ "${NW}" -ge 1 ] && [ "${NU}" -ge 1 ] || { echo "empty manifest"; exit 1; }
# ---- per-stage defaults (ESTIMATES): train slices per unit and time; memory per core
case "${SIM_STAGE}" in
  stage0)  D_TSLICES=8;  D_TTIME=04:00:00; D_RSLICES=6; D_MAXPAR=60;;      # 3,180 models x ~14.5 s = 12.8 h per unit -> ~1.6 h per task
  stage0d|stage0r|stage0n|stage0c|stage0s|stage0t|stage0u) D_TSLICES=10; D_TTIME=08:00:00; D_RSLICES=6; D_MAXPAR=80;;      # ADDED 2026-11-14: 3,180 models per unit as in stage 0, 10 slices (about 1.3 h per task), 8 h limit (stage 0: median 1.3 h, 2 of 80 tasks hit the 4 h limit)
  stage1a) D_TSLICES=4;  D_TTIME=04:00:00; D_RSLICES=6; D_MAXPAR=80;;     # 1,590 models = 6.4 h per unit -> ~1.6 h per task
  stage1d) D_TSLICES=6;  D_TTIME=04:00:00; D_RSLICES=6; D_MAXPAR=60;;      # 2,520 models = 10.1 h per unit -> ~1.7 h per task
  swap)    D_TSLICES=4;  D_TTIME=04:00:00; D_RSLICES=6; D_MAXPAR=60;;
  smoke)   D_TSLICES=2;  D_TTIME=00:30:00; D_RSLICES=2; D_MAXPAR=4;;
esac
TSLICES="${TRAIN_SLICES:-${D_TSLICES}}"; RSLICES="${RANK_SLICES:-${D_RSLICES}}"; MAXPAR="${MAXPAR_TRAIN:-${D_MAXPAR}}"
MEM_PREP="${MEM_PREP:-5G}"; MEM_RANK="${MEM_RANK:-2G}"; MEM_TRAIN="${MEM_TRAIN:-2G}"; MEM_ANALYZE="${MEM_ANALYZE:-4G}"      # CHANGED 2026-11-13: prep 10G -> 5G (measured on EVE, stage 0: prep peak 1.2 GB, rank peak 1.3 GB). CHANGED 2026-11-14: rank and train 3G -> 2G (measured peaks 1.2 GB and 0.87 GB, simulation/RESOURCE_LEDGER.md); analyze stays 4G until measured
export PREVAL_DATASET=birds
export SIM_STAGE
export SIM_ROOT="${SIM_ROOT:-/data/birds/PreVal/simulation/${SIM_STAGE}}"
export PREVAL_FINAL="${PREVAL_FINAL:-${SIM_ROOT}/data}"        # eve_env.sh would otherwise point birds jobs at the real bird tables
export PREVAL_OUT="${PREVAL_OUT:-${SIM_ROOT}/outputs}"
WORKDIR="${PREVAL_WORK:-/work/${USER}/preval}"
if [ "${DRYRUN:-0}" = "1" ]; then
  sbatch() { echo "DRYRUN sbatch $*" >&2; echo "$((RANDOM + 10000))"; }
  [ -d /work ] || WORKDIR="${TMPDIR:-/tmp}/preval_dryrun_${USER:-user}"
fi
mkdir -p "${WORKDIR}/logs"
PART=(); if [ -n "${EVE_PARTITION:-}" ]; then PART=(--partition="${EVE_PARTITION}"); fi
# ONE settings file per submission (stage and process id in the name): all PREVAL_* and SIM_* variables of this command, read by every job through eve/eve_env.sh
RUNENV="${WORKDIR}/run_env_sim_${SIM_STAGE}_$(date +%Y%m%d_%H%M%S)_$$.sh"
[ -e "${RUNENV}" ] && { echo "settings file ${RUNENV} already exists; refusing to overwrite it"; exit 1; }
env | grep -E '^(PREVAL_|SIM_)' | sed "s/^\([^=]*\)=\(.*\)\$/export \1='\2'/" > "${RUNENV}" || true
echo "Settings handed to every job (${RUNENV}):"; sed 's/^/  /' "${RUNENV}"
echo "Stage ${SIM_STAGE}: ${NW} worlds, ${NU} units (engine runs), ${NRANKU} units rank their own covariates, ${NCOPY} copy a ranking. Output folder: ${SIM_ROOT}"
EXPORT="--export=ALL,PREVAL_RUN_ENV=${RUNENV}"
# one submission of one step: $1 = step, $2 = array size, $3 = time, $4 = max running at once, $5 = dependency job id (empty = none), $6 = slices per unit (the task count is units x slices), $7 = memory per core
one() {
  local step="$1" size="$2" time="$3" par="$4" dep="$5" slices="${6:-1}" mem="${7:-3G}" depopt=() id
  if [ -n "${dep}" ]; then depopt=(--dependency=afterok:"${dep%%_*}" --kill-on-invalid-dep=yes); fi
  id=$(sbatch --parsable "${EXPORT},SIM_STEP=${step},SIM_NSLICES=${slices}" "${PART[@]+"${PART[@]}"}" "${depopt[@]+"${depopt[@]}"}" --job-name="sim-${SIM_STAGE}-${step}" --mem-per-cpu="${mem}" --time="${time}" --array=1-"${size}"%"${par}" eve/eve_sim_task.sbatch)
  local sl=""; case "${step}" in rank|train) sl=", ${slices} slice(s) per unit";; esac
  printf '%-9s %s  (%d tasks, at most %d at once, %s each%s%s)\n' "${step}:" "${id}" "${size}" "${par}" "${time}" "${sl}" "${dep:+, after ${dep%%_*}}" >&2
  echo "${id}"
}
prep=$(one prep "${NW}" "${PREP_TIME:-03:00:00}" "${MAXPAR_PREP:-40}" "" 1 "${MEM_PREP}")
last="${prep}"
if [ "${NRANKU}" -gt 0 ]; then rank=$(one rank $(( NRANKU * RSLICES )) "${RANK_TIME:-01:00:00}" "${MAXPAR_RANK:-60}" "${last}" "${RSLICES}" "${MEM_RANK}"); last="${rank}"; fi
if [ "${NCOPY}" -gt 0 ]; then rcopy=$(one rankcopy "${NCOPY}" 00:30:00 "${NCOPY}" "${last}" 1 "${MEM_RANK}"); last="${rcopy}"; fi
train=$(one train $(( NU * TSLICES )) "${TRAIN_TIME:-${D_TTIME}}" "${MAXPAR}" "${last}" "${TSLICES}" "${MEM_TRAIN}")
ana=$(one analyze "${NU}" "${ANALYZE_TIME:-01:00:00}" "${MAXPAR_ANALYZE:-20}" "${train}" 1 "${MEM_ANALYZE}")
echo "Chain: prep ${prep%%_*} -> ${rank:+rank ${rank%%_*} -> }${rcopy:+rankcopy ${rcopy%%_*} -> }train ${train%%_*} -> analyze ${ana%%_*}"
echo "Monitor: squeue -u \$USER   |   logs in ${WORKDIR}/logs/ (sim_<step>_<jobid>_<task>.out)   |   results: ${SIM_ROOT}/outputs/<world>/run*/analysis/"
