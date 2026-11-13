#!/bin/bash -l
# ONE sbatch file for every step of the controlled simulation (added 2026-11-05). eve/submit_sim.sh submits it five times (prep, rank, rankcopy, train, analyze) with the array size, memory and time of each step
# and with the dependencies between them; the step and the stage come from the settings file (SIM_STEP / SIM_STAGE are passed with --export). What a task does is in simulation/run_worlds.R (`run <stage> <step> <taskId> <nSlices>`):
#   prep      array index = world:            generate the world and its ladder files, engine stage prep (network pilot, global ranking, block size, first design), design of the other runs of the world
#   rank      array index = (unit, slice):    per-window covariate rankings of the natural-ladder, same-places runs
#   rankcopy  array index = unit:             the natural-ladder, new-places runs copy those rankings (same windows, same ladder)
#   train     array index = (unit, slice):    models, largest first, round-robin share of the unit's plan
#   analyze   array index = unit:             completeness check and the engine's own analysis
# Resources are set by the submit script on the command line (they override the lines below, which are the smallest values used).
#SBATCH --job-name=preval-sim
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=4G
#SBATCH --time=02:00:00
#SBATCH --output=/work/%u/preval/logs/sim_%x_%A_%a.out
#SBATCH --error=/work/%u/preval/logs/sim_%x_%A_%a.err

set -euo pipefail
mkdir -p "/work/${USER}/preval/logs"
source "${SLURM_SUBMIT_DIR}/eve/eve_env.sh"      # sources the settings file of this submission first (PREVAL_*, SIM_*), then loads the R modules
: "${SIM_STAGE:?SIM_STAGE missing from the settings file}"; : "${SIM_STEP:?SIM_STEP missing (passed with --export by submit_sim.sh)}"
export PREVAL_THREADS=1            # one torch thread per task; the parallelism comes from the array
cd "${PREVAL_ROOT}"
echo "[sim] stage=${SIM_STAGE} step=${SIM_STEP} task=${SLURM_ARRAY_TASK_ID} slices=${SIM_NSLICES:-1} root=${SIM_ROOT:-default} host=$(hostname)" >&2
Rscript simulation/run_worlds.R run "${SIM_STAGE}" "${SIM_STEP}" "${SLURM_ARRAY_TASK_ID}" "${SIM_NSLICES:-1}"
