#!/bin/bash -l
# Per-case decomposition of "new PreVal minus status quo" (checks M3 / M4 of the route-specific-memory hypothesis): birds/post_memory_percase.R.
# Read-only on the result folders; writes only to PREVAL_PERCASE_OUT. ONE command, from the repo root on an EVE login node:
#   Birds (one species):   PREVAL_SPECIES=Chloris_chloris sbatch eve/eve_percase.sbatch          (also: PREVAL_SPECIES=Dendrocopos_major)
#   Caribou:               PREVAL_UNIT=animal sbatch eve/eve_percase.sbatch
# Optional environment variables (all have defaults):
#   PREVAL_OUT            result root (birds: /data/birds/PreVal/birds/outputs/v4 as in eve/submit_post.sh; caribou: /data/birds/PreVal/caribou/outputs/engine)
#   PREVAL_SQ_RUN / PREVAL_PV_RUN   run folder names (birds: run and run_routes; caribou: run_main for both)
#   PREVAL_LEVELS=2,10    only these covariate levels (e.g. for a quick first look)      PREVAL_PERCASE_LOSS=lossCal|loss|both
#   PREVAL_PERCASE_OUT    output folder (default /work/$USER/preval/percase_<species>)
# The script file must exist on EVE in $PREVAL_ROOT/birds/ (it is not committed yet: copy it, or git pull when that is safe).
#SBATCH --job-name=preval-percase
#SBATCH --cpus-per-task=4
#SBATCH --mem-per-cpu=3G
#SBATCH --time=02:00:00
#SBATCH --output=/work/%u/preval/logs/percase_%j.out
#SBATCH --error=/work/%u/preval/logs/percase_%j.err

set -euo pipefail
mkdir -p "/work/${USER}/preval/logs"
if [ "${PREVAL_UNIT:-route}" = "animal" ]; then
  export PREVAL_DATASET=caribouNN                                  # eve_env.sh then defaults PREVAL_OUT to /data/birds/PreVal/caribou/outputs/engine
  export PREVAL_SPECIES="${PREVAL_SPECIES:-caribou}"
else
  export PREVAL_DATASET=birds
  : "${PREVAL_SPECIES:?set PREVAL_SPECIES, e.g. PREVAL_SPECIES=Chloris_chloris sbatch eve/eve_percase.sbatch}"
  export PREVAL_OUT="${PREVAL_OUT:-/data/birds/PreVal/birds/outputs/v4}"
fi
source "${SLURM_SUBMIT_DIR}/eve/eve_env.sh"
cd "${PREVAL_ROOT}"
export PREVAL_PERCASE_OUT="${PREVAL_PERCASE_OUT:-/work/${USER}/preval/percase_${PREVAL_SPECIES}}"
Rscript birds/post_memory_percase.R      # cores = SLURM_CPUS_PER_TASK; everything else from the PREVAL_* variables (see the header of the script)
echo "Done. Read ${PREVAL_PERCASE_OUT}/percase_summary.txt"
