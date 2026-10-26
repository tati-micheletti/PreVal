#!/bin/bash
# Submit the post-analysis of ONE species: three independent jobs (cross-arm comparisons, novelty for the same-places run, novelty for the new-places run).
#   PREVAL_SPECIES=Chloris_chloris bash eve/submit_post.sh
# The settings are passed explicitly with --export (not through a shared file), so submissions in the same second cannot overwrite each other.
set -euo pipefail
cd "$(dirname "$0")/.."
: "${PREVAL_SPECIES:?set PREVAL_SPECIES, e.g. Chloris_chloris}"
OUT="${PREVAL_OUT:-/data/birds/PreVal/birds/outputs/v4}"; FINAL="${PREVAL_FINAL:-/data/birds/PreVal/birds/data/v2}"
mkdir -p "/work/${USER}/preval/logs"
for part in arms nov:run nov:run_spatialTest; do
  id=$(sbatch --parsable --job-name=preval-post --export=ALL,PREVAL_DATASET=birds,PREVAL_SPECIES="${PREVAL_SPECIES}",PREVAL_OUT="${OUT}",PREVAL_FINAL="${FINAL}",PREVAL_POST="${part}" eve/eve_postanalysis.sbatch)
  echo "post [${part}] ${PREVAL_SPECIES}: ${id}   log: /work/${USER}/preval/logs/post_${id}.out"
done
