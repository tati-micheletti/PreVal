# Pre-specified analysis plan (caribou refit)

Written BEFORE any refit result exists (branch `feature/caribou-refit`). Anything not listed here is
exploratory and will be labelled as such in the paper.

## Hypotheses
- **H1** Random/mixed cross-validation overfits and under-reports future error.
- **H2** PreVal (tuning and early stopping on a genuinely out-of-sample later year) is more honest and forecasts better.
- **H3** Under PreVal, model complexity does not lead to overfitting.

## Design (one *split* = window start s, history end e, test year T > e; 2013 <= s < e < T <= 2022)
- Unit = stratum (1 observed step + 10 matched random steps, 11 rows). All sampling is at stratum level.
- **Shared test set**: a fixed random 50% of the year-T strata; identical in all regimes and complexity levels.
- **FutureUnseen (PreVal)**: train = years s..e-1, validation = year e.
- **FutureTainted (status quo)**: the *same strata* as FutureUnseen, randomly re-allocated to train/validation (same sizes).
- **Internal (random CV)**: train + validation drawn at random from years s..T, excluding the test strata (same sizes),
  restricted to the animals present in the PreVal/Tainted pool, so that all regimes use the same animals and the same
  number of strata. Bursts cannot be matched without changing what the regimes are (PreVal's validation year has its own
  bursts; random CV shares bursts between train and validation); burst counts per set are reported in `splitSummary.csv`.
- Sets are disjoint; sizes are equal across regimes; verified by `verifySplits()` before any training and
  re-verified from the saved manifests after the run (`auditSplitManifests()`).
- Complexity ladder: the 2/5/10/30 covariates ranked by within-stratum permutation importance of a global model
  (the ranking is the same for all regimes; it is not a selection leak for the *comparison*).
- **Spatial arm** (subset of splits, horizon 1, test years 2018/2020/2022): 100 km blocks, 4 folds; one fold's blocks
  (+10 km buffer) are removed from every training/validation pool of every regime; the test set is the year-T strata
  inside those blocks. Reported separately; it answers "does the result survive proper spatial blocking?".
- Replicates: network initialisation seeds are shared across the three regimes of a cell (paired), different across cells.

## Outcome measures (cross-entropy loss, lower is better; chance = ln 11 = 2.398)
- `reported` = the regime's own validation loss at the best epoch (what that regime would report).
- `realized` = loss on the shared test set.
- `optimism` = realized - reported.  `skill` = ln 11 - realized.  `trainTestGap` = realized - training loss.
- Seen vs unseen animals (embedding fallback = mean of trained animals) are always reported separately.

## Estimands and tests
- **H1**: optimism by regime x complexity. Prediction: optimism(Tainted), optimism(Internal) > optimism(PreVal) ~ 0, and
  growing with complexity for Tainted/Internal.
- **H2a**: paired difference in realized loss on the identical test strata: PreVal - Tainted, PreVal - Internal, per
  complexity and pooled. Prediction: <= 0 (PreVal not worse).
- **H2b (decision relevance)**: within each regime, choose the complexity with the best reported loss; compare the
  realized loss of the chosen model across regimes and its regret versus the best complexity in hindsight.
- **H3**: slope of realized loss and of trainTestGap on log2(number of covariates), per regime. Prediction: PreVal slope
  of the gap ~ 0 (equivalence margin set to 0.005 loss per doubling, fixed now), Tainted/Internal slopes > 0.
- Absolute skill is shown next to every contrast. If PreVal skill is indistinguishable from chance for a complexity
  level, H3 is *not* claimed for that level (no "no overfitting because nothing was learned").

## Uncertainty
Percentile bootstrap over **test years** (the independent unit; splits sharing a test year share their test strata),
2000 resamples, 95% intervals. Within-split paired contrasts use the identical test strata. Per-stratum losses and burst ids
are saved so burst-clustered intervals can be added.

## What would count as refuting each hypothesis (fixed in advance)
- H1 fails if optimism(Tainted) and optimism(Internal) do not exceed optimism(PreVal) (95% interval includes 0 or is negative).
- H2 fails if PreVal - Tainted and PreVal - Internal realized-loss contrasts are positive with intervals excluding 0.
- H3 fails if the PreVal slope of trainTestGap is positive with an interval excluding the equivalence margin.

## Sensitivity (pre-specified)
1. Spatial arm (above). 2. `matchAnimals = FALSE` (Internal may use every animal of its longer window). 3. Seen-animal-only test loss.
4. Replicates (3 initialisations) on a random 20% of splits. No pre-2013 runs.

## Provenance
Every seed (splits, models, spatial blocks, global model, importance, bootstrap) is written to `seedRegistry.csv`;
every model has a `_provenance.rds` (seeds, RNG kind, R/torch versions, host, SLURM ids); the run has `runProvenance_*.rds`.
