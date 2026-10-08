# Pre-specified analysis plan (caribou refit)

Written BEFORE any refit result was read (branch `feature/caribou-refit`). Anything not listed here is exploratory and
will be labelled as such in the paper. See `LIMITATIONS.md` for what can go wrong and how it would bias the results.

**Amendment of 2026-10-08 (made before any pooled result was read).** The first version of this plan used a bootstrap over
test years, a train-test-gap slope for H3 and no equivalence test. A review of the analysis code found these weak
(see "Changes" below). The only test losses seen when the amendment was made were those of six models trained for engineering
checks (epoch cap), none pooled or compared across the design. The first-run analysis job on EVE writes tables with the
OLD code; those tables are not to be interpreted. The amended analysis is run afterwards on the final results.

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
  number of strata. Bursts cannot be matched without changing what the regimes are; burst counts per set are reported (Table 1).
- Sets are disjoint; sizes are equal across regimes; verified by `verifySplits()` before any training and
  re-verified from the saved manifests after the run (`auditSplitManifests()`).
- **Complexity ladder: 2 / 5 / 10 / 20 / 30 covariates** ranked by within-stratum permutation importance of a global model
  (same ranking for all regimes). The 20-covariate level is added in the second pass.
- **Training length**: patience 10, maximum 300 epochs. Models that stopped at the first pass's 50-epoch cap are re-trained
  from scratch with the same seeds (second pass); the share of cap-stopped models and a cap-50 versus converged table are reported.
- **Spatial arm** (subset of splits, horizon 1, test years 2018/2020/2022): 100 km blocks, 4 folds; one fold's blocks
  (+10 km buffer) are removed from every training/validation pool of every regime; the test set is the year-T strata
  inside those blocks. Reported separately; never pooled with the temporal arm.
- Replicates: network initialisation seeds are shared across the three regimes of a cell (paired), different across cells.

## Outcome measures (cross-entropy loss, lower is better; chance = ln 11 = 2.398)
- `reported` = the regime's own validation loss at the best epoch. `realized` = loss on the shared test set.
- `optimism` = realized - reported. `skill` = ln 11 - realized. `skillTop1` = top-1 accuracy - 1/11.
- Seen vs unseen animals (embedding fallback = mean of trained animals) are always reported separately.

## Inference
The independent unit is the **test year**. Every estimate is averaged within test year first; across years we report the mean,
a t interval and an **exact sign-flip permutation p-value**. The per-year values are always plotted (forest plots). No bootstrap
(too few clusters). A mixed model on the split-level contrasts (random intercepts for test year and split; fixed effects for
log2 complexity, horizon, window length) is fitted when lme4 is available.

## Primary endpoints (fixed now)
- **P1 (H1, does not involve PreVal)**: is cross-validation a misleading measure of forecast quality? On the identical test
  strata, realized loss of the CV-trained forecast (FutureTainted) minus the reference CV (Internal), pooled over complexity
  and splits. Prediction: > 0 (forecasting is harder than the CV reference suggests). Secondary: the CV regime's own reported
  error against its forecast error (FutureTainted optimism = realized - reported; carries the early-stopping selection bias,
  see LIMITATIONS.md) and how much more optimistic CV is than PreVal (difference in optimism; descriptive).
- **P2 (H2)**: paired difference in realized loss on the identical test strata, PreVal - Tainted and PreVal - Internal, pooled.
  Prediction: <= 0.
- **P3 (H3, comparative)**: does adding covariates hurt LESS under PreVal than under the CV comparators? Per split, the slope of
  realized loss on log2(number of covariates) is fitted for each regime; the primary quantity is the slope DIFFERENCE,
  slope(PreVal) - slope(Tainted) and slope(PreVal) - slope(Internal), pooled over splits. Prediction: < 0 (a smaller complexity
  penalty under PreVal). This is a comparison between regimes and does not require any regime to be above chance.
  Secondary: each regime's own slope with a two one-sided equivalence test against +/- 0.005 loss per doubling (absolute
  "no penalty" claim) and the penalty versus the simplest model by complexity level.
Secondary (descriptive): contrasts per complexity, per horizon and window length, same-information contrast (test strata of
animals PreVal saw in training), selection regret and realized loss of the complexity chosen by each regime's reported loss,
penalty versus the simplest model, top-1 skill, training behaviour (best epoch, stopped by patience or cap).

## What would count as refuting each hypothesis (fixed in advance)
- H1 fails if P1 is not positive (t interval across test years includes 0 or is negative).
- H2 fails if P2 is positive for either comparator with the interval excluding 0. If PreVal is only worse overall but not on
  the same-information subset, that is reported as an information-deficit effect, not as support.
- H3 is not supported if the slope difference PreVal - comparator is not negative for both comparators (t interval across
  test years includes 0 or is positive). The absolute claim "no complexity penalty under PreVal" needs the equivalence test
  and is reported as such; skill above chance is always REPORTED next to it but does not gate the comparative claim.

## Changes in the amendment (and why)
1. Bootstrap over ~8 test years -> exact sign-flip test and t interval (bootstrap with so few clusters is unreliable).
2. H3 headline `trainTestGap` slope dropped: PreVal stops early (low epoch), the random-split regimes train longer, so their
   training loss is lower by construction; the gap is not comparable across regimes. Kept only as an unlabelled secondary column.
3. Equivalence test against the margin added (it was announced but not implemented); the primary H3 test is the
   comparative slope difference between regimes (amendment 2, same day, before results were read: an earlier draft gated H3 on
   PreVal skill above chance, removed because the comparison between regimes is what matters).
4. H1 primary contrast = FutureTainted forecast loss minus the reference CV (Internal) on identical test strata; PreVal does not
   enter H1 (amendment 3, same day, before results were read, after Tati corrected the first operationalisation).
5. Selection regret: regimes compared by paired differences, not separate bootstraps.
6. Added: horizon and window-length analyses, same-information contrast, top-1 skill, training-behaviour table, mixed model.
7. 20-covariate level and the cap-extension pass.

## Amendment 4 (2026-10-08, AFTER the first look at the caribou first-pass results; disclosed as such)
The pre-specified H3 primary test (slope of realized loss on log2 covariates, per split) was null for PreVal minus FutureTainted
(0.000) because the status-quo curve rises from 2 to 5-10 covariates and then partly returns, so a straight line has no slope.
The straight-line slope was a poor summary of a non-monotone curve and is kept in the output, labelled as pre-specified.
New H3 estimands (v2), paired within split, defined for the caribou second pass AND, before any data exist, for the bird and
simulated-tree datasets:
- **END** = loss(most complex) - loss(simplest); **PEAK** = mean loss of the intermediate levels - loss(simplest).
- Each regime's END and PEAK, and the differences PreVal - comparator, summarised (a) per test year (mean across years, exact
  sign-flip, conservative) and (b) with a mixed model on all splits (random intercepts for test year and split; regime x
  complexity interaction = difference in penalty; Wald intervals), plus per-split medians and shares.
- Complexity effect by forecast horizon (END difference by horizon bin and the mixed model with a horizon interaction).
- Primary confirmatory claims for the new datasets: PEAK(PreVal) - PEAK(status quo) < 0 and END(PreVal) - END(status quo) < 0.
What the caribou first pass already shows (exploratory, chosen after seeing the figure): status-quo PEAK about +0.013 to +0.022
(67% of splits above 0) versus about 0 for PreVal; mixed-model difference in penalty +0.021 (k=5) and +0.023 (k=10), intervals
excluding 0; at the most complex level the difference is small overall and emerges at long horizons (+0.011 at horizon 5, not
significant). These are hypotheses to be confirmed by the caribou replicates (new initialisation seeds) and the other datasets.

**Mechanism diagnostic (added with amendment 4).** Every model now also logs the future-year test loss after each epoch
(`diagLoss`). It is diagnostic only: it never enters stopping, scheduling or selection, and a test shows the fit is identical
with and without it. Figure 10 shows, per regime and complexity, whether validation loss keeps improving while the
future-year loss worsens (the hypothesis for why the status quo overfits). Together with the 300-epoch cap this tests the
truncation explanation. Models trained before this change (first pass) do not have it.

**Do complex models pay off under PreVal? (added after the first look; exploratory for the caribou first pass).**
Table `H3_shape_per_regime`, measure END, regime FutureUnseen: realized loss at 30 minus 2 covariates = -0.0052 [-0.0098, -0.0007]
across 8 test years, 7 of 8 years negative, exact sign-flip p = 0.039 (not corrected for the several shape questions asked).
A small benefit (about 0.005 in loss, on top of a total skill over chance of roughly 0.015-0.02), not a large one.
Pre-specified for the caribou replicates/second pass, birds and simulated trees: (a) one-sided END(PreVal) < 0 with the
per-year interval and the mixed model; (b) the loss of the complexity CHOSEN by PreVal's own validation loss minus the loss of
the simplest model (table `H3_penalty_vs_simplest` / selection tables). The simulated-tree data can additionally vary the TRUE
complexity of the generating process, which is the cleanest test of whether complexity pays when it truly exists.

## Follow-up experiment A: does the status-quo hump follow the covariates or the count? (pre-specified before any data)
Run after the second pass, on the same splits (temporal arm, 120 splits), for the two forecasting regimes only (PreVal and the
status quo; random CV is not a forecast and is never compared with PreVal). Covariate sets (R/featureSets.R): `habitatOnly`
(18 habitat covariates, the movement/interaction covariates removed = the ablation), `habitatFirst`, `movementFirst`, `randomA`,
`randomB`; levels 2, 5, 10, 20 (18 for habitatOnly); the main importance-ordered ladder is the reference.
Outcome per set: PEAK and END of the penalty curve (same definitions as amendment 4) per regime, and PreVal minus status quo.
Predictions that would support each reading: hypothesis B (step-length interactions carry non-transferable signal): hump absent in
`habitatOnly`, present and early in `movementFirst`; hypothesis C (stable habitat signal pulls the curve back): the return at high
counts appears when habitat covariates are added last; "it follows the count (effective capacity/training time)": the hump
appears in every ordering at similar levels. Any other outcome is reported as it is. 1 replicate per cell; exploratory.

## Sensitivity (pre-specified)
1. Spatial arm. 2. `matchAnimals = FALSE`. 3. Seen-animal-only test loss (same-information contrast). 4. Cap-50 versus converged.
5. Replicates (3 initialisations) on a random 20% of splits. No pre-2013 runs.

## Provenance
Every seed (splits, models, spatial blocks, global model, importance) is written to `seedRegistry.csv`; every model has a
`_provenance.rds` (seeds, RNG kind, R/torch versions, host, SLURM ids); the run has `runProvenance_*.rds`.
