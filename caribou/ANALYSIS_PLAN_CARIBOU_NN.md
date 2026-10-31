# Caribou through the standardised engine (preValNN) – SCOPED PLAN

Written 2026-10-29. Status: PLAN ONLY (no code, nothing run, nothing committed). Companion documents: `ANALYSIS_PLAN.md` (caribou refit plan, covers `caribouNN`) and `birds/ANALYSIS_PLAN_BIRDS.md` (amendments 18–32).

## 0. Why, and what we already know
Caribou was run with its own modules (`caribouNN`, `caribouNN_Global`), not with the engine used for birds (`preValNN`). The birds now have regimes caribou lacks:
- PreVal + spatial blocks
- PreVal on unseen routes (here: unseen animals)
- a new-places test arm
- per-window covariate ranking on training years only
- refit
- the covariate-novelty (DI) analysis

Running caribou through the same engine makes the two datasets comparable cell by cell. It also tests the flipping-covariate idea (birds amendment 32) in a dataset that seems to have flipping covariates naturally.

Existing caribou runs (old engine, all read, so NOT blind):
- **Design:** temporal arm, 138 splits, 36 windows, test years 2015–2022 (8 years), horizons 1–8, ladder 2/5/10/20/30, 3 replicates, 6,210 models, all converged.
- **H1:** +0.034, 8/8 years.
- **PreVal minus status quo:** −0.031, 8/8 years, at every level. The PreVal curve is flat.
- **Status-quo curve:** humps at 5–10 covariates. PEAK PreVal−SQ is −0.015 (8/8, p = .008). END is not significant.
- **First spatial arm (3 years):** PreVal−SQ −0.044 (3/3).

So for these contrasts the new run is a **replication on a re-engineered pipeline**. Only the new regimes, the new-places arm, the per-window ranking and the novelty analysis are blind tests.

A transferability screen already exists (`results_secondpass/covariate_heterogeneity_clogit.csv`, per-year conditional-logit coefficients over 10 years). The step-length and turn-angle terms and all step-length interactions flip sign (I² 0.93–0.995). Most end-of-step habitat effects keep their sign: paved-road distance, wetland and mixed forest do not flip. The nine interactions are mechanically tied to `logSl`, so this is about three independent groups, not 19 flips. The top of the caribou ladder (`logSl` and its interactions) is therefore the flipping group. This is the natural contrast to greenfinch and woodpecker.

## 1. What the engine needs
The engine assumes one case = one row with a covariate vector and a scalar response. Caribou has one case = one stratum: 1 used step + 10 available steps, with the response "which step was used" (conditional likelihood).

| Piece | Birds | Caribou |
|---|---|---|
| Case | route-year | stratum (11 steps); case index: unit = animal id, year, x/y = step start |
| Repeating unit | route | animal (137 animals; unseen animals use the mean embedding) |
| Response / loss | NB count, per-case NLL | softmax over the 11 steps, per-stratum cross-entropy (chance = log 11 = 2.398) |
| Calibration | dispersion re-estimated on own validation | optional: temperature re-estimated on own validation (D3) |
| Store | matrix [cases × features] | tensor [strata × 11 × features] (exists: `strataStore.R`) |
| Network | MLP + route intercept | 128-64 MLP scoring each step, 8-d animal embedding concatenated (exists: `stratumNet.R`) |
| Splits / gates / plan | regime builders, equal training size | unchanged (they only read the case index) |
| Spatial blocks | from `blockSize.rds` | existing 100 km + 10 km buffer; check needed (D5) |
| Ranking | per window, permutation importance | same rule; permute a covariate across all 11 steps of a stratum together |
| Novelty (DI) | reference = PreVal's training cases | same; case vector = mean over the 11 steps (D4) |
| Analysis | `analyzeRegimes`, `compareRefit`, `compareAddedRegime`, `novelty.R` | unchanged except the loss column; forecast year = unit |

**Regimes:**
- PreVal (FutureUnseen)
- status quo (FutureTainted)
- spatial CV (FutureTaintedSpatial)
- PreVal + spatial (FutureUnseenSpatial)
- PreVal on unseen animals (FutureUnseenGrouped)
- Internal, only as the H1 leaky reference and never compared with PreVal
- refit variants of PreVal, status quo and PreVal + spatial

**Arms:**
- (a) same animals, temporal (comparable with the first and second pass)
- (b) new places: test strata in blocks that no regime trains or validates on
- (c) refit
- (d) unseen-animals variants of (a) and (b)

"Route information off" corresponds to the animal embedding off.

**Ladder:** the natural importance ranking of all 30 candidates per window (levels 2, 5, 10, 20, 30). No manipulated orderings in the main design. The earlier feature-set experiments stay supplementary.

The engine's rule is equal training size. The old refit also equalised the animal set (`matchAnimals`), so there is a small design change (D6).

## 2. Reused and new pieces (line estimates)
**Reused unchanged:**
- engine: `caseIndex.R`, `regimeSplits.R`, `spatialTestSplits.R`, `verifyRegimes.R`, `generatePlan.R`, `analyzeRegimes.R`, `novelty.R`
- the refit logic
- the EVE scripts (birds pattern, unique run-env file)
- `post_analysis_birds.R` pattern

**Reused from caribou:**
- `prepareNNdata.R` (30 candidates, disturbance-clock recode with cap 40, z-clip)
- `strataStore.R`
- `stratumNet.R`
- the extracted-features backup table and `masterIdMap.csv`

**New:**
1. `modules/preValPrep_Caribou` (~150): final table from the backup table via `prepareNNdata`; case index and 3-D store; drops the 924 first-step strata; start year 2013.
2. Stratum adapter in `adapters.R` (~120): conditional-logit head, per-stratum cross-entropy, `summarise`, optional temperature calibration.
3. Engine hooks (~100–150 changed lines in `fitNet.R`, `runModel.R`, `rankCovariates.R`, `novelty.R`): 3-D store, slice, scale and score, selected by an adapter flag so the birds code path stays byte-identical. All 163 tests must still pass. This is the biggest unknown.
4. `caribou/runMe_caribou.R` and EVE scripts (~100): stages design, rank, train, analyze.
5. `caribou/transferability_screen.R` (~120).
6. Tests (~250):
   - the adapter loss equals the old `stratumNet` loss on a toy set
   - the new splits match the old temporal split sizes
   - the gates pass
   - a small end-to-end chain test
   - unseen animals get the mean embedding (the original stranger bug stays fixed)
7. Plan amendments and a caribou figures script.

Total: about 850 new lines plus about 150 changed.

## 3. Pre-specified predictions (freeze before any engine run)
Scale: per-stratum cross-entropy. The forecast year is the unit (8 years, exact sign-flip; the smallest two-sided p is 2/256 = 0.008, so only 8/8 or 7/8 agreement can be significant). Negative = first regime better.

- **P1 (replication, not blind):** same-animals arm, PreVal−SQ is about −0.03 at every level, negative in 8/8 years; H1 about +0.03. If the pooled PreVal−SQ falls outside −0.03 ± 0.01, find the engine difference before reading anything else.
- **P2 (shape, partly replication):** the status-quo curve rises from 2 to 5–10 covariates and does not fall back; PreVal stays flat or falls. PEAK and END contrasts are primary (amendment 4 of `ANALYSIS_PLAN.md`). "Does complexity stay low on loss under PreVal while the status quo rises" is answered by PEAK, END and the level of minimum loss per regime, in both the same-animals and the new-places arm.
- **P3 (blind):** new places, best to worst: PreVal+spatial ≤ spatial < PreVal < status quo, as in both bird species. This is a weak prior, because herds are few and clustered and blocks may remove whole herds from training.
- **P4 (blind):** PreValRoutes − PreVal < 0 in the new-places arm and > 0 at horizon 1 in the same-animals arm (birds amendment 31 carried over).
- **P5 (blind, flipping hypothesis, birds amendment 32):** PreVal's advantage is larger in splits where the top-k covariates are unstable.
  - Instability is measured only from the window's training years (share with a sign flip, mean I²).
  - It is regressed against the PreVal-over-SQ loss gain within each level; the slope is taken per forecast year, then sign-flip tested over years.
  - Warning: the second-pass gains by level are −0.021 at k = 2, −0.036 at k = 5 and −0.041 at k = 10. The smallest gain sits where the flipping group is most concentrated (k = 2 is `logSl` plus one interaction). That is not what the hypothesis alone predicts, and a flat or reversed slope counts against it for caribou.
- **P6 (blind):** novelty, as in birds amendments 27 and 29. The slope of (PreVal − SQ) on log(DI) is negative in the new-places arm at k = 20 and 30.
- **P7 (blind):** refit. PreVal-refit − SQ-refit is negative at all horizons, and refit helps PreVal at least as much as the status quo (birds: −0.033 / −0.043).

Cross-dataset flipping versus stable (greenfinch, woodpecker, caribou, up to four flipping bird species) is descriptive only. The paper must not call it an inferential test.

## 4. Transferability screen for caribou, before the models
**Purpose:** describe, once and in the same terms as the birds screen, which caribou covariates keep their effect across years. It runs before any engine result exists, so P5 cannot be tuned afterwards.

**What it does:**
- per year (2013–2022), fit the conditional logit (used versus the 10 available steps, strata = stratum id)
- run it for every covariate on its own and for the joint top-30 model, with standardised covariates and animal-clustered robust SEs
- report per covariate the per-year effect, I², the share of years with the opposite sign, and the same restricted to animals present in at least 3 years
- rank by the global importance order and report the flip share in the top 2, 5, 10, 20 and 30, as the birds screen did

**Three checks, because a flip may not be ecology:**
- **(a) Fix rate per year:** step length and turn angle depend on the collar schedule. A flip in `logSl` from a schedule change is a data effect, which still matters for forecasting but must be named correctly.
- **(b) Animal composition:** 2012 was mostly northern herds, so a year-to-year change can be composition, not time. The "animals in ≥ 3 years" version separates the two.
- **(c) Interaction terms:** they are tied mechanically to `logSl`, so their flips are one flip counted nine times. Report the number of independent flipping groups (movement, interactions, end-of-step habitat).

**Output:** `results/covariate_transferability_caribou.csv` plus a one-paragraph reading. Cost: a few CPU hours, no GPU.

## 5. Risks and unknowns
1. **Ladder length:** exactly 30 candidates. The "more than 50" extension needs re-extraction, and the raw layers are not on the Drive. Whether re-extraction is feasible is unknown. The birds claim at 157 covariates cannot be matched for caribou.
2. **Sentinel covariates:** `timeSinceHarvest_*` and `timeSinceFire_*` are year − 1900 for most strata.
   - `prepareNNdata` already recodes them to a cap of 40 years (the published iSSA rule). The engine run must go through that function and carry the recode report.
   - Only 42 strata have a real harvest value, so standardised harvest can give huge z-scores; the clip rule stays.
   - Check once that no covariate remains a year counter after the recode.
3. **Power:** 8 forecast years versus 14 in birds. The new-places arm will lose splits (the old spatial arm kept only horizon 1 and 3 years), so expect fewer than 138; the count is known only after the design stage.
4. **Few herds:** 137 animals. New places may mean new herds. PreVal on unseen animals removes animals that are in the last year and earlier years, so it may remove much of the training data and trigger minimum-size gate skips.
5. **Calibration:** temperature scaling on own validation may remove part of the overconfidence that is the status quo's overfitting signal in caribou (its forecasts were worse than chance). Report both versions.
6. **Engine change risk:** touching `fitNet.R` or `runModel.R` can break birds reproducibility. Mitigation: adapter flag, birds path untouched, all tests, and a check that an old birds model reproduces.
7. **Reproducibility:** in the second pass, replicate 1 reproduced k = 2 bit for bit but not k ≥ 5 (float differences across EVE nodes). Record host, torch and R versions per model.
8. **Unseen-animal embedding:** the original stranger-embedding bug is already fixed in the refit code, and the adapter must keep that handling.

**Model count and compute** (estimated from the second pass: 6,210 models took about 7 h wall at 100 parallel):
- same-animals arm: 138 splits × 6 regimes × 5 levels = 4,140 per replicate
- new-places arm: up to 138 × 5 × 5 = 3,450
- refit: about 2,100
- total about 10,000 models for one replicate, about 12–15 h wall at 100 parallel
- plus 36 window rankings on up to 200,000 strata each; the cost is unknown, likely 30–90 min per window
- 1 replicate in all new arms; the second pass already shows stability to training noise

## 6. Ordered steps
1. Decisions D1–D7.
2. Transferability screen (section 4), written up before any engine run.
3. Freeze section 3 as numbered amendments, including D3 and D4.
4. Build `preValPrep_Caribou`, the stratum adapter and the engine hooks. Run all tests plus the old-versus-new equivalence check on a few real splits.
5. Design stage for the same-animals arm and the new-places arm. Check split counts and skips.
6. Rank stage (36 windows) on EVE, then train arm by arm (one submission at a time), then analysis and novelty.
7. Compare with the old caribou numbers (P1) and report differences honestly.
8. Then figures, the cross-dataset table, and the simulation (smurfs) for the causal claim.

## 7. What this plan does not claim
It does not claim that flipping covariates cause PreVal's advantage. In caribou the flipping group is also the movement group. Step-start covariates are constant within a stratum, and the animals change across years. The observation fits the idea but cannot prove it. Only the simulation with controlled stable, drifting and flipping covariates can, and it is planned after caribou.

## 8. Decisions for Tati
- **D1:** the full standardised engine (~850 new lines) or only the missing regimes added to `caribouNN`. The latter is smaller but leaves caribou not cell-by-cell comparable with birds. Recommendation: the engine.
- **D2:** keep the original network (128-64, 8-d embedding) without a new pilot. A strict pilot would only have years up to 2014. Recommendation: keep it and state it.
- **D3:** primary loss uncalibrated (comparable with earlier caribou results) with calibrated as sensitivity, or the reverse (consistent with birds). Recommendation: uncalibrated primary, calibrated next to it.
- **D4:** stratum novelty vector = mean over the 11 steps (recommended) or observed step only.
- **D5:** new-places block size: keep 100 km + 10 km buffer, or derive from an autocorrelation check as for birds (caribou Moran's I was null in the old audit).
- **D6:** equal training size only (engine rule), or also the same animal set across regimes (old `matchAnimals`).
- **D7:** whether covariates beyond 30 are wanted, if re-extraction is feasible.

## 9. Decisions taken (Tati, 2026-10-29)
- **D1:** the full standardised engine (about 850 new lines). **D2:** keep the original network (128-64, 8-d animal embedding, learning rate 0.01 halved on plateau, batch 128, epoch cap 50, best validation epoch kept); it was never tuned, so no pilot is needed. **D3:** raw (uncalibrated) loss is primary; the temperature-calibrated loss is reported next to it. **D4:** the novelty vector of a stratum is the mean over its 11 steps. **D5:** keep 100 km blocks with a 10 km buffer (a sensitivity check only if reviewers ask). **D7:** no re-extraction; the ladder is the 30 candidates (comparable, not identical, to the birds).
- **D6 (checked in the old code):** PreVal and the status quo already used exactly the same strata and sizes (no random trimming); `matchAnimals` applied only to Internal. The replication arm therefore uses the old design as it was (full pools, matched Internal); the new arms follow the engine rule. The birds get the same design as a "full-pool" arm (birds amendment 33).
- **Order:** transferability screen -> freeze predictions -> engine hooks and tests -> old-versus-new equivalence on a few real splits (acceptance test: the replication arm must reproduce the old numbers up to float noise) -> design, rank, train, analyse. The smurfs simulation runs in parallel with the engine build.

## 10. Amendment of 2026-10-30 (written before any engine run on caribou; the engine and the acceptance test were built locally)
**Naming.** From now on "PreVal" = the regime `FutureUnseenGrouped`: validation on the LAST year AND on whole animals absent from training in every year (birds plan, amendments 34-35). The regime `FutureUnseen` (last year, animals still in training) is built ONLY for the replication arm (the old design, preset `replication`) and is called "future-only validation"; it is not a comparison regime in the main and new-places arms. The old caribou results (PreVal minus status quo about -0.03 in 8 of 8 years) are therefore results of the future-only validation.
**Settings correction.** The sections above quote the module defaults of the old caribou code (learning rate 0.01, no early stop). The old runs used Adam learning rate 0.001 (halved on plateau), batch 128, early stop after 10 epochs without a new best validation loss and a cap of 50 epochs (first pass) or 300 (second pass) (`results_firstpass/designParams.rds`). The engine uses exactly these (`caribouNetworkSettings()`, `PREVAL_EPOCHS` 300, `PREVAL_PATIENCE` 10); the network (128-64, 8-d animal embedding) is unchanged.
**Predictions, stated before any run of the engine on caribou.**
- P1' (replication, not blind): in the replication arm future-only validation minus status quo is about -0.03 (within +-0.01) in 8 of 8 forecast years and H1 about +0.03. A larger difference is an engine difference to be found before anything else is read (acceptance test: `caribou/acceptance_old_vs_engine.R`).
- P3' (blind, new places): PreVal (unseen animals) is not worse than PreVal + spatial blocks and is better than spatial CV and than the status quo; the median best epoch of PreVal is shorter than half of the status quo's (birds: 9-14 against 50-56 epochs).
- P4' (blind): in new places the reported (validation) loss of PreVal is closer to its realised test loss than that of the status quo (birds: optimism -0.03 / +0.19 against +0.51 / +0.92).
- P2' (blind, same animals): PreVal minus future-only validation is positive at horizon 1 (the animal embedding of animals seen before helps a one-year forecast) and not positive at horizons of 5 years or more (birds, amendment 34).
- P5/P6 (flipping, novelty) as in section 3; the novelty reference is PreVal's training set (`FutureUnseenGrouped`), the case vector is the mean over the 11 steps. The caution of section 7 stands: the flipping group is the movement block and follows the fix schedule, so P5 cannot separate flipping from schedule change.
**Implementation notes (decisions taken while building, for the record).** (a) Covariate importance in the stratum layout shuffles the values of a covariate among the 11 steps INSIDE each stratum (permuting whole strata would keep the observed step in position 1 with its values and destroy nothing); the marginal association is the Spearman correlation with being the observed step. (b) The raw cross-entropy is the primary loss (folder `analysis`); the temperature-calibrated loss is in `analysis_calibrated`. (c) The animal numbering and the seeds of the old module give the same initial network (tested). (d) Refit regimes and the unseen-animals refit are not built for the stratum layout in this step.
