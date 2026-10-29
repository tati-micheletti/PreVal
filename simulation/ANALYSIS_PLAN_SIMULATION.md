# Controlled simulation: does covariate flipping (and not just capacity) make PreVal beat the status quo?

Written 2026-10-29, updated the same day with Tati's decisions (four ladder orders, 5 worlds per cell, realism anchor, regime flips). Status: DESIGN AND GENERATOR ONLY. The generator is written and unit-tested, and the engine accepts its tables. No simulated world has been trained, so every prediction in section 5 is written before any result exists.
Companions: `birds/ANALYSIS_PLAN_BIRDS.md` (amendments 31–33), `caribou/ANALYSIS_PLAN_CARIBOU_NN.md` (section 7). Files: `simulation/simulate_world.R`, `simulation/test_simulate_world.R` (30 checks, all pass).
The older folder `Simulated Data/` (smurfs fleeing villains) is an agent-based movement simulation and is not used here. This simulation produces route counts in the birds' table format, so it plugs into the birds engine unchanged. A movement version with the same factors can follow for caribou.

## 1. Why a simulation
In the real data flipping is confounded. The caribou flipping group is also the movement group, and it tracks the collar fix schedule. In birds the flippers are weak and rarely reach the ladder. PreVal's gain in caribou is smallest at 2 covariates, where flippers are most concentrated, which the flipping idea alone does not predict. Only a world where we set how many covariates flip, how strongly, how much they matter, how much route memory exists and how novel the later years are can separate flipping from capacity (many covariates, much to overfit). The simulation is a separate, labelled controlled experiment, never mixed with real data. It shows what each protocol does under stated mechanisms, not that nature behaves like this.

## 2. The generating model (`simulateWorld`)
- **Routes and years:** 1,200 routes in the birds' EPSG:3035 box, surveyed in 2005–2024 with route-specific probability (mean 0.5), about 12,000 route-years. Columns as in `final_<species>.rds`: `SITE_ID` (character), `year` (integer), `Reviere` (count), `x`, `y`, then numeric covariates named `sim_cov_001…` in shuffled order. The name never reveals the family.
- **Counts:** negative binomial, `mu = exp(b0 + eta)`, size 3, intercept set so the mean count is 3.9.
- **eta = route + stable + drift + flip + year**, with target shares of var(eta) of 0.20 / 0.30 / 0.10 / 0.25 / 0.05. The shares are exact by construction.
- **Route effect:** 60% smooth spatial field (150 km correlation length) plus 40% independent. This is the source of route memory, the leak of random validation and what PreVal's validation year rewards when it contains seen routes.
- **Covariate families** (each = static route part + year-wide shift + route-year noise):
  - stable (12): constant effect.
  - drift (6): strength changes linearly over the years, same sign.
  - flip (12): effect in year t = `c_j (sqrt(1−φ²) + φ s_j(t))`, with `s_j(t)` a ±1 sign. `flipMode = "random"`: each flipper has its own sign chain that changes with probability `flipProb` per year. `flipMode = "regime"` (added 2026-10-29): ONE regime chain shared by all flippers with mean regime length `regimeLength` years (warm and cold spells), each flipper tied to it with a random fixed sign; closer to weather and climate. φ = 0 is a stable family (the matched control). φ = 1 is a pure flip with mean zero. φ = 0.85 keeps a persistent part.
  - noise (10–50): no effect.
- **Year fingerprint (`yearShare`, 0.25):** a year-wide shift that lets a model recognise a year. Random validation can then learn year-specific signs. With 0 the signs average out.
- **Novelty (`trend`):** shift of dynamic covariates over the series in SD units.
- **Common random numbers:** worlds that differ in one parameter share routes, covariates and noise (tested), so the matched comparison is exact.
- **Truth kept:** `trueBeta` per covariate and year, `flipSigns`, `contributions`.

## 3. Factorial design
Reference world R: 60 covariates, shares as above, flipProb 0.3, yearShare 0.25, trend 0, route share 0.20.

**Stage 0 (realism anchor, before stage 1):** see section 3a.

**Stage 1a (same-places design, PreVal vs status quo), F1 × F2 × F3 = 3 × 2 × 4 = 24 cells × 5 replicate worlds = 120 worlds, 30 noise covariates. Stage 1b (F4: noise 10 vs 50) only if stage 1a leaves capacity unresolved:**

| Factor | Levels | Separates |
|---|---|---|
| F1 flip strength φ | 0 (control), 0.85, 1.0 | (a) strength of the sign change |
| F2 flip share of var(eta) | 0.10, 0.30 | (b) importance in the training years |
| F3 ladder ORDER | four orders: natural (the engine's own ranking), flippers first, flippers last, random (all covariates incl. noise) | (b) whether flippers enter the ladder at all, and whether the order of the covariates matters |
| F4 noise covariates | 10 vs 50; ladder 2, 5, 10, 20, all | (c) capacity (stage 1b only, see below) |

Observation made while building (ranking only, no regime comparison): the engine's ranking demotes flippers as φ grows. Flipping covariates among the top 10 (of 12): φ 0: 5; φ 0.85: 1; φ 1: 2. In the natural ladder flippers are ignored until the ladder reaches "all". F3 therefore compares the natural ladder with three manipulated orders (flippers first, flippers last, random). The manipulated orders exist only in the simulation, where the truth is known; the real-data ladders always stay natural. The ladder levels are 2, 5, 10, 20 and all covariates, so at "all" every order contains the same covariates and the orders can only differ below it.

**Stage 2 (one factor at a time from R with φ 0.85, flip share 0.30, 30 noise, ladder = natural and flippers first), 5 replicates:** route share 0.05 / 0.20 / 0.35; trend 0 / 1.5 SD; `yearShare` 0 / 0.25; flipProb 0.1 / 0.3 / 0.5; driftRate 0 / 1; **flip rule: random per-year flips versus regime flips (`regimeLength` 3 / 6 / 10 years)**. About 12 extra cells.

**Stage 3 (decision table):** R and four extreme cells, with all four regimes, in both the same-places and new-places designs.

All worlds use the engine's normal pipeline: strict pilot (years ≤ 2010), per-window ranking, 100 km blocks, equal training size, calibrated loss, forecast years 2012–2024, horizons 1 and 5.

## 3a. Realism anchor (stage 0; Tati agreed 2026-10-29)
A simulation is only a valid test bed if it can reproduce what the real data did. Before stage 1:
1. **Null anchor.** Calibrate a φ = 0 world to the greenfinch (route share of the variance, mean count and dispersion, number of surveyed routes per year, covariate structure of the natural ranking) and run PreVal, status quo, spatial and PreVal + spatial in the same-places and new-places designs on 5 worlds. It must reproduce the qualitative real pattern: PreVal about equal to the status quo in the same places (within ±0.03), spatial blocking best in new places (order PreVal + spatial ≤ spatial < PreVal < status quo), H1 positive. If it cannot, the generator is changed (route share, year fingerprint) and the change is logged BEFORE any flip world is read; no flip world is run until the anchor passes.
2. **Flip anchor.** Calibrate flip frequency and amplitude to the per-year coefficient changes in the real transferability screen of the flipping species (Grünspecht, Fitis, Hausrotschwanz or Wacholderdrossel, once the species is chosen; for caribou, the screen in `results/covariate_transferability_caribou.csv`). The calibrated flip world is reported next to the factorial cells so a reader can see where nature sits in the design.
3. The calibration targets and the tolerances are fixed in this file before any simulated model is trained.

## 4. Analysis (the engine's own)
`analyzeRegimes` per world (PreVal − status quo by level and horizon, H1, shape per regime, slope on log level), spatial contrasts in stage 3, and novelty (DI). **Primary quantity:** `DiD = (PreVal − SQ in the flip world) − (PreVal − SQ in the matched φ = 0 world)`, per forecast year, averaged over replicate worlds. The unit of inference is the forecast year (13 years): estimate, t interval, nNegative/nPositive, exact sign-flip test. Replicates are averaged inside a year and never counted as extra years. The equivalence margin is 0.005. **Decision table:** per cell the lowest-loss protocol, called "best" only if it beats the next by more than the margin, otherwise "tie".

## 5. Predictions (before any run)
- **S1 (capacity alone is not enough):** at φ = 0, |PreVal − SQ| < 0.02 at every level with 10 or 50 noise covariates.
- **S2 (flipping works if flippers are in the ladder):** oracle ladder, φ ≥ 0.85, flip share 0.30: PreVal − SQ ≤ −0.03 at levels ≥ 10, DiD ≤ −0.02, negative in at least 11 of 13 years.
- **S3:** the natural-ladder DiD is smaller than the oracle one at levels ≤ 20 and approaches it only at "all".
- **S4 (dose-response):** |DiD| grows with φ and with flip share.
- **S5 (mechanism):** with `yearShare` = 0 the DiD disappears (|DiD| < 0.01).
- **S6 (route memory):** a larger route share makes PreVal worse at horizon 1, whatever φ.
- **S7 (novelty):** with trend 1.5 the DiD is larger in later forecast years, and the slope of PreVal − SQ on log(DI) is negative for φ > 0 and about zero for φ = 0.
- **S9 (order of the covariates):** with φ ≥ 0.85 and flip share 0.30, the DiD at levels 5 and 10 is largest (most negative) with flippers first, smallest with flippers last (near 0, because the flippers stay out of the ladder), and the natural and random orders lie in between; at "all" the four orders agree within 0.01. If the order does not matter below "all", flipping works through the whole covariate set and not through the ladder.
- **S10 (regime versus random flips):** regime flips (one shared chain, mean length 6) give a DiD of the same sign as random flips; the size may differ. A DiD that disappears with regime flips would mean the effect needs fast, independent sign changes, which is less like weather and climate.
- **S11 (anchor):** the null anchor reproduces the real greenfinch pattern (section 3a) before any flip world is read.
- **S8 (governing complexity):** the status quo's loss rises more with level in flip worlds than at φ = 0; PreVal's curve stays flat.

**Support for "flipping causes PreVal's advantage":** S2, S4 and S5 together. **Support for "capacity alone":** S1 fails, so PreVal − SQ is negative and grows with noise and level at φ = 0 while the DiD is about zero. **Mixed:** both nonzero, report the shares. **Neither:** all contrasts within the margin, so the caribou gain has another source (non-independence of animals and years, step structure, collar schedule). Any outcome is reported as found.

## 6. Size and compute
Horizons 1 and 5, forecast years 2012–2024, five levels give 159 splits and 1,590 models per world (PreVal + status quo), or 3,180 with four regimes (counted with `generatePlan`). Time per model is 1.1 s with a tiny network in the smoke run. Expect about 10–15 s on EVE with the birds' network.
- Stage 0 (anchor): 5 worlds × 4 regimes × 2 designs, about 32,000 models, about 110 CPU-hours.
- Stage 1a: 120 worlds, about 190,800 models, about 640 CPU-hours (about 13 hours at 48 tasks). Stage 1b (noise factor, 24 cells × 5 worlds more) is run only if capacity is unresolved.
- Stage 2: about 12 cells × 5 worlds = 60 worlds, about 95,000 models, about 320 CPU-hours.
- Stage 3: 15 worlds × 2 designs, about 95,000 models, about 330 CPU-hours.
- Total without stage 1b: about 410,000 models and 1,400 CPU-hours, roughly 30 hours at 48 tasks. Run stage by stage. After stage 1a we look at S1–S5 and S9: if the main question (does flipping explain PreVal's advantage, and does the order matter) is clear, stages 2 and 3 can be made smaller, but stage 3 (the decision table) is expected to be wanted for the paper in any case.

## 7. Run so far (local, scratch only)
The 22 generator tests pass. A smoke run on one 700-route world through `buildCaseIndex`, `generatePlan` (all gates pass), ranking, `runModel` (PreVal and status quo, 2 levels) and `analyzeRegimes` worked with no engine change. No regime comparison was read.

## 8. Decisions
Taken by Tati on 2026-10-29: four ladder orders (natural, flippers first, flippers last, random); 5 worlds per cell; realism anchor and regime-flip variant added; stage 1a first, then decide on the rest.
Still open: whether a movement (caribou-type, built on the old smurfs movement scripts) generator follows; it is considered only after the birds-type result.
