# Caribou covariate transferability screen

Descriptive screen from section 4 of `ANALYSIS_PLAN_CARIBOU_NN.md`. It runs BEFORE any engine run, so the flipping prediction (P5) cannot be tuned afterwards. It changes nothing in the caribou runs.

## What it does
For each of the 30 candidate covariates and each year 2013–2022 it fits a conditional logit (used step versus its 10 available steps, strata = stratum id) on the standardised covariate, with animal-clustered robust standard errors (`survival::coxph` with `strata` and `cluster`; with one event per stratum this is the conditional logit). It also fits one joint model per year with all identifiable covariates. Across years it computes, per covariate: the weighted mean effect, the range, Cochran's Q, I², the number of years with each sign, and the number of years that are significantly positive or negative.

Covariates that are constant within a stratum (the nine `*_start` main effects) cancel out of a conditional logit. They are listed as "not identifiable" and act only through their interactions with `logSl`. That leaves 21 identifiable covariates of the 30.

## Inputs
- `results_firstpass/preparedModelInputs.csv` (3.1 GB, 3,218,369 rows, 292,579 strata, 126 animals): the output of `prepareNNdata` (`preparedDataFinal`). Disturbance clocks are already recoded to the 40-year cap; the script stops if they are not. The recode is not re-implemented. Only the z-scaling of `prepareNNdata::scaledCol` is copied (global mean and sd, clipped to ±10), marked in the script.
- `results_firstpass/featureTable.csv`: the global importance order of the first pass, used to define the top 2, 5, 10, 20 and 30. It is the first-pass ranking (all years), not the per-window ranking the engine will use. That is fine for a description.

## Definitions (fixed before any engine result)
- **Flip (primary):** at least 3 of the 10 years have the sign opposite to the majority sign.
- **Flip (strict):** at least one year significantly positive and one significantly negative (|z| > 1.96).
- **Unstable:** I² > 0.75.
- **Independent group:** a family of covariates with at least one flipping member: movement (step length, turn angle and ALL interactions with step length, which are tied to `logSl` by construction), land cover, disturbance clocks, distance to infrastructure.

## Variants (each is a full set of yearly fits)
- `all`: all animals.
- `ge3yrs`: only animals with strata in at least 3 different years. Separates a change in time from a change in which animals are present.
- `modalfix`: only strata with the commonest fix interval (8 h ± 0.5 h). Separates ecology from a change of collar schedule.

## Outputs (default folder `results/`)
- `covariate_transferability_caribou.csv`: one row per covariate × model (single, joint) × variant.
- `…_byYear.csv`: every yearly estimate, with robust SE and z.
- `…_topk.csv`: flip and instability share in the top 2, 5, 10, 20 and 30.
- `…_check_fixrate.csv` and `…_check_fixrate_cor.csv`: fix interval, step length and turn angle per year, and the rank correlation of the yearly `logSl` effect with them.
- `…_check_composition.csv`: animals, new animals and herd composition per year.
- `…_check_groups.csv`, `…_check_groups_total.csv`: flipping covariates by family and the number of independent flipping groups.
- `…_check_movement_variants.csv`: the movement terms in the three variants side by side.

## How to run
Locally (about 30 min on 8 cores for the full data):

```
Rscript caribou/transferability_screen.R
```

On EVE, with the same script (set `CARIBOU_PREPARED` to the table's path and `CARIBOU_CORES` to the cores of the job; it needs about 6–8 GB):

```
CARIBOU_PREPARED=/path/to/preparedModelInputs.csv CARIBOU_CORES=8 Rscript caribou/transferability_screen.R
```

Other settings: `CARIBOU_FEATURETABLE`, `CARIBOU_OUT`.

## What a flip can mean
A flip is not automatically ecology. Step length and turn angle depend on the fix interval, so a change of collar schedule across years can flip `logSl` without any change in behaviour (check a). A change of which animals are collected can flip an effect through composition (check b; the `ge3yrs` variant). The interactions with step length carry the `logSl` flip nine times (check c). Read the flip counts together with these checks.
