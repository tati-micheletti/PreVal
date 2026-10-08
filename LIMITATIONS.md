# Limitations of the caribou experiment and what they can do to the results

Written before any refit result was read. Each item: what it is, what it can do to H1 (CV under-reports future error),
H2 (PreVal is more honest and forecasts better) and H3 (PreVal: complexity does not cause overfitting), and what we do about it.

## A. Design limitations

1. **The "reported" error is also the early-stopping set.** Each regime reports the validation loss at its best epoch, and
   that same set chose the epoch. The minimum over epochs is optimistically biased in EVERY regime, including PreVal.
   *Consequence:* optimism = realized - reported is positive even for PreVal; "PreVal optimism = 0" must not be expected
   or tested. *Handled:* the primary H1 contrast is the DIFFERENCE in optimism between regimes (same bias in all), not
   optimism > 0. *Not handled:* the absolute size of the optimism. A fully clean version needs an untouched hold-out fold
   per regime (design change and rerun); proposed for the bird and tree datasets.
2. **PreVal's "reported" error refers to year e, the realized error to year T.** Year-to-year drift enters PreVal's
   optimism, by design (that is what a forecaster would see), but it makes PreVal's optimism noisier than the CV regimes'.
3. **PreVal trains on fewer animals and a shorter, older window than the comparators** (animals that first appear in its
   validation year are unknown to it). All regimes have the same strata and the same animals in their pools, so this
   handicap belongs to the method. *Consequence:* it can make PreVal look worse than it would with the same animal
   information. *Handled:* the same-information contrast (test strata of animals PreVal saw in training only) and the
   seen/unseen split are reported; if PreVal wins only there, say so.
4. **Bursts cannot be equalised across regimes** without changing what the regimes are (PreVal's validation year has its
   own bursts; random CV shares bursts between train and validation). Burst counts are reported (Table 1).
5. **Epoch cap.** The first pass used a 50-epoch cap; models that stopped at the cap are re-trained to convergence
   (cap 300, patience 10) in the second pass and the share of cap-stopped models is reported. *Consequence if ignored:*
   the random-split regimes could be under-trained, which would UNDERSTATE their optimism and overstate their test
   performance. *Handled:* extension pass; original results are kept (`*_finalDT_cap50.csv`) for a cap-sensitivity table.
6. **Covariate ranking uses all years, including the test years.** The ladder (2/5/10/20/30 covariates) is the same for
   every regime, so it is not a leak for the comparison, but PreVal is then not a pure forecast. *Consequence:* a
   reviewer can argue the ladder is informed by the future; effect expected to be equal across regimes. *Handled:* none
   in this run; a sensitivity ranking from years <= 2015 only is possible.
7. **Covariates are the measured environment of the test year** (nearest 5-year road layer, same-year fire). The
   experiment tests forecasting animal behaviour given the environment, not forecasting environmental change.
   Identical for all regimes.
8. **Spatial arm covers a subset** (horizon 1; test years 2018, 2020, 2022) with a 100 km block design and a 10 km buffer.
   *Consequence:* low power for spatial conclusions; block size is a choice. Reported separately, never pooled with the
   temporal arm.

## B. Statistical limitations

9. **About 8 independent test years (2015-2022) and overlapping windows.** Effective sample size is small, p-values are
   coarse (minimum exact sign-flip p with 8 years = 2/256), and an effect that exists in most years can still be
   "not significant". *Handled:* per-year points are always shown; exact sign-flip tests instead of bootstrap;
   no claim from a single year.
10. **Many contrasts.** Primary endpoints (fixed in advance): pooled H1 difference in optimism, pooled H2 paired
    contrast, H3 slope with the equivalence margin. Everything else is descriptive and labelled so.
11. **One network initialisation per cell.** Training noise is averaged over splits, not estimated separately.
    Replicates for a subset are possible (`nReplicates`).
12. **Cross-entropy over 11 candidates measures relative habitat selection, not absolute habitat quality.** The skill
    above chance is small (about 0.05-0.13 in loss, a few points of top-1 accuracy). *Consequence:* the claims that matter
    here are COMPARISONS between regimes (PreVal against the CV comparators), which stay valid when all regimes are close
    to chance. What a small skill limits is the ABSOLUTE statement "PreVal shows no complexity penalty" (a flat curve near
    chance is easy to obtain). *Handled:* the comparative slope difference is the primary H3 test; skill is reported next to
    every contrast so readers can judge the absolute statement themselves.

## C. Scope limitations

13. **One study system, one learner** (a small neural network with an animal embedding; learning rate 0.001, batch 128,
    no tuning per regime). Conclusions are about this learner and this system; the bird and simulated-tree datasets are
    there to test generality.
14. **The random-split comparators are deliberately the common practice** (stratum-level random splits), not spatially or
    burst-blocked CV. Part of the CV optimism is therefore due to within-burst and within-animal dependence rather than
    temporal drift; the experiment shows that CV is optimistic, not why. The spatial arm removes the spatial part.
15. **Direction of the main risks.** Items 1, 2 and 5 can change the SIZE of optimism but act on all regimes alike or
    understate CV optimism; item 3 can handicap PreVal; items 9 and 12 limit what can be concluded from small
    differences. None of them was chosen to favour any hypothesis.
