# preValNN (generic validation engine)

Dataset-agnostic engine for the PreVal experiment: disjoint splits for five validation regimes (PreVal, status quo, route-grouped CV,
spatial-block CV, leaky random reference) with equal training size, hard leak gates, plan and manifests, one model per cell, and the
pre-specified analysis. Everything that depends on the response type lives in an **adapter** (`R/adapters.R`: count with a
negative-binomial head, continuous with a Gaussian head; the caribou choice softmax is the next adapter to port).

Pipeline per dataset and species: `preValPrep_<Dataset>` (final table for modeling) -> `preValNN_Global` (network pilot on the pre-test
history, covariate ranking, block size) -> `preValNN` (design, train, analyze). Run with `birds/runMe_birds.R`.

Tests: `Rscript modules/preValNN/tests/test_splits.R` (splits and gates), `test_engine.R` (end to end, both adapters),
`test_ranking_blocks.R`, `test_analysis.R`.
