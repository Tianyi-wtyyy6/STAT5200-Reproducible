# Code inventory and result map

The uploaded package lives under `research/original/`, retaining its original
layout and SHA256SUMS. Its numerical algorithms, seed (5200), optimization
settings, tolerances and scientific assumptions have not been edited. The root
`scripts/reproduce.R` adds isolation, manifest verification and execution checks.
The original ZIP remains outside the checkout as an additional local backup.

| Task | Source / function | Main result artifacts |
|---|---|---|
| Entry point / ordering | `run_all.R` | data → main → checks → experiments → summarize → figures → tables |
| Preprocessing / audit | `R/data.R::audit_data` | panel, data_audit, missingness, descriptive_statistics |
| Original-predictor SCM | `R/models.R::fit_adh` | Synth donor weights, V, optimization_diagnostics |
| Outcome-only SCM | `R/models.R::fit_outcome` | OSQP convex weights, objective and solver diagnostics |
| Equal-donor DiD | `R/models.R::fit_did` | training offset, equal weights, annual estimates |
| Registry and real-data experiments | `R/experiments.R` | 375 registered cases: 373 expected successes, 2 inapplicable early ADH folds |
| Tuning | `fit_adh` plus `config/project.yml` | outer predictor-weight optimization with Nelder–Mead/BFGS; preperiod loss selection; no newly added tuning |
| Evaluation | `common.R::error_metrics`, `experiments.R::case_metrics`, `summaries.R` | RMSE/MAE, mean gaps, pooled/macro summaries, placebo fractions, sensitivity ranges |
| Scientific tests | `R/checks.R`, `summaries.R::summarize_results` | leakage isolation, constraints, TWFE equivalence, case/status checks |
| Figures / CSV display tables | `R/figures.R`, `R/tables.R` | 59 figure pairs and 28 display CSVs |
| Fresh reference comparison | `scripts/verify_reproduction.R` | 93 checks for main; 250 for all results |
| Added synthetic smoke test | root `tests/smoke.R` | analytic known-effect, simplex, held-out isolation, interface rejection, raw checksum |

## Paper result mapping

All R paths below are relative to `research/original/`. The table maps the
implemented study results to their computational outputs. Manuscript-only layout
scripts and assets are outside this standalone repository.

| Paper result | Computation | Numerical outputs | R figures / display tables |
|---|---|---|---|
| Data and design | `data.R` | data_audit, descriptive_statistics | T1_data |
| ADH replication / donor composition | models + experiments main; summaries::published_comparison | annual_estimates, published_vs_replicated, donor_weights, predictor_balance | F1, F2a, F2b; T2, S_weights, S_balance, S_V |
| SCM/DiD comparison | three main fits | effect_summary, did_offsets | F3; T3_methods |
| California holdouts / 38 donor tasks | registry ca_validation + donor_validation; summarize_validation | validation_predictions, validation_case_metrics, validation_aggregate, paired_method_comparisons | F4a, F4b; T4, S_CA_validation, S_donor_validation, S_paired |
| Classic and clean placebos | registry classic_placebo; donor-validation reuse; summarize_placebo | placebo_summary, placebo_ranks, placebo_common_eligible, classic_clean_comparison | F5*, F6*; S_placebo, S_ranks, S_intersections, S_clean |
| Donor/window/predictor sensitivity | registry loo/training_window/predictor_deletion; summarize_robustness | loo_ranges, robustness_summary | F7a, F7b; S_LOO, S_windows, S_predictors |
| Timing diagnostics | registry time_placebo/near_policy/near_policy_diagnostic | robustness_summary | F7c; S_timing |
| Experiment inventory / numerical caveats | collect_cases, checks, summarize_results | run_status, optimization_diagnostics, acceptance_checks, batch_acceptance_checks | S_registry, S_status_summary, S_optimizer, S_qp |

The executable `figure_table_index.csv` supplies finer-grained figure-to-input
mapping, including every-state supplemental figure. All listed R outputs are
available through the root reproduction entry point.

## Missing implementations / limits

No empirical implementation of the proposed NNM/transfer-learning extension,
Synthetic DiD, or DiD-adjusted SCM is present. No original simulation study is
included. The manuscript identifies these as future work. The added synthetic
smoke fixture is a software check, not a substitute simulation experiment.
The uploaded R pipeline now supplies the previously missing model-fitting code;
old manuscript provenance statements describe the earlier document-only delivery.
