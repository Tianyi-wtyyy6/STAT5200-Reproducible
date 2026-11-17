# Outputs and reference results

`research/original/results/run_2f629de521a827b8/` is the immutable uploaded
reference. Its logs and validation claims describe earlier executions, not a new
reproduction. `research/original/models/` contains those original fits/predictions.

All new computations go to `results/runs/` (ignored by Git), or an explicit NEW
`--output` directory. Existing output directories are rejected. Keep the printed
path; the wrapper's `validation.json` records mode, source commit, package versions,
platform, elapsed time, status and errors. `metadata/current_run.txt` inside that
workspace identifies the newly computed result directory. Run IDs are hashes of
code/config/data/model-package versions, not chronological run numbers.

- `smoke_checks.csv`: 9 checks on synthetic data and the empirical input hash.
- `results/<run_id>/run_status.csv`: every registered case and its outcome.
- `effect_summary.csv`, `annual_estimates.csv`: effects and yearly predictions.
- `donor_weights.csv`, `predictor_weights.csv`, `did_offsets.csv`: fitted components.
- `optimization_diagnostics.csv`: solver status, warnings, retries and cache use.
- `validation_*.csv`, `paired_method_comparisons.csv`: held-out prediction results.
- `placebo_*.csv`, `robustness_summary.csv`: placebo and sensitivity results.
- `acceptance_checks.csv`, `batch_acceptance_checks.csv`: scientific invariants.
- `T1_*.csv` through `T4_*.csv` and `S_*.csv`: 28 display tables.
- `figures/<run_id>/{main,supplement}/`: 59 PDF/PNG figure pairs plus build hashes.
- `figure_table_index.csv`: exact output paths and input-result mapping.
- `reproduction_comparison.csv`: original verifier's scientific comparisons at
  absolute tolerance 1e-6; failures are reported, never converted to passes.

The full computation starts with no copied model or result caches. Identical fits
may be reused within that new execution (e.g. shared main/placebo cases); the
acceptance tests explicitly refit with caches disabled. The `figures` mode instead
copies frozen results/fits to an isolated workspace and redraws them. It does not
validate fresh estimation. PDF byte identity is not expected across renderers.
