# STAT5200: reproducible code, data and computational results

This submission contains the complete computational analysis for California's
Proposition 99, with SCM_ADH, SCM_OUTCOME and DID. It includes every registered
validation, placebo and robustness case. No academic report, LaTeX document,
report compiler or document-review images are included.

Reference experimental run: run_2f629de521a827b8.
Expected cases: 375 registered, 373 successful, 2 early ADH validation folds
explicitly not applicable, 0 failed. All 38 donor removals and all 39 classic
placebo targets are retained. Figures comprise 59 PDF/300-dpi PNG pairs.

## 1. Restore the environment

Use R 4.5.2. In R, from this extracted directory:

~~~r
install.packages("renv", repos = "https://cloud.r-project.org")
renv::restore(project = ".", prompt = FALSE)
~~~

Actual versions include Synth 1.1-10, osqp 1.0.0, Matrix 1.7-6 and ggplot2
4.0.1. Complete versions are in renv.lock and metadata/sessionInfo.txt.
Source installation of native packages may require a C++17 compiler, GNU make
and platform graphics/font libraries. R's Cairo PDF graphics capability is
needed for vector figures. LaTeX and Python are not needed to run this analysis.

## 2. Run the complete computation

From this directory in a terminal:

~~~bash
Rscript --vanilla run_all.R --config config/project.yml
Rscript --vanilla scripts/verify_reproduction.R
~~~

The first command audits data, runs main fits, actually refits the leakage and
holdout tests with cache disabled, executes all registered cases, exports
full result tables, recreates every figure and exports the display-table CSVs.
It uses strictly matching provided model caches when permitted. It does not
create or compile documents. The second command compares scientific values
with the supplied original reference results, ignoring run IDs/timestamps.

The packaged driver removes only the document stage. All seven original
computational R modules and the configuration/data remain byte-for-byte
unchanged. Original driver and manifest are retained under provenance/.
R/tables.R extracts the numeric CSV table derivations without document code.
Therefore the code identity/run_id can differ from the original reference
even though numerical model inputs and specifications are identical.

## 3. Recompute from the original raw data with no model/results cache

~~~bash
Rscript --vanilla scripts/run_fresh.R
~~~

This creates a new fresh_runs/ workspace containing only code, configuration,
the supplied raw/canonical data and environment definition. It copies no
models, frozen predictions, case objects or results. Every permitted fit is
computed anew, and its output is compared with the supplied reference.
This full cold execution is slower than cache-assisted regeneration.
The workspace and comparison CSV are retained. To check the three main
models first, use:

~~~bash
Rscript --vanilla scripts/run_fresh.R --stage main
~~~

Re-running an interrupted command resumes saved cases. A case failure is
recorded while independent cases continue; it never fills missing predictions
with zero. Individual stages are data, main, checks, experiments, summarize,
figures and tables. For example:

~~~bash
Rscript --vanilla run_all.R --config config/project.yml --stage figures
~~~

## 4. Files

| Path | Contents |
|---|---|
| R/ | All computational modules, numerical checks, table and plot generation |
| run_all.R | Complete computation entry point |
| scripts/run_fresh.R | Cold execution without existing fits or results |
| scripts/verify_reproduction.R | Case-key and numerical comparisons |
| config/project.yml | Frozen design, seed, solver settings, windows, thresholds |
| data/raw/smoking.rda | Original 17,505-byte frozen data |
| data/processed/ | Complete 1209-row panel, RDS and stable state IDs |
| models/ | Every referenced fit/frozen prediction, including zero donor weights |
| results/run_2f629de521a827b8/ | Original full numerical outputs and per-case RDS objects |
| figures/run_2f629de521a827b8/ | All main/supplemental and every-state PDF/PNG figures |
| metadata/, logs/ | Provenance, actual statuses, warnings, tests and execution records |
| provenance/ | Original run identity and packaging/verification records |

New executions write results/<new_run_id>/ and figures/<new_run_id>/.
metadata/current_run.txt identifies that execution; the immutable original
reference ID is provenance/reference_run_id.txt.

Raw SHA256:
a946d153e8faee62e6deba7a16db6f4c1251d9e56d5b41de7e79a593eb8110d9

## 5. Interpretation and numerical qualifications

Gap = observed minus counterfactual, in packs/person/year. California's
1989-2000 untreated outcome is unobserved; post-gap RMS is a deviation
statistic, not measured counterfactual prediction error. True holdout errors
are separately exported in validation_predictions.csv. Clean placebos reuse
donor-validation fits; overlapping temporal tests are not independent evidence.

62 of 122 distinct ADH fits selected finite feasible outer candidates with
nonzero optimizer codes, including the main Nelder-Mead candidate. These
qualifications remain in diagnostics and logs. No global outer optimum or
ordinary single-treated-state regression significance is claimed.

CSV display tables T1-T4 and S_* are recomputed directly from full numerical
tables. RDS objects preserve X, scaling, V, all donor weights, offsets and solver
objects. The raw snapshot is never automatically overwritten.

FILE_MANIFEST.csv and SHA256SUMS list package contents. The ZIP contains only
regular files and has no external symlink or absolute-path requirement.

## 6. Validation of this delivery

The packaged entry point was actually run in an isolated workspace. Its 250
scientific value/structure checks passed, all 28 display CSV tables matched
the original, and all 59 PNGs were regenerated (59 byte-identical).
A separate cold run computed all three main fits without provided caches;
all 93 main-value/structure comparisons passed. Full-case cold rerunning
is available through scripts/run_fresh.R. Execution logs and comparisons are
in provenance/package_validation/.
