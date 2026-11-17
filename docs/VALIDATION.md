> This report records validation of the computational package before extraction
> into this standalone repository. Commit IDs below belong to that earlier
> provenance and are not ancestors of this repository. The numerical code, data,
> wrapper and environment lock are unchanged. See `PUBLIC_PACKAGE.md` for tests
> performed after extraction. No manuscript files or original Git history were
> imported.

# Reproducibility validation report

## Scope and tested revision

The clean checkout was cloned independently with `git clone --no-hardlinks
--branch reproducible-code --single-branch`. A fresh R environment was installed
from `environment-linux-64.lock` (download cache reused; installed prefix and
library were independent). The tested code/lock commit is:

`62812354bbe0dd6a5aa725f4d0592e96274cef41`.

Later delivery changes are documentation and validation records only. The full
cold computation also started at that commit. All eight imported R modules,
original driver, configuration, data, reference results and historical provenance
are byte-identical to the uploaded package. Its manifest verifies 1,831 files;
the manifest itself is retained as the 1,832nd file. No estimator, numerical
tolerance, scientific assumption, or result was changed to obtain a pass.

## Environment and diagnosis

Tested: Debian 13, Linux x86_64, R 4.5.2, Netlib BLAS/LAPACK 3.11.0, Synth 1.1-10,
osqp 1.0.0, Matrix 1.7-6, optimx 2025-4.9, kernlab 0.9-33, rgenoud 5.9-0.11,
ggplot2 4.0.1. Exact builds and all 162 dependencies are in the binary lock.
Five CPU cores and about 33 GiB memory were visible; fitting was serial with
BLAS/OpenMP thread limits of one. Installed environment size measured about
1.7 GB. Peak memory was not measured; 4 GB RAM is only a planning estimate.

Initial OpenBLAS testing exposed a real reproducibility issue: all three main
fits completed, but the original verifier passed 61/93 checks and failed 32/93.
ADH selected a BFGS candidate (convcode 0), with mean gap -18.88610403177601,
instead of the reference -18.84561071629176. SCM_OUTCOME and DiD mean gaps matched.
The reference used R's bundled BLAS/LAPACK on RHEL; the initial conda environment
used OpenBLAS. A controlled environment change to the Netlib provider, with
research code and package versions unchanged, restored all 93 main comparisons.
The lock now pins this provider explicitly. The OpenBLAS partial full run was
stopped and is not counted as a completed full reproduction. Its main comparison
records are retained as diagnostic evidence, not as the final environment's result.

## Completed checks

| Check | Result | Measured elapsed time |
|---|---|---|
| Clean-clone synthetic smoke | 9/9 passed | 0.148 s inside wrapper, excluding R startup/package loading |
| Clean-clone uncached main fits | 93/93 original reference checks passed | 13.253 s inside wrapper |
| Clean-clone frozen-reference rendering | 250/250 checks passed; 59 PDF/PNG pairs and 28 display CSVs | 73.908 s inside wrapper |
| Display table comparison | 27/28 identical; `S_software.csv` correctly records the new installed software | included above |
| R parsing | entry point, smoke test and all 8 archived modules parsed | not timed |
| Existing-output protection | repeated output path rejected with nonzero status; earlier results retained | not timed |
| Git whitespace check | passed for new changes | not timed |

R run records and comparison CSVs are retained in `docs/validation/`. Original
pre-extraction commit identifiers remain in those records to preserve provenance.

## Full cold execution

Full cold Netlib execution **passed**, in 1,072.267 seconds (17.9 minutes),
including estimation, checks, figure/table generation and comparison. It started
without imported model or result caches and computed 362 distinct fits. Matching
training inputs could reuse a fit created earlier in that same execution.

- 375 registered cases: 373 successful, 2 explicitly inapplicable early ADH folds,
  0 failed and 0 remaining planned/running cases.
- 18/18 data-audit checks, 13/13 acceptance checks, and 5/5 batch checks passed.
- 250/250 original scientific value/structure comparisons passed at the unchanged
  absolute tolerance of 1e-6; the maximum reported numeric difference was 0.
- 59 PDF/PNG pairs and 28 display CSV tables were generated and checked nonempty.
- 27/28 display table contents match the reference; only `S_software.csv` differs,
  correctly describing the tested environment's software versions.
- 62 of 122 distinct ADH fits retain nonzero outer optimizer codes, matching the
  reference caveat. Successful output does not imply uniform convergence.

The complete execution log, comparison CSV, data/acceptance/batch checks, session
information and summary are saved under `docs/validation/full-*`. This validates
reproduction of the three implemented specifications and their registered
experiments in the locked Linux environment; it makes no cross-platform promise.

## Limitations retained

The test is Linux-specific. Historical ADH nonzero optimizer codes remain part of
the scientific result; reproducing a selected candidate does not prove global
optimization convergence. A run ID omits some native-library information; the
binary lock, commit and session records must accompany it. New R result paths
use the packaging driver's run ID and can differ from the original reference ID.

The original figure code emits manual-colour-scale warnings. They were not hidden
or "fixed" by changing the archived source. All figure files were generated;
the paired-donor RMSE figure was visually checked and its values and labels are
readable (points render gray). No claim of byte-identical graphics across fonts
or platforms is made. The software version table intentionally differs from the
historical one because some non-model R dependencies have different versions.

The smoke fixture is synthetic and does not constitute a new empirical or
simulation result. NNM/transfer-learning, Synthetic DiD and DiD-adjusted SCM remain
unimplemented future work. No required empirical dataset or model code is missing
for the three implemented specifications. The original manuscript repository was not changed during extraction.
