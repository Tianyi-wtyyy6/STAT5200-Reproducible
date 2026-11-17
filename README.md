# STAT5200: reproducible computation for the California tobacco-control study

This standalone repository contains the research code, public aggregate data,
frozen reference results and executable checks for reproducing the California
tobacco-control analysis. It has a new Git history and contains no manuscript,
slide deck, speaker notes or Overleaf synchronization configuration. LaTeX and
Python are not required for the computational workflows.

The analysis compares original-predictor synthetic control (SCM_ADH), outcome-only
synthetic control (SCM_OUTCOME), and equal-donor difference-in-differences (DID).
The empirical panel has 39 states observed over 1970–2000, with California treated
from 1989. Gaps are observed minus counterfactual sales, in packs/person/year.
See [docs/VALIDATION.md](docs/VALIDATION.md) for the actual validation outcomes and
limitations. A successful code run alone is not evidence of exact reproduction.

## Repository structure

| Location | Purpose |
|---|---|
| `scripts/reproduce.R` | Single safe entry point for smoke, main, full, and reference-figure runs |
| `tests/smoke.R` | Small synthetic test of SCM_OUTCOME/DiD, information boundaries, and data hash |
| `research/original/` | Entire supplied package, preserved byte-for-byte, including `R/`, `config/`, raw data, fits, reference results and historical logs |
| `environment.yml` | Linux conda specification, with R/model/plot/numerical libraries pinned |
| `environment-linux-64.lock` | Exact tested Linux x86_64 binaries and SHA256 checksums |
| `research/original/renv.lock` | Original 59-package R lockfile; retained separately from the tested binary environment |
| `data/README.md` | Source, permissions, variables, units and preprocessing contract |
| `results/README.md` | Expected outputs and distinction between fresh/reference results |
| `docs/CODE_MAP.md` | Script-to-paper-result map, inventory and missing future-work implementations |
| `docs/VALIDATION.md` | New execution report, tested commit and measured runtimes |
| `docs/import-provenance.json` | Uploaded archive checksum and original base commit |
| `results/runs/` | New, ignored execution workspaces; never uploaded as reference results |

The supplied R directory already separates data, models, experiments, checks,
summaries, figures and tables. It is retained rather than refactored into `src/`.
Likewise, `config/project.yml` remains the authoritative configuration. Uploaded
algorithms are backed up by the original ZIP and protected by their SHA256 manifest.

## System requirements and installation

Tested platform: Linux x86_64, Debian 13, R 4.5.2. Use Miniforge/conda or micromamba.
The binary lock pins Netlib BLAS/LAPACK and includes Cairo/PNG graphics support;
a system R installation or compiler is not required for this installation route.
A GPU is unnecessary. Fitting runs serially with BLAS/OpenMP threads set to 1.
Allow about 2 GB for the installed environment and several hundred MB per full run;
4 GB RAM is a planning estimate, not a measured minimum.

Clone the standalone code repository:

```bash
git clone https://github.com/Tianyi-wtyyy6/STAT5200-Reproducible.git
cd STAT5200-Reproducible
```

Exact Linux environment (the micromamba form was used in validation):

```bash
micromamba create -y -p "$PWD/.repro-env" -f environment-linux-64.lock
micromamba run -p "$PWD/.repro-env" Rscript --vanilla scripts/reproduce.R smoke
```

Optional conda equivalent (not executed here; micromamba is the verified installer):

```bash
conda create -y -p "$PWD/.repro-env" --file environment-linux-64.lock
conda run -p "$PWD/.repro-env" Rscript --vanilla scripts/reproduce.R smoke
```

These installers require access to `conda.anaconda.org`. After installation, all
four R workflows use bundled data and need no network, tokens or credentials.
Keep TLS and package-checksum verification enabled.

`environment.yml` also records the Linux solver specification. The exact Linux
build pins must not be used unchanged on macOS/Windows; those platforms have not
been validated. WSL2 Linux x86_64
can use the Linux workflow, but WSL2 itself was not tested. The upstream R lock can
also be restored using `renv` with R 4.5.2 and CRAN access; that source-install route
was not tested here. Do not run the legacy unlocked bootstrap installer when you
intend to reproduce the tested environment. Ancillary package versions can differ
between the original renv lock and the tested conda lock; the latter records every
binary exactly, including numerical-library builds.

## Data preparation

No manual transformation is needed. The wrapper verifies all original manifest
hashes before executing, then copies only the frozen raw RDA into a new cold-run
workspace. `R/data.R` recreates the canonical panel and audits its dimensions,
missingness and supplied benchmarks. See [data/README.md](data/README.md).
Do not replace the data with a newer `tidysynth` download without a separate audit.

## Quick-start smoke test

From the repository root after installation:

```bash
.repro-env/bin/Rscript --vanilla scripts/reproduce.R smoke --output results/runs/smoke
```

This creates 3 synthetic states, tests the known -10 effect with SCM_OUTCOME and
DiD, checks simplex weights and held-out-label isolation, rejects target outcomes
at the prediction interface, and verifies the empirical input hash (9 checks).
It does not fit ADH or reproduce the paper. `smoke_checks.csv` and `validation.json`
record the outcome. Every explicit output path must be NEW; choose another name
for a repeat run. No existing research output is deleted or overwritten.

## Main fits and complete reproduction

For a smaller empirical check, fit all three main specifications from raw data:

```bash
.repro-env/bin/Rscript --vanilla scripts/reproduce.R main --output results/runs/main
```

Run the complete empirical computation and reference comparison:

```bash
.repro-env/bin/Rscript --vanilla scripts/reproduce.R full --output results/runs/full
```

The full mode runs the original sequence: data audit, main fits, no-cache acceptance
checks, all remaining registered experiments, summaries, figures and display
tables. It starts without any uploaded model/results cache. Matching fits may be
reused within that fresh execution. Seed 5200, solver settings and numerical
methods remain exactly as supplied. Expected status counts are 375 registered,
373 successful and 2 intentionally inapplicable early ADH folds.

After computation, the original verifier compares scientific values and case keys
with `run_2f629de521a827b8` at absolute tolerance 1e-6. A nonzero exit status is
preserved on a failed driver, invariant, or reference comparison. Computed results
are retained for diagnosis even when comparison fails. Do not treat retained
outputs or an `experiments_complete` manifest alone as a passing reproduction.

The printed workspace contains `validation.json` and `reproduction_comparison.csv`.
Its `metadata/current_run.txt` identifies the result subdirectory. The full figure
and table inventory is `results/<run_id>/figure_table_index.csv`. Expected filenames
are described in [results/README.md](results/README.md).

### Command-line arguments

`Rscript --vanilla scripts/reproduce.R MODE [--output NEW_DIRECTORY]`

- `MODE`: `smoke`, `main`, `full`, or `figures`.
- `--output`: new output directory, absolute or relative to the calling directory.
  Default: unique timestamp/PID directory under the repository's `results/runs/`.
- `--help` or `-h`: print usage. Unknown or incomplete arguments fail.

The wrapper locates code relative to its own script, not a machine-specific path.
Archived expert tools retain their original interfaces: `run_all.R --config PATH
--stage {all,data,main,checks,experiments,summarize,figures,tables}`;
`verify_reproduction.R --candidate DIR --reference DIR --output CSV [--main-only]`;
`run_fresh.R [--stage {all,main}]`. See the preserved original README for details.
Use the root wrapper for normal execution to protect the archived inputs.

## Regenerate figures and tables

The full mode already regenerates figures/tables from NEW computations. To redraw
only the frozen reference results without fitting:

```bash
.repro-env/bin/Rscript --vanilla scripts/reproduce.R figures --output results/runs/reference-figures
```

This copies reference tables and fit objects to an isolated workspace, produces
59 PDF/300-dpi PNG pairs and 28 display CSV tables, and checks their presence and
reference values. This validates rendering, not fresh parameter estimation.

## Measured validation and runtime

The complete cold computation passed all 250 original reference comparisons in
1,072.267 seconds (17.9 minutes): 373 successful cases, 2 intentionally inapplicable
ADH folds and no failures. A clean-clone smoke test passed 9/9 checks; uncached main
fits passed 93/93 in 13.253 seconds; frozen-reference figure regeneration passed
250/250 in 73.908 seconds. These wrapper timings exclude initial R/package loading.
Hardware and full records are in `docs/VALIDATION.md`. Native BLAS choice matters:
the exact lock uses Netlib, which recovered the reference results without changing
research algorithms. All required data and implementations for these three
specifications are included. The future-work extensions remain outside this scope.

## Limitations and interpretation

- Native optimizer/library differences affect ADH solutions despite matching
  high-level package versions. An OpenBLAS installation failed 32/93 main reference
  checks; switching only the BLAS/LAPACK provider to Netlib passed all 93. Use the
  exact lock rather than accepting a default BLAS provider.
- The original reference disclosed 62/122 ADH selected fits with nonzero outer
  optimizer codes. Finite feasible outputs do not establish global convergence.
- California's postpolicy gap is an effect estimate, not observed prediction error
  for its missing untreated outcome. Clean placebos reuse donor-validation fits.
- NNM/transfer learning, Synthetic DiD, DiD-adjusted SCM and simulation experiments
  are future work with no supplied implementation. No substitutes were invented.
- Historical validation files inside the upload are retained as provenance and
  are distinct from the tests performed for this code package.
- A run ID does not encode every native numerical library. Keep the wrapper's
  commit/platform/package record and the exact binary lock alongside results.

## Troubleshooting

- Missing R/package: install the locked environment and call its Rscript, not a
  different system R. `.repro-env/bin/Rscript --version` should report R 4.5.2.
- Hash mismatch/missing file: restore the original archived file; do not change
  the expected checksum to force a pass.
- Existing output directory: choose a new `--output`; previous runs are preserved.
- Reference comparison failure: inspect `reproduction_comparison.csv`,
  `optimization_diagnostics.csv` and `logs/<run_id>/`. Preserve the tolerance and
  scientific settings; a new estimate is not automatically an implementation bug.
- Figure warnings: consult the captured log and `figure_table_index.csv`; verify
  nonempty output files. Renderer differences may prevent byte-identical PDFs/PNGs.
- Interrupted full run: retain the workspace. The legacy driver can resume cases
  within it, but that is a resumed run, not a new cold run. Re-run the verifier.
- Read-only user cache: set `XDG_CACHE_HOME` and `CONDA_PKGS_DIRS` to writable
  directories for installation; the workflow sets a per-run graphics cache.

This repository is independent of the manuscript repository and its Overleaf settings.
Third-party code/data attribution is in [docs/THIRD_PARTY.md](docs/THIRD_PARTY.md).
