# Data contract and access

The sole empirical dataset is the public, aggregate state-year `smoking` panel
from edunford/tidysynth, used in Abadie, Diamond and Hainmueller (2010).
It contains 39 US states over 1970–2000 (1,209 rows); no individual-level data,
credentials, restricted records or confidential inputs are required.

The immutable input is `research/original/data/raw/smoking.rda`, an R binary
containing the object `smoking`. SHA-256:
`a946d153e8faee62e6deba7a16db6f4c1251d9e56d5b41de7e79a593eb8110d9`.
The source is https://github.com/edunford/tidysynth ; acquisition provenance is
in `research/original/metadata/data_provenance.json`. The recorded source commit
is `58eda583ea28645a3531cd769b7852ff504656df`; the package declares MIT licensing
(`docs/licenses/tidysynth-LICENSE`). Cite the original empirical paper and the
upstream data package. No login or paid data subscription is required.

Required raw columns:

| Variable | Meaning / units used by this analysis |
|---|---|
| state | State name |
| year | Integer calendar year |
| cigsale | Cigarette sales, packs/person/year |
| lnincome | Original logged real income field |
| retprice | Retail cigarette price, cents/pack |
| age15to24 | Population share aged 15–24, fraction |
| beer | Beer consumption, gallons/person/year |

`R/data.R` loads the raw object, converts to data.frame, assigns stable state IDs
in alphabetical order, sorts by state/year and exports CSV/RDS. It retains every
row and missing value; no imputation or listwise deletion is performed. Outcome
and price must be complete. Expected missing counts are 195 income, 390 age and
663 beer cells. Model-specific covariate windows must be complete. The canonical
CSV adds `state_id`; it is not an additional independent dataset.

The upstream Rd file has conflicting per-100,000 unit descriptions; the supplied
analysis uses the ADH paper's units as documented in `R/data.R`. We preserve the
uploaded numerical treatment and do not silently rescale any variable.

Each run copies the raw input into its own `data/raw/` and recreates
`data/processed/smoking_panel.csv`, `panel.rds` and `state_id_mapping.csv`.
The archived processed copies are reference artifacts, not inputs to a cold run.
The wrapper verifies the uploaded manifest before execution. If the raw file is
missing or changed, restore the original package rather than substituting a new
upstream version. The legacy download helper uses an unpinned `master` URL;
normal documented execution is offline and does not call it.

`tests/smoke.R` constructs 3 synthetic states over 31 years, with a known -10
postperiod effect. These demonstration results are never used as paper results.
