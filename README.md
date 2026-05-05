# SEPI — Socio-Economic Peacebuilding Index

A composite indicator measuring relative socio-economic conditions relevant to peacebuilding across Admin-1 regions in **Kenya**, **Somalia**, and **South Sudan**.

## Quick Start

Open the project in RStudio (`SEPI_R&D.Rproj`) and source the relevant entry point. Required packages are installed automatically.

To run the full pipeline in one go:

```r
source("run_all.R")
```

---

## Pipeline Scripts

### `run_all.R` — Master pipeline runner

Runs all six scripts in sequence. Change `active_version` at the top to run the full pipeline for a different version. Scripts 05 and 06 always compare `v1_aligned_equal_geometric` against `v3_aligned_conflict_weighted` regardless of this setting.

---

### `01_build_data.R` — Rebuild the merged dataset

Run this when source CSVs, ACLED data, or remote sensing inputs have been updated. It merges all per-country source files into three shared assets consumed by the rest of the pipeline:

| Output | Description |
|--------|-------------|
| `data/sepi_indicators_all_countries_latest.csv` | Merged indicator data for all three countries |
| `data/sepi_indicators_metadata_all_countries.csv` | Indicator metadata (labels, directionality, sources) |
| `data/sepi_merge_qc_report.json` | QC report flagging merge issues |

You do not need to re-run this unless the underlying data changes.

#### ACLED credentials

ACLED data is fetched live from the API at runtime (2016–2025, all three countries). A `.env` file in the project root is required:

```
ACLED_EMAIL=your_email@example.com
ACLED_PASSWORD=your_acled_api_key
```

#### ACLED column naming

Conflict columns are produced with a yearly suffix for each year in the configured date range:

| Column | Description |
|--------|-------------|
| `total_fatalities_2025` | Total ACLED fatalities in 2025 per Admin-1 |
| `count_conflict_events_2025` | Total conflict events in 2025 per Admin-1 |
| `total_fatalities_per_1k_2025` | Fatalities per 1,000 population (2025) |
| `count_conflicts_events_per_1k_2025` | Conflict events per 1,000 population (2025) |

The same pattern repeats for every year from 2016 to 2025.

---

### `02_explore.R` — Indicator exploration and screening

Run this when evaluating new indicators, auditing the configured set, or reviewing v3 conflict weights. Set `version` at the top of the script to control which indicator definitions are used.

| Block | What it does |
|-------|-------------|
| A. Candidate exploration | Surveys all available variables, cross-references data dictionaries, produces `outputs/candidate_report_{country}.csv` and correlation matrix plots |
| B. Indicator screening | Validates the configured indicator set against OECD Handbook quality criteria (coverage, variance, collinearity) |
| C. Internal diagnostics | Missingness, within-pillar Spearman correlations, Cronbach's alpha (requires a version with pillars defined) |
| D. V3 indicator selection *(optional)* | Runs `select_v3_indicators()` to identify conflict-correlated candidates — uncomment, review output, then update `se_vars` in the JSON |

---

### `03_run_sepi.R` — Compute SEPI, visualise, export

The main pipeline. Set `version` at the top and source:

```r
source("03_run_sepi.R")
```

| Step | What it does |
|------|-------------|
| 1. Load data | Reads the merged global CSV and splits by country |
| 2. Compute SEPI | Normalise indicators, aggregate into pillars, then into SEPI |
| 3. Conflict analysis | Spearman correlations between SEPI/pillars and ACLED conflict indicators |
| 4. Visualisations | Rankings bar chart, pillar heatmap, SEPI-vs-conflict scatter (per country) |
| 5. Export | Single Excel workbook with all results |

---

### `04_evaluate.R` — Version comparison and criterion validity

Validates a version against external criteria and its robustness variants.

| Section | What it does |
|---------|-------------|
| A. Version comparison | Rank correlations across robustness variants declared in the version JSON |
| B. Criterion validity (IDP) | Spearman rho between SEPI and IOM IDP displacement density (H1: rho < −0.6) |
| C. Discriminatory capacity | ROC / AUC test: can SEPI identify displacement hotspots? (target: AUC ≥ 0.70) |
| D. Visualisations (displacement) | Scatter and ROC curve PNGs saved to `outputs/figures/criterion_validity/` |
| E. Criterion validity (conflict) | Parallel Spearman + AUC tests using ACLED conflict intensity across three time windows |

---

### `05_compare_versions.R` — V1 vs V3 head-to-head comparison

Always compares `v1_aligned_equal_geometric` against `v3_aligned_conflict_weighted` on rank stability, criterion validity (IDP), and discriminatory capacity (AUC). Produces a summary scorecard.

---

### `06_sensitivity_analysis.R` — SA1 and SA2 sensitivity analysis

Always runs for both `v1_aligned_equal_geometric` and `v3_aligned_conflict_weighted`.

| Analysis | What it does |
|----------|-------------|
| SA1 (indicator sensitivity) | Drops one indicator per multi-indicator pillar across all combinations; mean SEPI across combos = SA1 score |
| SA2 (pillar sensitivity) | Drops each of the five pillars in turn; mean SEPI across five runs = SA2 score |

Outputs:
- `outputs/sensitivity_analysis_comparison.xlsx`
- `outputs/figures/sensitivity/sensitivity_comparison_<country>.png`

---

## Project Structure

```
sepi/
├── run_all.R                   # Master pipeline runner (runs scripts 01–06 in sequence)
├── 01_build_data.R             # Rebuild merged global CSV
├── 02_explore.R                # Indicator exploration and screening
├── 03_run_sepi.R               # Main pipeline: compute, visualise, export
├── 04_evaluate.R               # Version comparison and criterion validity
├── 05_compare_versions.R       # V1 vs V3 head-to-head
├── 06_sensitivity_analysis.R   # SA1 + SA2 sensitivity analysis
├── versions/                   # Methodology versions (one JSON per version)
│   ├── v1_aligned_equal_geometric.json   # Default: equal weights, geometric across pillars
│   ├── v1_aligned_equal_arithmetic.json  # Variant: arithmetic across pillars
│   ├── v3_aligned_conflict_weighted.json # Conflict-correlation weighted flat sum
│   └── _template.json
├── robustness_checks/          # Robustness variants (referenced from version JSONs)
│   ├── v1_aligned_zscore.json
│   ├── v1_aligned_bod.json
│   ├── v3_aligned_zscore.json
│   └── v3_aligned_bod.json
├── R/
│   ├── setup.R                        # Shared package installs + source calls (used by 02–06)
│   ├── config.R                       # Global paths, version loader
│   ├── utils.R                        # Aggregation helpers, labels, ggplot theme
│   ├── load_data.R                    # Data loading and country-specific cleaning
│   ├── normalise.R                    # Min-max / z-score / rank normalisation
│   ├── compute_index.R                # SEPI computation engine
│   ├── sensitivity_analysis.R         # SA1 and SA2 sensitivity functions
│   ├── criterion_validity_conflict.R  # ACLED-based criterion validity functions
│   ├── conflict_analysis.R            # SEPI–conflict linkage (Spearman correlations)
│   ├── diagnostics.R                  # Data quality checks
│   ├── screen_indicators.R            # Candidate indicator triage
│   ├── explore_candidates.R           # Candidate exploration and correlation matrices
│   ├── visualise.R                    # Plot generation
│   ├── export_excel.R                 # Excel workbook export
│   └── build_global_data.R            # Merge per-country source files into global CSV
├── data/                       # Input data (not tracked — see .gitignore)
├── data_dictionnaries/         # Data dictionaries
└── outputs/                    # Generated plots and Excel results
```

---

## Version System

SEPI versions are defined as self-contained JSON files in `versions/`. Each file specifies the methodology parameters and full country indicator definitions. To switch versions, change one line in any pipeline script:

```r
version <- VERSIONS$v1_aligned_equal_geometric   # ← any key in VERSIONS
```

Adding a new version requires no R code changes — create a new JSON file and it appears automatically as `VERSIONS$<name>`.

| Version | Description |
|---------|-------------|
| `v1_aligned_equal_geometric` | **Default.** Arithmetic mean within pillars, geometric mean across, equal weights |
| `v1_aligned_equal_arithmetic` | Arithmetic mean both within and across pillars, equal weights |
| `v3_aligned_conflict_weighted` | Conflict-correlation weighted flat sum (no pillar structure) |

Robustness variants (declared inside version JSONs, always run alongside their parent):

| Variant | Description |
|---------|-------------|
| `v1_aligned_zscore` | v1 with z-score normalisation instead of min-max |
| `v1_aligned_bod` | v1 with Benefit of the Doubt (DEA) weighting |
| `v3_aligned_zscore` | v3 with z-score normalisation |
| `v3_aligned_bod` | v3 with Benefit of the Doubt weighting |

---

## Outputs

### Excel workbook: `outputs/sepi_results_v1_aligned_equal_weighted_raw_subindicators.xlsx` (main export)

| Sheet | Contents |
|-------|----------|
| README | Methodology description and interpretation guide |
| SEPI_Results | Pillar scores, SEPI score, and rank per region (all countries) |
| Indicator_Scores | Original raw sub-indicator values per region in source units |
| Indicator_Details | Pillar–indicator mapping, polarity, labels, and effective weights |
| Conflict_Data | Conflict event and fatality counts per Admin-1, including rates per 100,000 population |
| Pillar_Descriptions | Description of each pillar and its dashboard name |


### Plots (PNG)

Per country: `rankings_<country>.png`, `pillars_<country>.png`, `sepi_conflict_<country>.png`.

---

## Methodology (default: `v1_aligned_equal_geometric`)

1. **Normalisation** — Min-max scaling to [0, 1] with polarity alignment (negative-polarity indicators are inverted so higher always = better).
2. **Within-pillar aggregation** — Arithmetic mean of normalised indicators (equal indicator weights).
3. **Across-pillar aggregation** — Geometric mean of pillar scores (equal pillar weights). A small floor (0.001) prevents zero-products.
4. **Ranking** — 1 = best socio-economic conditions within each country.

## Dependencies

`tidyverse`, `psych`, `purrr`, `rlang`, `jsonlite`, `ggrepel`, `openxlsx`, `sf`, `patchwork`, `pROC`, `gt`, `rvest`, `caret`, `httr2`
