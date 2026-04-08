# SEPI — Socio-Economic Peacebuilding Index

A composite indicator measuring relative socio-economic conditions relevant to peacebuilding across Admin-1 regions in **Kenya**, **Somalia**, and **South Sudan**.

## Quick Start

Open the project in RStudio (`SEPI_R&D.Rproj`) and source the relevant entry point. Required packages are installed automatically.

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

Conflict columns are produced with a yearly suffix for each year in the configured date range. For example:

| Column | Description |
|--------|-------------|
| `total_fatalities_2025` | Total ACLED fatalities in 2025 per Admin-1 |
| `count_conflict_events_2025` | Total conflict events in 2025 per Admin-1 |
| `total_fatalities_per_1k_2025` | Fatalities per 1,000 population (2025) |
| `count_conflicts_events_per_1k_2025` | Conflict events per 1,000 population (2025) |

The same pattern repeats for every year from 2016 to 2025. Version JSON files and visualisations reference the `_2025` columns by default.

---

### `02_explore.R` — Indicator exploration and screening

Run this when evaluating new indicators, auditing the configured set, or reviewing v3 conflict weights. Set `version` at the top of the script to control which indicator definitions are used (use v1/v2 for pillar-based checks, v3 to audit `se_vars`).

It runs four analysis blocks in sequence:

| Block | What it does |
|-------|-------------|
| A. Candidate exploration | Surveys all available variables, cross-references data dictionaries, produces `outputs/candidate_report_{country}.csv` and correlation matrix plots |
| B. Indicator screening | Validates the configured indicator set against OECD Handbook quality criteria (coverage, variance, collinearity) |
| C. Internal diagnostics | Missingness, within-pillar Spearman correlations, Cronbach's alpha (requires a v1/v2 version with pillars defined) |
| D. V3 indicator selection *(optional)* | Runs `select_v3_indicators()` to identify conflict-correlated candidates — uncomment, review output, then update `se_vars` in the JSON |

---

### `03_run_sepi.R` — Compute SEPI, visualise, export

The main pipeline. Set `version` at the top and source:

```r
source("03_run_sepi.R")
```

Two optional blocks are included but commented out:
- **Sensitivity analysis** — leave-one-out indicator sensitivity (`sensitivity_all_countries()`)
- **Version comparison** — rank correlations across methodology variants (`compare_versions()`)

## Pipeline Steps (`03_run_sepi.R`)

| Step | What it does |
|------|-------------|
| 1. Load data | Reads the merged global CSV and splits by country |
| 2. Compute SEPI | Normalise indicators, aggregate into pillars, then into SEPI |
| 3. Conflict analysis | Spearman correlations between SEPI/pillars and ACLED conflict indicators |
| 4. Visualisations | Rankings bar chart, pillar heatmap, SEPI-vs-conflict scatter (per country) |
| 5. Export | Single Excel workbook with all results |

## Project Structure

```
SEPI_R&D/
├── 01_build_data.R         # Rebuild merged global CSV
├── 02_explore.R            # Indicator exploration and screening
├── 03_run_sepi.R           # Main pipeline entry point
├── versions/               # Methodology variants (one JSON per version)
│   ├── v1_equal_geometric.json
│   ├── v2_pca_geometric.json
│   └── v3_conflict_weighted.json
├── R/
│   ├── config.R            # Global paths, version loader
│   ├── utils.R             # Aggregation helpers, labels, ggplot theme
│   ├── load_data.R         # Data loading and country-specific cleaning
│   ├── normalise.R         # Min-max / z-score / rank normalisation
│   ├── compute_index.R     # SEPI computation engine
│   ├── diagnostics.R       # Data quality checks
│   ├── screen_indicators.R # Candidate indicator triage
│   ├── conflict_analysis.R # SEPI–conflict linkage (Spearman correlations)
│   ├── visualise.R         # Plot generation
│   ├── export_excel.R      # Excel workbook export
│   └── build_global_data.R # Merge per-country source files into global CSV
├── data/                   # Input data (not tracked — see .gitignore)
├── data_dictionnaries/     # Data dictionaries
└── outputs/                # Generated plots and Excel results
```

## Version System

SEPI versions are defined as self-contained JSON files in `versions/`. Each file specifies the methodology parameters and full country indicator definitions. To switch versions, change one line in `03_run_sepi.R`:

```r
version <- VERSIONS$v1_equal_geometric   # ← any key in VERSIONS
```

Adding a new version requires no R code changes — create a new JSON file and it appears automatically as `VERSIONS$<name>`.

| Version | Description |
|---------|-------------|
| `v1_equal_geometric` | Arithmetic mean within pillars, geometric mean across, equal weights |
| `v2_pca_geometric` | PCA-derived weights within pillars, geometric mean across |
| `v3_conflict_weighted` | Conflict-correlation weighted flat sum |

## Outputs

### Excel workbook: `outputs/sepi_results_<version>.xlsx`

| Sheet | Contents |
|-------|----------|
| README | Methodology description and interpretation guide |
| SEPI_Results | Pillar scores, SEPI score, and rank per region (all countries) |
| Indicator_Scores | Normalised (0–1), polarity-adjusted indicator values per region |
| Indicator_Details | Pillar–indicator mapping, polarity, labels, and effective weights |

### Plots (PNG)

Per country: `rankings_<country>.png`, `pillars_<country>.png`, `sepi_conflict_<country>.png`.

## Methodology (default: v1_equal_geometric)

1. **Normalisation** — Min-max scaling to [0, 1] with polarity alignment (negative-polarity indicators are inverted so higher always = better).
2. **Within-pillar aggregation** — Arithmetic mean of normalised indicators (equal indicator weights).
3. **Across-pillar aggregation** — Geometric mean of pillar scores (equal pillar weights). A small floor (0.001) prevents zero-products.
4. **Ranking** — 1 = best socio-economic conditions within each country.

## Dependencies

`tidyverse`, `readr`, `psych`, `ggrepel`, `openxlsx`, `purrr`, `rlang`, `jsonlite`, `sf`, `patchwork`, `httr2`
