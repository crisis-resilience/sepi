# SEPI — Socio-Economic Peacebuilding Index

A composite indicator measuring relative socio-economic conditions relevant to peacebuilding across Admin-1 regions in **Kenya**, **Somalia**, and **South Sudan**.

## Quick Start

Open the project in RStudio (`SEPI_R&D.Rproj`) and run:

```r
source("run_sepi.R")
```

The pipeline will install any missing packages automatically (including `openxlsx` for Excel export).

## Pipeline Steps

| Step | What it does |
|------|-------------|
| 1. Load data | Reads country CSVs from `data/` and standardises columns |
| 2. Diagnostics | Missingness, within-pillar correlations, Cronbach's alpha |
| 3. Compute SEPI | Normalise indicators, aggregate into pillars, then into SEPI |
| 4. Conflict analysis | Spearman correlations between SEPI/pillars and ACLED conflict indicators |
| 5. Visualisations | Rankings bar chart, pillar heatmap, SEPI-vs-conflict scatter (per country) |
| 6. Export | Single Excel workbook with all results |

## Project Structure

```
SEPI_R&D/
├── run_sepi.R              # Main pipeline entry point
├── R/
│   ├── config.R            # Indicator-to-pillar mappings & version definitions
│   ├── utils.R             # Aggregation helpers, labels, ggplot theme
│   ├── load_data.R         # Data loading and country-specific cleaning
│   ├── normalise.R         # Min-max / z-score / rank normalisation
│   ├── compute_index.R     # SEPI computation engine
│   ├── diagnostics.R       # Data quality checks
│   ├── conflict_analysis.R # SEPI–conflict linkage (Spearman correlations)
│   ├── visualise.R         # Plot generation
│   └── export_excel.R      # Excel workbook export
├── data/                   # Input CSVs (one per country)
├── data_dictionnaries/     # HTML data dictionaries
└── outputs/                # Generated plots and Excel results
```

## Outputs

### Excel workbook: `outputs/sepi_results_<version>.xlsx`

The filename includes the version name (e.g. `sepi_results_v1_equal_geometric.xlsx`) to support comparison across methodology variants.

| Sheet | Contents |
|-------|----------|
| README | Methodology description and interpretation guide |
| SEPI_Results | Pillar scores, SEPI score, and rank per region (all countries) |
| Indicator_Scores | Normalised (0–1), polarity-adjusted indicator values per region |
| Indicator_Details | Pillar–indicator mapping, polarity, labels, and effective weights |

### Plots (PNG)

Per country: `rankings_<country>.png`, `pillars_<country>.png`, `sepi_conflict_<country>.png`.

## Methodology (default version: v1_equal_geometric)

1. **Normalisation** — Min-max scaling to [0, 1] with polarity alignment (negative-polarity indicators are inverted so higher always = better).
2. **Within-pillar aggregation** — Arithmetic mean of normalised indicators (equal indicator weights).
3. **Across-pillar aggregation** — Geometric mean of pillar scores (equal pillar weights). A small floor (0.001) prevents zero-products.
4. **Ranking** — 1 = best socio-economic conditions within each country.

New versions can be defined in `R/config.R` via `create_version()` to test alternative weighting, aggregation, or normalisation schemes.

## Dependencies

`tidyverse`, `readr`, `psych`, `ggrepel`, `openxlsx`
