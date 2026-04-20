# SEPI — Methodology
### Version: `conflict-correlation weighted`

The **Socio-Economic Peacebuilding Index (SEPI)** is a composite indicator measuring the structural socio-economic capacity of sub-national regions to sustain peace. It is grounded in the Humanitarian–Development–Peace Nexus: durable peace requires a foundation of socio-economic sufficiency, and structural deficits — poverty, food insecurity, poor health access, environmental degradation — act as force multipliers for instability.

SEPI is computed at the **Admin-1 level** for **Kenya**, **Somalia**, and **South Sudan**. Scores are relative within each country; a high score means a region is performing better than other regions in the same country, not that it meets any global standard.

---

## Data Sources

All datasets are harmonised to OCHA COD administrative boundaries. The latest available year per source is used.

### Somalia

| Domain | Candidate indicators | Source | Reference year |
|--------|---------------------|--------|---------------|
| Food Security | Population fraction in IPC Phase 3+ | Integrated Food Security Phase Classification (IPC) via HDX HAPI | 2024 |
| Education | Literacy rates; school attendance (primary & secondary); % no formal education | Somalia Integrated Household Budget Survey (SIHBS) | 2022 |
| Health | Health facilities per 10k/100k population (by facility type) | WHO health facility database | 2024 |
| Income & Livelihoods | Poverty headcount; extreme poverty headcount; per capita expenditure; food expenditure share | Somalia National Bureau of Statistics (SNBS) | 2023 |
| Accessibility | Population within 1-hour of healthcare; within 5 km of school | Heidelberg Institute for Geoinformation Technology | 2025 |
| Climate / Environment | NDVI; PDSI; soil moisture; FAPAR | Earth Engine Data Catalog | 2024 |
| Conflict *(weighting only)* | Conflict events per 1,000 population; total fatalities per 1,000 population | ACLED API | 2016–2025 |

> Middle Juba is excluded. Al-Shabaab territorial control makes reliable survey data inaccessible, and its inclusion would skew rankings.

### Kenya

| Domain | Candidate indicators | Source | Reference year |
|--------|---------------------|--------|---------------|
| Food Security | Population fraction in IPC Phase 3+ | Integrated Food Security Phase Classification (IPC) via HDX HAPI | 2025 |
| Education | Primary & secondary net attendance rates; gender parity index | Kenya Population and Housing Census / SIHBS education extract | 2019 |
| Health | Health facilities per 10k/100k population (by facility type) | Government of Kenya / WHO health facility registry | 2025 |
| Income & Livelihoods | Poverty headcount; extreme poverty headcount; MPI; Gross County Product per capita | Oxford Poverty & Human Development Initiative; Kenya National Bureau of Statistics (KNBS) | 2022 |
| Multidimensional deprivation | Nutrition; child mortality; sanitation; water; electricity; housing; assets deprivation | Global Multidimensional Poverty Index (MPI) | 2022 |
| Accessibility | Population within 1-hour of healthcare; within 5 km of school | Heidelberg Institute for Geoinformation Technology | 2025 |
| Climate / Environment | NDVI; PDSI; soil moisture; FAPAR | Earth Engine Data Catalog | 2023 |
| Conflict *(weighting only)* | Conflict events per 1,000 population; total fatalities per 1,000 population | ACLED API | 2016–2025 |

### South Sudan

| Domain | Candidate indicators | Source | Reference year |
|--------|---------------------|--------|---------------|
| Food Security | Population fraction in IPC Phase 3+ | Integrated Food Security Phase Classification (HDX HAPI) | 2024 |
| Education | Total enrolment; gender parity index; dropout rate; pupil-teacher ratio; share of teachers on payroll | National Education Census Report | 2021 |
| Health | Health facilities per 10k/100k population (by facility type) | World Health Organization health facility database | 2024 |
| Income & Livelihoods | Poverty headcount; average annual minimum consumption basket cost; daily labour wages | Republic of South Sudan Poverty and Equity Assessment; CLiMIS South Sudan | 2024 |
| Food prices & wages | Yearly average food price; non-agricultural and agricultural wages | World Food Programme | 2025 |
| Accessibility | Population within 1-hour of healthcare; within 5 km of school | Heidelberg Institute for Geoinformation Technology | 2025 |
| Climate / Environment | NDVI; PDSI; soil moisture; FAPAR | Earth Engine Data Catalog | 2023 |
| Conflict *(weighting only)* | Conflict events per 1,000 population; total fatalities per 1,000 population | ACLED API | 2016–2025 |

> Abyei is excluded from the South Sudan index. Its contested administrative status means it lacks reliable survey coverage across multiple domains, and its inclusion would distort regional rankings.

---

## Indicator Selection

Indicator selection is a **two-step process**: a manual screening step informed by exploratory analysis, followed by automated multicollinearity pruning.

### Step 1 — Manual screening (`02_explore.R`)

Running `02_explore.R` (Blocks A and D) produces Pearson correlation matrices between every candidate variable and two conflict proxies: ACLED conflict events per 1,000 population and total fatalities. Outputs are saved as per-domain PNG plots (food security, education, health, economic, accessibility, remote sensing).

The analyst reviews these plots and applies two rules to build the final candidate list:

- **Conflict relevance:** only retain variables that show a meaningful correlation with conflict intensity.
- **Within-domain redundancy:** when two variables from the same domain correlate strongly with each other (r > 0.8), keep the one with the stronger relationship to conflict.

The final variable list is set in `se_vars` inside the relevant version JSON (e.g. `versions/v3_conflict_weighted.json`). Differences in indicator selection across countries (e.g. `hospitals_per_100k_pop` for Somalia vs `health_fac_per_10k_pop` for South Sudan) reflect analyst judgement from inspecting the correlation plots.

> Block D of `02_explore.R` (`select_v3_indicators()`) can assist this step by programmatically ranking conflict-correlated candidates. It is commented out by default — uncomment, review the output, then update `se_vars` in the JSON.

### Step 2 — Automated multicollinearity pruning (`02_explore.R` Block B)

After the candidate list is set, `02_explore.R` Block B (indicator screening via `R/screen_indicators.R`) automatically removes any remaining pair with pairwise Pearson r > 0.8, retaining whichever variable has the lower average correlation across all other candidates. In South Sudan, `gender_parity_index` is explicitly protected from removal even if a high correlation is detected (`protected_vars` in the version JSON).

### Indicators selected for the SEPI

#### Somalia

| Pillar | Indicator | Label | Source | Year | Polarity |
|--------|-----------|-------|--------|------|----------|
| Food Security | `pop_frac_3plus` | Fraction of population in IPC Phase 3+ | Integrated Food Security Phase Classification (IPC) via HDX HAPI | 2024 | Higher = worse |
| Education | `percent_no_formal_education` | % with no formal education | SIHBS | 2022 | Higher = worse |
| Health | `hospitals_per_100k_pop` | Hospitals per 100,000 population | WHO health facility database | 2024 | Higher = better |
| Income & Livelihoods | `poverty_headcount_pct` | Poverty headcount (%) | SNBS | 2023 | Higher = worse |
| Accessibility | `healthcare_access_pop` | Population with healthcare access | Heidelberg Institute for Geoinformation Technology | 2025 | Higher = better |
| Climate | `rs_ndvi` | NDVI | Earth Engine Data Catalog | 2024 | Higher = better |

#### Kenya

| Pillar | Indicator | Label | Source | Year | Polarity |
|--------|-----------|-------|--------|------|----------|
| Food Security | `pop_frac_3plus` | Fraction of population in IPC Phase 3+ | IPC via HDX HAPI | 2025 | Higher = worse |
| Education | `net_attendance_total` | Secondary attendance rate (total) | Kenya Population and Housing Census / SIHBS | 2019 | Higher = better |
| Health | `health_fac_per_10k_pop` | Health facilities per 10,000 population | Government of Kenya / WHO health facility registry | 2025 | Higher = better |
| Income & Livelihoods | `poverty_headcount_pct` | Poverty headcount (%) | Oxford Poverty & Human Development Initiative | 2022 | Higher = worse |
| Income & Livelihoods | `gcp_pc` | Gross County Product per capita | Kenya National Bureau of Statistics (KNBS) | 2022 | Higher = better |
| Accessibility | `healthcare_access_pop` | Population with healthcare access | Heidelberg Institute for Geoinformation Technology | 2025 | Higher = better |
| Climate | `rs_soil_moist` | Soil moisture | Earth Engine Data Catalog | 2023 | Higher = better |
| Climate | `rs_fapar` | FAPAR | Earth Engine Data Catalog | 2023 | Higher = better |

#### South Sudan

| Pillar | Indicator | Label | Source | Year | Polarity |
|--------|-----------|-------|--------|------|----------|
| Food Security | `pop_frac_3plus` | Fraction of population in IPC Phase 3+ | IPC via HDX HAPI | 2024 | Higher = worse |
| Education | `gender_parity_index` | Gender parity index (enrolment) | National Education Census Report | 2021 | Higher = better |
| Health | `health_fac_per_10k_pop` | Health facilities per 10,000 population | WHO health facility database | 2024 | Higher = better |
| Income & Livelihoods | `poverty_headcount_pct` | Poverty headcount (%) | Republic of South Sudan Poverty and Equity Assessment | 2024 | Higher = worse |
| Income & Livelihoods | `annual_cmb_mean` | Average annual minimum consumption basket cost | CLiMIS South Sudan | 2024 | Higher = worse |
| Accessibility | `healthcare_access_pop` | Population with healthcare access | Heidelberg Institute for Geoinformation Technology | 2025 | Higher = better |
| Climate | `rs_soil_moist` | Soil moisture | Earth Engine Data Catalog | 2023 | Higher = better |
| Climate | `rs_ndvi` | NDVI | Earth Engine Data Catalog | 2023 | Higher = better |

---

## Index Construction

### 1. Missing value handling

Missing values are handled differently per country:

- **Somalia & Kenya:** all regions are retained. `pop_frac_3plus` missing → 0 (absence of IPC data treated as no crisis); all other numeric indicators missing → column mean across regions.
- **South Sudan:** rows with any missing value across the selected indicators are dropped via listwise deletion (`na.omit`).

### 2. Normalisation

All indicators are Min-Max scaled to [0, 1]:

```
I_norm = (x − min(x)) / (max(x) − min(x))
```

Direction is preserved at this stage; inversion is applied at the aggregation step. Normalisation is handled by `R/normalise.R`.

### 3. Directionality

Each indicator is assigned a polarity sign, defined in `bad_vars` inside the version JSON:

- **+1 (good):** higher normalised value → better peacebuilding conditions (e.g. health facility density, school attendance).
- **−1 (bad):** higher normalised value → worse conditions (e.g. poverty headcount, IPC Phase 3+ fraction).

### 4. Conflict-informed weighting

Each indicator's weight is the absolute Pearson correlation between its normalised values and the ACLED conflict intensity rate (events per 1,000 population) for 2025 (`count_conflicts_events_per_1k_2025`):

```
w_i = |cor(I_i, C)|
```

Weights are then normalised so they sum to 1. Indicators with no empirical relationship to local conflict receive near-zero weight; those most associated with instability receive the highest weight. This calibrates the index to the specific political economy of each country rather than imposing arbitrary equal weights.

The conflict column used for weighting is set via `conflict_col` in the version JSON. ACLED data is available for 2016–2025 with yearly columns (e.g. `count_conflicts_events_per_1k_2016` through `count_conflicts_events_per_1k_2025`), allowing the weighting year to be changed by updating `conflict_col` in the JSON.

### 5. Aggregation

The SEPI score for each region is the weighted sum of polarity-adjusted normalised indicators:

```
SEPI_region = Σ (sign_i × w_i × I_i,norm) / Σ w_i
```

The raw scores are rescaled to [0, 1]. A score of **1.0** represents the strongest socio-economic conditions relative to other regions in that country; **0.0** represents the highest fragility. Aggregation is handled by `R/compute_index.R` and triggered via `03_run_sepi.R`.

### 6. Granular subindicator scores

Beyond the composite score, `03_run_sepi.R` applies min-max normalisation to the full candidate indicator universe (`granular_vars` in the version JSON), not just the indicators selected for the index. These normalised values — with no polarity adjustment or weighting applied — are exported per country and compiled into the final workbook (`outputs/sepi_results_<version>.xlsx`) as the `Indicator_Scores` sheet. They represent the relative position of each region on each raw indicator on a 0–1 scale.

---

## Interpretation

SEPI scores are on a continuous 0–1 scale. A score of **1.0** indicates the strongest socio-economic conditions relative to other regions in the same country; **0.0** indicates the highest fragility. Scores are not comparable across countries.

---

## Key Limitations

- **Relative scores.** Rankings are within-country. A high score means better than peers, not good in absolute terms.
- **Correlation ≠ causation.** Weights reflect empirical association with conflict, not causal pathways.
- **Temporal lag.** Household surveys (SNBS, SIHBS) are typically 1–2 years old. Rapid security shifts may outpace the data.
- **Manual selection step.** The indicator list for each country was chosen by analyst review of correlation plots produced by `02_explore.R`. Selection rationale is not formally recorded in the pipeline; differences across countries reflect contextual judgement.
- **Data gaps in fragile areas.** Inaccessibility can affect survey precision in conflict-affected zones.
