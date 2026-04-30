# SEPI — Robustness, Sensitivity, and Validity Assessment

This document covers the alternative versions tested during SEPI development, the robustness checks applied to those versions, sensitivity diagnostics, and external validity tests. The **core SEPI methodology** is documented in the project README.

## Additional Data Sources

The following data sources are used in this document and are not part of the main SEPI pillar computation documented in the README.

| Source | Used for | Coverage | Reference period |
|--------|----------|----------|-----------------|
| ACLED (Armed Conflict Location & Event Data) | Conflict-correlation weights in `v3_aligned_conflict_weighted`; external criterion in validity tests | Kenya, Somalia, South Sudan — Admin-1 | 2016–2025 (yearly) |
| IOM Displacement Tracking Matrix (DTM) | IDP origin displacement density — primary external validity criterion | Kenya (HDX HAPI), Somalia (DTM), South Sudan (DTM) | 2024–2026 (latest available per country) |

---

## 1. Alternative Version: Conflict-Correlation Weighting (`v3_aligned_conflict_weighted`)

This version assigns indicator weights based on each indicator's empirical correlation with local conflict intensity (ACLED). It was developed and evaluated as an alternative to the equal-weighted approach during index construction, before the final methodology was chosen. Data sources, administrative boundaries, and indicator selection logic documented here also underpin the final version.

### 1.1 Data Sources

All datasets are harmonised to OCHA COD administrative boundaries. The latest available year per source is used.

#### Somalia

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

#### Kenya

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

#### South Sudan

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

> Abyei is excluded. Its contested administrative status means it lacks reliable survey coverage across multiple domains, and its inclusion would distort regional rankings.

---

### 1.2 Indicator Selection

Indicator selection is a **two-step process**: a manual screening step informed by exploratory analysis, followed by automated multicollinearity pruning.

#### Step 1 — Manual screening (`02_explore.R`)

Running `02_explore.R` (Blocks A and D) produces Pearson correlation matrices between every candidate variable and two conflict proxies: ACLED conflict events per 1,000 population and total fatalities. Outputs are saved as per-domain PNG plots (food security, education, health, economic, accessibility, remote sensing).

The analyst reviews these plots and applies two rules to build the final candidate list:

- **Conflict relevance:** only retain variables that show a meaningful correlation with conflict intensity.
- **Within-domain redundancy:** when two variables from the same domain correlate strongly with each other (r > 0.8), keep the one with the stronger relationship to conflict.

The final variable list is set in `se_vars` inside the relevant version JSON (e.g. `versions/v3_aligned_conflict_weighted.json`). Differences in indicator selection across countries reflect analyst judgement from inspecting the correlation plots.

> Block D of `02_explore.R` (`select_v3_indicators()`) can assist this step by programmatically ranking conflict-correlated candidates. It is commented out by default — uncomment, review the output, then update `se_vars` in the JSON.

#### Step 2 — Automated multicollinearity pruning (`02_explore.R` Block B)

After the candidate list is set, `02_explore.R` Block B automatically removes any remaining pair with pairwise Pearson r > 0.8, retaining whichever variable has the lower average correlation across all other candidates. In South Sudan, `gender_parity_index` is explicitly protected from removal even if a high correlation is detected (`protected_vars` in the version JSON).

#### Indicators selected for the SEPI

**Somalia**

| Pillar | Indicator | Label | Source | Year | Polarity |
|--------|-----------|-------|--------|------|----------|
| Food Security | `pop_frac_3plus` | Fraction of population in IPC Phase 3+ | IPC via HDX HAPI | 2024 | Higher = worse |
| Education | `percent_no_formal_education` | % with no formal education | SIHBS | 2022 | Higher = worse |
| Health | `hospitals_per_100k_pop` | Hospitals per 100,000 population | WHO health facility database | 2024 | Higher = better |
| Income & Livelihoods | `poverty_headcount_pct` | Poverty headcount (%) | SNBS | 2023 | Higher = worse |
| Accessibility | `healthcare_access_pop` | Population with healthcare access | Heidelberg Institute for Geoinformation Technology | 2025 | Higher = better |
| Climate | `rs_ndvi` | NDVI | Earth Engine Data Catalog | 2024 | Higher = better |

**Kenya**

| Pillar | Indicator | Label | Source | Year | Polarity |
|--------|-----------|-------|--------|------|----------|
| Food Security | `pop_frac_3plus` | Fraction of population in IPC Phase 3+ | IPC via HDX HAPI | 2025 | Higher = worse |
| Education | `net_attendance_total` | Secondary attendance rate (total) | Kenya Population and Housing Census | 2019 | Higher = better |
| Health | `health_fac_per_10k_pop` | Health facilities per 10,000 population | Government of Kenya / WHO health facility registry | 2025 | Higher = better |
| Income & Livelihoods | `poverty_headcount_pct` | Poverty headcount (%) | Oxford Poverty & Human Development Initiative | 2022 | Higher = worse |
| Income & Livelihoods | `gcp_pc` | Gross County Product per capita | KNBS | 2022 | Higher = better |
| Accessibility | `healthcare_access_pop` | Population with healthcare access | Heidelberg Institute for Geoinformation Technology | 2025 | Higher = better |
| Climate | `rs_soil_moist` | Soil moisture | Earth Engine Data Catalog | 2023 | Higher = better |
| Climate | `rs_fapar` | FAPAR | Earth Engine Data Catalog | 2023 | Higher = better |

**South Sudan**

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

### 1.3 Index Construction

#### Step 1 — Missing value handling

- **Somalia & Kenya:** all regions are retained. `pop_frac_3plus` missing → 0 (absence of IPC data treated as no crisis); all other numeric indicators missing → column mean across regions.
- **South Sudan:** rows with any missing value across the selected indicators are dropped via listwise deletion (`na.omit`).

#### Step 2 — Normalisation

All indicators are Min-Max scaled to [0, 1]:

```
I_norm = (x − min(x)) / (max(x) − min(x))
```

Direction is preserved at this stage; inversion is applied at the aggregation step. Normalisation is handled by `R/normalise.R`.

#### Step 3 — Directionality

Each indicator is assigned a polarity sign, defined in `bad_vars` inside the version JSON:

- **+1 (good):** higher normalised value → better peacebuilding conditions (e.g. health facility density, school attendance).
- **−1 (bad):** higher normalised value → worse conditions (e.g. poverty headcount, IPC Phase 3+ fraction).

#### Step 4 — Conflict-informed weighting

Each indicator's weight is the absolute Pearson correlation between its normalised values and the ACLED conflict intensity rate (events per 1,000 population) for 2025 (`count_conflicts_events_per_1k_2025`):

```
w_i = |cor(I_i, C)|
```

Weights are then normalised so they sum to 1. Indicators with no empirical relationship to local conflict receive near-zero weight; those most associated with instability receive the highest weight. The conflict column used for weighting is set via `conflict_col` in the version JSON. ACLED data is available for 2016–2025, allowing the weighting year to be changed by updating `conflict_col`.

#### Step 5 — Aggregation

The SEPI score for each region is the weighted sum of polarity-adjusted normalised indicators:

```
SEPI_region = Σ (sign_i × w_i × I_i,norm) / Σ w_i
```

The raw scores are rescaled to [0, 1]. A score of **1.0** represents the strongest socio-economic conditions relative to other regions in that country; **0.0** represents the highest fragility. Aggregation is handled by `R/compute_index.R` and triggered via `03_run_sepi.R`.

#### Step 6 — Granular subindicator scores

Beyond the composite score, `03_run_sepi.R` applies min-max normalisation to the full candidate indicator universe (`granular_vars` in the version JSON). These normalised values — with no polarity adjustment or weighting applied — are exported per country and compiled into the final workbook (`outputs/sepi_results_<version>.xlsx`) as the `Indicator_Scores` sheet.

---

### 1.4 Interpretation

SEPI scores are on a continuous 0–1 scale. A score of **1.0** indicates the strongest socio-economic conditions relative to other regions in the same country; **0.0** indicates the highest fragility. Scores are not comparable across countries.

---

### 1.5 Key Limitations

- **Relative scores.** Rankings are within-country. A high score means better than peers, not good in absolute terms.
- **Correlation ≠ causation.** Weights reflect empirical association with conflict, not causal pathways.
- **Temporal lag.** Household surveys (SNBS, SIHBS) are typically 1–2 years old. Rapid security shifts may outpace the data.
- **Manual selection step.** The indicator list for each country was chosen by analyst review of correlation plots. Selection rationale is not formally recorded in the pipeline; differences across countries reflect contextual judgement.
- **Data gaps in fragile areas.** Inaccessibility can affect survey precision in conflict-affected zones.

---

## 2. Robustness Checks

Two methodological variants are used to test whether SEPI district rankings are sensitive to the choice of (a) normalisation method and (b) weighting scheme. In both cases, all data sources, administrative boundaries, exclusion criteria, and indicator sets are identical to the baseline `v3_aligned_conflict_weighted` version — only the targeted methodological element changes.

| Dimension | `v3_aligned_conflict_weighted` (baseline) | `v3_aligned_zscore` | `v3_aligned_bod` |
|-----------|-----------------------------------|-------------|----------|
| Normalisation | Min-max [0, 1] | Z-score (mean 0, sd 1) | Min-max [0, 1] |
| Weighting | Conflict-correlation weights | Conflict-correlation weights | Benefit of the Doubt (endogenous, per-district) |
| Indicator inputs | 6–8 conflict-correlated indicators | Same as baseline | One representative per pillar (5 total) |
| Final score range | [0, 1] | Unbounded | [0, 1] |

---

### 2.1 Z-Score Normalisation (`v3_aligned_zscore`)

#### Conceptual motivation

Min-max normalisation maps each indicator to [0, 1] relative to the observed range. It is simple and preserves proportional differences, but is sensitive to extreme values: a single outlier district can compress the rest of the distribution into a narrow band. Z-score normalisation instead centres each indicator on its cross-district mean and scales by its standard deviation, making normalised values robust to extreme observations. This robustness check tests whether the min-max assumption drives rankings.

#### Construction

Polarity is applied **before** normalisation — indicators in `bad_vars` are multiplied by −1, so that for all indicators a higher z-score corresponds to better conditions. Each indicator is then standardised independently:

```
z = (x − mean(x)) / sd(x)
```

If an indicator has zero variance, all values are set to 0. Conflict-correlation weights and the weighted-sum aggregation are unchanged:

```
SEPI_raw = Σ (sign_i × w_i × z_i)
```

The final rescaling to [0, 1] is **skipped** (`skip_final_rescale = true`). The composite z-score is reported directly: a positive score means a district is above the cross-district mean in aggregate; a negative score means below.

#### Limitations

- **Unbounded scores** are less intuitive for external audiences and preclude direct numerical comparison with baseline [0, 1] outputs. For communication purposes, baseline scores are preferred.
- **Sensitivity to small samples.** With few districts (notably South Sudan with 10), z-scores are more volatile: a single district shifting its raw value changes the mean and standard deviation for the whole country.
- **Conclusion from current results:** The two versions produce near-identical rankings, confirming the baseline min-max normalisation is not materially distorted by outliers.

---

### 2.2 Benefit of the Doubt Weighting (`v3_aligned_bod`)

#### Conceptual motivation

In the baseline, indicator weights are determined by empirical correlation with ACLED conflict data, introducing a dependency on a single external variable and requiring the implicit judgement that conflict correlation is a valid proxy for importance. The Benefit of the Doubt (BoD) method provides an alternative that makes no such assumption: each district is assigned the weighting scheme that presents it in the most favourable light, subject to the constraint that no district can score above 1 under any weighting scheme adopted by any other district (Cherchye et al., 2007). BoD is used here as a robustness check — if BoD rankings diverge substantially from conflict-weighted rankings, the weighting assumption is driving results.

#### Indicator structure

BoD weights across **dimensions**, not individual indicators, to avoid over-exploiting redundancy within pillars. One representative indicator per pillar is selected (the same indicators used to represent each pillar in the baseline):

**Kenya**

| Pillar | Representative Indicator | Source | Polarity |
|--------|--------------------------|--------|----------|
| Food Security | `pop_frac_3plus` — Fraction in IPC Phase 3+ | IPC via HDX HAPI (2025) | Higher = worse |
| Education | `net_attendance_total` — Secondary net attendance rate | Kenya Population and Housing Census (2019) | Higher = better |
| Health | `health_fac_per_10k_pop` — Health facilities per 10,000 pop | Government of Kenya / WHO (2025) | Higher = better |
| Economic | `gcp_pc` — Gross County Product per capita | KNBS (2022) | Higher = better |
| Climate | `rs_soil_moist` — Soil moisture | Earth Engine Data Catalog (2023) | Higher = better |

**Somalia**

| Pillar | Representative Indicator | Source | Polarity |
|--------|--------------------------|--------|----------|
| Food Security | `pop_frac_3plus` — Fraction in IPC Phase 3+ | IPC via HDX HAPI (2024) | Higher = worse |
| Education | `percent_no_formal_education` — % with no formal education | SIHBS (2022) | Higher = worse |
| Health | `hospitals_per_100k_pop` — Hospitals per 100,000 pop | WHO (2024) | Higher = better |
| Economic | `poverty_headcount_pct` — Poverty headcount (%) | SNBS (2023) | Higher = worse |
| Climate | `rs_ndvi` — NDVI | Earth Engine Data Catalog (2024) | Higher = better |

> Middle Juba is excluded.

**South Sudan**

| Pillar | Representative Indicator | Source | Polarity |
|--------|--------------------------|--------|----------|
| Food Security | `pop_frac_3plus` — Fraction in IPC Phase 3+ | IPC via HDX HAPI (2024) | Higher = worse |
| Education | `gender_parity_index` — Gender parity index (enrolment) | National Education Census Report (2021) | Higher = better |
| Health | `healthcare_access_pop` — Population with healthcare access | Heidelberg Institute (2025) | Higher = better |
| Economic | `poverty_headcount_pct` — Poverty headcount (%) | Republic of South Sudan Poverty Assessment (2024) | Higher = worse |
| Climate | `rs_soil_moist` — Soil moisture | Earth Engine Data Catalog (2023) | Higher = better |

> Abyei is excluded.

#### Construction

Missing value handling is identical to the baseline. Each pillar indicator is min-max scaled to [0, 1], then polarity-aligned (indicators in `bad_vars` are inverted: `I_norm_aligned = 1 − I_norm`) so that 1 always represents the best conditions.

For each district *c*, the BoD score solves the following linear programme:

```
maximise    Σᵢ wᵢ · xᵢ_c

subject to  Σᵢ wᵢ · xᵢ_d  ≤  1    for all districts d
            Σᵢ wᵢ          =  1
            Lᵢ  ≤  wᵢ  ≤  Uᵢ      for all pillars i
```

A score of **1.0** means the district lies on the efficiency frontier — no other district dominates it under its own optimal weights.

**Weight bounds** follow JRC Scenario I (±50% of the equal weight across 5 pillars):

```
Equal weight:  w_equal = 1/5 = 0.200
Lower bound:   L = 0.200 × 0.5 = 0.100
Upper bound:   U = 0.200 × 1.5 = 0.300
```

The flexibility parameter is set via `bod_weight_flex = 0.5` in `robustness_checks/v3_aligned_bod.json`. The LP is solved once per district using the `lpSolve` package (`R/compute_index.R`, function `compute_bod_sepi()`).

#### Results

| Country | v3_minmax vs v3_aligned_bod | v3_aligned_zscore vs v3_aligned_bod |
|---------|---------------------|---------------------|
| Kenya | 0.860 | 0.890 |
| Somalia | 0.905 | 0.884 |
| South Sudan | 0.794 | 0.794 |

The two conflict-weighted versions (min-max and z-score) are very similar to each other (ρ ≥ 0.952), confirming that normalisation method does not drive results. BoD diverges more, especially in South Sudan (ρ = 0.794) — BoD allows districts to emphasise their strongest pillar, whereas conflict-weighting assigns weight based on empirical association with instability.

#### Limitations

- **Relative scores.** A BoD score of 1 means frontier-relative within country, not well-performing in any absolute sense.
- **Indicator set inherited from conflict-weighted version.** A purpose-built BoD indicator selection (based on conceptual coverage and pillar orthogonality) was not performed. Appropriate for a robustness check; would need revision if BoD were adopted as the primary methodology.
- **Weight bounds are a choice.** The ±50% flexibility is a reasonable default following JRC guidance, but results are sensitive to this parameter.
- **Small samples.** South Sudan has only 10 districts; LP solutions may be less stable and the sensitivity analysis less informative.
- **Conflict data not used.** Unlike the baseline, ACLED data plays no role in score computation. Conflict correlation output from `03_run_sepi.R` is produced as a validation check only.

#### References

- Cherchye, L., Moesen, W., Rogge, N., & Van Puyenbroeck, T. (2007). An introduction to 'benefit of the doubt' composite indicators. *Social Indicators Research*, 82(1), 111–145.
- JRC-COIN (2019). Step 5: Weighting methods (I) — Benefit of the Doubt (DEA approach). 17th JRC Annual Training on Composite Indicators & Scoreboards, Ispra.

---

## 3. Sensitivity Analysis

### Purpose

Composite indices are sensitive to the choice of indicators and grouping structure. This analysis tests whether the relative ranking of regions changes materially when individual indicators or entire pillar domains are removed. Two SEPI versions are tested in parallel, allowing sensitivity to be assessed across both a structural (V1) and a data-driven weighting approach (V3).

| Version | Within-pillar aggregation | Across-pillar aggregation | Weighting |
|---------|--------------------------|--------------------------|-----------|
| `v1_aligned_equal_geometric` | Arithmetic mean | Geometric mean | Equal (1/n per indicator, 1/5 per pillar) |
| `v3_aligned_conflict_weighted` | Flat weighted sum | None (single composite) | Conflict-correlation: \|Pearson r(indicator, conflict events per 1k)\| |

Both versions use the same curated, country-aligned indicator sets, ensuring that differences in sensitivity reflect methodology rather than indicator selection.

---

### SA1 — Indicator-Level Sensitivity (Leave-One-Indicator-Out per Pillar)

For each pillar with two or more indicators, every possible combination of dropping exactly one indicator from that pillar is considered simultaneously across all eligible pillars. The SEPI is recomputed for each combination and the **SA1 score** for each region is the arithmetic mean across all combinations.

Food security is excluded from SA1 in all three countries (single indicator `pop_frac_3plus`); removing it would eliminate the pillar, which is the subject of SA2.

**Eligible pillars and combination counts (aligned versions):**

| Country | Eligible pillars (n indicators) | SA1 combinations |
|---------|--------------------------------|-----------------|
| Kenya | Education (2) × Health (3) × Economic (2) × Climate (4) | 48 |
| Somalia | Education (3) × Health (3) × Economic (2) × Climate (4) | 72 |
| South Sudan | Education (3) × Health (3) × Economic (2) × Climate (4) | 72 |

> For V3, pillar membership of each `se_var` is defined by the `pillar_groups` field in the version configuration, mirroring the V1 pillar structure exactly.

---

### SA2 — Pillar-Level Sensitivity (Leave-One-Pillar-Out)

Each of the five pillar domains (Education, Health, Food Security, Economic, Climate) is dropped entirely, and the SEPI is recomputed using the remaining four pillars. The **SA2 score** is the arithmetic mean of the five resulting scores.

For V1, dropping a pillar reduces the geometric mean to four terms. For V3, dropping a pillar removes all its `se_vars` from the weighted sum, and conflict-correlation weights are re-estimated on the reduced indicator set.

---

### Computation

All sensitivity runs use identical normalisation (min-max, [0, 1]) and polarity alignment as the baseline versions. For V3, conflict-correlation weights are re-estimated from scratch for each reduced indicator set. Regions dropped from a specific run due to missing data contribute only their available runs to the mean.

---

### Outputs

Produced by `06_sensitivity_analysis.R` (sources `R/sensitivity_analysis.R`):

| File | Content |
|------|---------|
| `sensitivity_analysis_comparison.xlsx` | Per-country sheets with baseline SEPI, SA1 mean, SA2 mean, and rank equivalents for V1 and V3 |
| `sensitivity_comparison_kenya.png` | Formatted comparison table — Kenya |
| `sensitivity_comparison_somalia.png` | Formatted comparison table — Somalia |
| `sensitivity_comparison_south_sudan.png` | Formatted comparison table — South Sudan |

Tables are colour-coded by score (red = low SEPI, green = high SEPI) and sorted by V1 baseline rank.

---

## 4. Criterion Validity and Version Comparison

### 4.1 Overview

Two complementary external criteria test whether SEPI scores correlate with outcomes the index should theoretically predict:

1. **IOM IDP origin data** — displacement density at ADM1 level (primary test).
2. **ACLED conflict intensity** — events per 1,000 population, aggregated over three time windows.

`05_compare_versions.R` uses these criteria — alongside rank stability metrics from the robustness checks — to evaluate whether **v1 (equal-weighted geometric mean)** or **v3 (conflict-weighted)** should be adopted as the primary SEPI method.

---

### 4.2 Rank Stability Metrics

For each version, two robustness variants are computed (z-score and BoD; see Section 2) and compared against their primary version on three metrics:

| Metric | Definition | Threshold for stability |
|--------|-----------|------------------------|
| Spearman ρ | Overall rank-order agreement | ≥ 0.90 |
| MARS | Mean Absolute Rank Shift — average places an ADM1 moves across variants | ≤ 1.5 |
| Top-5 stability | % of the 5 worst-off ADM1s in the primary version that remain in the bottom 5 in each variant | ≥ 80% |

---

### 4.3 External Criterion I: IOM IDP Origin Data

#### Hypothesis

**H₁:** There is a negative correlation between the SEPI score and displacement density at the ADM1 level within each country.

A higher SEPI score reflects better relative socio-economic performance; a higher displacement fraction reflects greater population flight. The hypothesis predicts a **negative** relationship.

#### Criterion variable

**Variable:** `pop_frac_idps` — the fraction of a region's population recorded as IDP *origins* (percentage of regional population that has fled), from IOM Displacement Tracking Matrix (DTM).

**Why origins, not destinations:** The data records the ADM1 region *from which* IDPs were displaced. This is the analytically correct frame for push-factor validity: the question is whether low SEPI scores predict where people flee *from*, not where they end up.

**Why displacement density:** Raw counts are confounded by population size. The population fraction normalises for size and measures the *intensity* of displacement pressure.

#### Data sources

| Country | Dataset | Source | Reference period | ADM1 units |
|---------|---------|--------|-----------------|------------|
| South Sudan | South Sudan — Emergency Event Tracking, January–December 2025 | IOM DTM | Full year 2025 | 10 |
| Kenya | Kenya — IOM DTM (from API) | IOM DTM via HDX HAPI | Full year 2024 | 23 |
| Somalia | Somalia — Emergency Trend Tracking Dataset (since February 2025) | IOM DTM | March 2026 snapshot | 6 |

> **Note on temporal alignment.** The most recent available snapshot is used for each country, consistent with the approach taken throughout the SEPI pipeline. For Kenya, full-year 2024 data is treated as a valid criterion measure for structural socio-economic fragility.

#### Analytical method

1. **Match ADM1 units** on `adm1_pcode` (OCHA P-code). Units absent from the IDP dataset are excluded.
2. **Within-country min-max normalisation** of `pop_frac_idps`, so that "high displacement" is interpreted relative to each country's own maximum.
3. **Spearman's ρ** is used (rather than Pearson's *r*) because displacement data is heavily right-skewed and the hypothesis is ordinal. Threshold: ρ < −0.6 for a strong negative result.
4. **ROC/AUC hotspot test:** hotspot = ADM1 units above the within-country median displacement. AUC ≥ 0.70 = acceptable; ≥ 0.80 = good. Countries with n < 8 matched units are omitted.

#### Results

| Country | Matched units | Spearman ρ | p-value | Verdict |
|---------|--------------|-----------|---------|---------|
| Kenya | 23 / 47 SEPI units | −0.612 | 0.002 | **Supported** |
| South Sudan | 10 / 10 SEPI units | −0.236 | 0.511 | Inconclusive — outlier (Western Equatoria) |
| Somalia | 6 / 18 SEPI units | +0.086 | 0.872 | Underpowered — criterion data inadequate |

**Kenya** provides the strongest evidence for criterion validity (ρ = −0.612, p = 0.002). Turkana (SEPI rank 47, worst in Kenya) has the highest displacement density (11.1%); Nyeri (rank 1) has 0.035%.

**South Sudan** — the direction is correct but not significant. Western Equatoria (SEPI rank 2) carries 11.8% displacement, driven by acute armed group activity (NAS, remnant SPLM-IO) rather than chronic socio-economic deterioration. SEPI captures structural deprivation; it does not measure armed group presence. Excluding Western Equatoria, the remaining nine states show a directionally consistent pattern.

**Somalia** — with only 6 matched units and a near-flat displacement range (0.007–0.203%), the test is uninformative regardless of direction.

#### Implementation

- **Section C** (`04_evaluate.R`) — Spearman rank correlation.
- **Section D** — ROC / hotspot test (AUC with 95% DeLong CI).
- **Section E** — Visualisations: `criterion_validity_scatter_displacement.png` (scatter plot, one panel per country) and `criterion_validity_roc_displacement.png` (ROC curves for Kenya and South Sudan).

---

### 4.4 External Criterion II: ACLED Conflict Intensity

#### Hypothesis

**H₁:** Within each country, SEPI scores are negatively correlated with conflict events per 1,000 population at the ADM1 level.

ACLED provides full ADM1 coverage across all three countries, eliminating the sparsity problem that limited the Somalia displacement test. Three time windows are tested in parallel:

| Window key | Years | Purpose |
|------------|-------|---------|
| `conflict_10y` | 2016–2025 | Longest available history; closest to "structural" conflict intensity |
| `conflict_5y` | 2021–2025 | Medium-term — captures recent intensity regime |
| `conflict_2025` | 2025 only | Contemporaneous snapshot; highest sensitivity but also highest year-to-year noise |

**Endogeneity caveat for v3:** `v3_aligned_conflict_weighted` derives its weights from 2025 ACLED data. The 2025 conflict window is therefore circular (reported as a consistency check only). The 5y and 10y windows remain informative despite partial endogeneity. For v1, all windows are independent.

#### Analytical method

For each window and country: sum per-capita event counts, min-max normalise within country, join to SEPI on `adm1_pcode`, compute Spearman ρ and ROC/AUC with the same thresholds as the displacement criterion. Implemented in `04_evaluate.R` Section F (shared helpers in `R/criterion_validity_conflict.R`).

#### Outputs

Produced by `04_evaluate.R` (Section F) and saved to `outputs/figures/`:
`criterion_validity_scatter_conflict_{10y|5y|2025}.png` and `criterion_validity_roc_conflict_{10y|5y|2025}.png`.

---

### 4.5 Version Comparison Scorecard

`05_compare_versions.R` compiles all metrics into a single colour-coded scorecard:

- `outputs/figures/version_comparison.png` — all metrics across v1, v3, and robustness variants
- `outputs/figures/ranks_v{1|3}_{country}.png` — unit-level rank tables showing every ADM1's rank across primary and both variants, with shifts (Δ) highlighted where |Δ| ≥ 3

The scorecard integrates: rank stability (Spearman ρ, MARS, Top-5) × robustness variant × criterion validity (Spearman ρ) × discriminatory capacity (AUC) × criterion source (displacement + three conflict windows) × version (v1, v3).

---

### 4.6 Limitations

**Time window mismatch.** IDP data covers different periods per country. Within-country normalisation mitigates but does not eliminate this as a confound.

**Security–socioeconomics decoupling.** In active conflict settings, acute security events can generate large displacement from regions that score relatively well on structural socio-economic indicators. SEPI is not designed to predict displacement driven primarily by armed group activity independent of underlying deprivation.

**IDP origin data coverage.** DTM coverage is uneven across ADM1 units, particularly in Somalia. ADM1 units absent from the IDP dataset are excluded from the validity test.

**Endogeneity in v3 conflict criterion.** The 2025 conflict window should not be treated as an independent validation test for v3.
