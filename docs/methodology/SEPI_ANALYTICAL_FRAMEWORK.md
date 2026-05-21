# SEPI — Analytical Framework & Validation

This document consolidates the full methodology for the Socio-Economic Peacebuilding Index (SEPI): the baseline construction approach, robustness checks, sensitivity analysis, version comparison, and criterion validity assessment. It is the single reference for how the index is built and how its results are interrogated.

---

## Table of Contents

1. [Baseline Methodology — `v1_equal_geometric`](#baseline-methodology-v1_equal_geometric)
2. [Robustness Check I — Z-Score Normalisation (`v1_zscore`)](#robustness-check-i-z-score-normalisation-v1_zscore)
3. [Robustness Check II — Benefit of the Doubt Weighting (`v1_bod`)](#robustness-check-ii-benefit-of-the-doubt-weighting-v1_bod)
4. [Sensitivity Analysis](#sensitivity-analysis)
5. [Version Comparison — v1 vs v2](#version-comparison-v1-vs-v2)
6. [Criterion Validity Assessment](#criterion-validity-assessment)

---

## 1. Baseline Methodology — `v1_equal_geometric`

The **Socio-Economic Peacebuilding Index (SEPI)** is a composite indicator measuring the structural socio-economic capacity of sub-national regions to sustain peace. It is grounded in the Humanitarian–Development–Peace Nexus: durable peace requires a foundation of socio-economic sufficiency, and structural deficits — poverty, food insecurity, poor health access, environmental degradation — act as force multipliers for instability.

SEPI is computed at the **Admin-1 level** for **Kenya**, **Somalia**, and **South Sudan**. The primary methodology (`v1_equal_geometric`) uses equal weights within a pillar-structured aggregation: indicators are averaged arithmetically within each of five pillar domains, then combined into a composite score using a geometric mean across pillars. Conflict data is not used in the construction of the index; it is used only for criterion validity assessment. Scores are relative within each country; a high score means a region is performing better than other regions in the same country, not that it meets any global standard.

---

### 1.1 Data Sources

All datasets are harmonised to OCHA COD administrative boundaries. Middle Juba (SO) and Abyei (SS) are excluded.

| Domain | Source | Ref. year |
|--------|--------|-----------|
| Food Security | Integrated Food Security Phase Classification (IPC) via HDX HAPI | 2024–2025 |
| Education | Kenya Population and Housing Census (KE); Somalia Integrated Household Budget Survey — SIHBS (SO); National Education Census Report (SS) | 2019–2022 |
| Health | Government of Kenya / WHO health facility registry (KE); WHO health facility database (SO, SS) | 2024–2025 |
| Income & Livelihoods | Oxford Poverty & Human Development Initiative — OPHI; Kenya National Bureau of Statistics — KNBS (KE); Somalia National Bureau of Statistics — SNBS (SO); South Sudan Poverty and Equity Assessment — SSNPEA; CLiMIS South Sudan (SS) | 2022–2024 |
| Accessibility | Heidelberg Institute for Geoinformation Technology | 2025 |
| Climate / Environment | Google Earth Engine Data Catalog | 2023–2024 |
| Conflict *(validation only)* | Armed Conflict Location & Event Data — ACLED API | 2016–2025 |

---

### 1.2 Indicator Selection

Candidates are reviewed for domain coverage in `02_explore.R` (Block A); pairs with Pearson r > 0.8 within the same pillar are pruned automatically (`R/screen_indicators.R`). Final lists are set in `versions/v1_equal_geometric.json`.

#### Indicators selected for the SEPI

KE = Kenya · SO = Somalia · SS = South Sudan. Middle Juba (SO) and Abyei (SS) are excluded.

| Pillar | Indicator | Label | Countries |
|--------|-----------|-------|-----------|
| Food Security | `pop_frac_3plus` | Population in IPC Phase 3+ | KE, SO, SS |
| Education | `primary_school_net_attendance_total` | Primary net attendance (%) | KE, SO |
| | `net_attendance_total` | Secondary net attendance (%) | KE |
| | `literacy_percent_total` | Literacy rate (%) | SO |
| | `percent_highest_level_secondary_education` | Secondary education attainment (%) | SO |
| | `school_access_pop` | Population with school access (%) | KE, SO, SS |
| | `on_payroll_pct` | Teachers on payroll (%) | SS |
| | `dropout_pct` | Student dropout rate (%) | SS |
| Health | `healthcare_access_pop` | Population with healthcare access (%) | KE, SO, SS |
| | `health_fac_per_10k_pop` | Health facilities per 10,000 pop. | KE, SO, SS |
| | `hospitals_per_100k_pop` | Hospitals per 100,000 pop. | KE, SO, SS |
| Economic | `poverty_headcount_pct` | Poverty headcount (%) | KE, SO, SS |
| | `gcp_pc` | Gross County Product per capita | KE |
| | `total_expenditure_usd` | Household expenditure (USD) | SO |
| | `annual_cmb_mean` | Avg. min. consumption basket cost | SS |
| Climate | `rs_ndvi` | NDVI | KE, SO, SS |
| | `rs_soil_moist` | Soil moisture | KE, SO, SS |
| | `rs_fapar` | FAPAR | KE, SO, SS |
| | `rs_pdsi` | Palmer Drought Severity Index | KE, SO, SS |

---

### 1.3 Index Construction

The construction follows the four-step summary in README §Methodology. Key parameters and imputation rules are:

**Normalisation:**
```
I_norm = (x − min(x)) / (max(x) − min(x))
```
Negative-polarity indicators are inverted: `I_aligned = 1 − I_norm`.

**Within-pillar aggregation** (arithmetic mean, equal indicator weights):
```
Pillar_score_p = (1 / n_p) × Σⱼ I_aligned_j
```

**Across-pillar aggregation** (geometric mean, equal pillar weights, floor = 0.001):
```
SEPI = exp( (1/5) × Σₚ log( max(Pillar_score_p, 0.001) ) )
```

The floor prevents log(0). The geometric mean penalises uneven pillar profiles — a very low score on one pillar cannot be fully compensated by high scores on others.

---

### 1.4 Key Limitations

- **Relative scores.** Rankings are within-country. A high score means better than peers, not good in absolute terms.
- **Equal weights assume equal pillar importance.** No empirical basis is used to prioritise one pillar domain over another; this is a deliberate transparency trade-off but may not reflect context-specific realities.
- **Geometric mean penalises extreme pillar deficits.** A region with a near-zero score on a single pillar will have a very low composite score regardless of performance elsewhere. This is a design feature, not a bug, but should be interpreted carefully.
- **Temporal lag.** Household surveys (SNBS, SIHBS) are typically 1–2 years old. Rapid security shifts may outpace the data.
- **Manual selection step.** The indicator list for each pillar was chosen by analyst review of domain-coverage plots. Differences across countries reflect data availability and contextual judgement.
- **Data gaps in fragile areas.** Inaccessibility can affect survey precision in conflict-affected zones.

---

## 2. Robustness Check I — Z-Score Normalisation (`v1_zscore`)

This check tests whether SEPI district rankings are sensitive to the choice of normalisation method — specifically, whether replacing bounded min-max scaling with z-score standardisation materially reorders districts.

---

### 2.1 What Differs from the Baseline

| Dimension | `v1_equal_geometric` | `v1_zscore` |
|---|---|---|
| Normalisation | Min-max [0, 1] | Z-score (mean 0, sd 1) |
| Within-pillar aggregation | Arithmetic mean of min-max values → pillar score in [0, 1] | Arithmetic mean of z-scores → pillar score potentially negative |
| Across-pillar aggregation | Geometric mean with floor 0.001 | Geometric mean with floor 0.001 |
| Weighting | Equal | Equal |
| Output score range | Effectively [0, 1] | Above-zero; effective maximum depends on z-score distribution |
| Indicator inputs | Same | Same |

---

---

### 2.2 Index Construction

Each indicator is standardised independently (`z = (x − mean) / sd`; zero-variance indicators → 0). Negative-polarity indicators have their z-score negated. Pillar aggregation is otherwise identical to the baseline:

```
Pillar_score_p = (1 / n_p) × Σⱼ (sign_j × z_j)
SEPI = exp( (1/5) × Σₚ log( max(Pillar_score_p, 0.001) ) )
```

Because z-scores can be negative, the within-pillar mean can be negative too; the floor clips these before the geometric mean. Scores are **not numerically comparable** to the baseline; rankings are directly comparable.

---

### 2.3 Key Limitations

- **Effective output scale shifts.** Pillar scores based on z-scores can be near the floor (0.001) for weak performers, making scores harder to communicate externally. Baseline scores are preferred for reporting.
- **Sensitivity to small samples.** With few districts (South Sudan has 10), z-scores are more volatile than min-max values — a single district shifting its raw value changes the mean and SD for the whole country.

---

## 3. Robustness Check II — Benefit of the Doubt Weighting (`v1_bod`)

This check tests whether SEPI district rankings are sensitive to the choice of weighting method — specifically, whether replacing equal pillar weights with data-driven, endogenous Benefit of the Doubt (BoD) weights materially reorders districts.

All data sources, administrative boundaries, and exclusion criteria are identical to the baseline. Only the weighting method and aggregation structure differ.

---

### 3.1 What Differs from the Baseline

| Dimension | `v1_equal_geometric` | `v1_bod` |
|---|---|---|
| Indicator inputs | Full pillar indicator sets (2–4 indicators per pillar) | One representative indicator per pillar (`pillar_map`) |
| Within-pillar aggregation | Arithmetic mean | BoD LP (optimises per-district weights across 5 pillar representatives) |
| Across-pillar aggregation | Geometric mean | None (single composite from LP) |
| Weighting | Equal (1/n per indicator, 1/5 per pillar) | Benefit of the Doubt — endogenous, per-district |
| Conflict variable role | Not used | Not used |
| Number of inputs to LP | N/A | 5 (one per pillar, all countries) |
| Normalisation | Min-max [0, 1] | Min-max [0, 1] |
| Missing value handling | Mean imputation (Kenya & Somalia); listwise (South Sudan) | Same |

---

### 3.2 Indicator Structure

One representative per pillar is used to prevent the LP over-exploiting within-pillar redundancy. Representatives are drawn from the v1 indicator pool.

| Pillar | KE | SO | SS |
|--------|----|----|-----|
| Food Security | `pop_frac_3plus` | `pop_frac_3plus` | `pop_frac_3plus` |
| Education | `net_attendance_total` | `primary_school_net_attendance_total` | `school_access_pop` |
| Health | `health_fac_per_10k_pop` | `health_fac_per_10k_pop` | `healthcare_access_pop` |
| Economic | `gcp_pc` | `poverty_headcount_pct` | `poverty_headcount_pct` |
| Climate | `rs_ndvi` | `rs_ndvi` | `rs_ndvi` |

---

### 3.3 Index Construction

Missing value handling, normalisation, and polarity alignment are identical to the baseline. After alignment, BoD is solved per district:

**BoD optimisation:** For each district *c*, the score is the solution to:

```
maximise    Σᵢ wᵢ · xᵢ_c

subject to  Σᵢ wᵢ · xᵢ_d  ≤  1    for all districts d
            Σᵢ wᵢ          =  1
            Lᵢ  ≤  wᵢ  ≤  Uᵢ      for all pillars i
```

A score of **1.0** means the district lies on the efficiency frontier. The LP is solved once per district using the `lpSolve` package (`R/compute_index.R`, function `compute_bod_sepi()`).

**Weight bounds** follow JRC Scenario I (±50% of equal weight):

```
Equal weight (5 pillars):  w_equal = 0.200
Lower bound:               L = 0.200 × 0.5 = 0.100
Upper bound:               U = 0.200 × 1.5 = 0.300
```

The flexibility parameter is set via `bod_weight_flex = 0.5` in `robustness_checks/v1_bod.json`.

**Ranking:** districts are ranked by BoD score within each country (rank 1 = strongest conditions).

---

### 3.4 Key Limitations

- **Relative scores.** A BoD score of 1 means frontier status relative to other districts in the same country — not well-performing in absolute terms.
- **Reduced indicator set.** The BoD uses one representative per pillar rather than the full v1 indicator sets. The representatives are drawn from the v1 indicator pool, so no additional data is required, but within-pillar variation is not captured.
- **Weight bounds are a choice.** The ±50% flexibility (Scenario I) is a reasonable default; tighter bounds push toward equal weighting while looser bounds allow a district to assign near-zero weight to a pillar where it performs poorly.
- **Small samples.** South Sudan has only 10 districts; LP solutions may be less stable.
---

### 3.5 References

- Cherchye, L., Moesen, W., Rogge, N., & Van Puyenbroeck, T. (2007). An introduction to 'benefit of the doubt' composite indicators. *Social Indicators Research*, 82(1), 111–145.
- Cherchye, L., Knox Lovell, C. A., Moesen, W., & Van Puyenbroeck, T. (2007). One market, one number? A composite indicator assessment of EU internal market dynamics. *European Economic Review*, 51(3), 749–779.
- JRC-COIN (2019). Step 5: Weighting methods (I) — Benefit of the Doubt (DEA approach). 17th JRC Annual Training on Composite Indicators & Scoreboards, Ispra.

---

## 4. Sensitivity Analysis

### Versions tested: `v1_equal_geometric` · `v2_conflict_weighted`

---

### 4.1 Two Sensitivity Tests

#### SA1 — Indicator-Level Sensitivity (Leave-One-Indicator-Out per Pillar)

For each pillar with two or more indicators, every possible combination of dropping exactly one indicator from that pillar is considered simultaneously across all eligible pillars. The SEPI is recomputed for each combination using the full methodology of the respective version. The **SA1 score** for each region is the arithmetic mean of SEPI scores across all combinations.

Food security is excluded from SA1 in all three countries because it contains only one indicator (`pop_frac_3plus`); removing it would eliminate the pillar entirely, which is the subject of SA2.

**Eligible pillars and combination counts (aligned versions):**

| Country | Eligible pillars (n indicators) | SA1 combinations |
|---------|--------------------------------|-----------------|
| Kenya | Education (3) × Health (3) × Economic (2) × Climate (4) | 72 |
| Somalia | Education (4) × Health (3) × Economic (2) × Climate (4) | 96 |
| South Sudan | Education (3) × Health (3) × Economic (2) × Climate (4) | 72 |

A region whose SA1 mean score is close to its baseline SEPI has a stable ranking. A large deviation signals dependence on one or a few particular indicators.

#### SA2 — Pillar-Level Sensitivity (Leave-One-Pillar-Out)

Each of the five pillar domains (Education, Health, Food Security, Economic, Climate) is dropped entirely, and the SEPI is recomputed using only the remaining four. This produces five alternative SEPI scores per region. The **SA2 score** is the arithmetic mean of these five scores.

For V1, dropping a pillar removes all its indicators from within-pillar aggregation and reduces the geometric mean to four terms. For v2, dropping a pillar removes all its `se_vars` from the flat weighted sum, and conflict-correlation weights are re-estimated on the reduced indicator set.

A region whose SA2 mean is close to its baseline performs consistently across all pillar combinations. A large deviation identifies which pillar most drives the region's ranking.

### 4.2 Versions Compared

| Version | Within-pillar aggregation | Across-pillar aggregation | Weighting |
|---------|--------------------------|--------------------------|-----------|
| `v1_equal_geometric` | Arithmetic mean | Geometric mean | Equal (1/n per indicator, 1/5 per pillar) |
| `v2_conflict_weighted` | Flat weighted sum | None (single composite) | Conflict-correlation: \|Pearson r(indicator, conflict events per 1k)\| |

Both versions use the same curated, country-aligned indicator sets from the indicator alignment exercise, ensuring that differences in sensitivity reflect methodology rather than indicator selection.

### 4.3 Computation

All runs use the same normalisation and polarity alignment as the baseline. For v2, conflict-correlation weights are re-estimated from scratch for each reduced indicator set. The SA1/SA2 score per region is an unweighted arithmetic mean across all valid runs; regions dropped due to missing data (v2 South Sudan listwise deletion) contribute only their available runs.

Outputs (`outputs/sensitivity_analysis_comparison.xlsx` and per-country PNG comparison tables) are produced by `06_sensitivity_analysis.R`.

---

## 5. Version Comparison — v1 vs v2

`05_compare_versions.R` evaluates whether **v1 (equal-weighted geometric mean)** or **v2 (conflict-weighted)** should be adopted as the primary SEPI method. It assesses each version on three dimensions.

---

### 5.1 A. Rank Stability

Two robustness variants are computed for each version family:

| Variant | What changes |
|---------|-------------|
| z-score | Min-max normalisation replaced by z-score |
| BoD | Equal weights replaced by Benefit of Doubt (DEA) weights |

Three metrics compare each variant against its primary version:

- **Spearman ρ** — overall rank-order agreement (1 = identical ordering)
- **MARS** — Mean Absolute Rank Shift: average number of places an ADM1 moves across variants
- **Top-5 stability** — % of the 5 worst-off ADM1s in the primary version that remain in the bottom 5 in each variant

*Thresholds:* ρ ≥ 0.90 / MARS ≤ 1.5 / Top-5 ≥ 80% = stable.

---

### 5.2 B. Criterion Validity

**Hypothesis:** lower SEPI (more deprived) → higher IDP displacement density.

Spearman ρ between the SEPI score and within-country min-max normalised IDP displacement fraction (IOM DTM data, ADM1 level). Target: ρ < −0.60.

---

### 5.3 Outputs

- `outputs/figures/compare_versions/scorecard.png` — colour-coded scorecard (all metrics)
- `outputs/figures/compare_versions/ranks_v{1|3}_{country}.png` — unit-level rank tables showing every ADM1's rank across primary version and both variants, with shift (Δ) highlighted where |Δ| ≥ 3

---

## 6. Criterion Validity Assessment

Two complementary external criteria test whether SEPI scores correlate with outcomes the index should theoretically predict:

1. **IOM IDP origin data** — displacement density at ADM1 level (primary test).
2. **ACLED conflict intensity** — events per 1,000 population, aggregated over three time windows (2016–2025, 2021–2025, 2025).

Each criterion is examined with a Spearman rank correlation (H₁: negative relationship). Results for both criteria feed the version-comparison scorecard in `outputs/figures/version_comparison.png`.

---

### 6.1 External Criterion I: IOM IDP Origin Data

#### Hypothesis

**H₁:** There is a negative correlation between the SEPI score and displacement density at the ADM1 level within each country.

**Logic:** Regions with the lowest socio-economic performance should, all else equal, generate higher rates of population flight. A higher SEPI score reflects better relative performance; a higher displacement fraction reflects greater population flight. The hypothesis predicts a **negative** relationship.

#### Criterion Variable

**Producer:** International Organization for Migration (IOM) — Displacement Tracking Matrix (DTM)

**Variable:** `pop_frac_idps` — the fraction of a region's population recorded as IDP *origins* (percentage of regional population that has fled).

**Unit of analysis — origins, not destinations:** The data records the ADM1 region *from which* IDPs were displaced. This is the analytically correct frame for push-factor validity: the question is whether low SEPI scores predict where people flee *from*, not where they end up.

**Why displacement density rather than raw counts:** Raw counts are confounded by population size. The population fraction normalises for size and measures the *intensity* of displacement pressure.

#### Data Sources by Country

| Country | Dataset | Source | Reference period | ADM1 units |
|---|---|---|---|---|
| South Sudan | South Sudan — Emergency Event Tracking, Jan–Dec 2025 | IOM DTM | Full year 2025 | 10 |
| Kenya | Kenya — IOM DTM (from API) | IOM DTM via HDX HAPI | Full year 2024 | 23 |
| Somalia | Somalia — Emergency Trend Tracking Dataset (since Feb 2025) | IOM DTM | March 2026 snapshot | 6 |

> Kenya IDP data is from 2024 (full calendar year); South Sudan and Somalia fall within 2025–2026. Within-country analysis mitigates the temporal mismatch.

#### Analytical Method

All analysis is within-country. `pop_frac_idps` is min-max normalised within each country before correlation, to control for the different time windows. **Spearman's ρ** is used (displacement data is right-skewed; hypothesis is ordinal). Threshold: ρ < −0.6 = strong negative result.

Outputs: `outputs/figures/criterion_validity_scatter_displacement.png` (scatter, one panel per country). Produced by `04_evaluate.R` Sections B–C.

---

### 6.2 External Criterion II: ACLED Conflict Intensity

**H₁:** SEPI scores are negatively correlated with conflict events per 1,000 population within each country. Conflict data covers all ADM1 units (no coverage gaps) and spans 2016–2025, enabling multi-window tests.

**Criterion variable:** `count_conflicts_events_per_1k` summed over the window (Battles, Explosions/Remote violence, Violence against civilians). Within-country min-max normalisation is applied for visualisation; Spearman's ρ is rank-based and unaffected.

#### Time Windows

| Window key | Years | Purpose |
|---|---|---|
| `conflict_10y` | 2016–2025 | Longest available history; closest to "structural" conflict intensity |
| `conflict_5y` | 2021–2025 | Medium-term — captures recent intensity regime |
| `conflict_2025` | 2025 only | Contemporaneous snapshot; highest sensitivity to current dynamics |

#### Endogeneity Note

`v1_equal_geometric` does not use ACLED data in construction, so the conflict criterion tests are fully independent for the primary methodology. When `v2_conflict_weighted` is included in the version comparison, its 2025 conflict window test is circular by construction (v2 weights are derived from 2025 ACLED data) and reported only as a consistency check; its 5y and 10y windows remain partially informative.

Per window and country: sum `count_conflicts_events_per_1k_YYYY`, join to SEPI on `adm1_pcode`, compute Spearman ρ. Threshold: ρ < −0.6 = strong negative result.

Outputs: scatter PNGs for each window (`criterion_validity_scatter_conflict_{10y|5y|2025}.png`) saved to `outputs/figures/`. Produced by `04_evaluate.R` Section D; helpers shared with `05_compare_versions.R` via `R/criterion_validity_conflict.R`.

---

### 6.3 Limitations (Criterion Validity)

- **Time window mismatch.** IDP data covers different periods for each country. Within-country normalisation mitigates but does not eliminate this as a confound.
- **Security–socioeconomics decoupling.** In active conflict settings, acute security events can generate large displacement from regions that score relatively well on structural indicators. SEPI is not designed to predict displacement driven primarily by armed group activity.
- **IDP origin data coverage.** DTM coverage is uneven across ADM1 units; ADM1 units absent from the IDP dataset are excluded, so the validation sample may not be representative.
- **Small n in Somalia.** Six observations are insufficient for a meaningful rank correlation test. The Somalia result should not be cited as evidence for or against the index's validity until more comprehensive displacement data is available.
- **Endogeneity in v2 conflict window (comparison only).** When `v2_conflict_weighted` is included in the version comparison, the 2025 conflict window test is circular by construction; only the 5y and 10y windows provide independent signal for that version.
