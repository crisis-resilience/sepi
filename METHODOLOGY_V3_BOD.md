# SEPI — Methodology
### Version: `v3_bod` (Robustness Check — Benefit of the Doubt Weighting)

This document describes the **Benefit of the Doubt (BoD)** robustness check for the Socio-Economic Peacebuilding Index (SEPI). It is a methodological variant of the baseline `v3_conflict_weighted` version. The purpose of this check is to test whether SEPI district rankings are sensitive to the choice of weighting method — specifically, whether replacing conflict-correlation weights with data-driven, endogenous BoD weights materially reorders districts.

All data sources, administrative boundaries, and exclusion criteria are identical to the baseline version. Only the weighting method and indicator structure differ.

---

## What Differs from the Baseline (`v3_conflict_weighted`)

| Dimension | `v3_conflict_weighted` | `v3_bod` |
|---|---|---|
| Indicator inputs | Flat list of 6–8 conflict-correlated indicators (`se_vars`) | One representative indicator per pillar (`pillar_map`) |
| Weighting | Conflict-correlation weights (empirically derived) | Benefit of the Doubt — endogenous, per-district |
| Conflict variable role | Drives indicator weights | Not used in computation |
| Number of inputs to aggregation | 6–8 (country-specific) | 5 (one per pillar, all countries) |
| Normalisation | Min-max [0, 1] | Min-max [0, 1] |
| Missing value handling | Same as baseline | Same as baseline |

---

## Conceptual Motivation

In the baseline version, indicator weights are determined by each indicator's empirical correlation with local conflict intensity. This calibrates the index to the specific political economy of each country but introduces a dependency on a single external variable — ACLED conflict data — and requires an implicit judgement that correlation with conflict is a valid proxy for importance.

The BoD method provides an alternative that makes no such assumption. Instead of imposing a common weighting scheme, BoD treats each district as entitled to the weighting scheme that presents it in the most favourable light, subject to the constraint that no district can score above 1 under any weighting scheme that any other district has adopted. This "benefit of the doubt" principle (Cherchye et al., 2007) reflects the idea that regions face different structural conditions and should not be penalised for being evaluated against criteria that do not reflect their context.

BoD is used here not as the primary SEPI methodology, but as a robustness check: if BoD rankings are substantially different from conflict-weighted rankings, it signals that the weighting assumption is driving results, not the underlying data. If rankings are similar, the index is robust to this methodological choice.

---

## Indicator Structure

BoD weights across **dimensions**, not individual indicators. Feeding a large flat list of indicators into BoD would allow the optimizer to over-exploit redundancy between indicators within the same domain. Instead, one representative indicator per pillar is selected, following the standard BoD setup for composite indicators (Cherchye et al., 2007; JRC-COIN, 2019).

The pillar representatives are defined in `pillar_map` inside `robustness_checks/v3_bod.json`. They are the same indicators used to represent each pillar in the baseline version — no new selection is performed for this robustness check.

### Kenya

| Pillar | Representative Indicator | Source | Polarity |
|--------|--------------------------|--------|----------|
| Food Security | `pop_frac_3plus` — Fraction of population in IPC Phase 3+ | IPC via HDX HAPI (2025) | Higher = worse |
| Education | `net_attendance_total` — Secondary net attendance rate | Kenya Population and Housing Census (2019) | Higher = better |
| Health | `health_fac_per_10k_pop` — Health facilities per 10,000 population | Government of Kenya / WHO (2025) | Higher = better |
| Economic | `gcp_pc` — Gross County Product per capita | Kenya National Bureau of Statistics (2022) | Higher = better |
| Climate | `rs_soil_moist` — Soil moisture | Earth Engine Data Catalog (2023) | Higher = better |

### Somalia

| Pillar | Representative Indicator | Source | Polarity |
|--------|--------------------------|--------|----------|
| Food Security | `pop_frac_3plus` — Fraction of population in IPC Phase 3+ | IPC via HDX HAPI (2024) | Higher = worse |
| Education | `percent_no_formal_education` — % with no formal education | SIHBS (2022) | Higher = worse |
| Health | `hospitals_per_100k_pop` — Hospitals per 100,000 population | WHO health facility database (2024) | Higher = better |
| Economic | `poverty_headcount_pct` — Poverty headcount (%) | SNBS (2023) | Higher = worse |
| Climate | `rs_ndvi` — NDVI | Earth Engine Data Catalog (2024) | Higher = better |

> Middle Juba is excluded. Al-Shabaab territorial control makes reliable survey data inaccessible.

### South Sudan

| Pillar | Representative Indicator | Source | Polarity |
|--------|--------------------------|--------|----------|
| Food Security | `pop_frac_3plus` — Fraction of population in IPC Phase 3+ | IPC via HDX HAPI (2024) | Higher = worse |
| Education | `gender_parity_index` — Gender parity index (enrolment) | National Education Census Report (2021) | Higher = better |
| Health | `healthcare_access_pop` — Population with healthcare access | Heidelberg Institute for Geoinformation Technology (2025) | Higher = better |
| Economic | `poverty_headcount_pct` — Poverty headcount (%) | Republic of South Sudan Poverty and Equity Assessment (2024) | Higher = worse |
| Climate | `rs_soil_moist` — Soil moisture | Earth Engine Data Catalog (2023) | Higher = better |

> Abyei is excluded. Its contested administrative status means it lacks reliable survey coverage across multiple domains.

---

## Index Construction

### 1. Missing Value Handling

Identical to the baseline:

- **Kenya & Somalia:** `pop_frac_3plus` missing → 0 (absence of IPC data treated as no crisis); all other missing values → column mean across districts.
- **South Sudan:** districts with any missing value across the five pillar indicators are dropped via listwise deletion.

### 2. Normalisation

Each pillar indicator is min-max scaled to [0, 1]:

```
I_norm = (x − min(x)) / (max(x) − min(x))
```

Normalisation is applied without polarity adjustment at this stage.

### 3. Polarity Alignment

Indicators where higher raw values represent worse conditions (`bad_vars` in the version JSON) are inverted after normalisation:

```
I_norm_aligned = 1 − I_norm
```

This ensures that for all five pillar inputs, a value of 1 represents the best conditions and 0 represents the worst. BoD maximises the composite score, so all inputs must point in the same direction before optimisation.

### 4. Benefit of the Doubt Weighting

For each district *c*, the BoD score is the solution to the following linear programme:

```
maximise    Σᵢ wᵢ · xᵢ_c

subject to  Σᵢ wᵢ · xᵢ_d  ≤  1    for all districts d
            Σᵢ wᵢ          =  1
            Lᵢ  ≤  wᵢ  ≤  Uᵢ      for all pillars i
```

Where:
- **xᵢ_c** is the polarity-aligned normalised value of pillar *i* for district *c*
- **wᵢ** are the indicator weights to be determined
- The frontier constraint (`Σ wᵢ · xᵢ_d ≤ 1`) ensures no district exceeds a score of 1 under any weight vector adopted by any other district
- **Lᵢ** and **Uᵢ** are lower and upper weight bounds

The BoD score for district *c* is the optimal value of this programme. A score of **1.0** means the district lies on the efficiency frontier — no other district dominates it under its own optimal weights. Scores below 1 indicate the distance from the frontier.

#### Weight Bounds

Unconstrained BoD allows any non-negative weights summing to 1. This is too permissive: a district excelling on one pillar and failing on all others could assign all weight to that single pillar and achieve a score of 1. This is not defensible for a deprivation index.

Weight bounds are applied following JRC Scenario I (±50% of the equal weight):

```
Equal weight (5 pillars):  w_equal = 1/5 = 0.200
Lower bound:               L = 0.200 × (1 − 0.5) = 0.100
Upper bound:               U = 0.200 × (1 + 0.5) = 0.300
```

Each pillar's weight therefore lies in [0.10, 0.30]. No single pillar can receive less than 10% or more than 30% of the total weight. The flexibility parameter is set via `bod_weight_flex = 0.5` in `robustness_checks/v3_bod.json` and can be adjusted to test stricter or looser bound scenarios.

The LP is solved once per district using the `lpSolve` package in R (`R/compute_index.R`, function `compute_bod_sepi()`).

### 5. Ranking

Districts are ranked by their BoD score within each country. Rank 1 = strongest socio-economic conditions (highest BoD score). Rankings are produced by `03_run_sepi.R` when `version` is set to `VERSIONS$v3_bod`.

---

## Robustness Evaluation (`04_evaluate.R`)

Two diagnostics are run against the BoD results:

### A. Sensitivity Analysis (Leave-One-Out)

Each pillar indicator is removed in turn and the BoD is recomputed on the remaining four pillars. The Spearman rank correlation between full-model and reduced-model rankings, and the mean absolute rank shift, are reported per pillar per country. Interpretation thresholds follow the baseline:

| Spearman ρ | Interpretation |
|---|---|
| > 0.95 | Redundant — removing the pillar barely changes rankings |
| 0.80 – 0.95 | Moderate influence |
| < 0.80 | Highly influential — the pillar substantially drives the index |

### B. Version Comparison

Spearman rank correlations are computed between `v3_conflict_weighted`, `v3_zscore`, and `v3_bod` district rankings within each country. This directly answers the robustness question: does the weighting method change conclusions about which districts are most deprived?

Results from the current run:

| Country | v3_minmax vs v3_bod | v3_zscore vs v3_bod |
|---|---|---|
| Kenya | 0.860 | 0.890 |
| Somalia | 0.905 | 0.884 |
| South Sudan | 0.794 | 0.794 |

The two conflict-weighted versions (min-max and z-score) are very similar to each other (ρ ≥ 0.952), confirming that normalisation method does not drive results. The BoD version diverges more, especially in South Sudan (ρ = 0.794), indicating that the weighting assumption has a meaningful effect on rankings there. This is expected: BoD allows districts to emphasise their strongest pillar, whereas conflict-weighting assigns weight based on empirical association with instability regardless of district-specific strengths.

---

## Key Limitations

- **Relative scores.** Rankings are within-country. A BoD score of 1 means a district is on the efficiency frontier relative to other districts in the same country — not that it is well-performing in any absolute sense.
- **Indicator set inherited from conflict-weighted version.** The five pillar representatives were originally selected based on their conflict correlation. A purpose-built BoD indicator selection process (based on conceptual coverage and orthogonality of pillars) was not performed. This is appropriate for a robustness check but would need revision if BoD were adopted as the primary methodology.
- **Weight bounds are a choice.** The ±50% flexibility (Scenario I) is a reasonable default following JRC guidance, but the results are sensitive to this parameter. Tighter bounds (e.g. ±25%) push toward equal weighting; looser bounds (±100%, Scenario II) allow pillars to drop to zero weight, which may be too permissive for five pillars.
- **Small samples.** South Sudan has only 10 districts. With few units and few pillars, the LP solutions may be less stable and the sensitivity analysis less informative.
- **Conflict data not used.** Unlike the baseline, this version does not use ACLED data in computation. The conflict correlation output in `03_run_sepi.R` is still produced as a validation check — it shows how well the BoD-derived rankings correlate with observed conflict, but conflict plays no role in generating the scores.

---

## References

- Cherchye, L., Moesen, W., Rogge, N., & Van Puyenbroeck, T. (2007). An introduction to 'benefit of the doubt' composite indicators. *Social Indicators Research*, 82(1), 111–145.
- Cherchye, L., Knox Lovell, C. A., Moesen, W., & Van Puyenbroeck, T. (2007). One market, one number? A composite indicator assessment of EU internal market dynamics. *European Economic Review*, 51(3), 749–779.
- JRC-COIN (2019). Step 5: Weighting methods (I) — Benefit of the Doubt (DEA approach). 17th JRC Annual Training on Composite Indicators & Scoreboards, Ispra.
- Sexton, T. R., Silkman, R. H., & Hogan, A. J. (1986). Data envelopment analysis: Critique and extensions. *New Directions for Program Evaluation*, 32, 73–105.
