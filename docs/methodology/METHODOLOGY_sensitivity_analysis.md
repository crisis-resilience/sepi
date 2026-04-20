# SEPI — Sensitivity Analysis Methodology

### Versions tested: `v1_aligned_equal_geometric` · `v3_aligned_conflict_weighted`

---

## Purpose

Composite indices are sensitive to the choice of indicators and the grouping structure used to aggregate them. This sensitivity analysis tests whether the relative ranking of regions changes materially when individual indicators or entire pillar domains are removed. If rankings are stable across these perturbations, confidence in the index is increased. If rankings shift substantially, those perturbations identify which indicators or pillars are driving the results and should receive additional scrutiny.

---

## Two Sensitivity Tests

### SA1 — Indicator-Level Sensitivity (Leave-One-Indicator-Out per Pillar)

For each pillar with two or more indicators, every possible combination of dropping exactly one indicator from that pillar is considered simultaneously across all eligible pillars. The SEPI is recomputed for each such combination using the full methodology of the respective version. The **SA1 score** for each region is the arithmetic mean of SEPI scores across all these combinations.

Food security is excluded from SA1 in all three countries because it contains only one indicator (`pop_frac_3plus`); removing it would eliminate the pillar entirely, which is the subject of SA2.

**Eligible pillars and combination counts (aligned versions):**

| Country | Eligible pillars (n indicators) | SA1 combinations |
|---------|--------------------------------|-----------------|
| Kenya | Education (2) × Health (3) × Economic (2) × Climate (4) | 48 |
| Somalia | Education (3) × Health (3) × Economic (2) × Climate (4) | 72 |
| South Sudan | Education (3) × Health (3) × Economic (2) × Climate (4) | 72 |

> For V3 (flat conflict-weighted model), pillar membership of each `se_var` is defined by the `pillar_groups` field in the version configuration, which mirrors the V1 pillar structure exactly.

**Interpretation:** A region whose SA1 mean score is close to its baseline SEPI has a stable ranking regardless of which specific indicators are included. A large deviation signals that the region's rank depends heavily on one or a few particular indicators.

---

### SA2 — Pillar-Level Sensitivity (Leave-One-Pillar-Out)

Each of the five pillar domains (Education, Health, Food Security, Economic, Climate) is dropped entirely, and the SEPI is recomputed using only the remaining four pillars. This produces five alternative SEPI scores per region. The **SA2 score** is the arithmetic mean of these five scores.

For V1, dropping a pillar removes all its indicators from the within-pillar aggregation and reduces the geometric mean to four terms. For V3, dropping a pillar removes all its `se_vars` from the flat weighted sum, and the conflict-correlation weights are re-estimated on the reduced indicator set.

**Interpretation:** A region whose SA2 mean is close to its baseline SEPI performs consistently across all pillar combinations — its relative standing is not contingent on any single domain. A large deviation identifies which pillar is most responsible for a region's ranking.

---

## Versions

Two SEPI versions are tested in parallel, allowing the sensitivity of the results to be assessed across both a structural (V1) and a data-driven weighting approach (V3).

| Version | Within-pillar aggregation | Across-pillar aggregation | Weighting |
|---------|--------------------------|--------------------------|-----------|
| `v1_aligned_equal_geometric` | Arithmetic mean | Geometric mean | Equal (1/n per indicator, 1/5 per pillar) |
| `v3_aligned_conflict_weighted` | Flat weighted sum | None (single composite) | Conflict-correlation: \|Pearson r(indicator, conflict events per 1k pop)\| |

Both versions use the same curated, country-aligned indicator sets from the indicator alignment exercise, ensuring that the two versions are comparable and that differences in sensitivity reflect methodology rather than indicator selection.

---

## Computation

All sensitivity runs use identical normalisation (min-max, [0, 1]) and polarity alignment as the baseline versions. For V3, the conflict-correlation weights are re-estimated from scratch for each reduced indicator set rather than rescaled from the full-model weights, ensuring that the weighting structure is internally consistent for each combination.

The mean SEPI across SA1 or SA2 combinations is computed as an unweighted arithmetic mean over all valid (non-NA) runs per region. Regions that are dropped from a specific run due to missing data (V3 `omit` imputation, applied to South Sudan) contribute only their available runs to the mean.

---

## Output

Results are saved to `outputs/`:

| File | Content |
|------|---------|
| `sensitivity_analysis_comparison.xlsx` | Per-country sheets with baseline SEPI, SA1 mean, SA2 mean, and their rank equivalents for both V1 and V3 |
| `sensitivity_comparison_kenya.png` | Formatted comparison table — Kenya |
| `sensitivity_comparison_somalia.png` | Formatted comparison table — Somalia |
| `sensitivity_comparison_south_sudan.png` | Formatted comparison table — South Sudan |

Tables are colour-coded by score (red = low SEPI, green = high SEPI) and sorted by V1 baseline rank. Rankings are assigned within-country: rank 1 = best socio-economic conditions.

---

## Script

`06_sensitivity_analysis.R` — sources `R/sensitivity_analysis.R` and produces all outputs in a single run.
