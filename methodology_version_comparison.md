# SEPI Version Comparison — Methodology Note

`05_compare_versions.R` evaluates whether **v1 (equal-weighted geometric mean)** or
**v3 (conflict-weighted)** should be adopted as the primary SEPI method.
It assesses each version on three dimensions.

---

## A. Rank Stability

Two robustness variants are computed for each version family:

| Variant | What changes |
|---------|-------------|
| z-score | Min-max normalisation replaced by z-score |
| BoD | Equal weights replaced by Benefit of Doubt (DEA) weights |

Three metrics compare each variant against its primary version:

- **Spearman ρ** — overall rank-order agreement (1 = identical ordering)
- **MARS** — Mean Absolute Rank Shift: average number of places an ADM1 moves across variants. Captures local disruption that high ρ can hide.
- **Top-5 stability** — % of the 5 worst-off ADM1s in the primary version that remain in the bottom 5 in each variant. Directly measures whether targeting decisions would change.

*Thresholds:* ρ ≥ 0.90 / MARS ≤ 1.5 / Top-5 ≥ 80% = stable.

---

## B. Criterion Validity

**Hypothesis:** lower SEPI (more deprived) → higher IDP displacement density.

Spearman ρ between the SEPI score and within-country min-max normalised IDP
displacement fraction (IOM DTM data, ADM1 level). Target: ρ < −0.60.

---

## C. Discriminatory Capacity (AUC)

ADM1 units above the median IDP displacement density are labelled hotspots.
A ROC curve tests whether lower SEPI predicts hotspot status.
AUC ≥ 0.70 = acceptable; ≥ 0.80 = good.

---

## D & E. Outputs

- `outputs/figures/version_comparison.png` — colour-coded scorecard (all metrics)
- `outputs/figures/ranks_v{1|3}_{country}.png` — unit-level rank tables showing every ADM1's rank across primary and both variants, with shift (Δ) highlighted where |Δ| ≥ 3
