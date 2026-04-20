# SEPI — Methodology
### Version: `v3_zscore` (Robustness Check — Z-Score Normalisation)

This document describes the **z-score normalisation** robustness check for the Socio-Economic Peacebuilding Index (SEPI). It is a methodological variant of the baseline `v3_conflict_weighted` version. The purpose of this check is to test whether SEPI district rankings are sensitive to the choice of normalisation method — specifically, whether replacing bounded min-max scaling with z-score standardisation materially reorders districts.

All data sources, administrative boundaries, exclusion criteria, indicator sets, and conflict-correlation weighting are identical to the baseline version. Only the normalisation method and the final output scale differ.

---

## What Differs from the Baseline (`v3_conflict_weighted`)

| Dimension | `v3_conflict_weighted` | `v3_zscore` |
|---|---|---|
| Normalisation | Min-max [0, 1] | Z-score (mean 0, sd 1) |
| Final rescaling | `sepi_raw` rescaled to [0, 1] | `sepi_raw` used directly (`skip_final_rescale = true`) |
| Output score range | [0, 1] | Unbounded; typically negative to positive |
| Indicator inputs | Same | Same |
| Weighting | Conflict-correlation weights | Same |
| Aggregation | Weighted sum | Same |

---

## Conceptual Motivation

Min-max normalisation maps each indicator to [0, 1] relative to the observed range. It is simple and preserves proportional differences, but it is sensitive to extreme values: a single outlier district can compress the rest of the distribution into a narrow band near one end of the scale.

Z-score normalisation instead centres each indicator on its cross-district mean and scales by its standard deviation. This makes the normalised values robust to extreme observations and treats each standard deviation of difference as equivalent regardless of the raw scale. The weighted sum of z-scored indicators then reflects how many standard deviations above or below the cross-district average each district sits in aggregate.

This robustness check is used to test whether the min-max assumption drives rankings. If z-score rankings are substantially different from min-max rankings, it indicates that the results are sensitive to outlier districts or scale compression. If rankings are similar, the index is robust to this methodological choice.

---

## Index Construction

### Polarity, Weighting, and Aggregation

All steps are identical to the baseline. Polarity is applied **before** normalisation — indicators in `bad_vars` (where higher raw values indicate worse conditions) are multiplied by −1 prior to standardisation, so that for all indicators a higher z-score corresponds to better conditions.

Conflict-correlation weights and the weighted-sum aggregation formula are unchanged:

```
SEPI_raw = Σ (sign_i × w_i × z_i)
```

where `z_i` is the z-score of indicator `i` after polarity alignment.

### Normalisation

Each indicator is standardised independently:

```
z = (x − mean(x)) / sd(x)
```

If an indicator has zero variance across districts, all values are set to 0 (no information contributed).

### Final Score Scale

The final rescaling step that maps `sepi_raw` to [0, 1] in the baseline is **skipped** (`skip_final_rescale = true`). The composite z-score is reported directly. A positive score means a district sits above the cross-district mean in aggregate socio-economic conditions; a negative score means below. Rank 1 = highest (most positive) score.

Because the scale is no longer bounded, scores from this version **cannot be compared numerically** to the baseline `v3_conflict_weighted` scores. Rankings are directly comparable.

---

## Key Limitations

- **Unbounded scores.** The composite z-score has no natural floor or ceiling. This makes scores less intuitive for external audiences and precludes direct comparison with the baseline [0, 1] output. For communication purposes, the baseline version scores are preferred.
- **Sensitivity to small samples.** With few districts (notably South Sudan with 10), z-scores are more volatile than min-max values: a single district shifting its raw value changes the mean and standard deviation for the whole country, cascading into all other districts' normalised values. Min-max only moves the range boundary if the extreme district changes.
- **Outlier compression not a concern in this dataset.** Given that the two versions produce near-identical rankings, the min-max normalisation in the baseline is not materially distorted by outliers. This version therefore does not reveal a problem with the baseline; it confirms the baseline is stable.
