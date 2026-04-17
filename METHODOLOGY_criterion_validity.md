# SEPI — Criterion Validity Assessment
### External Criterion: IOM IDP Origin Data

---

## Purpose

Criterion validity tests whether the SEPI scores correlate with an independent, real-world outcome that the index should theoretically predict. This document describes the validation strategy, data source, analytical method, and findings.

---

## Hypothesis

**H₁:** There is a negative correlation between the SEPI score and displacement density at the ADM1 level within each country.

**Logic:** SEPI is designed to capture structural socio-economic conditions that underpin or undermine stability. Regions with the lowest socio-economic performance should, all else equal, generate higher rates of population flight. If the index is a valid measure of conflict-relevant socio-economic conditions, the lowest-ranked states should correspond to the greatest displacement pressure.

A higher SEPI score reflects better relative socio-economic performance; a higher displacement fraction reflects greater population flight. The hypothesis therefore predicts a **negative** relationship between the two.

---

## Criterion Variable

**Producer:** International Organization for Migration (IOM) — Displacement Tracking Matrix (DTM)

**Variable:** `pop_frac_idps` — the fraction of a region's population recorded as IDP *origins* (percentage of regional population that has fled).

**IDP definition (IOM DTM standard):** Internally Displaced Persons are individuals or groups who have been forced or obliged to flee or leave their homes or places of habitual residence, in particular as a result of or in order to avoid the effects of armed conflict, situations of generalised violence, violations of human rights, or natural or human-made disasters, and who have not crossed an internationally recognised state border (UN Guiding Principles on Internal Displacement).

**Unit of analysis — origins, not destinations:** The data records the ADM1 region *from which* IDPs were displaced, not where they have settled. This is the analytically correct frame for push-factor validity: the question is whether low SEPI scores predict where people flee *from*, not where they end up. Destination data would conflate pull factors (urban services, relative security) with the structural conditions the index is designed to capture.

**Why displacement density rather than raw IDP counts:** Raw counts are confounded by population size. A region with 100,000 IDPs from a population of 10 million represents a different level of distress than 100,000 IDPs from a population of 500,000. The population fraction normalises for size and measures the *intensity* of displacement pressure.

### Data sources by country

### South Sudan

| Variable | Dataset | Source | Reference period | ADM1 units |
|---|---|---|---|---|
| IDP origins (`pop_frac_idps`) | [South Sudan — Emergency Event Tracking, January–December 2025](https://dtm.iom.int/datasets/south-sudan-emergency-event-tracking-january-december-2025) | IOM Displacement Tracking Matrix (DTM) | Full year 2025 (12 months) | 10 |

### Kenya

| Variable | Dataset | Source | Reference period | ADM1 units |
|---|---|---|---|---|
| IDP origins (`pop_frac_idps`) | [Kenya — IOM DTM (from API)](https://data.humdata.org/dataset/ken-iom-dtm-from-api) | IOM DTM via HDX HAPI | Full year 2024 (12 months) | 23 |

### Somalia

| Variable | Dataset | Source | Reference period | ADM1 units |
|---|---|---|---|---|
| IDP origins (`pop_frac_idps`) | Somalia — Emergency Trend Tracking Dataset (since February 2025) | IOM Displacement Tracking Matrix (DTM) | March 2026 snapshot | 6 |

> **Note on temporal alignment.** The IDP datasets used here represent the most recent available data for each country, consistent with the approach taken for ACLED conflict data and socio-economic indicators throughout the SEPI pipeline: the latest available snapshot is used on the assumption that it best reflects current structural conditions. For South Sudan and Somalia the data falls within 2025–2026, closely aligned with the SEPI reference period. For Kenya, the most recent available data is from 2024; however, as it covers the full calendar year — including the final quarter — it is considered sufficient to reflect the short-term consequences of structural socio-economic fragility and is therefore treated as a valid criterion measure for this validation exercise.

---

## Analytical Method

### Within-country correlation only

Because the IDP data covers different time windows across countries (a full year for South Sudan, approximately three months for Kenya and Somalia), raw displacement counts are not comparable across borders. All analysis is conducted **within each country separately**. The test is whether SEPI correctly identifies the relative socio-economic ranking of regions within a national context, not whether absolute scores are comparable internationally.

### Step 1 — Match ADM1 units

SEPI results and IDP origin data are joined on `adm1_pcode` (OCHA P-code). Only ADM1 units present in **both** datasets enter the correlation. Units with no IDP tracking data are excluded from the validity test. The number of matched units is reported alongside each result.

### Step 2 — Within-country min-max normalisation of displacement density

To make displacement intensities comparable within each country's own scale (and to handle the differing time windows), `pop_frac_idps` is min-max normalised separately for each country:

$$x_{\text{norm}} = \frac{x - \min(x_{\text{country}})}{\max(x_{\text{country}}) - \min(x_{\text{country}})}$$

This scales each country's displacement distribution to [0, 1] based on its own peak. "High displacement" in Somalia is therefore interpreted relative to Somalia's own maximum (0.2%), not South Sudan's (12.1%).

### Step 3 — Spearman's rank correlation

**Spearman's ρ** is used rather than Pearson's *r* for two reasons:

1. Displacement data is heavily right-skewed and subject to outliers. Spearman's tests whether the *rankings* of SEPI match the *rankings* of displacement density — a more stable measure than a linear fit.
2. The hypothesis is ordinal in nature: lower-ranked regions *should* have higher displacement pressure. Rank correlation directly tests this.

**Threshold:** ρ < −0.6 is used as the threshold for a *strong* negative result consistent with the hypothesis. p-values are computed using the asymptotic approximation (exact = FALSE, suitable for n ≥ 6).

---

## Results

### Kenya — **Supported** (ρ = −0.612, p = 0.002, n = 23)

The correlation is statistically significant and exceeds the −0.6 threshold. The gradient is substantively clear: Turkana (SEPI rank 47, worst in Kenya) has the highest displacement density (11.1%); Nyeri (rank 1, best) has 0.035%. Counties with the most severe socio-economic deprivation — Turkana, Marsabit, Mandera, Garissa, Wajir — cluster at both the bottom of the SEPI distribution and the top of displacement density. Kenya provides the strongest evidence for criterion validity.

### South Sudan — **Inconclusive** (ρ = −0.236, p = 0.511, n = 10)

The direction of the correlation is correct (negative) but the result is not statistically significant. The dominant source of noise is a single outlier: **Western Equatoria** sits at SEPI rank 2 (second-best in South Sudan) but records 11.8% displacement — nearly as high as Jonglei (rank 10, worst, 12.1%).

A substantively plausible explanation is that Western Equatoria's displacement is driven primarily by **acute armed group activity** (National Salvation Front / NAS and remnant SPLM-IO factions operating in heavily forested terrain) rather than chronic socio-economic deterioration. SEPI captures structural deprivation; it does not measure armed group presence or acute security conditions. Where security dynamics decouple from socio-economic conditions, SEPI's predictive power for displacement is expected to be weaker. This is a known and legitimate limitation of any composite socio-economic index applied in active conflict settings.

Excluding Western Equatoria, the remaining nine states produce a directionally consistent pattern: Jonglei and Upper Nile (worst SEPI, highest displacement) versus Western Bahr el Ghazal (best SEPI, lowest displacement at 0.2%).

### Somalia — **Underpowered, not interpretable** (ρ = +0.086, p = 0.872, n = 6)

The result should not be interpreted as evidence against the index. Two structural features of the data make the test uninformative:

1. **Insufficient sample size:** Only 6 of 18 SEPI units had IDP tracking data. With n = 6, Spearman's correlation has very low statistical power; even a true population correlation of −0.6 would rarely reach significance.
2. **Near-flat displacement range:** All six fractions fall between 0.007% and 0.203% — an extremely compressed distribution. After min-max normalisation, five of six values are below 0.22. Somalia's displacement crisis is large in absolute terms but the available DTM origin data captures only a marginal fraction of actual flows, likely because large-scale displacement in Somalia is concentrated in urban inflows and internal movements not systematically recorded at the regional origin level.

The directional signal within the matched units is partially consistent with the hypothesis: Bay (SEPI rank 17, worst in the matched set) has the highest displacement (0.203%). The near-zero positive rho is driven by Lower Shabelle (SEPI rank 7, best in the matched set) carrying moderate displacement — a region with complex dynamics given its adjacency to Mogadishu.

---

## Summary

| Country | Matched units | Spearman ρ | p-value | Verdict |
|---|---|---|---|---|
| Kenya | 23 / 47 SEPI units | −0.612 | 0.002 | **Supported** |
| South Sudan | 10 / 10 SEPI units | −0.236 | 0.511 | Inconclusive — outlier (Western Equatoria) |
| Somalia | 6 / 18 SEPI units | +0.086 | 0.872 | Underpowered — criterion data inadequate |

---

## Limitations

**Time window mismatch.** The IDP data covers different periods for each country. Within-country normalisation mitigates but does not eliminate this as a confound. Ideally, annual DTM data aligned to the SEPI reference year would be used.

**Security–socioeconomics decoupling.** In active conflict settings, acute security events can generate large displacement from regions that score relatively well on structural socio-economic indicators. SEPI is not designed to predict displacement driven primarily by armed group activity independent of underlying deprivation.

**IDP origin data coverage.** DTM coverage is uneven across ADM1 units, particularly in Somalia. ADM1 units absent from the IDP dataset are excluded from the test, which means the validation sample may not be representative of the full SEPI distribution.

**Small n in Somalia.** Six observations are insufficient for a meaningful rank correlation test. The Somalia result should not be cited as evidence for or against the index's validity until a more comprehensive displacement dataset is available.

---

## Visualisations

Two figures are produced by `04_evaluate.R` (Section E) and saved to `outputs/figures/`.

### `criterion_validity_scatter.png` — SEPI vs Displacement Density

A three-panel scatter plot, one panel per country. Each point represents a matched ADM1 unit, labelled by name. The x-axis shows the SEPI score (higher = better socio-economic conditions); the y-axis shows within-country min-max normalised displacement density. A linear regression line with 95% confidence band is overlaid to indicate the direction and strength of the relationship. Spearman ρ and the associated p-value are annotated in the top-right corner of each panel.

The scatter plot is the primary visual companion to the Spearman results (Section C). It makes the individual unit-level fit visible — including outliers such as Western Equatoria (South Sudan) — which a summary statistic alone cannot convey.

### `criterion_validity_roc.png` — ROC Curves (Discriminatory Capacity)

A panel of ROC curves, one per country with sufficient matched units (Kenya and South Sudan; Somalia is omitted as n = 6 falls below the minimum threshold of 8). Each curve plots sensitivity (true positive rate: correctly flagging hotspot regions) against 1 − specificity (false positive rate) across all possible SEPI classification thresholds. The shaded area under the curve represents the AUC. The dashed diagonal represents a random classifier (AUC = 0.5). The AUC value and its 95% DeLong confidence interval are annotated on each panel.

Hotspot is defined as an ADM1 unit with `pop_frac_idps` above the within-country median. The South Sudan panel is labelled as exploratory given the small sample (n = 10).

---

## Implementation

The criterion validity analysis is implemented in `04_evaluate.R`:

- **Section C** — Spearman rank correlation: loads `data/socio-economic/criterion_validity_data.csv`, performs within-country min-max normalisation, joins on `adm1_pcode`, and computes Spearman's ρ for each country using the SEPI version configured at the top of the script.
- **Section D** — ROC / hotspot test: binarises displacement density at the within-country median and computes AUC with 95% DeLong CI for countries with n ≥ 8 matched units.
- **Section E** — Visualisations: produces and saves the scatter and ROC figures described above. Requires the `ggrepel` and `pROC` packages (auto-installed if absent).
