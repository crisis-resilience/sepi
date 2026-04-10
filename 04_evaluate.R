# ============================================================================
# 04_evaluate.R — Sensitivity analysis and version comparison
# ============================================================================
# Run when: validating a new or active version, checking indicator influence,
#           comparing rank stability across methodologies.
#
# Prerequisites: 03_run_sepi.R should have been run for the same version
#               to confirm outputs before running evaluation.
#
# Set `version` below to the version under evaluation.
# The baseline version for comparison is set in Section B.
# ============================================================================

for (pkg in c("tidyverse", "psych", "purrr", "rlang", "jsonlite")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(tidyverse)

source("R/config.R")
source("R/utils.R")
source("R/load_data.R")
source("R/normalise.R")
source("R/compute_index.R")

# ── Configure + Load ──────────────────────────────────────────────────────────
version      <- VERSIONS$v3_bod   # ← change to switch version under evaluation
all_data     <- load_all_data(version = version)
sepi_results <- compute_all_countries(all_data, version)

# ── A. Sensitivity analysis ───────────────────────────────────────────────────
# Leave-one-out: how much do SEPI ranks change when each indicator is removed?
sensitivity_results <- sensitivity_all_countries(all_data, version)

# ── B. Version comparison ─────────────────────────────────────────────────────
# Compare SEPI ranks between v3_conflict_weighted (min-max), v3_zscore, and
# v3_bod (Benefit of the Doubt weighting).
# Each version is computed explicitly so results are independent of whatever
# `version` is set to at the top of this script.
results_v3_minmax <- compute_all_countries(all_data, VERSIONS$v3_conflict_weighted)
results_v3_zscore <- compute_all_countries(all_data, VERSIONS$v3_zscore)
results_v3_bod    <- compute_all_countries(all_data, VERSIONS$v3_bod)

comparison <- compare_versions(list(
  v3_minmax = results_v3_minmax,
  v3_zscore = results_v3_zscore,
  v3_bod    = results_v3_bod
))

for (country in names(comparison)) {
  cat("\n", country_label(country), "— Rank correlations:\n")
  print(round(comparison[[country]]$rank_correlation, 3))
}

cat("\nDone.\n")

# ── C. Criterion Validity — IOM IDP origin correlation ────────────────────────
# Tests H1: SEPI is negatively correlated with IDP displacement density
# (pop_frac_idps) within each country. Uses Spearman's rho on within-country
# min-max normalised displacement fractions to handle differing time windows.
# Source data: data/criterion_validity_data.csv

cat("\n========================================\n")
cat(" Criterion Validity — IOM IDP Origin Data\n")
cat("========================================\n")
cat(" H1: lower SEPI -> higher displacement density (rho < 0)\n")
cat(" Target: rho < -0.6 (strong negative)\n\n")

idp_raw <- read.csv("data/criterion_validity_data.csv", stringsAsFactors = FALSE)

# Min-max normalise pop_frac_idps within each country
idp_data <- idp_raw |>
  dplyr::group_by(country) |>
  dplyr::mutate(
    pop_frac_norm = (pop_frac_idps - min(pop_frac_idps)) /
                   (max(pop_frac_idps) - min(pop_frac_idps))
  ) |>
  dplyr::ungroup()

# Use the primary version (set at top of script) for criterion validity
for (country in names(sepi_results)) {
  sepi_df <- sepi_results[[country]]

  country_code_map <- c(
    south_sudan = "SSD",
    kenya       = "KEN",
    somalia     = "SOM"
  )
  cc <- country_code_map[[country]]
  if (is.null(cc)) next

  idp_country <- dplyr::filter(idp_data, country_code == cc)
  if (nrow(idp_country) == 0) {
    cat(sprintf("  %s: no IDP data found — skipping\n", country_label(country)))
    next
  }

  # inner_join: only ADM1 units present in BOTH SEPI and IDP data are tested.
  # Units with no IDP tracking are excluded from the correlation.
  merged <- dplyr::inner_join(
    dplyr::select(sepi_df, adm1_pcode, adm1_name, sepi, sepi_rank),
    dplyr::select(idp_country, adm1_pcode, pop_frac_idps, pop_frac_norm),
    by = "adm1_pcode"
  )

  n_matched <- nrow(merged)
  n_sepi    <- nrow(sepi_df)
  n_idp     <- nrow(idp_country)

  if (n_matched < 3) {
    cat(sprintf(
      "  %s: only %d matched ADM1 units (need >= 3) — skipping\n",
      country_label(country), n_matched
    ))
    next
  }

  rho   <- stats::cor(merged$sepi, merged$pop_frac_norm,
                      method = "spearman", use = "complete.obs")
  p_val <- stats::cor.test(merged$sepi, merged$pop_frac_norm,
                            method = "spearman", exact = FALSE)$p.value

  verdict <- if (is.na(rho))         "insufficient data"
             else if (rho < -0.6)    "SUPPORTED (rho < -0.6)"
             else if (rho < 0)       "weak negative — not conclusive"
             else                    "NOT supported (positive or near-zero)"

  cat(sprintf(
    "%s\n  Matched: %d / %d SEPI units  (%d IDP regions)\n  Spearman rho = %.3f  (p = %.3f)\n  Verdict: %s\n\n",
    country_label(country), n_matched, n_sepi, n_idp,
    rho, p_val, verdict
  ))

  # Print the matched unit-level table for inspection
  out_tbl <- merged |>
    dplyr::arrange(sepi_rank) |>
    dplyr::mutate(
      sepi           = round(sepi, 3),
      pop_frac_idps  = round(pop_frac_idps, 3),
      pop_frac_norm  = round(pop_frac_norm, 3)
    ) |>
    dplyr::select(adm1_name, sepi_rank, sepi, pop_frac_idps, pop_frac_norm)

  print(as.data.frame(out_tbl), row.names = FALSE)
  cat("\n")
}

cat("Criterion validity check complete.\n")

# ── D. Discriminatory Capacity — ROC / Hotspot Test ───────────────────────────
# Binary complement to the Spearman test (Section C).
# Question: can SEPI discriminate displacement hotspots from non-hotspots?
# Hotspot definition: ADM1 units above the within-country median pop_frac_idps.
# Predictor: SEPI score (lower SEPI -> higher P(hotspot), so direction = ">").
# Requires idp_data from Section C — run sections together.
# Somalia (n = 6) is skipped: too few units for a meaningful ROC curve.

if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")

cat("\n========================================\n")
cat(" Discriminatory Capacity — ROC / Hotspot Test\n")
cat("========================================\n")
cat(" Hotspot = pop_frac_idps above within-country median\n")
cat(" Predictor = SEPI score (lower -> more likely hotspot)\n")
cat(" Target: AUC >= 0.70 (acceptable discrimination)\n\n")

MIN_N_ROC <- 8  # minimum matched units to attempt ROC

for (country in names(sepi_results)) {
  sepi_df <- sepi_results[[country]]

  country_code_map <- c(south_sudan = "SSD", kenya = "KEN", somalia = "SOM")
  cc <- country_code_map[[country]]
  if (is.null(cc)) next

  idp_country <- dplyr::filter(idp_data, country_code == cc)
  if (nrow(idp_country) == 0) next

  merged <- dplyr::inner_join(
    dplyr::select(sepi_df, adm1_pcode, adm1_name, sepi, sepi_rank),
    dplyr::select(idp_country, adm1_pcode, pop_frac_idps),
    by = "adm1_pcode"
  )

  n_matched <- nrow(merged)

  if (n_matched < MIN_N_ROC) {
    cat(sprintf("  %s: n = %d — too few units for ROC (skipped)\n\n",
                country_label(country), n_matched))
    next
  }

  # Define hotspot: above median displacement density within matched units
  threshold <- median(merged$pop_frac_idps)
  merged$hotspot <- as.integer(merged$pop_frac_idps > threshold)
  n_hotspot <- sum(merged$hotspot)

  if (n_hotspot < 2 || n_hotspot > (n_matched - 2)) {
    cat(sprintf("  %s: hotspot class too imbalanced (n_hotspot = %d / %d) — skipping\n\n",
                country_label(country), n_hotspot, n_matched))
    next
  }

  # ROC: direction = ">" because higher SEPI -> lower P(hotspot)
  roc_obj <- pROC::roc(merged$hotspot, merged$sepi,
                        direction = ">",
                        quiet     = TRUE,
                        ci        = TRUE,
                        ci.method = "delong")

  auc_val <- as.numeric(pROC::auc(roc_obj))
  ci_vals  <- as.numeric(pROC::ci(roc_obj))  # lower, AUC, upper

  # Best SEPI cut-off by Youden's J (maximises sensitivity + specificity - 1)
  coords_df <- pROC::coords(roc_obj, x = "best", best.method = "youden",
                             ret       = c("threshold", "sensitivity", "specificity"),
                             transpose = FALSE)

  verdict <- if (auc_val >= 0.80)      "GOOD discrimination (AUC >= 0.80)"
             else if (auc_val >= 0.70) "ACCEPTABLE discrimination (AUC >= 0.70)"
             else if (auc_val >= 0.60) "poor — weak discrimination"
             else                      "NO discrimination (near random)"

  note <- if (country == "south_sudan") " [exploratory — n = 10]" else ""

  cat(sprintf(
    "%s%s\n  n = %d  |  Hotspots (above median %.3f%%): %d / %d\n  AUC = %.3f  (95%% CI: %.3f – %.3f)\n  Verdict: %s\n  Optimal SEPI cut-off (Youden's J): %.3f  |  Sensitivity: %.2f  |  Specificity: %.2f\n\n",
    country_label(country), note,
    n_matched, threshold, n_hotspot, n_matched,
    auc_val, ci_vals[1], ci_vals[3], verdict,
    coords_df$threshold[1], coords_df$sensitivity[1], coords_df$specificity[1]
  ))

  # Unit-level table showing classification
  out_tbl <- merged |>
    dplyr::arrange(sepi_rank) |>
    dplyr::mutate(
      sepi          = round(sepi, 3),
      pop_frac_idps = round(pop_frac_idps, 3),
      hotspot       = ifelse(hotspot == 1, "YES", "no")
    ) |>
    dplyr::select(adm1_name, sepi_rank, sepi, pop_frac_idps, hotspot)

  print(as.data.frame(out_tbl), row.names = FALSE)
  cat("\n")
}

cat("Discriminatory capacity check complete.\n")

# ── E. Criterion Validity Visualisations ──────────────────────────────────────
# Two figures saved to outputs/figures/:
#   criterion_validity_scatter.png — SEPI vs displacement density per country
#   criterion_validity_roc.png     — ROC curves for Kenya & South Sudan

source("R/visualise.R")

dir.create(file.path("outputs", "figures"), showWarnings = FALSE, recursive = TRUE)

country_code_map <- c(south_sudan = "SSD", kenya = "KEN", somalia = "SOM")

# ---- E1. Scatter: SEPI vs displacement density --------------------------------

scatter_plots <- list()

for (country in names(sepi_results)) {
  cc      <- country_code_map[[country]]
  sepi_df <- sepi_results[[country]]

  idp_country <- dplyr::filter(idp_data, country_code == cc)
  merged <- dplyr::inner_join(
    dplyr::select(sepi_df, adm1_pcode, adm1_name, sepi),
    dplyr::select(idp_country, adm1_pcode, pop_frac_idps, pop_frac_norm),
    by = "adm1_pcode"
  )
  if (nrow(merged) < 3) next

  rho   <- round(stats::cor(merged$sepi, merged$pop_frac_norm,
                             method = "spearman", use = "complete.obs"), 3)
  p_val <- stats::cor.test(merged$sepi, merged$pop_frac_norm,
                            method = "spearman", exact = FALSE)$p.value
  p_lab <- if (p_val < 0.001) "p < 0.001" else sprintf("p = %.3f", p_val)

  scatter_plots[[country]] <- ggplot(merged,
      aes(x = sepi, y = pop_frac_norm, label = adm1_name)) +
    geom_point(colour = "#2c7fb8", size = 2.5, alpha = 0.8) +
    ggrepel::geom_text_repel(size = 2.8, colour = "grey30",
                              max.overlaps = 15, seed = 42) +
    geom_smooth(method = "lm", se = TRUE, colour = "#e34a33",
                linewidth = 0.8, alpha = 0.15) +
    annotate("text", x = Inf, y = Inf,
             label = sprintf("Spearman \u03c1 = %s\n%s", rho, p_lab),
             hjust = 1.1, vjust = 1.5, size = 3.2, colour = "grey20") +
    labs(
      title    = country_label(country),
      x        = "SEPI score (higher = better)",
      y        = "Displacement density (within-country normalised)"
    ) +
    theme_sepi()
}

if (length(scatter_plots) > 0) {
  if (!requireNamespace("ggrepel", quietly = TRUE)) install.packages("ggrepel")
  library(ggrepel)

  # Rebuild plots now that ggrepel is loaded
  scatter_plots <- list()
  for (country in names(sepi_results)) {
    cc      <- country_code_map[[country]]
    sepi_df <- sepi_results[[country]]
    idp_country <- dplyr::filter(idp_data, country_code == cc)
    merged <- dplyr::inner_join(
      dplyr::select(sepi_df, adm1_pcode, adm1_name, sepi),
      dplyr::select(idp_country, adm1_pcode, pop_frac_idps, pop_frac_norm),
      by = "adm1_pcode"
    )
    if (nrow(merged) < 3) next

    rho   <- round(stats::cor(merged$sepi, merged$pop_frac_norm,
                               method = "spearman", use = "complete.obs"), 3)
    p_val <- stats::cor.test(merged$sepi, merged$pop_frac_norm,
                              method = "spearman", exact = FALSE)$p.value
    p_lab <- if (p_val < 0.001) "p < 0.001" else sprintf("p = %.3f", p_val)

    scatter_plots[[country]] <- ggplot(merged,
        aes(x = sepi, y = pop_frac_norm, label = adm1_name)) +
      geom_point(colour = "#2c7fb8", size = 2.5, alpha = 0.8) +
      ggrepel::geom_text_repel(size = 2.8, colour = "grey30",
                                max.overlaps = 15, seed = 42) +
      geom_smooth(method = "lm", se = TRUE, colour = "#e34a33",
                  linewidth = 0.8, alpha = 0.15) +
      annotate("text", x = Inf, y = Inf,
               label = sprintf("Spearman \u03c1 = %s\n%s", rho, p_lab),
               hjust = 1.1, vjust = 1.5, size = 3.2, colour = "grey20") +
      labs(
        title    = country_label(country),
        x        = "SEPI score (higher = better)",
        y        = "Displacement density\n(within-country normalised)"
      ) +
      theme_sepi()
  }

  combined_scatter <- patchwork::wrap_plots(scatter_plots, ncol = 3) +
    patchwork::plot_annotation(
      title    = "Criterion Validity: SEPI vs IDP Displacement Density",
      subtitle = "Spearman rank correlation | IOM DTM origin data | matched ADM1 units only",
      theme    = theme_sepi()
    )

  scatter_path <- file.path("outputs", "figures", "criterion_validity_scatter.png")
  ggsave(scatter_path, combined_scatter, width = 15, height = 6, dpi = 150)
  message("Saved: ", scatter_path)
}

# ---- E2. ROC curves for countries with sufficient n -------------------------

roc_plots    <- list()
roc_countries <- list()

for (country in names(sepi_results)) {
  cc      <- country_code_map[[country]]
  sepi_df <- sepi_results[[country]]
  idp_country <- dplyr::filter(idp_data, country_code == cc)

  merged <- dplyr::inner_join(
    dplyr::select(sepi_df, adm1_pcode, adm1_name, sepi),
    dplyr::select(idp_country, adm1_pcode, pop_frac_idps),
    by = "adm1_pcode"
  )
  n_matched <- nrow(merged)
  if (n_matched < MIN_N_ROC) next

  threshold      <- median(merged$pop_frac_idps)
  merged$hotspot <- as.integer(merged$pop_frac_idps > threshold)
  n_hotspot      <- sum(merged$hotspot)
  if (n_hotspot < 2 || n_hotspot > (n_matched - 2)) next

  roc_obj <- pROC::roc(merged$hotspot, merged$sepi,
                        direction = ">", quiet = TRUE,
                        ci = TRUE, ci.method = "delong")

  auc_val <- as.numeric(pROC::auc(roc_obj))
  ci_vals  <- as.numeric(pROC::ci(roc_obj))

  roc_df <- data.frame(
    specificity = roc_obj$specificities,
    sensitivity = roc_obj$sensitivities
  )

  note  <- if (country == "south_sudan") " (exploratory, n=10)" else ""
  p_roc <- ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                colour = "grey60", linewidth = 0.5) +
    geom_line(colour = "#2c7fb8", linewidth = 1.1) +
    geom_area(fill = "#2c7fb8", alpha = 0.1) +
    annotate("text", x = 0.65, y = 0.15,
             label = sprintf("AUC = %.3f\n95%% CI: %.3f\u2013%.3f",
                             auc_val, ci_vals[1], ci_vals[3]),
             size = 3.3, colour = "grey20", hjust = 0) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    labs(
      title    = paste0(country_label(country), note),
      subtitle = sprintf("Hotspot = pop_frac_idps > %.3f%% (median)", threshold),
      x        = "1 - Specificity (False Positive Rate)",
      y        = "Sensitivity (True Positive Rate)"
    ) +
    theme_sepi()

  roc_plots[[country]] <- p_roc
}

if (length(roc_plots) > 0) {
  combined_roc <- patchwork::wrap_plots(roc_plots, ncol = length(roc_plots)) +
    patchwork::plot_annotation(
      title    = "Discriminatory Capacity: ROC Curves",
      subtitle = "Can SEPI identify displacement hotspots? | Hotspot = above-median displacement density",
      theme    = theme_sepi()
    )

  roc_path <- file.path("outputs", "figures", "criterion_validity_roc.png")
  ggsave(roc_path, combined_roc,
         width = length(roc_plots) * 5.5, height = 5.5, dpi = 150)
  message("Saved: ", roc_path)
}

cat("Visualisations complete.\n")
