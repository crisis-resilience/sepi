# ============================================================================
# 04_evaluate.R — Version comparison and criterion validity
# ============================================================================
# Run when: validating a new or active version, checking criterion validity,
#           comparing rank stability across robustness variants.
#           For full sensitivity analysis (SA1/SA2), use 06_sensitivity_analysis.R.
#
# Prerequisites: 03_run_sepi.R should have been run for the same version
#               to confirm outputs before running evaluation.
#
# Set `version` below to the version under evaluation.
# The baseline version for comparison is set in Section A.
# ============================================================================

source("R/setup.R")

for (pkg in c("pROC", "ggrepel")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(ggrepel)

source("R/visualise.R")
source("R/criterion_validity_conflict.R")

# ── Configure + Load ──────────────────────────────────────────────────────────
# When sourced from run_all.R, .sepi_run_version is set there; otherwise use the
# version defined below.
version      <- if (exists(".sepi_run_version")) .sepi_run_version else VERSIONS$v1_aligned_equal_geometric  # ← change to switch version under evaluation
all_data     <- load_all_data(version = version)
sepi_results <- compute_all_countries(all_data, version)

# ── A. Version comparison ─────────────────────────────────────────────────────
# Compare SEPI ranks between the active version and its robustness variants.
# Variants are declared in each version's JSON as "robustness_variants".
# Changing `version` at the top automatically picks the right robustness checks.

variant_keys <- c(version$name, version$robustness_variants)

if (length(variant_keys) < 2) {
  cat("No robustness_variants defined for", version$name, "— skipping comparison.\n")
} else {
  variant_results <- lapply(rlang::set_names(variant_keys), function(vname) {
    vobj <- VERSIONS[[vname]]
    if (is.null(vobj)) stop("Version '", vname, "' not found in VERSIONS.")
    compute_all_countries(all_data, vobj)
  })

  comparison <- compare_versions(variant_results)

  for (country in names(comparison)) {
    cat("\n", country_label(country), "— Rank correlations:\n")
    print(round(comparison[[country]]$rank_correlation, 3))
  }
}

cat("\nDone.\n")

# ── B. Criterion Validity — IOM IDP origin correlation ────────────────────────
# Tests H1: SEPI is negatively correlated with IDP displacement density
# (pop_frac_idps) within each country. Uses Spearman's rho on within-country
# min-max normalised displacement fractions to handle differing time windows.
# Source data: data/socio-economic/criterion_validity_data.csv

cat("\n========================================\n")
cat(" Criterion Validity — IOM IDP Origin Data\n")
cat("========================================\n")
cat(" H1: lower SEPI -> higher displacement density (rho < 0)\n")
cat(" Target: rho < -0.6 (strong negative)\n\n")

idp_data <- load_idp_data()

# Use the primary version (set at top of script) for criterion validity
for (country in names(sepi_results)) {
  sepi_df <- sepi_results[[country]]

  cc <- COUNTRY_CODE_MAP[[country]]
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

# ── C. Discriminatory Capacity — ROC / Hotspot Test ───────────────────────────
# Binary complement to the Spearman test (Section C).
# Question: can SEPI discriminate displacement hotspots from non-hotspots?
# Hotspot definition: ADM1 units above the within-country median pop_frac_idps.
# Predictor: SEPI score (lower SEPI -> higher P(hotspot), so direction = ">").
# Requires idp_data from Section B — run sections together.
# Somalia (n = 6) is skipped: too few units for a meaningful ROC curve.

cat("\n========================================\n")
cat(" Discriminatory Capacity — ROC / Hotspot Test\n")
cat("========================================\n")
cat(" Hotspot = pop_frac_idps above within-country median\n")
cat(" Predictor = SEPI score (lower -> more likely hotspot)\n")
cat(" Target: AUC >= 0.70 (acceptable discrimination)\n\n")

MIN_N_ROC <- 8  # minimum matched units to attempt ROC

for (country in names(sepi_results)) {
  sepi_df <- sepi_results[[country]]

  cc <- COUNTRY_CODE_MAP[[country]]
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

# ── D. Criterion Validity Visualisations (Displacement) ──────────────────────
# Two figures saved to outputs/figures/criterion_validity/:
#   criterion_validity_scatter_displacement_{version}.png — SEPI vs displacement density
#   criterion_validity_roc_displacement_{version}.png     — ROC curves for Kenya & South Sudan

# ---- D1. Scatter: SEPI vs displacement density --------------------------------

scatter_plots <- list()

for (country in names(sepi_results)) {
  cc          <- COUNTRY_CODE_MAP[[country]]
  sepi_df     <- sepi_results[[country]]
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

if (length(scatter_plots) > 0) {
  combined_scatter <- patchwork::wrap_plots(scatter_plots, ncol = 3) +
    patchwork::plot_annotation(
      title    = "Criterion Validity: SEPI vs IDP Displacement Density",
      subtitle = "Spearman rank correlation | IOM DTM origin data | matched ADM1 units only",
      theme    = theme_sepi()
    )

  scatter_path <- versioned_output_path(
    version, "figures", "criterion_validity",
    "criterion_validity_scatter_displacement"
  )
  ggsave(scatter_path, combined_scatter, width = 15, height = 6, dpi = 150)
  message("Saved: ", scatter_path)

  # Per-country individual scatter plots with footnote
  displacement_footnote <- paste0(
    "ρ (rho): Spearman rank correlation coefficient between SEPI and IDP displacement density.\n",
    "A negative value indicates higher socio-economic conditions are associated with lower displacement.\n",
    "Displacement data: IOM DTM origin-based tracking | within-country min-max normalised."
  )
  for (country in names(scatter_plots)) {
    p_single <- scatter_plots[[country]] +
      labs(caption = displacement_footnote) +
      theme(
        plot.caption          = element_text(size = 7, colour = "grey40", hjust = 0,
                                             margin = margin(t = 6)),
        plot.caption.position = "plot"
      )
    single_path <- versioned_output_path(
      version, "figures", "criterion_validity",
      paste0("criterion_validity_scatter_displacement_", country)
    )
    ggsave(single_path, p_single, width = 7, height = 6.5, dpi = 150)
    message("Saved: ", single_path)
  }
}

# ---- D2. ROC curves for countries with sufficient n -------------------------

roc_plots    <- list()
roc_countries <- list()

for (country in names(sepi_results)) {
  cc          <- COUNTRY_CODE_MAP[[country]]
  sepi_df     <- sepi_results[[country]]
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

  roc_path <- versioned_output_path(
    version, "figures", "criterion_validity",
    "criterion_validity_roc_displacement"
  )
  ggsave(roc_path, combined_roc,
         width = length(roc_plots) * 5.5, height = 5.5, dpi = 150)
  message("Saved: ", roc_path)
}

cat("Displacement visualisations complete.\n")

# ── E. Criterion Validity — ACLED Conflict Intensity ──────────────────────────
# Parallel test using conflict events per 1k population (ACLED) as the
# external criterion, over three time windows:
#   "10y"  -> 2016–2025
#   "5y"   -> 2021–2025
#   "2025" -> 2025 only (circular for v3_conflict_weighted)
# Produces 3 scatter PNGs and 3 ROC PNGs (one per window), mirroring Section E.

cat("\n========================================\n")
cat(" Criterion Validity — ACLED Conflict Intensity\n")
cat("========================================\n")
cat(" H1: lower SEPI -> higher conflict events per 1k (rho < 0)\n")
cat(" Target: rho < -0.6 (strong negative)\n")
cat(" Note: 2025 window is circular for v3_conflict_weighted\n\n")

conflict_windows <- c("10y", "5y", "2025")

for (window in conflict_windows) {
  cat("\n---- Window:", conflict_window_label(window), "----\n\n")

  for (country in names(sepi_results)) {
    sepi_df <- sepi_results[[country]]
    merged  <- prepare_conflict_match(sepi_df, window)
    n_matched <- nrow(merged)

    if (n_matched < 3) {
      cat(sprintf("  %s: %d matched units — skipping\n",
                  country_label(country), n_matched))
      next
    }

    cv  <- criterion_validity_conflict(sepi_results, country, window)
    auc <- auc_capacity_conflict(sepi_results, country, window)

    cat(sprintf("%s\n  Matched: %d / %d SEPI units\n  Spearman rho = %.3f  (p = %.3f)  [%s]\n",
                country_label(country), n_matched, nrow(sepi_df),
                cv$rho, cv$p, cv$verdict))
    if (!is.na(auc$auc)) {
      cat(sprintf("  AUC = %.3f  (95%% CI: %.3f\u2013%.3f)  [%s]\n",
                  auc$auc, auc$ci_lo, auc$ci_hi, auc$verdict))
    } else {
      cat(sprintf("  AUC = n/a  [%s]\n", auc$verdict))
    }

    out_tbl <- merged |>
      dplyr::left_join(
        dplyr::select(sepi_df, adm1_pcode, sepi_rank),
        by = "adm1_pcode"
      ) |>
      dplyr::arrange(sepi_rank) |>
      dplyr::mutate(
        sepi            = round(sepi, 3),
        conflict_per_1k = round(conflict_per_1k, 4),
        conflict_norm   = round(conflict_norm, 3),
        hotspot         = ifelse(hotspot == 1, "YES", "no")
      ) |>
      dplyr::select(adm1_name, sepi_rank, sepi, conflict_per_1k, conflict_norm, hotspot)

    print(as.data.frame(out_tbl), row.names = FALSE)
    cat("\n")
  }

  save_conflict_scatter(sepi_results, window, version)
  save_conflict_roc(sepi_results, window, version, min_n = MIN_N_ROC)
}

save_conflict_scatter_by_country(sepi_results, version)

cat("\nConflict criterion validity check complete.\n")
