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

for (pkg in c("ggrepel")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(ggrepel)

source("R/visualise.R")
source("R/criterion_validity_conflict.R")

# ── Configure + Load ──────────────────────────────────────────────────────────
# When sourced from run_all.R, .sepi_run_version is set there; otherwise use the
# version defined below.
version      <- if (exists(".sepi_run_version")) .sepi_run_version else VERSIONS$v1_equal_geometric  # ← change to switch version under evaluation
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

# ── D. Criterion Validity Visualisations (Displacement) ──────────────────────
# Figure saved to outputs/figures/criterion_validity/:
#   criterion_validity_scatter_displacement_{version}.png — SEPI vs displacement density

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

cat("Displacement visualisations complete.\n")

# ── E. Criterion Validity — ACLED Conflict Intensity ──────────────────────────
# Parallel test using conflict events per 1k population (ACLED) as the
# external criterion, over three time windows:
#   "10y"  -> 2016–2025
#   "5y"   -> 2021–2025
#   "2025" -> 2025 only (circular for v2_conflict_weighted)
# Produces 3 scatter PNGs (one per window).

cat("\n========================================\n")
cat(" Criterion Validity — ACLED Conflict Intensity\n")
cat("========================================\n")
cat(" H1: lower SEPI -> higher conflict events per 1k (rho < 0)\n")
cat(" Target: rho < -0.6 (strong negative)\n")
cat(" Note: 2025 window is circular for v2_conflict_weighted\n\n")

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

    cat(sprintf("%s\n  Matched: %d / %d SEPI units\n  Spearman rho = %.3f  (p = %.3f)  [%s]\n",
                country_label(country), n_matched, nrow(sepi_df),
                cv$rho, cv$p, cv$verdict))

    out_tbl <- merged |>
      dplyr::left_join(
        dplyr::select(sepi_df, adm1_pcode, sepi_rank),
        by = "adm1_pcode"
      ) |>
      dplyr::arrange(sepi_rank) |>
      dplyr::mutate(
        sepi            = round(sepi, 3),
        conflict_per_1k = round(conflict_per_1k, 4),
        conflict_norm   = round(conflict_norm, 3)
      ) |>
      dplyr::select(adm1_name, sepi_rank, sepi, conflict_per_1k, conflict_norm)

    print(as.data.frame(out_tbl), row.names = FALSE)
    cat("\n")
  }

  save_conflict_scatter(sepi_results, window, version)
}

save_conflict_scatter_by_country(sepi_results, version)

cat("\nConflict criterion validity check complete.\n")
