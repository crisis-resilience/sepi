# ============================================================================
# SEPI — Main Pipeline
# ============================================================================
#
# Run this script to execute the full SEPI workflow:
#
#   Step A  [OPTIONAL] Candidate Exploration
#             Surveys all available variables in the raw data and cross-
#             references the data dictionaries to produce a triage report
#             (outputs/candidate_report_{country}.csv) for indicator selection.
#             Run once when adding new indicators to config.R.
#
#   Step B  Indicator Screening
#             Validates the configured indicator set against OECD Handbook
#             quality criteria and PCA feasibility checks. Run after any
#             change to INDICATOR_CONFIG in config.R.
#
#   Step 1  Load and clean country data
#   Step 2  Internal consistency diagnostics (Cronbach's alpha, correlations)
#   Step 3  Compute the SEPI (choose version in Step 3 below)
#   Step 4  [OPTIONAL] Sensitivity analysis — leave-one-out per indicator
#   Step 5  Analyse SEPI–conflict linkages
#   Step 6  Generate visualisations
#   Step 7  Export results to Excel
#
# VERSION COMPARISON: see the section at the bottom of this file.
# ============================================================================

# ---- 0. Packages -----------------------------------------------------------

required_packages <- c("tidyverse", "readr", "psych", "ggrepel", "openxlsx",
                        "rvest", "purrr", "rlang")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}
library(tidyverse)

# ---- 1. Source modules -----------------------------------------------------

source("R/config.R")
source("R/utils.R")
source("R/load_data.R")
source("R/normalise.R")
source("R/compute_index.R")
source("R/diagnostics.R")
source("R/screen_indicators.R")
source("R/explore_candidates.R")
source("R/conflict_analysis.R")
source("R/visualise.R")
source("R/export_excel.R")
source("R/build_global_data.R")

# ---- 1.5 Optional: rebuild global merged files ----------------------------
# Set to TRUE to regenerate data/sepi_indicators_all_countries_latest.csv
# and data/sepi_indicators_metadata_all_countries.csv from source files.
# The rest of the pipeline reads from these pre-built CSVs.

run_global_merge_build <- TRUE

if (run_global_merge_build) {
  cat("\n--- Building merged 3-country indicators + metadata ---\n")
  merged_outputs <- build_global_indicator_assets()
  cat("Merged indicators:", merged_outputs$files$indicators_file, "\n")
  cat("Merged metadata:  ", merged_outputs$files$metadata_file, "\n")
  cat("QC report:        ", merged_outputs$files$qc_file, "\n")
}

# ---- 2. Load data ----------------------------------------------------------

cat("Loading data from:", GLOBAL_DATA$data_file, "\n")
all_data <- load_all_data()

cat("  Kenya:       ", nrow(all_data$kenya),       "regions\n")
cat("  Somalia:     ", nrow(all_data$somalia),      "regions\n")
cat("  South Sudan: ", nrow(all_data$south_sudan),  "regions\n")

# ============================================================================
# Step A  [OPTIONAL] Candidate Indicator Exploration
# ============================================================================
# Set to TRUE to survey what additional variables are available in the raw
# data files for potential inclusion in config.R.
# Outputs CSV reports to outputs/candidate_report_{country}.csv.
# Requires the 'rvest' package for HTML dictionary parsing.
#
# Run the full pipeline first (Steps 3 onward) so sepi_results is available,
# then re-run with run_step_a = TRUE to include the SEPI-correlation signal.

run_step_a <- TRUE

if (run_step_a) {
  candidate_reports <- explore_all_candidates(
    all_data,
    sepi_results = NULL   # replace with sepi_results after Step 3
  )

  cat("\n--- Generating candidate correlation matrices ---\n")
  corr_matrix_plots <- plot_all_correlation_matrices(all_data = all_data)
}

# ============================================================================
# Step B  Indicator Screening
# ============================================================================
# Validates the configured indicator set.  Produces a per-indicator flag table
# and prints a pillar-level PCA diagnostic (PC1 variance, loadings, weights).
# Run after any change to INDICATOR_CONFIG in config.R.

cat("\n--- Screening configured indicators ---\n")
screening_results <- screen_all_countries(all_data)

# ---- 3. Internal diagnostics -----------------------------------------------

cat("\n--- Running internal diagnostics ---\n")
diagnostics <- run_all_diagnostics(all_data)

# ---- 4. Select version and compute SEPI ------------------------------------
#
# Available versions (defined in config.R):
#   VERSIONS$v1_equal_geometric  — equal weights, arithmetic within pillars,
#                                   geometric across pillars
#   VERSIONS$v2_pca_geometric    — PCA weights within pillars (PC1 loadings),
#                                   equal weights across pillars, geometric agg

version <- VERSIONS$v1_equal_geometric
cat("\n--- Computing SEPI (", version$name, ") ---\n")
print(version)

sepi_results <- compute_all_countries(all_data, version)

# Print summary per country
for (country in names(sepi_results)) {
  res <- sepi_results[[country]]
  cat("\n", country_label(country), "— SEPI summary:\n")
  cat("  Min: ",  round(min(res$sepi,  na.rm = TRUE), 3),
      " Max: ",  round(max(res$sepi,  na.rm = TRUE), 3),
      " Mean: ", round(mean(res$sepi, na.rm = TRUE), 3), "\n")

  top <- res |>
    dplyr::arrange(sepi_rank) |>
    dplyr::select(adm1_name, sepi, sepi_rank) |>
    utils::head(5)
  cat("  Top 5:\n")
  print(as.data.frame(top), row.names = FALSE)
}

# ============================================================================
# Step 4  [OPTIONAL] Indicator Sensitivity Analysis
# ============================================================================
# Leave-one-out test: removes each indicator in turn and measures how much
# the final rankings change.  Identifies redundant or highly influential
# indicators.  Interpret results:
#   spearman_rho > 0.95  ->  redundant (rankings barely change without it)
#   spearman_rho < 0.80  ->  highly influential (verify data quality)
#
# sensitivity_results <- sensitivity_all_countries(all_data, version)

# ---- 5. Conflict analysis --------------------------------------------------

cat("\n--- SEPI–Conflict linkage ---\n")
conflict_results <- analyse_conflict_all(sepi_results)

# ---- 6. Visualisations -----------------------------------------------------

cat("\n--- Generating plots ---\n")
generate_all_plots(sepi_results, conflict_results)

# ---- 7. Export results -----------------------------------------------------

export_sepi_excel(sepi_results, version)

cat("\nDone.\n")

# ============================================================================
# VERSION COMPARISON WORKFLOW
# ============================================================================
# Compare v1 (equal weights) against v2 (PCA within-pillar weights) to see
# how much rankings change when data-driven weighting is applied.
#
# results_v2 <- compute_all_countries(all_data, VERSIONS$v2_pca_geometric)
#
# comparison <- compare_versions(list(
#   v1_equal = sepi_results,
#   v2_pca   = results_v2
# ))
#
# # Spearman rank correlation between versions per country
# for (country in names(comparison)) {
#   cat("\n", country_label(country), "— Rank correlation v1 vs v2:\n")
#   print(round(comparison[[country]]$rank_correlation, 3))
# }
#
# # Slope chart (visual rank change)
# for (country in names(comparison)) {
#   plot_version_comparison(comparison, country)
# }
