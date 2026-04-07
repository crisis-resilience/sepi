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
version      <- VERSIONS$v3_zscore   # ← change to switch version under evaluation
all_data     <- load_all_data(version = version)
sepi_results <- compute_all_countries(all_data, version)

# ── A. Sensitivity analysis ───────────────────────────────────────────────────
# Leave-one-out: how much do SEPI ranks change when each indicator is removed?
sensitivity_results <- sensitivity_all_countries(all_data, version)

# ── B. Version comparison ─────────────────────────────────────────────────────
# Compare SEPI ranks between v3_conflict_weighted (min-max) and v3_zscore.
# sepi_results (v3_zscore) is already computed above — reuse it.
baseline_v3 <- compute_all_countries(all_data, VERSIONS$v3_conflict_weighted)

comparison <- compare_versions(list(
  v3_minmax = baseline_v3,
  v3_zscore = sepi_results
))

for (country in names(comparison)) {
  cat("\n", country_label(country), "— Rank correlations:\n")
  print(round(comparison[[country]]$rank_correlation, 3))
}

cat("\nDone.\n")
