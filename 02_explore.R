# ============================================================================
# 02_explore.R — Indicator exploration, screening, and diagnostics
# ============================================================================
# Run when: evaluating new indicators, auditing the configured indicator set,
#           checking internal consistency, or reviewing v3 conflict weights.
#
# Set `version` below to control which pillar/indicator definitions are used
# for screening and diagnostics.  Use a v1/v2 version for pillar-based checks;
# use v3_conflict_weighted to audit its se_vars.
# ============================================================================

for (pkg in c("tidyverse", "psych", "rvest", "caret", "jsonlite")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(tidyverse)

source("R/config.R")
source("R/utils.R")
source("R/load_data.R")
source("R/normalise.R")
source("R/screen_indicators.R")
source("R/explore_candidates.R")
source("R/diagnostics.R")
source("R/compute_index.R")

# ── Configure ─────────────────────────────────────────────────────────────────
version <- VERSIONS$v3_conflict_weighted   # ← change to switch version

# ── Load data ─────────────────────────────────────────────────────────────────
all_data <- load_all_data(version = version)

# ── A. Candidate exploration ──────────────────────────────────────────────────
# Surveys all available variables and cross-references data dictionaries.
# Produces outputs/candidate_report_{country}.csv.
candidate_reports <- explore_all_candidates(all_data, sepi_results = NULL)

cat("\n--- Generating candidate correlation matrices ---\n")
corr_matrix_plots <- plot_all_correlation_matrices(all_data = all_data)

# ── B. Indicator screening ────────────────────────────────────────────────────
# Validates the configured indicator set against OECD Handbook quality criteria.
cat("\n--- Screening configured indicators ---\n")
screening_results <- screen_all_countries(all_data, version)

# ── C. Internal diagnostics ───────────────────────────────────────────────────
# Missingness, within-pillar Spearman correlations, Cronbach's alpha.
# Note: pillar-based diagnostics require a version with 'pillars' defined (v1/v2).
cat("\n--- Running internal diagnostics ---\n")
diagnostics <- run_all_diagnostics(all_data, version)

# ── D. [OPTIONAL] V3 indicator selection ──────────────────────────────────────
# Run once, review output, then update se_vars in versions/v3_conflict_weighted.json.
# v3_selection <- select_v3_indicators(all_data, version)

cat("\nDone.\n")
