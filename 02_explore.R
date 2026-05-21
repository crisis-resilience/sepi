# ============================================================================
# 02_explore.R — Indicator exploration, screening, and diagnostics
# ============================================================================
# Run when: evaluating new indicators, auditing the configured indicator set,
#           checking internal consistency, or reviewing v2 conflict weights.
#
# Set `version` below to control which pillar/indicator definitions are used
# for screening and diagnostics.  Use v1_equal_geometric for pillar-based
# checks; use v2_conflict_weighted to audit its se_vars.
# ============================================================================

source("R/setup.R")

for (pkg in c("rvest", "caret")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

source("R/screen_indicators.R")
source("R/explore_candidates.R")
source("R/diagnostics.R")

# ── Configure ─────────────────────────────────────────────────────────────────
# When sourced from run_all.R, .sepi_run_version is set there; otherwise use the
# version defined below.
version <- if (exists(".sepi_run_version")) .sepi_run_version else VERSIONS$v1_equal_geometric  # ← change to switch version

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

# ── D. [OPTIONAL] v2 indicator selection ──────────────────────────────────────
# Run once, review output, then update se_vars in versions/v2_conflict_weighted.json.
# v2_selection <- select_v2_indicators(all_data, version)

cat("\nDone.\n")
