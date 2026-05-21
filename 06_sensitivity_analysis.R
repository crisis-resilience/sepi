# ============================================================================
# 06_sensitivity_analysis.R — Run SA1 and SA2 for V1 and v2, compare results
# ============================================================================
#
# SA1 (indicator sensitivity): For every combination where one indicator is
#   dropped from each multi-indicator pillar simultaneously, compute SEPI.
#   The mean SEPI across all combinations is the SA1 score per region.
#   Single-indicator pillars (e.g. food security) are never touched.
#
# SA2 (pillar sensitivity): For each of the five pillars, drop it entirely
#   and compute SEPI with the remaining four pillars.  The mean SEPI across
#   the five runs is the SA2 score per region.
#
# Versions compared:
#   V1 = v1_equal_geometric  (equal weights, geometric across pillars, aligned indicators)
#   V2 = v2_conflict_weighted (conflict-correlation weighted flat sum, aligned indicators)
#
# Outputs:
#   outputs/sensitivity_analysis_comparison.xlsx         — scores + ranks for all countries
#   outputs/figures/sensitivity/sensitivity_comparison_<country>.png
#                                                        — formatted gt table per country
# ============================================================================

source("R/setup.R")

for (pkg in c("openxlsx", "gt")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

source("R/sensitivity_analysis.R")

dir.create("outputs", showWarnings = FALSE)

# ── Load versions ─────────────────────────────────────────────────────────────
v1 <- VERSIONS$v1_equal_geometric
v2 <- VERSIONS$v2_conflict_weighted

if (is.null(v1)) stop("Version 'v1_equal_geometric' not found in versions/.")
if (is.null(v2)) stop("Version 'v2_conflict_weighted' not found in versions/.")

cat("\nVersions loaded:\n")
cat("  V1:", v1$name, "\n")
cat("  V2:", v2$name, "\n")

# ── Load data (once per version — region exclusions differ by version config) ─
cat("\n=== Loading data ===\n")
all_data_v1 <- load_all_data(version = v1)
all_data_v2 <- load_all_data(version = v2)

# ── Compute main (baseline) SEPI ──────────────────────────────────────────────
cat("\n=== Computing baseline SEPI ===\n")
main_v1 <- compute_all_countries(all_data_v1, v1)
main_v2 <- compute_all_countries(all_data_v2, v2)

# ── Run sensitivity analyses ──────────────────────────────────────────────────
sa_v1 <- run_sensitivity_all(all_data_v1, v1)
sa_v2 <- run_sensitivity_all(all_data_v2, v2)

# ── Build comparison tables ───────────────────────────────────────────────────
cat("\n=== Building comparison tables ===\n")
comparison <- build_comparison_table(main_v1, sa_v1, main_v2, sa_v2)

# ── Export Excel ──────────────────────────────────────────────────────────────
cat("\n=== Exporting Excel ===\n")
export_sensitivity_excel(
  comparison,
  out_path = "outputs/sensitivity_analysis_comparison.xlsx"
)

# ── Render PNG tables ─────────────────────────────────────────────────────────
cat("\n=== Rendering PNG comparison tables ===\n")
for (country in names(comparison)) {
  n_sa1_v1 <- sa_v1[[country]]$sa1_n_combos[1]
  n_sa1_v2 <- sa_v2[[country]]$sa1_n_combos[1]
  out_png   <- versioned_output_path(NULL, "figures", "sensitivity",
                                     paste0("sensitivity_comparison_", country))
  render_comparison_png(comparison[[country]], country, n_sa1_v1, n_sa1_v2, out_png)
}

cat("\nDone.\n")
cat("Outputs:\n")
cat("  outputs/sensitivity_analysis_comparison.xlsx\n")
for (country in names(comparison)) {
  cat("  outputs/figures/sensitivity/sensitivity_comparison_", country, ".png\n", sep = "")
}
