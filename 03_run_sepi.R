# ============================================================================
# run_sepi.R — Compute SEPI, visualise, export
# ============================================================================
# To rebuild source data:        source("01_build_data.R")
# To explore / screen indicators: source("02_explore.R")
# ============================================================================

source("R/setup.R")

for (pkg in c("ggrepel", "openxlsx", "sf", "patchwork")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

source("R/conflict_analysis.R")
source("R/visualise.R")
source("R/export_excel.R")

# ── Configure ─────────────────────────────────────────────────────────────────
# When sourced from run_all.R, .sepi_run_version is set there; otherwise use the
# version defined below.
version <- if (exists(".sepi_run_version")) .sepi_run_version else VERSIONS$v1_aligned_equal_geometric  # ← change to switch version

# ── Run ───────────────────────────────────────────────────────────────────────
all_data         <- load_all_data(version = version)
sepi_results     <- compute_all_countries(all_data, version)
conflict_results <- analyse_conflict_all(sepi_results, version)

generate_all_plots(sepi_results, conflict_results, version)
export_sepi_excel(sepi_results, version)

if (isTRUE(version$conflict_weighting)) {
  render_polarity_audits(sepi_results, version)
}

cat("\nDone. Output: outputs/sepi_results_", version$name, ".xlsx\n", sep = "")
