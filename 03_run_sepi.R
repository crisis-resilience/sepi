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
source("R/export_somalia_excel.R")
source("R/export_geojson.R")

# ── Configure ─────────────────────────────────────────────────────────────────
# When sourced from run_all.R, .sepi_run_version is set there; otherwise use the
# version defined below.
version <- if (exists(".sepi_run_version")) .sepi_run_version else VERSIONS$v1_equal_geometric  # ← change to switch version

# ── Run ───────────────────────────────────────────────────────────────────────
all_data         <- load_all_data(version = version)
sepi_results     <- compute_all_countries(all_data, version)
conflict_results <- analyse_conflict_all(sepi_results, version)

generate_all_plots(sepi_results, conflict_results, version)
out_file         <- export_sepi_excel_raw_subindicators(sepi_results, version)
somalia_out_file <- export_sepi_excel_somalia(sepi_results, version)
export_sepi_geojson(sepi_results, version)

if (isTRUE(version$conflict_weighting)) {
  render_polarity_audits(sepi_results, version)
}

cat("Done. Output saved to:", out_file, "and", somalia_out_file, "\n")
