# ============================================================================
# run_sepi.R — Compute SEPI, visualise, export
# ============================================================================
# To rebuild source data:        source("01_build_data.R")
# To explore / screen indicators: source("02_explore.R")
# ============================================================================

for (pkg in c("tidyverse", "psych", "ggrepel", "openxlsx", "purrr", "rlang",
              "jsonlite", "sf", "patchwork")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(tidyverse)

source("R/config.R")
source("R/utils.R")
source("R/load_data.R")
source("R/normalise.R")
source("R/compute_index.R")
source("R/conflict_analysis.R")
source("R/visualise.R")
source("R/export_excel.R")

# ── Configure ─────────────────────────────────────────────────────────────────
version <- VERSIONS$v3_conflict_weighted   # ← change to switch version

# ── Run ───────────────────────────────────────────────────────────────────────
all_data         <- load_all_data(version = version)
sepi_results     <- compute_all_countries(all_data, version)
conflict_results <- analyse_conflict_all(sepi_results, version)

generate_all_plots(sepi_results, conflict_results, version)
export_sepi_excel(sepi_results, version)

cat("\nDone. Output: outputs/sepi_results_", version$name, ".xlsx\n", sep = "")

# ── [OPTIONAL] Sensitivity analysis ──────────────────────────────────────────
# sensitivity_results <- sensitivity_all_countries(all_data, version)

# ── [OPTIONAL] Version comparison ─────────────────────────────────────────────
# results_v1 <- compute_all_countries(all_data, VERSIONS$v1_equal_geometric)
# comparison <- compare_versions(list(v1_equal = results_v1,
#                                     v3_conflict_wt = sepi_results))
# for (country in names(comparison)) {
#   cat("\n", country_label(country), "— Rank correlation v1 vs v3:\n")
#   print(round(comparison[[country]]$rank_correlation, 3))
# }
