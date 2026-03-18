# ============================================================================
# 01_build_data.R — Rebuild merged 3-country indicator dataset
# ============================================================================
# Run when: source CSVs updated, ACLED data refreshed, remote sensing updated.
# Outputs: data/sepi_indicators_all_countries_latest.csv
#          data/sepi_indicators_metadata_all_countries.csv
#          data/sepi_merge_qc_report.json
#
# The rest of the pipeline (run_sepi.R) reads from these pre-built CSVs.
# ============================================================================

for (pkg in c("tidyverse", "readxl", "openxlsx", "httr2", "jsonlite")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(tidyverse)

source("R/config.R")
source("R/build_global_data.R")

cat("\n--- Building merged 3-country indicators + metadata ---\n")
merged <- build_global_indicator_assets()

cat("\nOutputs:\n")
cat("  Indicators:", merged$files$indicators_file, "\n")
cat("  Metadata:  ", merged$files$metadata_file,   "\n")
cat("  QC report: ", merged$files$qc_file,          "\n")
cat("\nDone. Run run_sepi.R to compute SEPI.\n")
