# ============================================================================
# Somalia-Only Excel Export
# ============================================================================
# Produces a Somalia-only .xlsx workbook mirroring the structure of the full
# multi-country workbook (export_sepi_excel_raw_subindicators), restricted to
# Somalia's Admin-1 regions:
#   1. README           - methodology description, scoped to Somalia
#   2. SEPI_Results     - pillar scores, SEPI, and ranks (Somalia only)
#   3. Indicator_Scores - raw (non-normalised) sub-indicator values (Somalia only)
#   4. Indicator_Details - pillar/indicator mapping with weights (Somalia only)
#   5. Conflict_Data    - ACLED conflict data (Somalia only)
#   6. Pillar_Descriptions
#
# Reuses the sheet builders from R/export_excel.R by subsetting sepi_results /
# version$countries down to "somalia" — each builder already loops over
# whatever countries are present, so no per-sheet Somalia-specific logic is
# needed beyond the README text and the Conflict_Data country filter.
#
# Dependencies (already sourced by 03_run_sepi.R before this file):
#   - R/export_excel.R → build_readme_sheet(), build_results_sheet(),
#                         build_raw_subindicator_scores_sheet(),
#                         build_indicator_details_sheet(),
#                         build_conflict_data_sheet(),
#                         build_pillar_descriptions_sheet()
# ============================================================================

export_sepi_excel_somalia <- function(sepi_results,
                                      version,
                                      output_dir = "outputs") {

  if (!"somalia" %in% names(sepi_results)) {
    stop("No Somalia results found in sepi_results — cannot build Somalia workbook.")
  }

  som_results <- sepi_results["somalia"]
  som_config  <- version$countries["somalia"]

  fname <- file.path(output_dir, paste0("somalia_", version$name, ".xlsx"))
  wb    <- openxlsx::createWorkbook()

  header_style <- openxlsx::createStyle(textDecoration = "bold")

  # ---- Sheet 1: README -----------------------------------------------------
  build_readme_sheet(wb, version, header_style, raw_subindicators = TRUE, country = "somalia")

  # ---- Sheet 2: SEPI_Results -----------------------------------------------
  build_results_sheet(wb, som_results, som_config, version, header_style)

  # ---- Sheet 3: Indicator_Scores (raw sub-indicator values) -----------------
  build_raw_subindicator_scores_sheet(wb, som_results, som_config, version, header_style)

  # ---- Sheet 4: Indicator_Details -------------------------------------------
  build_indicator_details_sheet(wb, som_results, version, som_config, header_style)

  # ---- Sheet 5: Conflict_Data (Somalia only) --------------------------------
  build_conflict_data_sheet(wb, header_style, country_code = "SOM")

  # ---- Sheet 6: Pillar_Descriptions -----------------------------------------
  build_pillar_descriptions_sheet(wb, header_style)

  # ---- Write ---------------------------------------------------------------
  dir.create(tempdir(), recursive = TRUE, showWarnings = FALSE)
  openxlsx::saveWorkbook(wb, fname, overwrite = TRUE)
  cat("Exported:", fname, "\n")
  invisible(fname)
}
