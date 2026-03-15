# ============================================================================
# Excel Export
# ============================================================================
# Produces a single multi-sheet .xlsx workbook with:
#   1. README           – methodology description
#   2. SEPI_Results     – pillar scores, SEPI, and ranks (all countries)
#   3. Indicator_Scores – normalised indicator values (all countries)
#   4. Indicator_Details – pillar/indicator mapping with weights
# ============================================================================

export_sepi_excel <- function(sepi_results,
                              version,
                              config = INDICATOR_CONFIG,
                              output_dir = "outputs") {

  fname <- file.path(output_dir, paste0("sepi_results_", version$name, ".xlsx"))
  wb    <- openxlsx::createWorkbook()

  header_style <- openxlsx::createStyle(textDecoration = "bold")

  # ---- Sheet 1: README -----------------------------------------------------
  build_readme_sheet(wb, version, header_style)

  # ---- Sheet 2: SEPI_Results -----------------------------------------------
  build_results_sheet(wb, sepi_results, config, header_style)

  # ---- Sheet 3: Indicator_Scores -------------------------------------------
  build_indicator_scores_sheet(wb, sepi_results, config, header_style)

  # ---- Sheet 4: Indicator_Details ------------------------------------------
  build_indicator_details_sheet(wb, version, config, header_style)

  # ---- Write ---------------------------------------------------------------
  openxlsx::saveWorkbook(wb, fname, overwrite = TRUE)
  cat("Exported:", fname, "\n")
  invisible(fname)
}

# ---- Sheet builders --------------------------------------------------------

build_readme_sheet <- function(wb, version, header_style) {

  norm_label <- switch(version$normalisation,
    min_max = "Min-Max (0-1)",
    z_score = "Z-Score",
    rank    = "Rank-based (0-1)"
  )

  readme <- data.frame(
    Section = c(
      "Title",
      "Version",
      "Objective",
      "",
      "How to Use This File",
      "",
      "Sheet: SEPI_Results",
      "Sheet: Indicator_Scores",
      "Sheet: Indicator_Details",
      "",
      "Methodology Overview",
      "",
      "1. Indicator Selection",
      "2. Normalisation & Directionality",
      "3. Within-Pillar Aggregation",
      "4. Across-Pillar Aggregation",
      "5. Ranking",
      "",
      "Interpretation"
    ),
    Description = c(
      "Socio-Economic Peacebuilding Index (SEPI)",
      paste0("Version: ", version$name, " (Generated: ", format(Sys.Date(), "%B %Y"), ")"),
      paste0(
        "This file presents a composite indicator designed to measure relative ",
        "socio-economic conditions relevant to peacebuilding across Admin-1 regions ",
        "in Kenya, Somalia, and South Sudan, based on the latest available ",
        "cross-sectional data."
      ),
      "",
      "This workbook contains several sheets detailing the index construction and results:",
      "",
      paste0(
        "Contains the final SEPI score, pillar-level scores, and relative rank ",
        "for each Admin-1 region, for all countries."
      ),
      paste0(
        "Shows the normalised (0-1) and direction-adjusted score for each ",
        "individual indicator. This helps explain why a region scored high or low."
      ),
      paste0(
        "Documents the pillar-indicator mapping, polarity, and effective weight ",
        "assigned to each indicator, providing full transparency on the index construction."
      ),
      "",
      "The index was constructed using the following steps:",
      "",
      paste0(
        "Socio-economic indicators were selected across five pillars: ",
        "Education, Health, Food Security, Economic, and Climate."
      ),
      paste0(
        "All indicators were transformed to a common 0-1 scale using ",
        norm_label, " normalisation. Indicators where a high raw value is ",
        "negative (e.g. poverty rates) were inverted, so that a higher ",
        "normalised score always represents a more favourable condition."
      ),
      paste0(
        "Indicators within each pillar were combined using an ",
        version$within_pillar_agg, " mean",
        if (version$weighting == "equal") " (equal indicator weights)." else "."
      ),
      paste0(
        "Pillar scores were aggregated into a single SEPI score using a ",
        version$across_pillar_agg, " mean",
        if (version$weighting == "equal") {
          " with equal pillar weights."
        } else {
          paste0(" with custom pillar weights: ",
                 paste(names(version$pillar_weights), "=",
                       round(version$pillar_weights, 3), collapse = ", "),
                 ".")
        }
      ),
      "Regions are ranked within each country (1 = best socio-economic conditions).",
      "",
      paste0(
        "The final index is a relative measure. A score closer to 1 indicates ",
        "relatively better socio-economic conditions for peacebuilding compared ",
        "to other regions in this analysis. A score closer to 0 indicates ",
        "relatively more challenging conditions. It does not represent an ",
        "absolute measure of 'peace' or 'development'."
      )
    ),
    stringsAsFactors = FALSE
  )

  openxlsx::addWorksheet(wb, "README")
  openxlsx::writeData(wb, "README", readme, headerStyle = header_style)
  openxlsx::setColWidths(wb, "README", cols = 1, widths = 35)
  openxlsx::setColWidths(wb, "README", cols = 2, widths = 100)
}

build_results_sheet <- function(wb, sepi_results, config, header_style) {

  rows <- purrr::imap(sepi_results, function(res, country) {
    id_cols     <- config[[country]]$id_cols
    pillar_cols <- grep("^pillar_", names(res), value = TRUE)
    out_cols    <- c(id_cols, pillar_cols, "sepi", "n_pillars", "sepi_rank")

    res |>
      dplyr::select(dplyr::all_of(out_cols)) |>
      dplyr::arrange(sepi_rank) |>
      dplyr::mutate(country = country_label(country), .before = 1)
  })

  combined <- dplyr::bind_rows(rows)

  openxlsx::addWorksheet(wb, "SEPI_Results")
  openxlsx::writeData(wb, "SEPI_Results", combined, headerStyle = header_style)
  openxlsx::setColWidths(wb, "SEPI_Results", cols = seq_len(ncol(combined)),
                         widths = "auto")
}

build_indicator_scores_sheet <- function(wb, sepi_results, config, header_style) {

  rows <- purrr::imap(sepi_results, function(res, country) {
    id_cols   <- config[[country]]$id_cols
    norm_cols <- grep("_norm$", names(res), value = TRUE)
    out_cols  <- c(id_cols, norm_cols)

    res |>
      dplyr::select(dplyr::all_of(out_cols)) |>
      dplyr::mutate(country = country_label(country), .before = 1)
  })

  combined <- dplyr::bind_rows(rows)

  openxlsx::addWorksheet(wb, "Indicator_Scores")
  openxlsx::writeData(wb, "Indicator_Scores", combined, headerStyle = header_style)
  openxlsx::setColWidths(wb, "Indicator_Scores",
                         cols = seq_len(ncol(combined)), widths = "auto")
}

build_indicator_details_sheet <- function(wb, version, config, header_style) {

  detail_list <- list()

  for (country in names(config)) {
    cc           <- config[[country]]
    pillar_names <- names(cc$pillars)
    n_pillars    <- length(pillar_names)

    if (version$weighting == "equal") {
      pw <- stats::setNames(rep(1 / n_pillars, n_pillars), pillar_names)
    } else {
      pw <- version$pillar_weights[pillar_names]
    }

    for (p_name in pillar_names) {
      p_def <- cc$pillars[[p_name]]
      n_ind <- length(p_def$indicators)

      detail_list[[length(detail_list) + 1]] <- tibble::tibble(
        country   = country_label(country),
        pillar    = p_name,
        indicator = p_def$indicators,
        polarity  = p_def$polarity,
        label     = p_def$labels,
        weight    = pw[[p_name]] / n_ind
      )
    }
  }

  rows <- dplyr::bind_rows(detail_list)

  openxlsx::addWorksheet(wb, "Indicator_Details")
  openxlsx::writeData(wb, "Indicator_Details", rows, headerStyle = header_style)
  openxlsx::setColWidths(wb, "Indicator_Details",
                         cols = seq_len(ncol(rows)), widths = "auto")
}
