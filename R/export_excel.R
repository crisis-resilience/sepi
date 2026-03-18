# ============================================================================
# Excel Export
# ============================================================================
# Produces a single multi-sheet .xlsx workbook with:
#   1. README           - methodology description
#   2. SEPI_Results     - pillar scores, SEPI, and ranks (all countries)
#   3. Indicator_Scores - normalised indicator values (all countries)
#   4. Indicator_Details - pillar/indicator mapping with weights
# ============================================================================

export_sepi_excel <- function(sepi_results,
                              version,
                              output_dir = "outputs") {

  fname <- file.path(output_dir, paste0("sepi_results_", version$name, ".xlsx"))
  wb    <- openxlsx::createWorkbook()

  header_style <- openxlsx::createStyle(textDecoration = "bold")

  # ---- Sheet 1: README -----------------------------------------------------
  build_readme_sheet(wb, version, header_style)

  # ---- Sheet 2: SEPI_Results -----------------------------------------------
  build_results_sheet(wb, sepi_results, version$countries, version, header_style)

  # ---- Sheet 3: Indicator_Scores -------------------------------------------
  build_indicator_scores_sheet(wb, sepi_results, version$countries, version, header_style)

  # ---- Sheet 4: Indicator_Details ------------------------------------------
  build_indicator_details_sheet(wb, sepi_results, version, version$countries, header_style)

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

  if (isTRUE(version$conflict_weighting)) {
    # V3-specific README content
    readme <- data.frame(
      Section = c(
        "Title",
        "Version",
        "Objective",
        "",
        "How to Use This File",
        "",
        "  Sheet: SEPI_Results",
        "  Sheet: Indicator_Scores",
        "  Sheet: Indicator_Details",
        "",
        "Methodology Overview",
        "",
        "  1. Indicator Selection",
        "  2. Normalisation & Directionality",
        "  3. Weighting",
        "  4. Direction Handling",
        "  5. Aggregation",
        "  6. Ranking",
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
          "Shows the normalised (0-1) score for each individual granular sub-indicator. ",
          "This helps explain why a region scored high or low."
        ),
        paste0(
          "Documents the indicator, polarity, pillar mapping, and effective weight ",
          "assigned to each indicator, providing full transparency on the index construction."
        ),
        "",
        "The index was constructed using the following steps:",
        "",
        paste0(
          "Socio-economic indicators were selected per country based on data availability, ",
          "multicollinearity filtering (|r| > 0.8 cutoff), and relevance to conflict dynamics."
        ),
        paste0(
          "All indicators were transformed to a common 0-1 scale using ",
          norm_label, " normalisation. Unlike v1/v2, the original direction of each ",
          "indicator is preserved (no polarity inversion at this stage)."
        ),
        paste0(
          "Weight magnitudes are derived from the absolute Pearson correlation between ",
          "each normalised indicator and the conflict indicator (conflict events per 1k pop). ",
          "Magnitudes are normalised to sum to 1."
        ),
        paste0(
          "Each indicator's contribution sign is determined by its nature: indicators where ",
          "a higher value is 'bad' for peacebuilding (e.g. poverty rate) receive a -1 sign, ",
          "while 'good' indicators (e.g. school attendance) receive +1. The effective weight ",
          "is sign x magnitude."
        ),
        paste0(
          "The final SEPI score is a flat weighted sum: sum(normalised_i x effective_weight_i), ",
          "rescaled to [0, 1] via min-max normalisation. There is no pillar-level aggregation; ",
          "pillar scores shown are the normalised value of one representative indicator per pillar."
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
  } else {
    # V1/V2 README content (unchanged)
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
  }

  openxlsx::addWorksheet(wb, "README")
  openxlsx::writeData(wb, "README", readme, headerStyle = header_style)
  openxlsx::setColWidths(wb, "README", cols = 1, widths = 35)
  openxlsx::setColWidths(wb, "README", cols = 2, widths = 120)

  # Wrap text in Description column
  if (isTRUE(version$conflict_weighting)) {
    wrap_style <- openxlsx::createStyle(wrapText = TRUE)
    openxlsx::addStyle(wb, "README", style = wrap_style,
                       rows = 2:(nrow(readme) + 1), cols = 2, gridExpand = TRUE)
  }
}

build_results_sheet <- function(wb, sepi_results, config, version, header_style) {

  rows <- purrr::imap(sepi_results, function(res, country) {
    cc <- config[[country]]
    id_cols     <- cc$id_cols
    pillar_cols <- grep("^pillar_", names(res), value = TRUE)
    out_cols    <- c(id_cols, pillar_cols, "sepi", "n_pillars", "sepi_rank")
    out_cols    <- out_cols[out_cols %in% names(res)]

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

build_indicator_scores_sheet <- function(wb, sepi_results, config, version, header_style) {

  rows <- purrr::imap(sepi_results, function(res, country) {
    cc <- config[[country]]
    id_cols <- cc$id_cols

    if (isTRUE(version$conflict_weighting) && !is.null(cc$granular_vars)) {
      # V3: use granular_vars for full sub-indicator export
      norm_cols <- paste0(cc$granular_vars, "_norm")
      norm_cols <- norm_cols[norm_cols %in% names(res)]
    } else {
      # V1/V2: all _norm columns present in data
      norm_cols <- grep("_norm$", names(res), value = TRUE)
    }

    out_cols <- c(id_cols, norm_cols)
    out_cols <- out_cols[out_cols %in% names(res)]

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

build_indicator_details_sheet <- function(wb, sepi_results, version, config, header_style) {

  detail_list <- list()

  if (isTRUE(version$conflict_weighting)) {
    # V3: use se_vars, bad_vars, pillar_map, and effective weights from results
    for (country in names(config)) {
      cc        <- config[[country]]
      se_vars   <- cc$se_vars
      bad_vars  <- cc$bad_vars
      pillar_map <- cc$pillar_map

      # Build reverse pillar lookup: indicator -> pillar name
      ind_to_pillar <- character(0)
      if (!is.null(pillar_map)) {
        for (p_name in names(pillar_map)) {
          ind_to_pillar[pillar_map[[p_name]]] <- p_name
        }
      }

      # Try to get effective weights from computed results
      eff_wts <- NULL
      if (!is.null(sepi_results[[country]])) {
        eff_wts <- attr(sepi_results[[country]], "v3_effective_weights")
      }

      for (v in se_vars) {
        polarity <- ifelse(v %in% bad_vars, -1, 1)
        pillar   <- if (v %in% names(ind_to_pillar)) ind_to_pillar[v] else NA_character_
        wt       <- if (!is.null(eff_wts) && v %in% names(eff_wts)) eff_wts[v] else NA_real_

        detail_list[[length(detail_list) + 1]] <- tibble::tibble(
          country   = country_label(country),
          pillar    = pillar,
          indicator = v,
          polarity  = polarity,
          label     = v,
          weight    = wt
        )
      }
    }
  } else {
    # V1/V2: use pillars config
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
  }

  rows <- dplyr::bind_rows(detail_list)

  openxlsx::addWorksheet(wb, "Indicator_Details")
  openxlsx::writeData(wb, "Indicator_Details", rows, headerStyle = header_style)
  openxlsx::setColWidths(wb, "Indicator_Details",
                         cols = seq_len(ncol(rows)), widths = "auto")
}
