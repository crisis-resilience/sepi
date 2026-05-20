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

  # ---- Sheet 5: Conflict_Data -----------------------------------------------
  build_conflict_data_sheet(wb, header_style)

  # ---- Sheet 6: Pillar_Descriptions -----------------------------------------
  build_pillar_descriptions_sheet(wb, header_style)

  # ---- Write ---------------------------------------------------------------
  # openxlsx creates a temp subdirectory; recreate the base temp dir in case
  # Windows has cleaned it up during the session (causes "No such file or directory")
  dir.create(tempdir(), recursive = TRUE, showWarnings = FALSE)
  openxlsx::saveWorkbook(wb, fname, overwrite = TRUE)
  cat("Exported:", fname, "\n")
  invisible(fname)
}

# ---- Sheet builders --------------------------------------------------------

build_readme_sheet <- function(wb, version, header_style, raw_subindicators = FALSE) {

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
        if (raw_subindicators) {
          paste0(
            "Shows the original (non-normalised) raw value for each sub-indicator, ",
            "taken directly from the source data. This helps explain why a region ",
            "scored high or low in its original units."
          )
        } else {
          paste0(
            "Shows the normalised (0-1) and direction-adjusted score for each ",
            "individual indicator. This helps explain why a region scored high or low."
          )
        },
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
          } else if (version$weighting == "conflict") {
            " with conflict-correlation derived weights."
          } else if (version$weighting == "bod") {
            " with Benefit of the Doubt (BoD/DEA) endogenous weights per district."
          } else if (!is.null(version$pillar_weights)) {
            paste0(" with custom pillar weights: ",
                   paste(names(version$pillar_weights), "=",
                         round(version$pillar_weights, 3), collapse = ", "),
                   ".")
          } else {
            "."
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

    sepi_vars <- get_sepi_vars(cc)
    if (length(sepi_vars) > 0) {
      norm_cols <- paste0(sepi_vars, "_norm")
      norm_cols <- norm_cols[norm_cols %in% names(res)]
    } else {
      norm_cols <- grep("_norm$", names(res), value = TRUE)
    }

    out_cols <- c(id_cols, norm_cols)
    out_cols <- out_cols[out_cols %in% names(res)]

    out <- res |>
      dplyr::select(dplyr::all_of(out_cols)) |>
      dplyr::mutate(country = country_label(country), .before = 1)

    # The _norm columns stored during computation are polarity-flipped (higher = better
    # for the index). Re-invert negative-polarity indicators here so the displayed value
    # matches the raw indicator direction (e.g. Unity shows ~1.0 for pop_frac_3plus,
    # not ~0.0). This does not affect any SEPI calculations.
    if (!is.null(cc$pillars)) {
      for (p in cc$pillars) {
        for (i in seq_along(p$indicators)) {
          if (isTRUE(p$polarity[i] == -1)) {
            nc <- paste0(p$indicators[i], "_norm")
            if (nc %in% names(out)) out[[nc]] <- 1 - out[[nc]]
          }
        }
      }
    }

    out
  })

  combined <- dplyr::bind_rows(rows)

  openxlsx::addWorksheet(wb, "Indicator_Scores")
  openxlsx::writeData(wb, "Indicator_Scores", combined, headerStyle = header_style)
  openxlsx::setColWidths(wb, "Indicator_Scores",
                         cols = seq_len(ncol(combined)), widths = "auto")
}

# ---- Indicator Details helpers ----------------------------------------------

# Returns a named character vector: indicator -> pillar name.
# Works for any country config that has either 'pillars' or 'pillar_map'.
get_ind_to_pillar <- function(cc) {
  result <- character(0)
  if (!is.null(cc$pillars)) {
    for (p in names(cc$pillars))
      for (ind in cc$pillars[[p]]$indicators) result[ind] <- p
  } else if (!is.null(cc$pillar_map)) {
    for (p in names(cc$pillar_map)) result[cc$pillar_map[[p]]] <- p
  }
  result
}

# Returns the character vector of indicators that enter the SEPI score.
# Works for pillars-based (v1/v2), se_vars-based (v3 conflict), and
# pillar_map-based (BoD) configs.
get_sepi_vars <- function(cc) {
  if (!is.null(cc$pillars)) {
    unlist(lapply(cc$pillars, `[[`, "indicators"), use.names = FALSE)
  } else if (!is.null(cc$pillar_map)) {
    unname(unlist(cc$pillar_map))
  } else {
    cc$se_vars %||% character(0)
  }
}

# Returns a named character vector: indicator -> weight label/value (as string).
# Adding a new version only requires adding a branch here.
get_indicator_weights <- function(cc, version, sepi_result, sepi_vars) {
  if (isTRUE(version$conflict_weighting)) {
    eff_wts <- attr(sepi_result, "v3_effective_weights")
    if (!is.null(eff_wts)) {
      as.character(round(eff_wts[sepi_vars[sepi_vars %in% names(eff_wts)]], 4))
    } else {
      stats::setNames(rep(NA_character_, length(sepi_vars)), sepi_vars)
    }

  } else if (isTRUE(version$bod_weighting)) {
    n      <- length(cc$pillar_map)
    eq_w   <- 1 / n
    flex   <- version$bod_weight_flex %||% 0.5
    label  <- sprintf("BoD endogenous [%.3f, %.3f]", eq_w * (1 - flex), eq_w * (1 + flex))
    stats::setNames(rep(label, length(sepi_vars)), sepi_vars)

  } else if (!is.null(cc$pillars)) {
    pillar_names <- names(cc$pillars)
    n_pillars    <- length(pillar_names)
    pw <- if (version$weighting == "equal") {
      stats::setNames(rep(1 / n_pillars, n_pillars), pillar_names)
    } else if (!is.null(version$pillar_weights)) {
      version$pillar_weights[pillar_names]
    } else {
      stats::setNames(rep(NA_real_, n_pillars), pillar_names)
    }
    result <- character(0)
    for (p in pillar_names) {
      inds <- cc$pillars[[p]]$indicators
      for (ind in inds) result[ind] <- as.character(round(pw[[p]] / length(inds), 4))
    }
    result

  } else {
    stats::setNames(rep(NA_character_, length(sepi_vars)), sepi_vars)
  }
}

build_indicator_details_sheet <- function(wb, sepi_results, version, config, header_style) {

  # Extract 4-digit year numbers from a reference period string.
  # Returns a single year ("2025") or a range ("2016-2025").
  extract_years <- function(ref_period) {
    if (is.na(ref_period) || nchar(trimws(ref_period)) == 0) return(NA_character_)
    years <- unique(regmatches(ref_period, gregexpr("20\\d{2}", ref_period))[[1]])
    if (length(years) == 0) return(NA_character_)
    years_int <- sort(as.integer(years))
    if (length(years_int) == 1) as.character(years_int) else paste(range(years_int), collapse = "-")
  }

  # Build polarity lookup from metadata — ground truth for directionality.
  # bad_vars serves a computational role (weight sign / data flip) and may be
  # trimmed in some versions, so it is not reliable for display polarity.
  polarity_lookup <- list()
  label_lookup    <- list()
  unit_lookup     <- list()
  source_lookup   <- list()
  year_lookup     <- list()
  meta_path <- GLOBAL_DATA$metadata_file
  if (file.exists(meta_path)) {
    meta <- utils::read.csv(meta_path, stringsAsFactors = FALSE, check.names = FALSE)
    for (i in seq_len(nrow(meta))) {
      raw_name <- meta[["global_variable_name"]][i]
      pol <- if (grepl("more deprived", meta[["Directionality"]][i], ignore.case = TRUE)) -1L else 1L
      ind_label  <- meta[["Indicator name"]][i]
      unit_val   <- meta[["Unit of measurement"]][i]
      source_val <- meta[["Data source"]][i]
      ref_val    <- meta[["Reference period"]][i]
      # Index under the literal name and a sanitised variant (e.g. "pop_frac_3+" -> "pop_frac_3plus")
      for (nm in unique(c(raw_name, gsub("\\+", "plus", raw_name)))) {
        key <- paste0(tolower(meta[["country"]][i]), ".", nm)
        polarity_lookup[[key]] <- pol
        label_lookup[[key]]    <- ind_label
        unit_lookup[[key]]     <- unit_val
        source_lookup[[key]]   <- source_val
        year_lookup[[key]]     <- ref_val
      }
    }
  }

  climate_label_overrides <- c(
    rs_pdsi        = "Palmer Drought Severity Index",
    rs_ndvi        = "Normalized Difference Vegetation Index",
    rs_fapar       = "Fraction of Absorbed Photosynthetically Active Radiation",
    rs_soil_moist  = "Soil Moisture Anomaly",
    pop_frac_3plus = "Fraction of population in IPC (Integrated Food Security Phase Classification) Phase 3 or higher",
    annual_cmb_mean = "Average annual CMB (Cost of Minimum Expenditure Basket) cost"
  )

  detail_list <- list()

  for (country in names(config)) {
    cc            <- config[[country]]
    bad_vars      <- cc$bad_vars %||% character(0)
    ind_to_pillar <- get_ind_to_pillar(cc)
    sepi_vars     <- get_sepi_vars(cc)
    all_vars      <- cc$granular_vars %||% sepi_vars
    weights       <- get_indicator_weights(cc, version, sepi_results[[country]], sepi_vars)

    for (v in all_vars) {
      meta_key <- paste0(tolower(country), ".", v)
      pol <- if (!is.null(polarity_lookup[[meta_key]])) {
        polarity_lookup[[meta_key]]
      } else {
        ifelse(v %in% bad_vars, -1L, 1L)   # fallback: bad_vars for vars absent from metadata
      }
      detail_list[[length(detail_list) + 1]] <- tibble::tibble(
        country             = country_label(country),
        pillar              = if (v %in% names(ind_to_pillar)) ind_to_pillar[[v]] else NA_character_,
        indicator           = v,
        polarity            = pol,
        label               = if (!is.na(climate_label_overrides[v])) climate_label_overrides[v]
                              else if (!is.null(label_lookup[[meta_key]])) label_lookup[[meta_key]]
                              else v,
        unit_of_measurement = if (!is.null(unit_lookup[[meta_key]])) unit_lookup[[meta_key]] else NA_character_,
        data_source         = if (!is.null(source_lookup[[meta_key]])) source_lookup[[meta_key]] else NA_character_,
        data_year           = extract_years(year_lookup[[meta_key]] %||% NA_character_),
        used_in_sepi        = v %in% sepi_vars,
        weight              = if (v %in% names(weights)) weights[[v]] else NA_character_
      )
    }
  }

  rows <- dplyr::bind_rows(detail_list)

  # Append conflict variable summary rows (one set per country, year range 2016-2025)
  conflict_var_defs <- list(
    list(indicator = "total_fatalities",
         label     = "Total Fatalities",
         unit      = "Number of fatalities"),
    list(indicator = "total_fatalities_per_100k",
         label     = "Total Fatalities per 100,000 Population",
         unit      = "Fatality count per 100,000 population"),
    list(indicator = "count_conflict_events",
         label     = "Count of Conflict Events",
         unit      = "Event counts"),
    list(indicator = "count_conflict_events_per_100k",
         label     = "Count of Conflict Events per 100,000 Population",
         unit      = "Event count per 100,000 population")
  )
  conflict_rows <- dplyr::bind_rows(lapply(unique(rows$country), function(ctry) {
    dplyr::bind_rows(lapply(conflict_var_defs, function(cv) {
      tibble::tibble(
        country             = ctry,
        pillar              = "Conflict",
        indicator           = cv$indicator,
        polarity            = -1L,
        label               = cv$label,
        unit_of_measurement = cv$unit,
        data_source         = "ACLED API",
        data_year           = "2016-2025",
        used_in_sepi        = FALSE,
        weight              = NA_character_
      )
    }))
  }))
  rows <- dplyr::bind_rows(rows, conflict_rows)

  openxlsx::addWorksheet(wb, "Indicator_Details")
  openxlsx::writeData(wb, "Indicator_Details", rows, headerStyle = header_style)
  openxlsx::setColWidths(wb, "Indicator_Details",
                         cols = seq_len(ncol(rows)), widths = "auto")
}

build_raw_subindicator_scores_sheet <- function(wb, sepi_results, config, version, header_style) {

  rows <- purrr::imap(sepi_results, function(res, country) {
    cc <- config[[country]]
    id_cols <- cc$id_cols

    sepi_vars <- get_sepi_vars(cc)
    raw_cols  <- sepi_vars[sepi_vars %in% names(res)]

    out <- res |>
      dplyr::select(dplyr::all_of(c(id_cols, raw_cols))) |>
      dplyr::mutate(country = country_label(country), .before = 1)

    # pop_frac_3plus: NA means not monitored by IPC (no acute crisis) → display as 0
    if ("pop_frac_3plus" %in% names(out)) {
      out[["pop_frac_3plus"]][is.na(out[["pop_frac_3plus"]])] <- 0
    }

    out
  })

  combined <- dplyr::bind_rows(rows)

  openxlsx::addWorksheet(wb, "Indicator_Scores")
  openxlsx::writeData(wb, "Indicator_Scores", combined, headerStyle = header_style)
  openxlsx::setColWidths(wb, "Indicator_Scores",
                         cols = seq_len(ncol(combined)), widths = "auto")
}

export_sepi_excel_raw_subindicators <- function(sepi_results,
                                                version,
                                                output_dir = "outputs") {

  fname <- file.path(output_dir, "sepi_results_v1_aligned_equal_weighted_raw_subindicators.xlsx")
  wb    <- openxlsx::createWorkbook()

  header_style <- openxlsx::createStyle(textDecoration = "bold")

  build_readme_sheet(wb, version, header_style, raw_subindicators = TRUE)
  build_results_sheet(wb, sepi_results, version$countries, version, header_style)
  build_raw_subindicator_scores_sheet(wb, sepi_results, version$countries, version, header_style)
  build_indicator_details_sheet(wb, sepi_results, version, version$countries, header_style)
  build_conflict_data_sheet(wb, header_style)
  build_pillar_descriptions_sheet(wb, header_style)

  dir.create(tempdir(), recursive = TRUE, showWarnings = FALSE)
  openxlsx::saveWorkbook(wb, fname, overwrite = TRUE)
  cat("Exported:", fname, "\n")
  invisible(fname)
}

build_pillar_descriptions_sheet <- function(wb, header_style) {
  pillar_desc <- data.frame(
    Pillar = c(
      "Food Security",
      "Education",
      "Health",
      "Income & Livelihoods",
      "Climate"
    ),
    Description = c(
      "Population-level food and nutrition adequacy",
      "Access to and participation in education",
      "Healthcare services availability based on facilities per population and density",
      "Economic welfare per capita",
      "Climate resilience based on temperature, vegetation change, and elevation factors"
    ),
    `Dashboard Pillar Name` = c(
      "Food Security Index",
      "Education Index",
      "Health Access Index",
      "Poverty Reduction Index",
      "Climate Resilience Index"
    ),
    pillar_overview = c(
      paste(
        "This pillar measures the severity of food insecurity across Admin-1 regions using the share of the population classified in IPC Phase 3 (Crisis) or higher.",
        "Populations at this threshold and above are those experiencing significant food consumption gaps reflected in acute malnutrition, or who are only marginally able to meet minimum food needs by depleting essential livelihood assets or resorting to crisis-coping strategies.",
        "The Integrated Food Security Phase Classification (IPC), as the primary international mechanism for food security analysis underpinned by multistakeholder technical consensus, provides the evidence base for this classification.",
        "A higher pillar score indicates greater socio-economic resilience in this dimension."
      ),
      paste(
        "This pillar assesses the degree to which populations have access to and participate in formal education.",
        "It draws on indicators such as school attendance rates and literacy levels across the school-age population.",
        "Education is treated as a foundational enabler of economic opportunity, social cohesion, and long-term resilience to conflict.",
        "A higher pillar score indicates greater socio-economic resilience in this dimension."
      ),
      paste(
        "This pillar evaluates the availability of healthcare services relative to population size and geographic density.",
        "It is primarily measured through the number of functional health facilities per population at the Admin-1 level.",
        "Better health access is associated with greater community resilience and reduced vulnerability to shocks that can drive or sustain conflict.",
        "A higher pillar score indicates greater socio-economic resilience in this dimension."
      ),
      paste(
        "This pillar captures the economic welfare of the population, proxied through per capita income or consumption expenditure measures.",
        "Regions with lower economic welfare face heightened livelihood stress, limiting households' ability to cope with shocks and increasing susceptibility to conflict.",
        "It serves as a broad indicator of material deprivation and economic marginalisation.",
        "A higher pillar score indicates greater socio-economic resilience in this dimension."
      ),
      paste(
        "This pillar measures environmental conditions that affect livelihoods, food production, and population stability.",
        "It incorporates indicators such as drought severity (PDSI), vegetation health (NDVI), absorbed solar radiation (FAPAR), and soil moisture anomalies — all derived from remote sensing data.",
        "In line with the humanitarian-development-peace nexus, climate-induced stressors are recognised as compounding drivers of fragility, forced displacement, and conflict vulnerability across the Horn of Africa.",
        "A higher pillar score indicates greater socio-economic resilience in this dimension."
      )
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  openxlsx::addWorksheet(wb, "Pillar_Descriptions")
  openxlsx::writeData(wb, "Pillar_Descriptions", pillar_desc, headerStyle = header_style)
  openxlsx::setColWidths(wb, "Pillar_Descriptions", cols = 1, widths = 25)
  openxlsx::setColWidths(wb, "Pillar_Descriptions", cols = 2, widths = 80)
  openxlsx::setColWidths(wb, "Pillar_Descriptions", cols = 4, widths = 120)
  wrap_style <- openxlsx::createStyle(wrapText = TRUE)
  openxlsx::addStyle(wb, "Pillar_Descriptions", style = wrap_style,
                     rows = 2:6, cols = 4, gridExpand = TRUE)
}

build_conflict_data_sheet <- function(wb, header_style) {
  conflict_path <- "data/socio-economic/conflict.csv"
  if (!file.exists(conflict_path)) {
    warning("conflict.csv not found at '", conflict_path, "' — Conflict_Data sheet skipped.")
    return(invisible(NULL))
  }

  conflict_data <- utils::read.csv(conflict_path, stringsAsFactors = FALSE, check.names = FALSE)

  # Convert per-1k rate columns to per-100k (multiply by 100, round to 2 dp, rename)
  per1k_cols <- grep("_per_1k_", names(conflict_data), value = TRUE)
  for (col in per1k_cols) {
    conflict_data[[col]] <- round(conflict_data[[col]] * 100, 2)
  }
  names(conflict_data) <- gsub("_per_1k_", "_per_100k_", names(conflict_data))

  openxlsx::addWorksheet(wb, "Conflict_Data")
  openxlsx::writeData(wb, "Conflict_Data", conflict_data, headerStyle = header_style)
  openxlsx::setColWidths(wb, "Conflict_Data",
                         cols = seq_len(ncol(conflict_data)), widths = "auto")
}
