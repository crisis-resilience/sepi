# ============================================================================
# sensitivity_analysis.R — SA1 (drop-one-indicator) and SA2 (drop-one-pillar)
# ============================================================================
#
# SA1: For every combination where exactly one indicator is dropped from each
#      multi-indicator pillar simultaneously, compute SEPI.  The mean SEPI
#      across all combinations is the SA1 score.  Single-indicator pillars
#      (incl. food security) are left intact in every combination.
#
# SA2: For each of the five pillars, drop it entirely and compute SEPI with
#      the remaining four pillars.  The mean SEPI across the five runs is the
#      SA2 score.
#
# For V3 (conflict-weighted flat model), pillar membership is defined by the
# pillar_groups field added to each country block in the JSON config.
# ============================================================================


# ── SA1: generate config variants ────────────────────────────────────────────

#' All SA1 config variants for a V1 country config
#'
#' Generates every combination where one indicator is dropped from each
#' pillar that has >= 2 indicators.  Single-indicator pillars are unchanged.
#' Returns a list with at least one element (the full config if no eligible
#' pillar exists).
sa1_configs_v1 <- function(country_config) {
  pillars  <- country_config$pillars
  eligible <- Filter(function(p) length(p$indicators) >= 2, pillars)

  if (length(eligible) == 0) return(list(country_config))

  elig_names  <- names(eligible)
  drop_ranges <- lapply(elig_names, function(nm) seq_along(eligible[[nm]]$indicators))
  names(drop_ranges) <- elig_names
  combos <- do.call(expand.grid, c(drop_ranges, list(stringsAsFactors = FALSE)))

  lapply(seq_len(nrow(combos)), function(row_i) {
    cfg <- country_config
    for (j in seq_along(elig_names)) {
      p_name <- elig_names[j]
      drop_i <- combos[row_i, j]
      pillar  <- cfg$pillars[[p_name]]
      keep    <- seq_along(pillar$indicators) != drop_i
      cfg$pillars[[p_name]]$indicators <- pillar$indicators[keep]
      cfg$pillars[[p_name]]$polarity   <- pillar$polarity[keep]
      if (!is.null(pillar$labels))
        cfg$pillars[[p_name]]$labels   <- pillar$labels[keep]
    }
    cfg
  })
}

#' All SA1 config variants for a V3 country config
#'
#' Uses the pillar_groups field to determine pillar membership of each se_var.
#' Eligible pillars are those whose group has >= 2 se_vars.
sa1_configs_v3 <- function(country_config) {
  groups   <- lapply(country_config$pillar_groups, as.character)
  eligible <- Filter(function(g) length(g) >= 2, groups)

  if (length(eligible) == 0) return(list(country_config))

  elig_names  <- names(eligible)
  drop_ranges <- lapply(elig_names, function(nm) seq_along(eligible[[nm]]))
  names(drop_ranges) <- elig_names
  combos <- do.call(expand.grid, c(drop_ranges, list(stringsAsFactors = FALSE)))

  se_vars_base <- as.character(country_config$se_vars)

  lapply(seq_len(nrow(combos)), function(row_i) {
    cfg     <- country_config
    se_vars <- se_vars_base
    for (j in seq_along(elig_names)) {
      p_name   <- elig_names[j]
      drop_i   <- combos[row_i, j]
      var_drop <- eligible[[p_name]][drop_i]
      se_vars  <- se_vars[se_vars != var_drop]
    }
    cfg$se_vars <- se_vars
    cfg
  })
}


# ── SA2: generate config variants ────────────────────────────────────────────

#' SA2 config variants for a V1 country config (drop each pillar once)
sa2_configs_v1 <- function(country_config) {
  lapply(names(country_config$pillars), function(p_name) {
    cfg <- country_config
    cfg$pillars <- cfg$pillars[names(cfg$pillars) != p_name]
    cfg
  })
}

#' SA2 config variants for a V3 country config (drop all se_vars per pillar)
sa2_configs_v3 <- function(country_config) {
  groups       <- lapply(country_config$pillar_groups, as.character)
  se_vars_base <- as.character(country_config$se_vars)

  lapply(names(groups), function(p_name) {
    cfg          <- country_config
    vars_to_drop <- groups[[p_name]]
    cfg$se_vars  <- se_vars_base[!se_vars_base %in% vars_to_drop]
    cfg
  })
}


# ── Core runner ───────────────────────────────────────────────────────────────

#' Run a list of config variants and return row-wise mean SEPI
#'
#' @param data     Country data frame (already loaded and region-filtered)
#' @param cfg_list List of modified country_config objects
#' @param version  sepi_version object
#' @param id_col   Column name identifying regions
#' @return Numeric vector (length = nrow(data)) of mean SEPI across variants
run_sa_mean_sepi <- function(data, cfg_list, version, id_col) {
  ids <- data[[id_col]]

  sepi_mat <- vapply(cfg_list, function(cfg) {
    res <- tryCatch(
      suppressWarnings(compute_sepi(data, version, country_config = cfg)),
      error = function(e) NULL
    )
    if (is.null(res)) return(rep(NA_real_, length(ids)))
    # Align by region ID (handles row-dropping in V3 "omit" imputation)
    res$sepi[match(ids, res[[id_col]])]
  }, FUN.VALUE = numeric(length(ids)))

  rowMeans(sepi_mat, na.rm = TRUE)
}


# ── Public API ────────────────────────────────────────────────────────────────

#' Compute SA1 and SA2 mean SEPI scores for one country
#'
#' @param data           Country data frame (already loaded and region-filtered)
#' @param country_config Single-country entry from version$countries
#' @param version        sepi_version object
#' @return Data frame: region_id, region_name, sa1_sepi, sa2_sepi,
#'         sa1_n_combos, sa2_n_combos
run_sensitivity_country <- function(data, country_config, version) {
  id_col   <- country_config$id_cols[1]
  name_col <- country_config$id_cols[2]
  is_v3    <- isTRUE(version$conflict_weighting)

  cfg_sa1 <- if (is_v3) sa1_configs_v3(country_config) else sa1_configs_v1(country_config)
  cfg_sa2 <- if (is_v3) sa2_configs_v3(country_config) else sa2_configs_v1(country_config)

  cat(sprintf("  SA1: %d combination(s) | SA2: %d combination(s)\n",
              length(cfg_sa1), length(cfg_sa2)))

  sa1_sepi <- run_sa_mean_sepi(data, cfg_sa1, version, id_col)
  sa2_sepi <- run_sa_mean_sepi(data, cfg_sa2, version, id_col)

  data.frame(
    region_id    = data[[id_col]],
    region_name  = data[[name_col]],
    sa1_sepi     = sa1_sepi,
    sa2_sepi     = sa2_sepi,
    sa1_n_combos = length(cfg_sa1),
    sa2_n_combos = length(cfg_sa2),
    stringsAsFactors = FALSE
  )
}

#' Run sensitivity analysis for all countries in a version
#'
#' @param all_data Named list of country data frames (from load_all_data)
#' @param version  sepi_version object
#' @return Named list of per-country sensitivity result data frames
run_sensitivity_all <- function(all_data, version) {
  cat("\n========================================\n")
  cat(" Sensitivity Analysis —", version$name, "\n")
  cat("========================================\n")

  purrr::imap(all_data, function(data, country) {
    cat("\n--", country_label(country), "--\n")
    run_sensitivity_country(data, version$countries[[country]], version)
  })
}


# ── Comparison table builders ─────────────────────────────────────────────────

#' Combine main SEPI + SA results into a per-country comparison data frame
#'
#' @param main_v1  Named list of compute_sepi() results for V1
#' @param sa_v1    Named list of run_sensitivity_country() results for V1
#' @param main_v3  Named list of compute_sepi() results for V3
#' @param sa_v3    Named list of run_sensitivity_country() results for V3
#' @return Named list of per-country data frames ready for display/export
build_comparison_table <- function(main_v1, sa_v1, main_v3, sa_v3) {
  countries <- names(main_v1)

  lapply(
    stats::setNames(countries, countries),
    function(country) {
      m1 <- main_v1[[country]] %>%
        dplyr::select(adm1_pcode, adm1_name,
                      v1_main = sepi, v1_main_rank = sepi_rank)

      s1 <- sa_v1[[country]] %>%
        dplyr::select(region_id,
                      v1_sa1 = sa1_sepi, v1_sa2 = sa2_sepi,
                      sa1_n_combos, sa2_n_combos)

      m3 <- main_v3[[country]] %>%
        dplyr::select(adm1_pcode,
                      v3_main = sepi, v3_main_rank = sepi_rank)

      s3 <- sa_v3[[country]] %>%
        dplyr::select(region_id,
                      v3_sa1 = sa1_sepi, v3_sa2 = sa2_sepi)

      m1 %>%
        dplyr::left_join(s1, by = c("adm1_pcode" = "region_id")) %>%
        dplyr::left_join(m3, by = "adm1_pcode") %>%
        dplyr::left_join(s3, by = c("adm1_pcode" = "region_id")) %>%
        dplyr::mutate(
          v1_sa1_rank = rank(-v1_sa1, na.last = NA, ties.method = "min"),
          v1_sa2_rank = rank(-v1_sa2, na.last = NA, ties.method = "min"),
          v3_sa1_rank = rank(-v3_sa1, na.last = NA, ties.method = "min"),
          v3_sa2_rank = rank(-v3_sa2, na.last = NA, ties.method = "min")
        ) %>%
        dplyr::arrange(v1_main_rank)
    }
  )
}

#' Render a per-country comparison table as a PNG using {gt}
#'
#' @param tbl_data  One-country data frame from build_comparison_table()
#' @param country   Country key string (e.g. "kenya")
#' @param n_sa1_v1  Number of SA1 combinations for V1 (for subtitle annotation)
#' @param n_sa1_v3  Number of SA1 combinations for V3
#' @param out_path  File path for the saved PNG
render_comparison_png <- function(tbl_data, country, n_sa1_v1, n_sa1_v3,
                                  out_path) {
  if (!requireNamespace("gt", quietly = TRUE)) install.packages("gt")
  library(gt)

  country_label_nice <- stringr::str_to_title(gsub("_", " ", country))
  n_pillars_v1 <- length(unique(c(
    names(which(!is.na(tbl_data$v1_main)))
  )))  # just use n_sa2 combos from first row
  n_sa2 <- if ("sa2_n_combos" %in% names(tbl_data)) tbl_data$sa2_n_combos[1] else 5L

  tbl <- tbl_data %>%
    dplyr::select(
      Region         = adm1_name,
      `V1 main`      = v1_main,
      `V1 SA1 mean`  = v1_sa1,
      `V1 SA2 mean`  = v1_sa2,
      `V3 main`      = v3_main,
      `V3 SA1 mean`  = v3_sa1,
      `V3 SA2 mean`  = v3_sa2,
      `V1 rank`      = v1_main_rank,
      `V1 SA1 rank`  = v1_sa1_rank,
      `V1 SA2 rank`  = v1_sa2_rank,
      `V3 rank`      = v3_main_rank,
      `V3 SA1 rank`  = v3_sa1_rank,
      `V3 SA2 rank`  = v3_sa2_rank
    ) %>%
    gt() %>%
    tab_header(
      title    = paste0(country_label_nice, " — SEPI Sensitivity Analysis"),
      subtitle = paste0(
        "SA1 = mean SEPI over all combinations dropping one indicator per multi-indicator pillar ",
        "(V1: ", n_sa1_v1, " combos; V3: ", n_sa1_v3, " combos).  ",
        "SA2 = mean SEPI over ", n_sa2, " runs each dropping one pillar entirely.  ",
        "Higher score = better socio-economic conditions."
      )
    ) %>%
    tab_spanner(
      label   = "V1 Equal-Weight Geometric",
      columns = c(`V1 main`, `V1 SA1 mean`, `V1 SA2 mean`)
    ) %>%
    tab_spanner(
      label   = "V3 Conflict-Weighted",
      columns = c(`V3 main`, `V3 SA1 mean`, `V3 SA2 mean`)
    ) %>%
    tab_spanner(
      label   = "V1 Rankings (1 = best)",
      columns = c(`V1 rank`, `V1 SA1 rank`, `V1 SA2 rank`)
    ) %>%
    tab_spanner(
      label   = "V3 Rankings (1 = best)",
      columns = c(`V3 rank`, `V3 SA1 rank`, `V3 SA2 rank`)
    ) %>%
    fmt_number(
      columns  = c(`V1 main`, `V1 SA1 mean`, `V1 SA2 mean`,
                   `V3 main`, `V3 SA1 mean`, `V3 SA2 mean`),
      decimals = 3
    ) %>%
    data_color(
      columns = c(`V1 main`, `V1 SA1 mean`, `V1 SA2 mean`,
                  `V3 main`, `V3 SA1 mean`, `V3 SA2 mean`),
      method  = "numeric",
      palette = c("#d73027", "#fee08b", "#1a9850")
    ) %>%
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    cols_width(
      Region        ~ px(140),
      `V1 main`     ~ px(80), `V1 SA1 mean` ~ px(80), `V1 SA2 mean` ~ px(80),
      `V3 main`     ~ px(80), `V3 SA1 mean` ~ px(80), `V3 SA2 mean` ~ px(80),
      `V1 rank`     ~ px(55), `V1 SA1 rank` ~ px(60), `V1 SA2 rank` ~ px(60),
      `V3 rank`     ~ px(55), `V3 SA1 rank` ~ px(60), `V3 SA2 rank` ~ px(60)
    ) %>%
    tab_options(
      table.font.size            = 11,
      heading.title.font.size    = 13,
      heading.subtitle.font.size = 10,
      column_labels.font.size    = 10,
      table.width                = px(1050)
    )

  gt::gtsave(tbl, out_path)
  cat("  Saved:", out_path, "\n")
  invisible(tbl)
}

#' Export all comparison tables to a single Excel workbook
#'
#' Each country gets one sheet with all six SEPI columns (main + SA1 + SA2)
#' plus their rank equivalents.
#'
#' @param comparison Named list from build_comparison_table()
#' @param out_path   Path for the .xlsx file
export_sensitivity_excel <- function(comparison, out_path) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")

  wb <- openxlsx::createWorkbook()

  readme_text <- data.frame(
    Description = c(
      "SEPI Sensitivity Analysis Comparison",
      "",
      "Columns:",
      "  *_main      = SEPI from the full indicator set (baseline)",
      "  *_sa1_mean  = mean SEPI across all combinations dropping one indicator",
      "                per multi-indicator pillar simultaneously (SA1)",
      "  *_sa2_mean  = mean SEPI across runs dropping each pillar entirely (SA2)",
      "  *_rank      = rank of the corresponding SEPI score (1 = best)",
      "",
      "Versions:",
      "  V1 = v1_aligned_equal_geometric  (equal weights, arithmetic within, geometric across pillars)",
      "  V3 = v3_aligned_conflict_weighted (conflict-correlation weighted flat sum, aligned indicators)",
      "",
      "sa1_n_combos = number of indicator-drop combinations used in SA1",
      "sa2_n_combos = number of pillar-drop runs used in SA2 (always = n_pillars)"
    ),
    stringsAsFactors = FALSE
  )

  openxlsx::addWorksheet(wb, "README")
  openxlsx::writeData(wb, "README", readme_text, colNames = FALSE)

  score_cols <- c("v1_main", "v1_sa1", "v1_sa2", "v3_main", "v3_sa1", "v3_sa2")
  num_style  <- openxlsx::createStyle(numFmt = "0.000")

  for (country in names(comparison)) {
    sheet <- stringr::str_to_title(gsub("_", " ", country))
    openxlsx::addWorksheet(wb, sheet)

    out_df <- comparison[[country]] %>%
      dplyr::select(
        Region        = adm1_name,
        v1_main, v1_sa1, v1_sa2,
        v3_main, v3_sa1, v3_sa2,
        v1_main_rank, v1_sa1_rank, v1_sa2_rank,
        v3_main_rank, v3_sa1_rank, v3_sa2_rank,
        sa1_n_combos, sa2_n_combos
      )

    openxlsx::writeData(wb, sheet, out_df)

    # Format score columns as 3 d.p.
    score_idx <- which(names(out_df) %in% score_cols) + 1  # +1 for row header
    openxlsx::addStyle(wb, sheet, num_style,
                       rows = seq_len(nrow(out_df)) + 1,
                       cols = which(names(out_df) %in% score_cols),
                       gridExpand = TRUE)
  }

  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  cat("  Excel saved:", out_path, "\n")
}
