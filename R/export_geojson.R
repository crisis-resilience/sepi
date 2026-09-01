# ============================================================================
# GeoJSON Export
# ============================================================================
# Produces per-country GeoJSON files (and one combined) that join Admin-1
# polygon boundaries with all SEPI attribute data: SEPI results, normalised
# indicator scores, raw indicator values, and yearly conflict data.
#
# Dependencies (already sourced by 03_run_sepi.R before this file):
#   - R/visualise.R   → load_adm1_sf()
#   - R/export_excel.R → get_sepi_vars(), get_ind_to_pillar() (not used here)
#   - R/utils.R        → country_label()
#   - R/config.R       → GIS_CONFIG, MERGE_BUILD_CONFIG
# ============================================================================

export_sepi_geojson <- function(sepi_results,
                                version,
                                output_dir = "outputs",
                                gis_config = GIS_CONFIG) {

  if (!requireNamespace("sf", quietly = TRUE))
    stop("Package 'sf' is required. Install with: install.packages('sf')")

  geojson_dir <- file.path(output_dir, "geojson")
  dir.create(geojson_dir, recursive = TRUE, showWarnings = FALSE)

  # ── Conflict data ─────────────────────────────────────────────────────────
  # Read conflict.csv.  For GeoJSON we multiply _per_1k_ values ×100 (giving
  # per-100k magnitude) but intentionally keep the original _per_1k_ column
  # names to match the reference GeoJSON files produced by the prior QGIS step.
  conflict_path <- "data/socio-economic/conflict.csv"
  conflict_all  <- NULL
  if (file.exists(conflict_path)) {
    conflict_all <- utils::read.csv(conflict_path, stringsAsFactors = FALSE,
                                    check.names = FALSE)
    per1k_cols <- grep("_per_1k_", names(conflict_all), value = TRUE)
    for (col in per1k_cols) {
      conflict_all[[col]] <- conflict_all[[col]] * 100
    }
  } else {
    warning("conflict.csv not found at '", conflict_path,
            "' — conflict columns will be omitted from GeoJSON.")
  }

  # ── Superset of _norm column names across all countries ───────────────────
  # Every country gets all _norm columns (NA where not applicable) so features
  # in a combined GeoJSON share a consistent schema.
  all_norm_cols <- unique(unlist(lapply(version$countries, function(cc) {
    sv <- if (!is.null(cc$pillars)) {
      unlist(lapply(cc$pillars, `[[`, "indicators"), use.names = FALSE)
    } else if (!is.null(cc$pillar_map)) {
      unname(unlist(cc$pillar_map))
    } else {
      cc$se_vars
    }
    paste0(sv, "_norm")
  }), use.names = FALSE))

  per_country_paths <- character(0)
  country_sf_list   <- list()

  for (country_name in names(sepi_results)) {
    res <- sepi_results[[country_name]]
    cc  <- version$countries[[country_name]]

    # ── Shapefile: keep only pcode + geometry ────────────────────────────────
    shp <- load_adm1_sf(country_name, gis_config)
    shp <- dplyr::select(shp, "adm1_pcode") # drops all non-geometry non-pcode cols

    # ── SEPI results ─────────────────────────────────────────────────────────
    id_cols     <- cc$id_cols
    pillar_cols <- grep("^pillar_", names(res), value = TRUE)
    sepi_cols   <- c(id_cols, pillar_cols, "sepi", "n_pillars", "sepi_rank")
    sepi_cols   <- sepi_cols[sepi_cols %in% names(res)]

    sepi_df <- res |>
      dplyr::select(dplyr::all_of(sepi_cols)) |>
      dplyr::mutate(country = country_label(country_name), .before = 1)

    # ── Normalised indicators (polarity re-inverted to match display direction)
    sepi_vars <- if (!is.null(cc$pillars)) {
      unlist(lapply(cc$pillars, `[[`, "indicators"), use.names = FALSE)
    } else if (!is.null(cc$pillar_map)) {
      unname(unlist(cc$pillar_map))
    } else {
      cc$se_vars
    }

    norm_cols_present <- paste0(sepi_vars, "_norm")
    norm_cols_present <- norm_cols_present[norm_cols_present %in% names(res)]

    norm_df <- dplyr::select(res, "adm1_pcode", dplyr::all_of(norm_cols_present))

    # Re-invert negative-polarity indicators (matches build_indicator_scores_sheet)
    if (!is.null(cc$pillars)) {
      for (p in cc$pillars) {
        for (i in seq_along(p$indicators)) {
          if (isTRUE(p$polarity[i] == -1)) {
            nc <- paste0(p$indicators[i], "_norm")
            if (nc %in% names(norm_df)) norm_df[[nc]] <- 1 - norm_df[[nc]]
          }
        }
      }
    }

    # Add missing norm columns (from other countries) as NA
    for (nc in setdiff(all_norm_cols, names(norm_df))) {
      norm_df[[nc]] <- NA_real_
    }
    norm_df <- dplyr::select(norm_df, "adm1_pcode", dplyr::all_of(all_norm_cols))

    # ── Raw indicator values ──────────────────────────────────────────────────
    raw_cols <- sepi_vars[sepi_vars %in% names(res)]
    raw_df   <- dplyr::select(res, "adm1_pcode", dplyr::all_of(raw_cols))

    # NA → 0 for pop_frac_3plus (not monitored by IPC = no acute crisis)
    if ("pop_frac_3plus" %in% names(raw_df)) {
      raw_df[["pop_frac_3plus"]][is.na(raw_df[["pop_frac_3plus"]])] <- 0
    }

    # ── Conflict data for this country ────────────────────────────────────────
    conf_df <- NULL
    if (!is.null(conflict_all)) {
      iso_code <- MERGE_BUILD_CONFIG$country_mapping[[country_name]]$country_code
      conf_sub <- conflict_all[
        conflict_all[["country_code"]] == iso_code, , drop = FALSE
      ]
      # Drop columns that are already present via the SEPI join
      conf_df <- dplyr::select(conf_sub,
                               -dplyr::any_of(c("country", "country_code",
                                                "adm1_name", "adm1_na")))
    }

    # ── Join everything to geometry ───────────────────────────────────────────
    sf_data <- shp |>
      dplyr::left_join(sepi_df, by = "adm1_pcode") |>
      dplyr::left_join(norm_df, by = "adm1_pcode") |>
      dplyr::left_join(raw_df,  by = "adm1_pcode")

    if (!is.null(conf_df)) {
      sf_data <- dplyr::left_join(sf_data, conf_df, by = "adm1_pcode")
    }

    # Middle Juba (SO27) has no sufficient data for a reliable SEPI score —
    # suppress it in GeoJSON so the dashboard renders it as no-data
    if (country_name == "somalia" && "sepi" %in% names(sf_data)) {
      sf_data$sepi[sf_data$adm1_pcode == "SO27"] <- NA_real_
    }

    # Ensure WGS 84 (required by the GeoJSON spec)
    if (!sf::st_is_longlat(sf_data)) {
      sf_data <- sf::st_transform(sf_data, 4326)
    }

    # ── Write per-country GeoJSON ─────────────────────────────────────────────
    out_file <- file.path(geojson_dir,
                          paste0(country_name, "_", version$name, ".geojson"))
    sf::st_write(sf_data, out_file, delete_dsn = TRUE, quiet = TRUE)
    cat("Exported GeoJSON:", out_file, "\n")

    per_country_paths[country_name] <- out_file
    country_sf_list[[country_name]]  <- sf_data
  }

  # ── Combined GeoJSON (all countries) ─────────────────────────────────────
  # dplyr::bind_rows fills columns absent in some countries with NA and
  # preserves the sfc geometry column across sf objects
  combined <- dplyr::bind_rows(country_sf_list)
  combined <- sf::st_as_sf(combined)
  all_file <- file.path(geojson_dir,
                        paste0("all_countries_", version$name, ".geojson"))
  sf::st_write(combined, all_file, delete_dsn = TRUE, quiet = TRUE)
  cat("Exported GeoJSON:", all_file, "\n")

  invisible(list(per_country = per_country_paths, combined = all_file))
}
