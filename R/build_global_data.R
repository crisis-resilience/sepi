# ============================================================================
# Global Merge Build (3-country indicators + metadata)
# ============================================================================

normalize_admin_name <- function(x) {
  x <- as.character(x)
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", "")
  dplyr::na_if(x, "")
}

normalize_country_key <- function(x) {
  x <- as.character(x)
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", "_")
  x <- stringr::str_replace_all(x, "^_+|_+$", "")
  dplyr::na_if(x, "")
}

load_env_vars <- function(env_file = ".env") {
  if (!file.exists(env_file)) return(list())

  lines <- readLines(env_file, warn = FALSE)
  out <- list()

  for (line in lines) {
    line <- trimws(line)
    if (nchar(line) == 0 || startsWith(line, "#")) next
    parts <- strsplit(line, "=", fixed = TRUE)[[1]]
    if (length(parts) < 2) next
    key <- trimws(parts[1])
    val <- trimws(paste(parts[-1], collapse = "="))
    out[[key]] <- val
  }

  out
}

load_country_indicator_files <- function(base_path = ".", config = MERGE_BUILD_CONFIG) {
  purrr::imap_dfr(config$paths$indicator_files, function(rel_path, country_key) {
    country_code <- config$country_mapping[[country_key]]$country_code
    filepath <- file.path(base_path, rel_path)

    d <- readr::read_csv(filepath, show_col_types = FALSE) |>
      dplyr::mutate(
        country = country_key,
        country_code = country_code,
        adm1_pcode = as.character(adm1_pcode),
        adm1_name = as.character(adm1_name)
      )

    d
  })
}

load_country_metadata_files <- function(base_path = ".", config = MERGE_BUILD_CONFIG) {
  purrr::imap_dfr(config$paths$metadata_files, function(rel_path, country_key) {
    country_code <- config$country_mapping[[country_key]]$country_code
    filepath <- file.path(base_path, rel_path)

    d <- readr::read_csv(
      filepath,
      show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_character())
    ) |>
      dplyr::mutate(
        country = country_key,
        country_code = country_code,
        `SEPI Indicator ID` = paste(country_code, `SEPI Indicator ID`, sep = "_"),
        global_variable_name = `Original variable name`,
        build_rule = "source_country_metadata"
      )

    d
  })
}

load_remote_sensing_latest <- function(base_path = ".", config = MERGE_BUILD_CONFIG) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required for remote sensing ingestion.")
  }

  filepath <- file.path(base_path, config$paths$remote_sensing_file)
  sheet <- config$paths$remote_sensing_sheet

  country_lookup <- purrr::imap_chr(config$country_mapping, ~ .x$remote_sensing_name)
  country_lookup <- setNames(names(country_lookup), country_lookup)

  rs <- readxl::read_excel(filepath, sheet = sheet) |>
    dplyr::mutate(
      country = unname(country_lookup[as.character(adm0_name)]),
      adm1_pcode = as.character(adm1_pcode),
      adm1_name = as.character(adm1_name),
      year = as.integer(year)
    ) |>
    dplyr::filter(!is.na(country), !is.na(adm1_pcode)) |>
    dplyr::arrange(dplyr::desc(year)) |>
    dplyr::group_by(country, adm1_pcode) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      country,
      adm1_pcode,
      rs_year = year,
      rs_pdsi = as.numeric(pdsi),
      rs_soil_moist = as.numeric(soil_moist),
      rs_fapar = as.numeric(fapar),
      rs_ndvi = as.numeric(ndvi)
    )

  rs
}

load_population_admin1 <- function(base_path = ".", config = MERGE_BUILD_CONFIG) {
  filepath <- file.path(base_path, config$paths$population_file)
  pop <- readr::read_csv(filepath, show_col_types = FALSE) |>
    dplyr::mutate(
      country = normalize_country_key(country),
      adm1_pcode = as.character(adm1_pcode),
      population = suppressWarnings(as.numeric(population))
    ) |>
    dplyr::filter(!is.na(country), !is.na(adm1_pcode)) |>
    dplyr::select(country, adm1_pcode, population)

  pop
}

fetch_acled_country_events <- function(country_key,
                                       env_vars,
                                       config = MERGE_BUILD_CONFIG) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required for ACLED API calls.")
  }

  email <- env_vars$ACLED_EMAIL
  password <- env_vars$ACLED_PASSWORD
  if (is.null(email) || is.null(password)) {
    stop("ACLED credentials missing. Define ACLED_EMAIL and ACLED_PASSWORD in .env.")
  }

  country_query <- config$country_mapping[[country_key]]$acled_name
  start_date <- config$conflict$start_date
  end_date <- config$conflict$end_date
  api_limit <- config$conflict$api_limit
  api_max_pages <- config$conflict$api_max_pages
  if (is.null(api_limit) || !is.numeric(api_limit) || api_limit <= 0) api_limit <- 5000
  if (is.null(api_max_pages) || !is.numeric(api_max_pages) || api_max_pages <= 0) api_max_pages <- 100

  cat(
    sprintf(
      "[ACLED] Requesting %s (%s to %s), limit=%d...\n",
      country_query,
      start_date,
      end_date,
      as.integer(api_limit)
    )
  )

  api_url <- "https://acleddata.com/api/acled/read?_format=json"
  token_url <- "https://acleddata.com/oauth/token"

  pages <- list()
  for (page in seq_len(as.integer(api_max_pages))) {
    resp <- httr2::request(api_url) |>
      httr2::req_oauth_password(
        client = httr2::oauth_client("acled", token_url),
        username = email,
        password = password
      ) |>
      httr2::req_url_query(
        country = country_query,
        event_date = paste0(start_date, "|", end_date),
        event_date_where = "BETWEEN",
        limit = as.integer(api_limit),
        page = page
      ) |>
      httr2::req_perform()

    body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
    page_data <- if (is.null(body$data)) tibble::tibble() else tibble::as_tibble(body$data)
    n_page <- nrow(page_data)
    cat(sprintf("[ACLED] %s: page %d -> %d events\n", country_query, page, n_page))

    if (n_page == 0) break
    pages[[length(pages) + 1]] <- page_data

    # Last page is shorter than limit.
    if (n_page < as.integer(api_limit)) break
  }

  if (length(pages) == 0) {
    cat(sprintf("[ACLED] %s: 0 events returned.\n", country_query))
    return(tibble::tibble())
  }

  out <- dplyr::bind_rows(pages)
  if ("event_id_cnty" %in% names(out)) {
    out <- dplyr::distinct(out, event_id_cnty, .keep_all = TRUE)
  }

  cat(sprintf("[ACLED] %s: %d total events returned after pagination.\n", country_query, nrow(out)))
  out
}

aggregate_acled_to_adm1 <- function(acled_events,
                                    country_key,
                                    adm_lookup,
                                    config = MERGE_BUILD_CONFIG) {
  start_yr  <- as.integer(substr(config$conflict$start_date, 1, 4))
  end_yr    <- as.integer(substr(config$conflict$end_date,   1, 4))
  all_years <- seq(start_yr, end_yr)

  empty_wide <- function() {
    base <- tibble::tibble(country = character(), adm1_pcode = character())
    for (yr in all_years) {
      base[[paste0("total_fatalities_",      yr)]] <- numeric()
      base[[paste0("count_conflict_events_", yr)]] <- integer()
    }
    base
  }

  if (nrow(acled_events) == 0) {
    return(list(
      data = empty_wide(),
      unmatched = tibble::tibble(
        country               = character(),
        admin1                = character(),
        admin1_norm           = character(),
        total_fatalities      = numeric(),
        count_conflict_events = integer()
      )
    ))
  }

  event_types <- config$conflict$event_types

  agg <- acled_events |>
    dplyr::filter(event_type %in% event_types) |>
    dplyr::mutate(
      fatalities = suppressWarnings(as.numeric(fatalities)),
      year       = as.integer(year)
    ) |>
    dplyr::group_by(admin1, year) |>
    dplyr::summarise(
      total_fatalities      = sum(fatalities, na.rm = TRUE),
      count_conflict_events = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(admin1_norm = normalize_admin_name(admin1))

  lookup_country <- adm_lookup |>
    dplyr::filter(country == country_key) |>
    dplyr::mutate(admin1_norm = normalize_admin_name(adm1_name)) |>
    dplyr::distinct(country, adm1_pcode, adm1_name, admin1_norm)

  joined <- agg |>
    dplyr::left_join(
      lookup_country |>
        dplyr::select(country, adm1_pcode, admin1_norm),
      by = "admin1_norm"
    ) |>
    dplyr::mutate(country = country_key)

  overrides <- config$country_mapping[[country_key]]$acled_admin1_overrides
  if (length(overrides) > 0) {
    override_tbl <- tibble::tibble(
      admin1_norm         = normalize_admin_name(names(overrides)),
      adm1_pcode_override = unname(overrides)
    )
    joined <- joined |>
      dplyr::left_join(override_tbl, by = "admin1_norm") |>
      dplyr::mutate(adm1_pcode = dplyr::coalesce(adm1_pcode, adm1_pcode_override)) |>
      dplyr::select(-adm1_pcode_override)
  }

  matched_long <- joined |>
    dplyr::filter(!is.na(adm1_pcode)) |>
    dplyr::group_by(country, adm1_pcode, year) |>
    dplyr::summarise(
      total_fatalities      = sum(total_fatalities,      na.rm = TRUE),
      count_conflict_events = sum(count_conflict_events, na.rm = TRUE),
      .groups = "drop"
    )

  matched_wide <- matched_long |>
    tidyr::pivot_wider(
      names_from  = year,
      values_from = c(total_fatalities, count_conflict_events),
      values_fill = 0
    )

  # Ensure all expected year columns exist (years with zero events may be absent)
  for (yr in all_years) {
    fat_col <- paste0("total_fatalities_",      yr)
    evt_col <- paste0("count_conflict_events_", yr)
    if (!fat_col %in% names(matched_wide)) matched_wide[[fat_col]] <- 0
    if (!evt_col %in% names(matched_wide)) matched_wide[[evt_col]] <- 0L
  }

  unmatched <- joined |>
    dplyr::filter(is.na(adm1_pcode)) |>
    dplyr::group_by(admin1, admin1_norm) |>
    dplyr::summarise(
      total_fatalities      = sum(total_fatalities,      na.rm = TRUE),
      count_conflict_events = sum(count_conflict_events, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(country = country_key) |>
    dplyr::select(country, admin1, admin1_norm, total_fatalities, count_conflict_events)

  list(data = matched_wide, unmatched = unmatched)
}

build_conflict_cross_section <- function(adm_lookup,
                                         base_path = ".",
                                         config = MERGE_BUILD_CONFIG) {
  env_vars <- load_env_vars(file.path(base_path, config$paths$env_file))
  cat("[ACLED] Starting API fetch for all countries.\n")

  out <- purrr::imap(config$country_mapping, function(country_cfg, country_key) {
    cat(sprintf("[ACLED] Processing country key: %s\n", country_key))
    events <- fetch_acled_country_events(country_key, env_vars, config)
    aggregate_acled_to_adm1(events, country_key, adm_lookup, config)
  })

  conflict_data <- dplyr::bind_rows(purrr::map(out, "data"))
  unmatched <- dplyr::bind_rows(purrr::map(out, "unmatched"))
  cat(
    sprintf(
      "[ACLED] Finished API fetch. Matched ADM1 rows: %d | Unmatched admin1 rows: %d\n",
      nrow(conflict_data),
      nrow(unmatched)
    )
  )

  list(data = conflict_data, unmatched = unmatched)
}

merge_all_indicator_domains <- function(se_base, rs_latest, conflict_data,
                                        config = MERGE_BUILD_CONFIG) {
  start_yr  <- as.integer(substr(config$conflict$start_date, 1, 4))
  end_yr    <- as.integer(substr(config$conflict$end_date,   1, 4))
  all_years <- seq(start_yr, end_yr)

  merged <- se_base |>
    dplyr::left_join(rs_latest,     by = c("country", "adm1_pcode")) |>
    dplyr::left_join(conflict_data, by = c("country", "adm1_pcode"))

  for (yr in all_years) {
    fat_col <- paste0("total_fatalities_",            yr)
    evt_col <- paste0("count_conflict_events_",       yr)
    fat_1k  <- paste0("total_fatalities_per_1k_",     yr)
    evt_1k  <- paste0("count_conflicts_events_per_1k_", yr)

    merged[[fat_col]] <- tidyr::replace_na(merged[[fat_col]], 0)
    merged[[evt_col]] <- tidyr::replace_na(as.integer(merged[[evt_col]]), 0L)

    merged[[fat_1k]] <- dplyr::if_else(
      !is.na(merged$population) & merged$population > 0,
      (merged[[fat_col]] / merged$population) * 1000,
      NA_real_
    )
    merged[[evt_1k]] <- dplyr::if_else(
      !is.na(merged$population) & merged$population > 0,
      (merged[[evt_col]] / merged$population) * 1000,
      NA_real_
    )
  }

  merged |>
    dplyr::select(-population) |>
    dplyr::arrange(country, adm1_name)
}

build_remote_metadata_rows <- function(se_base,
                                       rs_latest,
                                       config = MERGE_BUILD_CONFIG) {
  id_suffix <- c("CLM_01", "CLM_02", "CLM_03", "CLM_04")
  vars <- c("rs_pdsi", "rs_soil_moist", "rs_fapar", "rs_ndvi")
  names_lbl <- c("PDSI", "Soil Moisture", "FAPAR", "NDVI")
  descr <- c(
    "Palmer Drought Severity Index, latest available per ADM1.",
    "Remote-sensed soil moisture, latest available per ADM1.",
    "Remote-sensed Fraction of Absorbed Photosynthetically Active Radiation, latest available per ADM1.",
    "Remote-sensed NDVI, latest available per ADM1."
  )

  year_by_country <- rs_latest |>
    dplyr::group_by(country) |>
    dplyr::summarise(rs_year = max(rs_year, na.rm = TRUE), .groups = "drop")

  purrr::imap_dfr(config$country_mapping, function(country_cfg, country_key) {
    y <- year_by_country |>
      dplyr::filter(country == country_key) |>
      dplyr::pull(rs_year)
    y <- ifelse(length(y) == 0 || !is.finite(y), NA_integer_, y)

    tibble::tibble(
      `Original variable name` = vars,
      `SEPI Indicator ID` = paste(country_cfg$country_code, id_suffix, sep = "_"),
      Pillar = "Climate",
      `Indicator name` = names_lbl,
      Description = descr,
      `Unit of measurement` = c("index", "index", "index", "index"),
      `Data source` = "Remote sensing aggregate workbook",
      `Source file or URL` = config$paths$remote_sensing_file,
      `Data collection year` = as.character(y),
      `Reference period` = paste0(as.character(y), " (latest available per ADM1)"),
      `Geographic level` = "Admin 1",
      `Indicator type` = "Input",
      Directionality = "Higher = less deprived",
      country = country_key,
      country_code = country_cfg$country_code,
      global_variable_name = vars,
      build_rule = "latest_per_adm1_remote_sensing"
    )
  })
}

build_conflict_metadata_rows <- function(config = MERGE_BUILD_CONFIG) {
  start_yr  <- as.integer(substr(config$conflict$start_date, 1, 4))
  end_yr    <- as.integer(substr(config$conflict$end_date,   1, 4))
  all_years <- seq(start_yr, end_yr)

  purrr::imap_dfr(config$country_mapping, function(country_cfg, country_key) {
    purrr::map_dfr(all_years, function(yr) {
      tibble::tibble(
        `Original variable name` = c(
          paste0("total_fatalities_",              yr),
          paste0("count_conflict_events_",         yr),
          paste0("total_fatalities_per_1k_",       yr),
          paste0("count_conflicts_events_per_1k_", yr)
        ),
        `SEPI Indicator ID` = paste0(
          country_cfg$country_code, "_",
          c(
            paste0("CFT_FAT_",   yr),
            paste0("CFT_EVT_",   yr),
            paste0("CFT_FAT1K_", yr),
            paste0("CFT_EVT1K_", yr)
          )
        ),
        Pillar = "Conflict",
        `Indicator name` = c(
          paste0("Total conflict fatalities (", yr, ")"),
          paste0("Count of conflict events (",  yr, ")"),
          paste0("Total conflict fatalities per 1,000 population (", yr, ")"),
          paste0("Count of conflict events per 1,000 population (", yr, ")")
        ),
        Description = c(
          paste0("Total ACLED fatalities aggregated at ADM1 for selected conflict event types (", yr, ")."),
          paste0("Count of ACLED events (Battles, Explosions/Remote violence, Violence against civilians) aggregated at ADM1 (", yr, ")."),
          paste0("Total ACLED fatalities aggregated at ADM1 and normalized per 1,000 population (", yr, ")."),
          paste0("Count of ACLED events aggregated at ADM1 and normalized per 1,000 population (", yr, ").")
        ),
        `Unit of measurement` = c("count (deaths)", "count (events)", "count per 1,000", "count per 1,000"),
        `Data source`          = "ACLED API",
        `Source file or URL`   = "https://acleddata.com/api/acled/read?_format=json",
        `Data collection year` = as.character(yr),
        `Reference period`     = as.character(yr),
        `Geographic level`     = "Admin 1",
        `Indicator type`       = "Output",
        Directionality         = "Higher = more deprived",
        country                = country_key,
        country_code           = country_cfg$country_code,
        global_variable_name   = c(
          paste0("total_fatalities_",              yr),
          paste0("count_conflict_events_",         yr),
          paste0("total_fatalities_per_1k_",       yr),
          paste0("count_conflicts_events_per_1k_", yr)
        ),
        build_rule = paste0("acled_cy", yr, "_aggregation")
      )
    })
  })
}

apply_sepi_id_columns <- function(indicators_merged, metadata_merged) {
  allowed_vars <- metadata_merged |>
    dplyr::pull(global_variable_name) |>
    unique()

  out <- indicators_merged |>
    dplyr::rename(adm1_na = adm1_name)

  key_first <- c("country", "country_code", "adm1_pcode", "adm1_na")
  keep_vars <- names(out)[names(out) %in% allowed_vars]
  keep_cols <- c(key_first, keep_vars)

  out[, keep_cols[keep_cols %in% names(out)], drop = FALSE]
}

merge_all_metadata <- function(se_metadata,
                               se_base,
                               rs_latest,
                               config = MERGE_BUILD_CONFIG) {
  rs_meta <- build_remote_metadata_rows(se_base, rs_latest, config)
  conflict_meta <- build_conflict_metadata_rows(config)

  dplyr::bind_rows(se_metadata, rs_meta, conflict_meta)
}

run_merge_qc_checks <- function(indicators_merged,
                                metadata_merged,
                                conflict_unmatched,
                                config = MERGE_BUILD_CONFIG) {
  key_cols <- config$keys

  key_dups <- indicators_merged |>
    dplyr::count(dplyr::across(dplyr::all_of(key_cols)), name = "n") |>
    dplyr::filter(n > 1)

  id_dups <- metadata_merged |>
    dplyr::count(`SEPI Indicator ID`, name = "n") |>
    dplyr::filter(n > 1)

  list(
    indicator_rows = nrow(indicators_merged),
    metadata_rows = nrow(metadata_merged),
    duplicate_indicator_keys = nrow(key_dups),
    duplicate_indicator_ids = nrow(id_dups),
    unmatched_acled_admin1 = nrow(conflict_unmatched),
    unmatched_acled_examples = utils::head(conflict_unmatched, 10)
  )
}

write_global_outputs <- function(indicators_merged,
                                 metadata_merged,
                                 qc_report,
                                 base_path = ".",
                                 config = MERGE_BUILD_CONFIG) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required to write QC JSON.")
  }

  out_dir <- file.path(base_path, config$paths$output_dir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  indicators_file <- file.path(out_dir, config$paths$output_indicators_file)
  metadata_file <- file.path(out_dir, config$paths$output_metadata_file)
  qc_file <- file.path(out_dir, config$paths$output_qc_file)

  readr::write_csv(indicators_merged, indicators_file)
  readr::write_csv(metadata_merged, metadata_file)
  jsonlite::write_json(qc_report, qc_file, pretty = TRUE, auto_unbox = TRUE, na = "null")

  list(
    indicators_file = indicators_file,
    metadata_file = metadata_file,
    qc_file = qc_file
  )
}

build_global_indicator_assets <- function(base_path = ".", config = MERGE_BUILD_CONFIG) {
  cat("[Global Build] Loading socio-economic indicators...\n")
  se_base <- load_country_indicator_files(base_path, config)
  cat(sprintf("[Global Build] Loaded socio-economic rows: %d\n", nrow(se_base)))

  cat("[Global Build] Loading socio-economic metadata...\n")
  se_metadata <- load_country_metadata_files(base_path, config)
  cat(sprintf("[Global Build] Loaded metadata rows: %d\n", nrow(se_metadata)))

  cat("[Global Build] Loading remote sensing latest values...\n")
  rs_latest <- load_remote_sensing_latest(base_path, config)
  cat(sprintf("[Global Build] Loaded remote sensing rows: %d\n", nrow(rs_latest)))

  cat("[Global Build] Loading ADM1 population file...\n")
  population <- load_population_admin1(base_path, config)
  cat(sprintf("[Global Build] Loaded population rows: %d\n", nrow(population)))

  adm_lookup <- se_base |>
    dplyr::select(country, adm1_pcode, adm1_name) |>
    dplyr::distinct()

  conflict_out <- build_conflict_cross_section(adm_lookup, base_path, config)
  cat(sprintf("[Global Build] Conflict rows after aggregation: %d\n", nrow(conflict_out$data)))

  indicators_merged <- merge_all_indicator_domains(
    se_base = dplyr::left_join(se_base, population, by = c("country", "adm1_pcode")),
    rs_latest = rs_latest,
    conflict_data = conflict_out$data,
    config = config
  )

  metadata_merged <- merge_all_metadata(
    se_metadata = se_metadata,
    se_base = se_base,
    rs_latest = rs_latest,
    config = config
  )

  indicators_merged <- apply_sepi_id_columns(indicators_merged, metadata_merged)
  cat(sprintf("[Global Build] Final indicator rows: %d | columns: %d\n", nrow(indicators_merged), ncol(indicators_merged)))
  cat(sprintf("[Global Build] Final metadata rows: %d\n", nrow(metadata_merged)))

  qc_report <- run_merge_qc_checks(
    indicators_merged = indicators_merged,
    metadata_merged = metadata_merged,
    conflict_unmatched = conflict_out$unmatched,
    config = config
  )

  files <- write_global_outputs(
    indicators_merged = indicators_merged,
    metadata_merged = metadata_merged,
    qc_report = qc_report,
    base_path = base_path,
    config = config
  )
  cat("[Global Build] Outputs written:\n")
  cat(sprintf("  - %s\n", files$indicators_file))
  cat(sprintf("  - %s\n", files$metadata_file))
  cat(sprintf("  - %s\n", files$qc_file))

  list(
    indicators_merged = indicators_merged,
    metadata_merged = metadata_merged,
    qc_report = qc_report,
    files = files
  )
}
