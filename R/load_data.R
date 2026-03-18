# ============================================================================
# Data Loading and Cleaning
# ============================================================================
# Reads from the pre-built merged CSV (GLOBAL_DATA$data_file) which contains
# all countries in a single file, then splits by country and applies
# per-country preprocessing (exclude_regions) from the version config.
# ============================================================================

load_country_data <- function(country,
                              global_data    = GLOBAL_DATA,
                              country_config = NULL,
                              .merged_df     = NULL) {

  if (is.null(.merged_df)) {
    .merged_df <- readr::read_csv(global_data$data_file, show_col_types = FALSE)
  }

  data <- .merged_df |>
    dplyr::filter(country == !!country) |>
    dplyr::select(-dplyr::any_of(c("country", "country_code")))

  # Standardise admin name column: merged file uses adm1_na
  if ("adm1_na" %in% names(data) && !"adm1_name" %in% names(data)) {
    data <- dplyr::rename(data, adm1_name = adm1_na)
  }

  # Replace "+" with "plus" in column names (e.g. pop_frac_3+ → pop_frac_3plus)
  names(data) <- stringr::str_replace_all(names(data), "\\+", "plus")

  # --- Country-specific preprocessing ---

  if (country == "south_sudan") {
    if (all(c("attendance_total", "population_total") %in% names(data))) {
      data <- dplyr::mutate(
        data,
        enrollment_rate = attendance_total / population_total * 100
      )
    }
  }

  # Exclude regions with insufficient data (configured per country)
  if (!is.null(country_config) && length(country_config$exclude_regions) > 0) {
    data <- dplyr::filter(data, !adm1_pcode %in% country_config$exclude_regions)
  }

  data
}

load_all_data <- function(global_data = GLOBAL_DATA, version = NULL) {
  merged_df <- readr::read_csv(global_data$data_file, show_col_types = FALSE)

  countries <- if (!is.null(version)) {
    names(version$countries)
  } else {
    unique(merged_df$country)
  }

  purrr::map(
    rlang::set_names(countries),
    function(country) {
      country_config <- if (!is.null(version)) version$countries[[country]] else NULL
      load_country_data(country, global_data, country_config, .merged_df = merged_df)
    }
  )
}
