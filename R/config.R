# ============================================================================
# SEPI Configuration
# ============================================================================
#
# Defines:
#   1. INDICATOR_CONFIG   - country-specific indicator-to-pillar mappings
#   2. GIS_CONFIG         - country-specific ADM1/ADM0 shapefile paths
#   3. create_version()   - factory for SEPI methodology variants
#   5. VERSIONS           - pre-defined version catalogue
#
# To test a new weighting scheme or aggregation method, add an entry to
# VERSIONS (or call create_version() directly).  The computation engine in
# compute_index.R is fully parameterised by the version object.
# ============================================================================


# ============================================================================
# Global Data Paths
# ============================================================================
# The pipeline reads from pre-built merged CSV files (produced by
# build_global_data.R).  All country data lives in a single file.

GLOBAL_DATA <- list(
  data_file     = "data/sepi_indicators_all_countries_latest.csv",
  metadata_file = "data/sepi_indicators_metadata_all_countries.csv"
)

# ============================================================================
# Country Configurations
# ============================================================================

# ---- Indicator Definitions -------------------------------------------------
# polarity: +1 = higher value is better socio-economic outcome
#           -1 = higher value is worse socio-economic outcome

INDICATOR_CONFIG <- list(

  kenya = list(
    id_cols        = c("adm1_pcode", "adm1_name"),
    population_col = "population_total",
    exclude_regions = character(0),

    pillars = list(
      education = list(
        indicators = c("primary_school_net_attendance_total",
                        "net_attendance_total"),
        polarity   = c(1, 1),
        labels     = c("Primary net attendance (%)",
                        "Secondary net attendance (%)")
      ),
      health = list(
        indicators = c("health_fac_per_10k_pop",
                        "hospitals_per_100k_pop"),
        polarity   = c(1, 1),
        labels     = c("Health facilities per 10k pop",
                        "Hospitals per 100k pop")
      ),
      food_security = list(
        indicators = c("nutrition"),
        polarity   = c(-1),
        labels     = c("Nutrition deprivation (%, MPI)")
      ),
      economic = list(
        indicators = c("poverty_headcount_pct", "gcp_pc"),
        polarity   = c(-1, 1),
        labels     = c("Poverty headcount (%)",
                        "Gross county product per capita (KSh)")
      ),
      climate = list(
        indicators = c("rs_ndvi"),
        polarity   = c(1),
        labels     = c("NDVI (remote sensing)")
      )
    ),

    conflict = list(
      indicators = c("total_fatalities",
                      "count_conflict_events",
                      "total_fatalities_per_1k",
                      "count_conflicts_events_per_1k"),
      labels     = c("Total fatalities",
                      "Total conflict events",
                      "Fatalities per 1k pop",
                      "Conflict events per 1k pop")
    )
  ),

  somalia = list(
    id_cols        = c("adm1_pcode", "adm1_name"),
    population_col = "population_total",
    exclude_regions = character(0),

    pillars = list(
      education = list(
        indicators = c("literacy_percent_total",
                        "primary_school_net_attendance_total"),
        polarity   = c(1, 1),
        labels     = c("Literacy rate (%)",
                        "Primary net attendance (%)")
      ),
      health = list(
        indicators = c("health_fac_per_10k_pop",
                        "hospitals_per_100k_pop"),
        polarity   = c(1, 1),
        labels     = c("Health facilities per 10k pop",
                        "Hospitals per 100k pop")
      ),
      food_security = list(
        indicators = c("pop_frac_3plus"),
        polarity   = c(-1),
        labels     = c("Population in IPC Phase 3+ (proportion)")
      ),
      economic = list(
        indicators = c("poverty_headcount_pct",
                        "total_expenditure_usd"),
        polarity   = c(-1, 1),
        labels     = c("Poverty headcount (%)",
                        "Household expenditure (USD)")
      ),
      climate = list(
        indicators = c("rs_ndvi"),
        polarity   = c(1),
        labels     = c("NDVI (remote sensing)")
      )
    ),

    conflict = list(
      indicators = c("total_fatalities",
                      "count_conflict_events",
                      "total_fatalities_per_1k",
                      "count_conflicts_events_per_1k"),
      labels     = c("Total fatalities",
                      "Total conflict events",
                      "Fatalities per 1k pop",
                      "Conflict events per 1k pop")
    )
  ),

  south_sudan = list(
    id_cols        = c("adm1_pcode", "adm1_name"),
    population_col = "population_total",
    exclude_regions = c("SS00"),

    pillars = list(
      education = list(
        indicators = c("enrollment_rate", "dropout_pct"),
        polarity   = c(1, -1),
        labels     = c("Enrollment rate (%, derived)",
                        "Dropout rate (%)")
      ),
      health = list(
        indicators = c("health_fac_per_10k_pop",
                        "hospitals_per_100k_pop"),
        polarity   = c(1, 1),
        labels     = c("Health facilities per 10k pop",
                        "Hospitals per 100k pop")
      ),
      food_security = list(
        indicators = c("pop_frac_3plus"),
        polarity   = c(-1),
        labels     = c("Population in IPC Phase 3+ (proportion)")
      ),
      economic = list(
        indicators = c("poverty_headcount_pct",
                        "daily_non_q_labour_mean_wage_USD"),
        polarity   = c(-1, 1),
        labels     = c("Poverty headcount (%)",
                        "Daily non-qual. wage (USD)")
      ),
      climate = list(
        indicators = c("rs_ndvi"),
        polarity   = c(1),
        labels     = c("NDVI (remote sensing)")
      )
    ),

    conflict = list(
      indicators = c("total_fatalities",
                      "count_conflict_events",
                      "total_fatalities_per_1k",
                      "count_conflicts_events_per_1k"),
      labels     = c("Total fatalities",
                      "Total conflict events",
                      "Fatalities per 1k pop",
                      "Conflict events per 1k pop")
    )
  )
)

# ---- GIS Definitions -------------------------------------------------------
# Maps each country to its ADM1/ADM0 shapefile paths and the column names
# used for joining with SEPI results (adm1_pcode / adm1_name).

GIS_CONFIG <- list(

  kenya = list(
    adm1_shp  = "data/gis/ken_adm_ocha_20250108_ab_shp/ken_admin1.shp",
    adm0_shp  = "data/gis/ken_adm_ocha_20250108_ab_shp/ken_admin0.shp",
    pcode_col = "adm1_pcode",
    name_col  = "adm1_name"
  ),

  somalia = list(
    adm1_shp  = "data/gis/som_adm_ocha_20250108_ab_shp/som_admin1.shp",
    adm0_shp  = "data/gis/som_adm_ocha_20250108_ab_shp/som_admin0.shp",
    pcode_col = "adm1_pcode",
    name_col  = "adm1_name"
  ),

  south_sudan = list(
    adm1_shp  = "data/gis/ssd_admbnda_imwg_nbs_20230829_shp/ssd_admbnda_adm1_imwg_nbs_20230829.shp",
    adm0_shp  = "data/gis/ssd_admbnda_imwg_nbs_20230829_shp/ssd_admbnda_adm0_imwg_nbs_20230829.shp",
    pcode_col = "ADM1_PCODE",
    name_col  = "ADM1_EN"
  )
)

# ============================================================================
# Version System
# ============================================================================

# ---- Version Factory -------------------------------------------------------

#' Create a SEPI version specification
#'
#' @param name              Character label for this version
#' @param weighting         "equal" (default) or "custom"
#' @param pillar_weights    Named numeric vector (required when weighting = "custom")
#' @param within_pillar_agg How to aggregate indicators within a pillar
#' @param across_pillar_agg How to aggregate pillar scores into SEPI
#' @param normalisation     Normalisation method for raw indicators
#' @param geometric_floor   Floor value to avoid zero-products in geometric mean
create_version <- function(
    name,
    weighting         = c("equal", "custom"),
    pillar_weights    = NULL,
    within_pillar_agg = c("arithmetic", "geometric", "pca"),
    across_pillar_agg = c("geometric", "arithmetic"),
    normalisation     = c("min_max", "z_score", "rank"),
    geometric_floor   = 0.001
) {
  weighting         <- match.arg(weighting)
  within_pillar_agg <- match.arg(within_pillar_agg)
  across_pillar_agg <- match.arg(across_pillar_agg)
  normalisation     <- match.arg(normalisation)

  if (weighting == "custom" && is.null(pillar_weights)) {
    stop("Custom weighting requires 'pillar_weights': a named numeric vector.")
  }

  if (weighting == "custom") {
    total <- sum(pillar_weights)
    if (abs(total - 1) > 1e-6) {
      message("Pillar weights sum to ", round(total, 4), "; rescaling to 1.")
      pillar_weights <- pillar_weights / total
    }
  }

  structure(
    list(
      name              = name,
      weighting         = weighting,
      pillar_weights    = pillar_weights,
      within_pillar_agg = within_pillar_agg,
      across_pillar_agg = across_pillar_agg,
      normalisation     = normalisation,
      geometric_floor   = geometric_floor
    ),
    class = "sepi_version"
  )
}

print.sepi_version <- function(x, ...) {
  cat("SEPI Version:", x$name, "\n")
  cat("  Normalisation:     ", x$normalisation, "\n")
  cat("  Weighting:         ", x$weighting, "\n")
  if (x$weighting == "custom") {
    cat("  Pillar weights:    ",
        paste(names(x$pillar_weights), "=",
              round(x$pillar_weights, 3), collapse = ", "), "\n")
  }
  cat("  Within-pillar agg: ", x$within_pillar_agg, "\n")
  cat("  Across-pillar agg: ", x$across_pillar_agg, "\n")
  invisible(x)
}

# ---- Pre-defined Versions --------------------------------------------------

VERSIONS <- list(

  # V1: Baseline — equal weights, arithmetic within pillars, geometric across
  v1_equal_geometric = create_version(
    name              = "v1_equal_geometric",
    weighting         = "equal",
    within_pillar_agg = "arithmetic",
    across_pillar_agg = "geometric",
    normalisation     = "min_max"
  ),

  # V2: PCA within pillars — PC1 loadings weight indicators; equal pillar weights
  # Single-indicator pillars use their indicator directly (PCA skipped).
  v2_pca_geometric = create_version(
    name              = "v2_pca_geometric",
    weighting         = "equal",
    within_pillar_agg = "pca",
    across_pillar_agg = "geometric",
    normalisation     = "min_max"
  )
)

# Examples for future versions (uncomment / extend as needed):
#
# VERSIONS$v3_equal_arithmetic <- create_version(
#   name              = "v3_equal_arithmetic",
#   weighting         = "equal",
#   across_pillar_agg = "arithmetic"
# )
#
# VERSIONS$v4_custom_weights <- create_version(
#   name           = "v4_custom_weights",
#   weighting      = "custom",
#   pillar_weights = c(education = 0.25, health = 0.20,
#                      food_security = 0.25, economic = 0.20, climate = 0.10)
# )


# ============================================================================
# Global Merge Build Configuration
# ============================================================================

MERGE_BUILD_CONFIG <- list(
  paths = list(
    indicator_files = list(
      kenya = "data/socio-economic/Kenya/for_global_SEPI/KENYA_SEPI_Indicators.csv",
      somalia = "data/socio-economic/Somalia/for_global_SEPI/SOMALIA_SEPI_Indicators.csv",
      south_sudan = "data/socio-economic/SSD/for_global_SEPI/SSD_SEPI_Indicators.csv"
    ),
    metadata_files = list(
      kenya = "data/socio-economic/Kenya/for_global_SEPI/KENYA_SEPI_Indicator_Metadata.csv",
      somalia = "data/socio-economic/Somalia/for_global_SEPI/SOMALIA_SEPI_Indicator_Metadata.csv",
      south_sudan = "data/socio-economic/SSD/for_global_SEPI/SSD_SEPI_Indicator_Metadata.csv"
    ),
    remote_sensing_file = "data/remote_sensing/east_africa_remote_sensing.xlsx",
    remote_sensing_sheet = "in",
    population_file = "data/socio-economic/population_admin1_all_countries.csv",
    env_file = ".env",
    output_dir = "data",
    output_indicators_file = "sepi_indicators_all_countries_latest.csv",
    output_metadata_file = "sepi_indicators_metadata_all_countries.csv",
    output_qc_file = "sepi_merge_qc_report.json"
  ),
  keys = c("country", "country_code", "adm1_pcode", "adm1_na"),
  conflict = list(
    start_date = "2025-01-01",
    end_date = "2025-12-31",
    api_limit = 5000,
    api_max_pages = 100,
    event_types = c(
      "Battles",
      "Explosions/Remote violence",
      "Violence against civilians"
    )
  ),
  country_mapping = list(
    kenya = list(
      country_code = "KEN",
      acled_name = "Kenya",
      remote_sensing_name = "Kenya",
      acled_admin1_overrides = c()
    ),
    somalia = list(
      country_code = "SOM",
      acled_name = "Somalia",
      remote_sensing_name = "Somalia",
      acled_admin1_overrides = c()
    ),
    south_sudan = list(
      country_code = "SSD",
      acled_name = "South Sudan",
      remote_sensing_name = "SSD",
      acled_admin1_overrides = c()
    )
  )
)
