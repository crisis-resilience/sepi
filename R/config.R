# ============================================================================
# SEPI Configuration
# ============================================================================
#
# Defines:
#   1. GLOBAL_DATA        - paths to merged 3-country CSVs
#   2. GIS_CONFIG         - country-specific ADM1/ADM0 shapefile paths
#   3. VERSIONS           - all SEPI versions, loaded from versions/*.json
#   4. MERGE_BUILD_CONFIG - paths and parameters for building global merged files
#
# To add a new SEPI version: create versions/<name>.json.
# It will appear automatically in VERSIONS$<name> without any R code changes.
# ============================================================================


# ============================================================================
# Global Data Paths
# ============================================================================
# The pipeline reads from pre-built merged CSV files (produced by
# 01_build_data.R).  All country data lives in a single file.

GLOBAL_DATA <- list(
  data_file     = "data/sepi_indicators_all_countries_latest.csv",
  metadata_file = "data/sepi_indicators_metadata_all_countries.csv"
)

# ============================================================================
# GIS Configuration
# ============================================================================
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
# Version System — loaded from versions/*.json
# ============================================================================
# Each JSON file is self-contained: methodology params + full country
# indicator definitions.  Methodology fields are flattened to the top level
# for backward compatibility with the computation engine.

print.sepi_version <- function(x, ...) {
  cat("SEPI Version:", x$name, "\n")
  cat("  Normalisation:     ", x$normalisation, "\n")
  cat("  Weighting:         ", x$weighting, "\n")
  if (!is.null(x$pillar_weights)) {
    cat("  Pillar weights:    ",
        paste(names(x$pillar_weights), "=",
              round(x$pillar_weights, 3), collapse = ", "), "\n")
  }
  cat("  Within-pillar agg: ", x$within_pillar_agg, "\n")
  cat("  Across-pillar agg: ", x$across_pillar_agg, "\n")
  if (isTRUE(x$conflict_weighting)) {
    cat("  Conflict weighting: TRUE (flat weighted sum using |r| with conflict)\n")
  }
  invisible(x)
}

VERSIONS <- local({
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required to load SEPI versions. ",
         "Install with: install.packages('jsonlite')")
  }

  files <- list.files("versions", pattern = "\\.json$", full.names = TRUE)
  if (length(files) == 0) {
    warning("No version JSON files found in versions/")
    return(list())
  }

  vs <- lapply(files, function(f) {
    raw <- jsonlite::read_json(f, simplifyVector = TRUE)

    # Flatten methodology fields to top level for compatibility with compute_index.R
    v <- c(
      list(
        name          = raw$name,
        description   = raw$description,
        pillar_weights = NULL   # only used by "custom" weighting (not in any current version)
      ),
      raw$methodology,
      list(countries = raw$countries)
    )

    # Ensure exclude_regions is always a character vector (JSON [] can become list())
    for (cname in names(v$countries)) {
      er <- v$countries[[cname]]$exclude_regions
      if (is.null(er) || length(er) == 0) {
        v$countries[[cname]]$exclude_regions <- character(0)
      }
    }

    class(v) <- "sepi_version"
    v
  })

  names(vs) <- vapply(vs, `[[`, character(1), "name")
  vs
})


# ============================================================================
# Global Merge Build Configuration
# ============================================================================

MERGE_BUILD_CONFIG <- list(
  paths = list(
    indicator_files = list(
      kenya       = "data/socio-economic/KENYA_SEPI_Indicators.csv",
      somalia     = "data/socio-economic/SOMALIA_SEPI_Indicators.csv",
      south_sudan = "data/socio-economic/SSD_SEPI_Indicators.csv"
    ),
    metadata_files = list(
      kenya       = "data/socio-economic/KENYA_SEPI_Indicator_Metadata.csv",
      somalia     = "data/socio-economic/SOMALIA_SEPI_Indicator_Metadata.csv",
      south_sudan = "data/socio-economic/SSD_SEPI_Indicator_Metadata.csv"
    ),
    remote_sensing_file  = "data/remote_sensing/east_africa_remote_sensing.xlsx",
    remote_sensing_sheet = "in",
    population_file      = "data/socio-economic/population_admin1_all_countries.csv",
    env_file             = ".env",
    output_dir           = "data",
    output_indicators_file = "sepi_indicators_all_countries_latest.csv",
    output_metadata_file   = "sepi_indicators_metadata_all_countries.csv",
    output_qc_file         = "sepi_merge_qc_report.json"
  ),
  keys = c("country", "country_code", "adm1_pcode", "adm1_na"),
  conflict = list(
    start_date    = "2025-01-01",
    end_date      = "2025-12-31",
    api_limit     = 5000,
    api_max_pages = 100,
    event_types   = c(
      "Battles",
      "Explosions/Remote violence",
      "Violence against civilians"
    )
  ),
  country_mapping = list(
    kenya = list(
      country_code            = "KEN",
      acled_name              = "Kenya",
      remote_sensing_name     = "Kenya",
      acled_admin1_overrides  = c()
    ),
    somalia = list(
      country_code            = "SOM",
      acled_name              = "Somalia",
      remote_sensing_name     = "Somalia",
      acled_admin1_overrides  = c()
    ),
    south_sudan = list(
      country_code            = "SSD",
      acled_name              = "South Sudan",
      remote_sensing_name     = "SSD",
      acled_admin1_overrides  = c()
    )
  )
)
