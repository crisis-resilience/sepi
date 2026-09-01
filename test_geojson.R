# ============================================================================
# test_geojson.R — Validate generated GeoJSONs against reference files
# ============================================================================
# Prerequisites:
#   1. Run 03_run_sepi.R (or run_all.R) to generate outputs/geojson/
#   2. Reference GeoJSONs are already in data/gis/:
#        sepi_with_pillars_June_09_Kenya.geojson
#        sepi_with_pillars_June_09_Somalia.geojson
#        sepi_with_pillars_June_09_South_Sudan.geojson
# ============================================================================

for (pkg in c("sf", "dplyr")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(sf)
library(dplyr)

references <- c(
  kenya       = "data/gis/sepi_with_pillars_June_09_Kenya.geojson",
  somalia     = "data/gis/sepi_with_pillars_June_09_Somalia.geojson",
  south_sudan = "data/gis/sepi_with_pillars_June_09_South_Sudan.geojson"
)

geojson_dir <- "outputs/geojson"
tol         <- 1e-3

# ── Helpers ───────────────────────────────────────────────────────────────────

find_generated <- function(country_name, dir = geojson_dir) {
  pattern <- paste0("^", country_name, "_.*\\.geojson$")
  files   <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return(NULL)
  files[order(file.info(files)$mtime, decreasing = TRUE)][1]
}

pass_n <- 0L
fail_n <- 0L
warn_n <- 0L

report_ok <- function(msg) {
  cat("  ✓", msg, "\n")
  pass_n <<- pass_n + 1L
}
report_fail <- function(msg) {
  cat("  ✗", msg, "\n")
  fail_n <<- fail_n + 1L
}
report_warn <- function(msg) {
  cat("  !", msg, "\n")
  warn_n <<- warn_n + 1L
}

compare_cols <- function(ref_aligned, gen_df, cols, tolerance = tol) {
  mismatches <- character(0)
  for (col in cols) {
    r        <- suppressWarnings(as.numeric(ref_aligned[[col]]))
    g        <- suppressWarnings(as.numeric(gen_df[[col]]))
    both_na  <- is.na(r) & is.na(g)
    if (all(both_na)) next
    if (any(abs(r - g) > tolerance, na.rm = TRUE)) {
      mismatches <- c(mismatches, col)
    }
  }
  mismatches
}

# ── Main loop ─────────────────────────────────────────────────────────────────

for (country_name in names(references)) {

  ref_path <- references[[country_name]]
  gen_path <- find_generated(country_name)

  cat("\n", strrep("─", 60), "\n", sep = "")
  cat(toupper(gsub("_", " ", country_name)), "\n")
  cat(strrep("─", 60), "\n", sep = "")

  if (!file.exists(ref_path)) {
    report_warn(paste("Reference file not found:", ref_path))
    next
  }
  if (is.null(gen_path) || !file.exists(gen_path)) {
    report_fail(paste(
      "Generated GeoJSON not found in", geojson_dir,
      "— run 03_run_sepi.R first"
    ))
    next
  }

  cat("  Reference:", ref_path, "\n")
  cat("  Generated:", gen_path, "\n\n")

  ref <- tryCatch(sf::st_read(ref_path, quiet = TRUE), error = function(e) NULL)
  gen <- tryCatch(sf::st_read(gen_path, quiet = TRUE), error = function(e) NULL)

  if (is.null(ref)) {
    report_fail("Cannot read reference GeoJSON")
    next
  }
  if (is.null(gen)) {
    report_fail("Cannot read generated GeoJSON")
    next
  }

  ref_df        <- sf::st_drop_geometry(ref)
  gen_df        <- sf::st_drop_geometry(gen)
  ref_pcode_col <- if ("adm1_pcode" %in% names(ref_df)) "adm1_pcode" else "ADM1_PCODE"

  # 1. Feature count ──────────────────────────────────────────────────────────
  if (nrow(ref) == nrow(gen)) {
    report_ok(sprintf("Feature count: %d", nrow(gen)))
  } else {
    # Extra pcodes in generated that are absent from the (older) reference are
    # not a data error — they are valid regions the old QGIS workflow excluded.
    # Missing pcodes (in ref but not generated) ARE a failure.
    extra_pcodes   <- setdiff(
      as.character(gen_df[["adm1_pcode"]]),
      as.character(ref_df[[ref_pcode_col]])
    )
    missing_pcodes <- setdiff(
      as.character(ref_df[[ref_pcode_col]]),
      as.character(gen_df[["adm1_pcode"]])
    )
    if (length(missing_pcodes) > 0) {
      report_fail(sprintf(
        "Generated is missing pcodes present in reference: %s",
        paste(missing_pcodes, collapse = ", ")
      ))
    } else if (length(extra_pcodes) > 0) {
      report_warn(sprintf(
        "Feature count: ref=%d, gen=%d — generated includes extra: %s",
        nrow(ref), nrow(gen), paste(extra_pcodes, collapse = ", ")
      ))
    }
  }

  # 2. CRS ────────────────────────────────────────────────────────────────────
  ref_epsg <- sf::st_crs(ref)$epsg
  gen_epsg <- sf::st_crs(gen)$epsg
  if (isTRUE(sf::st_crs(ref) == sf::st_crs(gen))) {
    report_ok(paste("CRS matches:", if (is.null(ref_epsg)) "WGS84" else ref_epsg))
  } else {
    report_warn(sprintf(
      "CRS differs — reference: EPSG:%s, generated: EPSG:%s",
      if (is.null(ref_epsg)) "?" else ref_epsg,
      if (is.null(gen_epsg)) "?" else gen_epsg
    ))
  }

  # 3. PCodes ─────────────────────────────────────────────────────────────────
  ref_pcodes      <- as.character(ref_df[[ref_pcode_col]])
  gen_pcodes      <- as.character(gen_df[["adm1_pcode"]])
  missing_from_gen <- setdiff(ref_pcodes, gen_pcodes)
  extra_in_gen     <- setdiff(gen_pcodes, ref_pcodes)

  if (length(missing_from_gen) == 0 && length(extra_in_gen) == 0) {
    report_ok(sprintf("PCodes: all %d present", length(ref_pcodes)))
  } else {
    if (length(missing_from_gen) > 0) {
      report_fail(sprintf(
        "PCodes in reference but missing from generated: %s",
        paste(missing_from_gen, collapse = ", ")
      ))
    }
    if (length(extra_in_gen) > 0) {
      report_warn(sprintf(
        "Extra PCodes in generated (not in reference): %s",
        paste(extra_in_gen, collapse = ", ")
      ))
    }
  }

  # Align rows by pcode for value comparisons (extras in gen get NA in ref)
  ref_df$pcode_key <- as.character(ref_df[[ref_pcode_col]])
  gen_df$pcode_key <- as.character(gen_df[["adm1_pcode"]])
  ref_aligned      <- ref_df[match(gen_df$pcode_key, ref_df$pcode_key), ]

  # 4. Column inventory ───────────────────────────────────────────────────────
  meta_cols   <- c(ref_pcode_col, "adm1_pcode", "pcode_key")
  ref_cols    <- setdiff(names(ref_df), meta_cols)
  gen_cols    <- setdiff(names(gen_df), meta_cols)
  common_cols <- intersect(ref_cols, gen_cols)
  ref_only    <- setdiff(ref_cols, gen_cols)
  gen_only    <- setdiff(gen_cols, ref_cols)

  cat(sprintf(
    "  Columns — common: %d  |  ref-only: %d  |  gen-only: %d\n",
    length(common_cols), length(ref_only), length(gen_only)
  ))
  if (length(ref_only) > 0) {
    cat("  Ref-only (QGIS artifacts / old aggregates):",
        paste(sort(ref_only), collapse = ", "), "\n")
  }
  if (length(gen_only) > 0) {
    cat("  Gen-only (new indicators added since reference):",
        paste(sort(gen_only), collapse = ", "), "\n")
  }

  # 5. Value comparison by column group ───────────────────────────────────────
  cat("\n  Value checks (tolerance:", tol, "):\n")

  check_group <- function(cols, label) {
    cols <- intersect(cols, common_cols)
    if (length(cols) == 0) return()
    mm <- compare_cols(ref_aligned, gen_df, cols)
    if (length(mm) == 0) {
      report_ok(sprintf("%-35s (%d cols): all match", label, length(cols)))
    } else {
      report_fail(sprintf(
        "%-35s %d/%d cols mismatch: %s",
        label, length(mm), length(cols), paste(mm, collapse = ", ")
      ))
    }
  }

  sepi_key        <- c("sepi", "n_pillars", "sepi_rank",
                       grep("^pillar_", common_cols, value = TRUE))
  conflict_counts <- grep(
    "^(total_fatalities|count_conflict_events)_\\d{4}$",
    common_cols, value = TRUE
  )
  conflict_rates  <- grep("_per_1k_\\d{4}$", common_cols, value = TRUE)
  norm_cols       <- grep("_norm$", common_cols, value = TRUE)
  other_cols      <- setdiff(
    common_cols,
    c(sepi_key, conflict_counts, conflict_rates, norm_cols)
  )

  check_group(sepi_key,        "SEPI scores & ranks")
  check_group(conflict_counts, "Conflict yearly counts")
  check_group(conflict_rates,  "Conflict per-1k rates")
  check_group(norm_cols,       "Normalised indicators (_norm)")
  check_group(other_cols,      "Other common columns")

  # 6. Geometry bounding-box spot-check ───────────────────────────────────────
  ref_bb  <- as.numeric(sf::st_bbox(ref))
  gen_bb  <- as.numeric(sf::st_bbox(gen))
  bb_diff <- max(abs(ref_bb - gen_bb))
  if (bb_diff < 0.01) {
    report_ok(sprintf(
      "Geometry bounding box matches (max diff: %.6f deg)", bb_diff
    ))
  } else {
    report_warn(sprintf(
      "Geometry bounding box differs by up to %.4f deg", bb_diff
    ))
  }
}

# ── Summary ───────────────────────────────────────────────────────────────────
cat("\n", strrep("═", 60), "\n", sep = "")
cat(sprintf(
  "SUMMARY  %d passed  |  %d warnings  |  %d failed\n",
  pass_n, warn_n, fail_n
))
cat(strrep("═", 60), "\n", sep = "")

if (fail_n > 0) {
  message("\nSome checks FAILED. See details above.")
} else if (warn_n > 0) {
  cat("All value checks passed (non-critical warnings above).\n")
} else {
  cat("All checks passed.\n")
}
