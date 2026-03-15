# ============================================================================
# Stage 2: SEPI–Conflict Linkage Analysis
# ============================================================================
# Analyses the statistical relationship between SEPI scores / pillar scores
# and ACLED conflict indicators.
# ============================================================================

# ---- Prepare conflict metrics ----------------------------------------------

prepare_conflict <- function(data, country_config) {
  conflict_cols <- country_config$conflict$indicators
  conflict_cols <- conflict_cols[conflict_cols %in% names(data)]

  pop <- data[[country_config$population_col]]

  # Per-capita conflict rates (per 100k population)
  for (col in conflict_cols) {
    pc_col <- paste0(col, "_per_100k")
    data[[pc_col]] <- ifelse(
      !is.na(pop) & pop > 0,
      data[[col]] / pop * 1e5,
      NA_real_
    )
  }

  data
}

# ---- Correlation analysis --------------------------------------------------

sepi_conflict_correlation <- function(sepi_result, country_config) {
  pillar_cols  <- grep("^pillar_", names(sepi_result), value = TRUE)
  sepi_cols    <- c("sepi", pillar_cols)

  conflict_raw <- country_config$conflict$indicators
  conflict_raw <- conflict_raw[conflict_raw %in% names(sepi_result)]
  conflict_pc  <- paste0(conflict_raw, "_per_100k")
  conflict_pc  <- conflict_pc[conflict_pc %in% names(sepi_result)]

  all_cols <- c(sepi_cols, conflict_raw, conflict_pc)
  all_cols <- all_cols[all_cols %in% names(sepi_result)]

  mat <- sepi_result[, all_cols, drop = FALSE]
  mat <- dplyr::select(mat, dplyr::where(is.numeric))

  cor_mat <- stats::cor(mat, use = "pairwise.complete.obs", method = "spearman")
  cor_mat
}

# ---- Focused SEPI vs conflict table ----------------------------------------

sepi_conflict_summary <- function(sepi_result, country_config) {
  conflict_raw <- country_config$conflict$indicators
  conflict_raw <- conflict_raw[conflict_raw %in% names(sepi_result)]
  conflict_pc  <- paste0(conflict_raw, "_per_100k")
  conflict_pc  <- conflict_pc[conflict_pc %in% names(sepi_result)]
  conflict_all <- c(conflict_raw, conflict_pc)

  pillar_cols <- grep("^pillar_", names(sepi_result), value = TRUE)
  sepi_cols   <- c("sepi", pillar_cols)

  results <- tidyr::expand_grid(
    sepi_var     = sepi_cols,
    conflict_var = conflict_all
  )

  results$rho <- purrr::map2_dbl(
    results$sepi_var, results$conflict_var,
    function(sv, cv) {
      x <- sepi_result[[sv]]
      y <- sepi_result[[cv]]
      if (sum(complete.cases(x, y)) < 4) return(NA_real_)
      stats::cor(x, y, use = "complete.obs", method = "spearman")
    }
  )

  results$p_value <- purrr::map2_dbl(
    results$sepi_var, results$conflict_var,
    function(sv, cv) {
      x <- sepi_result[[sv]]
      y <- sepi_result[[cv]]
      cc <- complete.cases(x, y)
      if (sum(cc) < 4) return(NA_real_)
      stats::cor.test(x[cc], y[cc], method = "spearman", exact = FALSE)$p.value
    }
  )

  results
}

# ---- Run conflict analysis for one country ---------------------------------

analyse_conflict <- function(sepi_result, country_config, country_name = NULL) {
  label <- if (!is.null(country_name)) country_label(country_name) else "Country"

  # Add per-capita conflict metrics
  sepi_result <- prepare_conflict(sepi_result, country_config)

  # Correlation summary
  cor_summary <- sepi_conflict_summary(sepi_result, country_config)

  cat("\n========================================\n")
  cat(" SEPI–Conflict Correlations:", label, "\n")
  cat("========================================\n\n")

  # Show only SEPI (not individual pillars) against key conflict indicators
  key <- dplyr::filter(cor_summary, sepi_var == "sepi")
  print(as.data.frame(key), row.names = FALSE)
  cat("\n")

  list(
    data        = sepi_result,
    full_matrix = sepi_conflict_correlation(sepi_result, country_config),
    summary     = cor_summary
  )
}

# ---- Wrapper for all countries ---------------------------------------------

analyse_conflict_all <- function(sepi_results, config = INDICATOR_CONFIG) {
  purrr::imap(sepi_results, function(result, country) {
    analyse_conflict(result, config[[country]], country)
  })
}
