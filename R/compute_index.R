# ============================================================================
# SEPI Computation Engine
# ============================================================================
# Core function: compute_sepi()
#   - Normalises indicators (polarity-aware)
#   - Computes pillar scores (within-pillar aggregation)
#   - Aggregates pillars into a single SEPI score
#   - Returns the augmented data frame with all intermediate columns
#
# The entire pipeline is parameterised by a *version* object (see config.R).
#
# When version$within_pillar_agg == "pca":
#   - Pillars with >= 2 indicators: PC1 loadings become the within-pillar weights.
#   - Pillars with 1 indicator: the single normalised indicator is used directly.
# ============================================================================

# ---- PCA weight computation -------------------------------------------------

#' Derive within-pillar weights from the first principal component
#'
#' Runs PCA on the matrix of normalised (polarity-aligned) indicator columns
#' and returns absolute PC1 loadings rescaled to sum to 1.
#' Returns equal weights if PCA cannot be computed (too few observations,
#' constant columns, or fewer than 2 indicators).
#'
#' @param norm_mat  Numeric matrix — rows = regions, cols = normalised indicators
#' @return Named numeric vector of weights that sum to 1
compute_pca_weights <- function(norm_mat) {
  if (ncol(norm_mat) < 2) {
    # Single indicator — trivial weight
    return(stats::setNames(1, colnames(norm_mat)))
  }

  complete <- norm_mat[stats::complete.cases(norm_mat), , drop = FALSE]

  # Need at least 3 complete observations and 2 non-constant columns
  nonconst <- apply(complete, 2, function(x) stats::sd(x) > 1e-10)
  if (nrow(complete) < 3 || sum(nonconst) < 2) {
    # Fall back to equal weights
    n <- ncol(norm_mat)
    return(stats::setNames(rep(1 / n, n), colnames(norm_mat)))
  }

  pca <- tryCatch(
    stats::prcomp(complete[, nonconst, drop = FALSE],
                  center = TRUE, scale. = TRUE),
    error = function(e) NULL
  )

  if (is.null(pca)) {
    n <- ncol(norm_mat)
    return(stats::setNames(rep(1 / n, n), colnames(norm_mat)))
  }

  # Use absolute PC1 loadings for the non-constant columns
  loadings_nonconst <- abs(pca$rotation[, 1])

  # Build full weight vector (constant columns get weight 0)
  weights_full <- stats::setNames(rep(0, ncol(norm_mat)), colnames(norm_mat))
  weights_full[nonconst] <- loadings_nonconst

  # Rescale to sum to 1
  if (sum(weights_full) > 1e-10) {
    weights_full <- weights_full / sum(weights_full)
  } else {
    weights_full <- rep(1 / ncol(norm_mat), ncol(norm_mat))
    names(weights_full) <- colnames(norm_mat)
  }

  weights_full
}

# ---- V3 helpers: imputation and conflict weights ----------------------------

#' Impute missing values for v3 conflict-weighted computation
#'
#' @param data      Data frame
#' @param se_vars   Character vector of SE indicator column names
#' @param strategy  "mean" (pop_frac_3plus→0, others→column mean) or "omit" (na.omit)
#' @return Data frame with imputed values (or reduced rows if strategy="omit")
impute_missing_v3 <- function(data, se_vars, strategy = "mean") {
  if (strategy == "mean") {
    if ("pop_frac_3plus" %in% se_vars && "pop_frac_3plus" %in% names(data)) {
      data[["pop_frac_3plus"]][is.na(data[["pop_frac_3plus"]])] <- 0
    }
    for (v in se_vars) {
      if (v %in% names(data) && any(is.na(data[[v]]))) {
        data[[v]][is.na(data[[v]])] <- mean(data[[v]], na.rm = TRUE)
      }
    }
  } else {
    # "omit" — drop rows with any NA in se_vars
    avail <- se_vars[se_vars %in% names(data)]
    data <- data[stats::complete.cases(data[, avail, drop = FALSE]), ]
  }
  data
}

#' Compute signed conflict-correlation weights for v3
#'
#' @param data         Data frame (already imputed)
#' @param se_vars      Character vector of SE indicator column names (normalised)
#' @param conflict_col Name of the conflict column
#' @param bad_vars     Character vector of indicators where higher = worse
#' @return Named numeric vector of effective weights (sign × magnitude)
compute_conflict_weights <- function(data, se_vars, conflict_col, bad_vars) {
  conflict_vals <- data[[conflict_col]]

  cors <- vapply(se_vars, function(v) {
    xv <- data[[v]]
    if (stats::sd(xv, na.rm = TRUE) == 0 ||
        stats::sd(conflict_vals, na.rm = TRUE) == 0 ||
        sum(!is.na(xv) & !is.na(conflict_vals)) < 3) {
      return(NA_real_)
    }
    stats::cor(xv, conflict_vals, method = "pearson", use = "complete.obs")
  }, numeric(1))

  abs_cors <- abs(cors)
  total    <- sum(abs_cors, na.rm = TRUE)

  if (total == 0 || all(is.na(abs_cors))) {
    weight_mag <- stats::setNames(rep(1 / length(se_vars), length(se_vars)), se_vars)
  } else {
    weight_mag <- abs_cors / total
    weight_mag[is.na(weight_mag)] <- 0
    if (sum(weight_mag) > 0 && abs(sum(weight_mag) - 1) > 1e-6) {
      weight_mag <- weight_mag / sum(weight_mag)
    }
  }

  signs <- ifelse(se_vars %in% bad_vars, -1, 1)
  effective <- signs * weight_mag
  names(effective) <- se_vars
  effective
}

# ---- Main index function ----------------------------------------------------

compute_sepi <- function(data, version, country_name = NULL, country_config = NULL) {

  # Resolve country config: explicit override wins, otherwise look up from version
  cfg_resolved <- if (!is.null(country_config)) {
    country_config
  } else if (!is.null(country_name)) {
    version$countries[[country_name]]
  } else {
    stop("compute_sepi() requires either 'country_name' or 'country_config'.")
  }

  # ---- V3 conflict-weighted path --------------------------------------------
  if (isTRUE(version$conflict_weighting)) {
    cfg <- cfg_resolved
    if (is.null(cfg$se_vars) || is.null(cfg$conflict_col)) {
      stop("V3 conflict_weighting is TRUE but country config is missing ",
           "se_vars or conflict_col. Check versions/v3_conflict_weighted.json.")
    }

    se_vars      <- cfg$se_vars
    conflict_col <- cfg$conflict_col
    bad_vars     <- cfg$bad_vars
    pillar_map   <- cfg$pillar_map
    granular_vars <- cfg$granular_vars

    # 1. Impute
    data <- impute_missing_v3(data, c(se_vars, conflict_col), cfg$imputation)

    # 2. Min-max normalise SE vars (preserving original direction, no polarity flip)
    for (v in se_vars) {
      if (v %in% names(data)) {
        data[[paste0(v, "_norm")]] <- normalise_min_max(data[[v]])
      }
    }

    # 3. Compute conflict weights on normalised columns
    norm_cols <- paste0(se_vars, "_norm")
    norm_cols <- norm_cols[norm_cols %in% names(data)]

    # Build a temporary df with norm col names = se_var names for weight computation
    norm_data_for_weights <- data[, norm_cols, drop = FALSE]
    names(norm_data_for_weights) <- sub("_norm$", "", norm_cols)

    eff_weights <- compute_conflict_weights(
      cbind(norm_data_for_weights, data[, conflict_col, drop = FALSE]),
      se_vars[paste0(se_vars, "_norm") %in% names(data)],
      conflict_col,
      bad_vars
    )

    # 4. Compute sepi_raw = sum(norm_i * effective_weight_i)
    weight_cols <- paste0(names(eff_weights), "_norm")
    weight_cols <- weight_cols[weight_cols %in% names(data)]
    if (length(weight_cols) == 0) {
      stop("No normalised SE indicator columns found for v3 computation.")
    }
    norm_mat <- as.matrix(data[, weight_cols, drop = FALSE])
    # Match weight vector to the columns actually present
    matched_weights <- eff_weights[sub("_norm$", "", weight_cols)]
    data$sepi_raw <- as.numeric(norm_mat %*% matched_weights)

    # 5. Rescale to 0-1
    data$sepi <- as.numeric(normalise_min_max(data$sepi_raw))

    # 6. Rank (1 = best socio-economic conditions = highest SEPI)
    data$sepi_rank <- rank(-data$sepi, na.last = "last", ties.method = "min")

    # 7. Pillar columns from pillar_map (normalised representative indicator)
    pillar_names <- character(0)
    if (!is.null(pillar_map)) {
      for (p_name in names(pillar_map)) {
        rep_var  <- pillar_map[[p_name]]
        norm_col <- paste0(rep_var, "_norm")
        if (norm_col %in% names(data)) {
          data[[paste0("pillar_", p_name)]] <- data[[norm_col]]
        } else {
          data[[paste0("pillar_", p_name)]] <- NA_real_
        }
        pillar_names <- c(pillar_names, p_name)
      }
    }

    # Track pillar completeness
    if (length(pillar_names) > 0) {
      pillar_cols <- paste0("pillar_", pillar_names)
      data$n_pillars <- apply(
        data[, pillar_cols, drop = FALSE], 1,
        function(row) sum(!is.na(row))
      )
    } else {
      data$n_pillars <- NA_integer_
    }

    # 8. Normalise all granular_vars for Indicator_Scores export
    if (!is.null(granular_vars)) {
      for (v in granular_vars) {
        norm_col <- paste0(v, "_norm")
        if (v %in% names(data) && !norm_col %in% names(data)) {
          data[[norm_col]] <- normalise_min_max(data[[v]])
        }
      }
    }

    # Store effective weights as an attribute for export
    attr(data, "v3_effective_weights") <- eff_weights
    attr(data, "sepi_version") <- version$name

    return(data)
  }

  # ---- V1/V2 standard path --------------------------------------------------

  cfg <- cfg_resolved

  # 1. Normalise all indicators
  data <- normalise_country(data, cfg, version$normalisation)

  pillar_names <- names(cfg$pillars)
  n_pillars    <- length(pillar_names)

  # 2. Resolve pillar weights
  if (version$weighting == "equal") {
    pillar_weights <- rep(1 / n_pillars, n_pillars)
    names(pillar_weights) <- pillar_names
  } else if (version$weighting == "custom") {
    pillar_weights <- version$pillar_weights[pillar_names]
    if (any(is.na(pillar_weights))) {
      missing <- pillar_names[is.na(pillar_weights)]
      stop("Missing weights for pillars: ", paste(missing, collapse = ", "))
    }
  }

  # 3. Compute pillar scores (within-pillar aggregation, row by row)
  for (p_name in pillar_names) {
    pillar    <- cfg$pillars[[p_name]]
    norm_cols <- paste0(pillar$indicators, "_norm")
    norm_cols <- norm_cols[norm_cols %in% names(data)]

    if (length(norm_cols) == 0) {
      warning("No normalised indicators found for pillar '", p_name, "'.")
      data[[paste0("pillar_", p_name)]] <- NA_real_
      next
    }

    if (version$within_pillar_agg == "pca") {

      if (length(norm_cols) == 1) {
        # Single-indicator pillar: use directly, no PCA needed
        data[[paste0("pillar_", p_name)]] <- data[[norm_cols]]

      } else {
        # PCA-derived weights from the full normalised matrix
        norm_mat  <- as.matrix(data[, norm_cols, drop = FALSE])
        ind_wts   <- compute_pca_weights(norm_mat)

        data[[paste0("pillar_", p_name)]] <- apply(
          norm_mat, 1,
          function(row) aggregate_scores(row, w = ind_wts, method = "arithmetic")
        )
      }

    } else {
      data[[paste0("pillar_", p_name)]] <- apply(
        data[, norm_cols, drop = FALSE], 1,
        function(row) {
          aggregate_scores(row,
                           method = version$within_pillar_agg,
                           floor  = version$geometric_floor)
        }
      )
    }
  }

  # 4. Aggregate pillar scores into SEPI
  pillar_cols <- paste0("pillar_", pillar_names)

  data$sepi <- apply(
    data[, pillar_cols, drop = FALSE], 1,
    function(row) {
      aggregate_scores(row,
                       w      = pillar_weights,
                       method = version$across_pillar_agg,
                       floor  = version$geometric_floor %||% 0.001)
    }
  )

  # 5. Track pillar completeness
  data$n_pillars <- apply(
    data[, pillar_cols, drop = FALSE], 1,
    function(row) sum(!is.na(row))
  )

  # 6. Rank (1 = best socio-economic conditions)
  data$sepi_rank <- rank(-data$sepi, na.last = "last", ties.method = "min")

  # 7. Record which version produced these results
  attr(data, "sepi_version") <- version$name

  data
}

# ---- Convenience wrapper for all countries ---------------------------------

compute_all_countries <- function(all_data, version) {
  countries <- names(all_data)
  purrr::map(rlang::set_names(countries), function(country) {
    compute_sepi(all_data[[country]], version, country_name = country)
  })
}

# ---- Leave-one-out sensitivity analysis ------------------------------------

#' Assess how sensitive SEPI rankings are to each individual indicator
#'
#' For each configured indicator, this function removes it from its pillar,
#' re-computes the SEPI, and measures how much the final rankings change
#' compared to the full model.  This implements Handbook Step 7 (robustness
#' and sensitivity analysis) at the indicator level.
#'
#' Interpretation:
#'   spearman_rho > 0.95  ->  indicator is largely redundant given its peers.
#'   spearman_rho < 0.80  ->  indicator is highly influential; verify quality.
#'
#' Single-indicator pillars are included but flagged: removing the only
#' indicator in a pillar drops that pillar entirely from the SEPI, which
#' produces a structural change rather than a marginal sensitivity measure.
#'
#' @param data           Country data frame (loaded, not yet normalised)
#' @param country_config Single country entry from INDICATOR_CONFIG
#' @param version        A sepi_version object (from create_version())
#'
#' @return A data frame with columns:
#'   pillar, indicator, n_pillar_indicators, is_sole_indicator,
#'   spearman_rho, mean_abs_rank_shift, max_abs_rank_shift, interpretation
indicator_sensitivity <- function(data, country_config, version) {

  # Full-model SEPI rankings
  full_result  <- compute_sepi(data, version, country_config = country_config)
  full_ranks   <- full_result$sepi_rank
  id_col       <- country_config$id_cols[1]
  full_ids     <- full_result[[id_col]]

  rows <- list()

  for (p_name in names(country_config$pillars)) {
    pillar     <- country_config$pillars[[p_name]]
    indicators <- pillar$indicators
    n_inds     <- length(indicators)

    for (ind in indicators) {

      # Build a reduced config with this indicator removed
      keep_idx       <- indicators != ind
      reduced_config <- country_config
      reduced_config$pillars[[p_name]]$indicators <- indicators[keep_idx]
      reduced_config$pillars[[p_name]]$polarity   <- pillar$polarity[keep_idx]
      reduced_config$pillars[[p_name]]$labels     <- pillar$labels[keep_idx]

      is_sole <- n_inds == 1

      # Compute reduced SEPI (suppress warnings for empty pillars)
      reduced_result <- tryCatch(
        suppressWarnings(compute_sepi(data, version, country_config = reduced_config)),
        error = function(e) NULL
      )

      if (is.null(reduced_result)) {
        rows[[length(rows) + 1]] <- data.frame(
          pillar               = p_name,
          indicator            = ind,
          n_pillar_indicators  = n_inds,
          is_sole_indicator    = is_sole,
          spearman_rho         = NA_real_,
          mean_abs_rank_shift  = NA_real_,
          max_abs_rank_shift   = NA_real_,
          interpretation       = "computation_failed",
          stringsAsFactors     = FALSE
        )
        next
      }

      # Align by region id before comparing ranks
      reduced_ids   <- reduced_result[[id_col]]
      common_ids    <- intersect(full_ids, reduced_ids)
      full_r        <- full_ranks[match(common_ids, full_ids)]
      reduced_r     <- reduced_result$sepi_rank[match(common_ids, reduced_ids)]

      valid         <- !is.na(full_r) & !is.na(reduced_r)
      if (sum(valid) < 3) {
        rho  <- NA_real_
        mean_shift <- NA_real_
        max_shift  <- NA_real_
      } else {
        rho        <- round(
          stats::cor(full_r[valid], reduced_r[valid], method = "spearman"), 3
        )
        rank_diffs <- abs(full_r[valid] - reduced_r[valid])
        mean_shift <- round(mean(rank_diffs), 2)
        max_shift  <- round(max(rank_diffs), 0)
      }

      interpretation <- if (is_sole) {
        "sole_indicator_pillar_dropped"
      } else if (is.na(rho)) {
        "insufficient_data"
      } else if (rho > 0.95) {
        "redundant"
      } else if (rho < 0.80) {
        "highly_influential"
      } else {
        "moderate_influence"
      }

      rows[[length(rows) + 1]] <- data.frame(
        pillar               = p_name,
        indicator            = ind,
        n_pillar_indicators  = n_inds,
        is_sole_indicator    = is_sole,
        spearman_rho         = rho,
        mean_abs_rank_shift  = mean_shift,
        max_abs_rank_shift   = max_shift,
        interpretation       = interpretation,
        stringsAsFactors     = FALSE
      )
    }
  }

  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

#' Run sensitivity analysis for all countries
#'
#' @param all_data Named list of country data frames
#' @param version  sepi_version object
#'
#' @return Named list of per-country sensitivity data frames
sensitivity_all_countries <- function(all_data, version) {
  cat("\n========================================\n")
  cat(" Indicator Sensitivity Analysis\n")
  cat(" Version:", version$name, "\n")
  cat("========================================\n")

  purrr::imap(all_data, function(data, country) {
    cat("\n--", country_label(country), "--\n")
    result <- indicator_sensitivity(data, version$countries[[country]], version)

    # Print summary table
    print_cols <- c("pillar", "indicator", "spearman_rho",
                    "mean_abs_rank_shift", "interpretation")
    print(as.data.frame(result[, print_cols]), row.names = FALSE)

    invisible(result)
  })
}

# ---- Version comparison ----------------------------------------------------

compare_versions <- function(results_list) {
  version_names <- names(results_list)
  if (is.null(version_names)) version_names <- paste0("V", seq_along(results_list))

  countries <- names(results_list[[1]])

  purrr::map(rlang::set_names(countries), function(country) {

    # Extract SEPI + rank from each version
    version_dfs <- purrr::imap(results_list, function(vr, vname) {
      vr[[country]] |>
        dplyr::select(adm1_pcode, adm1_name, sepi, sepi_rank) |>
        dplyr::rename(
          !!paste0("sepi_", vname)  := sepi,
          !!paste0("rank_", vname)  := sepi_rank
        )
    })

    combined <- purrr::reduce(version_dfs, dplyr::left_join,
                              by = c("adm1_pcode", "adm1_name"))

    # Spearman rank correlations across versions
    rank_cols <- grep("^rank_", names(combined), value = TRUE)
    rank_cor  <- stats::cor(
      combined[, rank_cols, drop = FALSE],
      use    = "pairwise.complete.obs",
      method = "spearman"
    )

    list(
      scores           = combined,
      rank_correlation = rank_cor
    )
  })
}
