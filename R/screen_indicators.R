# ============================================================================
# Indicator Screening
# ============================================================================
# Applies a battery of statistical quality checks to the configured indicator
# set before the SEPI index is computed.  Checks follow the OECD/JRC Handbook
# on Constructing Composite Indicators (2008), Steps 2 and 4, extended with
# PCA-specific requirements for within-pillar weighting.
#
# Main entry points:
#   screen_indicators(data, country_config)       -> per-indicator flag table
#   screen_all_countries(all_data)                -> runs all countries
#
# Checks implemented:
#   Always (n >= 1):
#     - Missingness      : > 5% missing values      (Handbook §1.3)
#     - Near-zero variance: CV < 0.01               (PCA instability)
#     - High skewness    : |skew| > 2               (distorts min-max + PCA)
#
#   Only when pillar has n >= 2 indicators:
#     - PCA n/p ratio    : n_obs / n_indicators < 5 (Handbook §1.4)
#     - Redundancy       : within-pillar Spearman r > 0.95
#     - Incoherence      : within-pillar Spearman r < 0.20
#     - PC1 variance     : < 40% of total variance in pillar
#
#   Informational (no action needed):
#     - Single-indicator pillar: PCA skipped, indicator used directly
# ============================================================================

# ---- Helper: skewness -------------------------------------------------------

.skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  mn  <- mean(x)
  m3  <- mean((x - mn)^3)
  m2  <- mean((x - mn)^2)
  if (m2 < 1e-20) return(0)
  m3 / m2^1.5
}

# ---- Helper: per-indicator univariate stats ---------------------------------

.indicator_stats <- function(x, n_obs) {
  n_avail  <- sum(!is.na(x))
  miss_pct <- (n_obs - n_avail) / n_obs * 100
  vals     <- x[!is.na(x)]

  if (length(vals) < 2) {
    return(list(missing_pct = miss_pct, cv = NA_real_, skewness = NA_real_))
  }

  mn  <- mean(vals)
  sdv <- sd(vals)
  cv  <- if (abs(mn) > 1e-10) abs(sdv / mn) else NA_real_

  list(
    missing_pct = round(miss_pct, 1),
    cv          = round(cv, 4),
    skewness    = round(.skewness(vals), 3)
  )
}

# ---- Helper: PCA summary for a pillar ---------------------------------------
#
# Runs PCA on polarity-aligned, raw (not normalised) pillar columns.
# Returns a list with:
#   pc1_var_pct   : % of variance explained by PC1
#   loadings      : named vector of PC1 loadings (absolute)
#   weights       : PC1 loadings rescaled to sum to 1
#   n_components  : number of components with eigenvalue >= 1 (Kaiser)

.pillar_pca <- function(data, indicators) {
  mat <- as.matrix(data[, indicators, drop = FALSE])
  complete <- mat[stats::complete.cases(mat), , drop = FALSE]

  if (nrow(complete) < 3 || ncol(complete) < 2) return(NULL)

  # Remove constant columns (would break prcomp)
  nonconst <- apply(complete, 2, function(x) sd(x) > 1e-10)
  if (sum(nonconst) < 2) return(NULL)
  complete <- complete[, nonconst, drop = FALSE]

  pca <- tryCatch(
    stats::prcomp(complete, center = TRUE, scale. = TRUE),
    error = function(e) NULL
  )
  if (is.null(pca)) return(NULL)

  var_explained <- pca$sdev^2 / sum(pca$sdev^2)
  pc1_var_pct   <- round(var_explained[1] * 100, 1)

  # PC1 loadings — use absolute values for weighting
  loadings_raw <- abs(pca$rotation[, 1])
  weights      <- loadings_raw / sum(loadings_raw)

  # Kaiser criterion: eigenvalues >= 1
  n_kaiser <- sum(pca$sdev^2 >= 1)

  list(
    pc1_var_pct  = pc1_var_pct,
    loadings_raw = round(loadings_raw, 4),
    weights      = round(weights, 4),
    n_kaiser     = n_kaiser
  )
}

# ---- Core screening function ------------------------------------------------

#' Screen configured indicators for one country
#'
#' @param data           Loaded country data frame (from load_country_data())
#' @param country_config Single country entry from INDICATOR_CONFIG
#' @param country_name   Optional label used in printed output
#'
#' @return A data frame with one row per indicator and columns:
#'   pillar, indicator, label,
#'   n_obs, n_available, missing_pct, cv, skewness,
#'   n_pillar_indicators, pca_applies,
#'   within_pillar_r_min, within_pillar_r_max,  (NA if pca_applies = FALSE)
#'   pc1_var_pct, pc1_loading, pca_weight,       (NA if pca_applies = FALSE)
#'   flag_missingness, flag_low_variance, flag_skewness,
#'   flag_pca_n_p, flag_redundant, flag_incoherent, flag_pc1_low,
#'   flags_summary
screen_indicators <- function(data, country_config, country_name = NULL) {

  n_obs      <- nrow(data)
  label_str  <- if (!is.null(country_name)) country_label(country_name) else "Country"

  cat("\n========================================\n")
  cat(" Indicator Screening:", label_str, "\n")
  cat(" n observations:", n_obs, "\n")
  cat("========================================\n")

  rows <- list()

  for (p_name in names(country_config$pillars)) {
    pillar      <- country_config$pillars[[p_name]]
    indicators  <- pillar$indicators
    polarities  <- pillar$polarity
    labels      <- pillar$labels
    n_inds      <- length(indicators)
    pca_applies <- n_inds >= 2

    # ---- Polarity-align the pillar columns for PCA --------------------------
    aligned_cols <- paste0(indicators, "_screen_aligned")
    for (i in seq_along(indicators)) {
      if (!indicators[i] %in% names(data)) next
      x <- data[[indicators[i]]]
      data[[aligned_cols[i]]] <- if (polarities[i] == -1) -x else x
    }

    # ---- Per-pillar correlation matrix and PCA (if n >= 2) ------------------
    avail_aligned <- aligned_cols[aligned_cols %in% names(data)]

    pillar_cor   <- NULL
    pillar_pca_r <- NULL

    if (pca_applies && length(avail_aligned) >= 2) {
      mat_aligned <- as.matrix(data[, avail_aligned, drop = FALSE])

      pillar_cor <- tryCatch(
        stats::cor(mat_aligned, use = "pairwise.complete.obs", method = "spearman"),
        error = function(e) NULL
      )

      pillar_pca_r <- .pillar_pca(data, avail_aligned)
    }

    # ---- Check PCA n/p ratio for the whole pillar ---------------------------
    flag_pca_np_pillar <- pca_applies && (n_obs / n_inds < 5)

    # ---- Per-indicator rows -------------------------------------------------
    for (i in seq_along(indicators)) {
      ind   <- indicators[i]
      lbl   <- if (i <= length(labels)) labels[i] else ind

      if (!ind %in% names(data)) {
        rows[[length(rows) + 1]] <- data.frame(
          pillar = p_name, indicator = ind, label = lbl,
          n_obs = n_obs, n_available = 0L, missing_pct = 100,
          cv = NA_real_, skewness = NA_real_,
          n_pillar_indicators = n_inds, pca_applies = pca_applies,
          within_pillar_r_min = NA_real_, within_pillar_r_max = NA_real_,
          pc1_var_pct = NA_real_, pc1_loading = NA_real_, pca_weight = NA_real_,
          flag_missingness = TRUE, flag_low_variance = FALSE,
          flag_skewness = FALSE, flag_pca_n_p = FALSE,
          flag_redundant = FALSE, flag_incoherent = FALSE,
          flag_pc1_low = FALSE,
          flags_summary = "not_in_data",
          stringsAsFactors = FALSE
        )
        next
      }

      x    <- data[[ind]]
      stat <- .indicator_stats(x, n_obs)

      # ---- Univariate flags --------------------------------------------------
      flag_miss <- stat$missing_pct > 5
      flag_var  <- !is.na(stat$cv) && stat$cv < 0.01
      flag_skew <- !is.na(stat$skewness) && abs(stat$skewness) > 2

      # ---- Within-pillar correlation flags (only if pca_applies) ------------
      flag_redundant   <- FALSE
      flag_incoherent  <- FALSE
      r_min            <- NA_real_
      r_max            <- NA_real_

      if (pca_applies && !is.null(pillar_cor)) {
        aligned_col <- aligned_cols[i]
        if (aligned_col %in% rownames(pillar_cor)) {
          row_cors <- pillar_cor[aligned_col, ]
          row_cors <- row_cors[names(row_cors) != aligned_col]   # exclude self
          if (length(row_cors) > 0) {
            r_min <- round(min(row_cors, na.rm = TRUE), 3)
            r_max <- round(max(row_cors, na.rm = TRUE), 3)
            flag_redundant  <- !is.na(r_max) && r_max >  0.95
            flag_incoherent <- !is.na(r_max) && r_max <  0.20
          }
        }
      }

      # ---- PCA output for this indicator ------------------------------------
      pc1_var_pct <- NA_real_
      pc1_loading <- NA_real_
      pca_weight  <- NA_real_
      flag_pc1_low <- FALSE

      if (pca_applies && !is.null(pillar_pca_r)) {
        pc1_var_pct  <- pillar_pca_r$pc1_var_pct
        flag_pc1_low <- !is.na(pc1_var_pct) && pc1_var_pct < 40

        aligned_col <- aligned_cols[i]
        if (aligned_col %in% names(pillar_pca_r$loadings_raw)) {
          pc1_loading <- pillar_pca_r$loadings_raw[[aligned_col]]
          pca_weight  <- pillar_pca_r$weights[[aligned_col]]
        }
      }

      # ---- Combine flags ----------------------------------------------------
      active_flags <- c(
        if (flag_miss)      "high_missingness",
        if (flag_var)       "near_zero_variance",
        if (flag_skew)      "high_skewness",
        if (flag_pca_np_pillar) "pca_n_p_too_low",
        if (flag_redundant) "redundant_r>0.95",
        if (flag_incoherent)"incoherent_r<0.20",
        if (flag_pc1_low)   "pc1_var_pct<40"
      )

      rows[[length(rows) + 1]] <- data.frame(
        pillar              = p_name,
        indicator           = ind,
        label               = lbl,
        n_obs               = n_obs,
        n_available         = stat$missing_pct |> {\(p) round(n_obs * (1 - p / 100))}(),
        missing_pct         = stat$missing_pct,
        cv                  = stat$cv,
        skewness            = stat$skewness,
        n_pillar_indicators = n_inds,
        pca_applies         = pca_applies,
        within_pillar_r_min = r_min,
        within_pillar_r_max = r_max,
        pc1_var_pct         = pc1_var_pct,
        pc1_loading         = pc1_loading,
        pca_weight          = pca_weight,
        flag_missingness    = flag_miss,
        flag_low_variance   = flag_var,
        flag_skewness       = flag_skew,
        flag_pca_n_p        = flag_pca_np_pillar,
        flag_redundant      = flag_redundant,
        flag_incoherent     = flag_incoherent,
        flag_pc1_low        = flag_pc1_low,
        flags_summary       = if (length(active_flags) == 0) "OK" else
                                paste(active_flags, collapse = "; "),
        stringsAsFactors    = FALSE
      )
    }

    # Clean up temporary aligned columns
    data[, aligned_cols[aligned_cols %in% names(data)]] <- NULL

    # ---- Print pillar summary -----------------------------------------------
    cat("\n-- Pillar:", pillar_label(p_name), "--\n")
    cat("   Indicators:", n_inds, "|",
        if (pca_applies) "PCA applies" else "Single indicator — PCA skipped", "\n")

    if (pca_applies && !is.null(pillar_pca_r)) {
      cat("   PC1 variance explained:", pillar_pca_r$pc1_var_pct, "%",
          if (pillar_pca_r$pc1_var_pct < 40) "[FLAG: < 40%]" else "[OK]", "\n")
      cat("   PC1 loadings (absolute):",
          paste(names(pillar_pca_r$loadings_raw),
                round(pillar_pca_r$loadings_raw, 3), sep = "=", collapse = "  "),
          "\n")
      cat("   PCA weights:",
          paste(names(pillar_pca_r$weights),
                round(pillar_pca_r$weights, 3), sep = "=", collapse = "  "),
          "\n")
      if (flag_pca_np_pillar) {
        cat("   [FLAG] n/p ratio =", round(n_obs / n_inds, 1),
            "< 5 — consider reducing indicators\n")
      }
    }
  }

  result <- do.call(rbind, rows)
  rownames(result) <- NULL

  # ---- Print flag summary ---------------------------------------------------
  n_flags <- sum(result$flags_summary != "OK")
  cat("\n-- Summary --\n")
  cat("  Total indicators configured:", nrow(result), "\n")
  cat("  Indicators with flags:", n_flags, "\n")
  if (n_flags > 0) {
    flagged <- result[result$flags_summary != "OK",
                      c("pillar", "indicator", "flags_summary")]
    print(as.data.frame(flagged), row.names = FALSE)
  } else {
    cat("  All indicators pass screening.\n")
  }

  invisible(result)
}

# ---- Wrapper for all countries ----------------------------------------------

#' Run indicator screening for all countries
#'
#' @param all_data Named list of country data frames (from load_all_data())
#' @param config   INDICATOR_CONFIG (defaults to global)
#'
#' @return Named list of per-country screening result data frames
screen_all_countries <- function(all_data, config = INDICATOR_CONFIG) {
  purrr::imap(all_data, function(data, country) {
    screen_indicators(data, config[[country]], country)
  })
}
