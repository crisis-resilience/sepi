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

  # ---- Polarity audit: empirical r sign vs normative bad_vars sign ----------
  # Normative sign comes from bad_vars (author's theory-driven polarity).
  # Empirical sign comes from the Pearson correlation with conflict.
  # Under "indicators that track conflict are worse for peacebuilding",
  # the SEPI-direction implied by the data is the *opposite* sign of r:
  #   r > 0 (indicator tracks conflict up)   -> data implies sign = -1
  #   r < 0 (indicator tracks conflict down) -> data implies sign = +1
  # Mismatches are only flagged when |r| >= MIN_ABS_R; near-zero correlations
  # carry an arbitrary sign that is not informative.
  MIN_ABS_R <- 0.10

  empirical_sign    <- sign(cors)
  data_implied_sign <- -empirical_sign

  mismatch <- !is.na(cors) &
              abs(cors) >= MIN_ABS_R &
              data_implied_sign != signs

  audit <- data.frame(
    indicator         = se_vars,
    correlation       = round(cors, 3),
    empirical_sign    = empirical_sign,
    in_bad_vars       = se_vars %in% bad_vars,
    normative_sign    = as.integer(signs),
    data_implied_sign = as.integer(data_implied_sign),
    weight_magnitude  = round(weight_mag, 4),
    effective_weight  = round(effective, 4),
    mismatch          = mismatch,
    stringsAsFactors  = FALSE
  )
  rownames(audit) <- NULL

  attr(effective, "polarity_audit") <- audit
  attr(effective, "polarity_audit_min_abs_r") <- MIN_ABS_R
  effective
}

# ---- V3 polarity-audit renderer --------------------------------------------

#' Render the v3 polarity audit for one country as a PNG table
#'
#' Produces a gt-styled table that compares the empirical sign of each
#' indicator's correlation with conflict against the normative polarity
#' declared in bad_vars.  Rows are colour-coded:
#'   - red    : MISMATCH (|r| >= min_abs_r and signs disagree)
#'   - grey   : uninformative (|r| < min_abs_r)
#'   - green  : consistent
#'
#' @param audit             Data frame returned by compute_conflict_weights()
#'                          and stored as attr(sepi_results[[c]], "v3_polarity_audit").
#' @param country_label_str Pretty country label for the title.
#' @param min_abs_r         Threshold below which |r| is deemed uninformative.
#' @param out_path          Destination PNG path.
#' @return Invisibly returns the gt object.
render_polarity_audit_png <- function(audit,
                                      country_label_str,
                                      min_abs_r = 0.10,
                                      out_path) {
  for (pkg in c("gt", "webshot2")) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  }

  if (is.null(audit) || nrow(audit) == 0) {
    message("No polarity audit data for ", country_label_str, " — skipping PNG.")
    return(invisible(NULL))
  }

  uninformative <- is.na(audit$correlation) | abs(audit$correlation) < min_abs_r
  status <- ifelse(audit$mismatch, "MISMATCH",
            ifelse(uninformative, "uninformative",
                                  "consistent"))

  fmt_sign <- function(x) {
    ifelse(is.na(x), "\u2014",
      ifelse(x > 0, "+", "\u2212"))
  }

  df <- data.frame(
    Indicator         = audit$indicator,
    r                 = audit$correlation,
    `Data sign`       = fmt_sign(audit$data_implied_sign),
    `bad_vars sign`   = fmt_sign(audit$normative_sign),
    `|w|`             = audit$weight_magnitude,
    `Effective w`     = audit$effective_weight,
    Status            = status,
    stringsAsFactors  = FALSE,
    check.names       = FALSE
  )

  ord <- order(
    factor(df$Status, levels = c("MISMATCH", "uninformative", "consistent")),
    -abs(df$r)
  )
  df <- df[ord, , drop = FALSE]

  n_total    <- nrow(df)
  n_mismatch <- sum(df$Status == "MISMATCH")
  n_uninf    <- sum(df$Status == "uninformative")

  tbl <- gt::gt(df) |>
    gt::tab_header(
      title    = gt::md(paste0("**", country_label_str, " — v3 polarity audit**")),
      subtitle = sprintf(
        "%d / %d indicator(s) mismatched; %d uninformative (|r| < %.2f).",
        n_mismatch, n_total, n_uninf, min_abs_r
      )
    ) |>
    gt::fmt_number(columns = c("r", "|w|", "Effective w"), decimals = 3) |>
    gt::sub_missing(missing_text = "\u2014") |>
    gt::tab_style(
      style     = list(gt::cell_fill(color = "#f8d7da"),
                       gt::cell_text(weight = "bold")),
      locations = gt::cells_body(rows = Status == "MISMATCH")
    ) |>
    gt::tab_style(
      style     = gt::cell_fill(color = "#eeeeee"),
      locations = gt::cells_body(rows = Status == "uninformative")
    ) |>
    gt::tab_style(
      style     = gt::cell_fill(color = "#d4edda"),
      locations = gt::cells_body(rows = Status == "consistent")
    ) |>
    gt::tab_footnote(
      footnote = paste0(
        "Data sign = -sign(r), the SEPI direction implied by the empirical ",
        "correlation with conflict. bad_vars sign is the normative polarity ",
        "applied in the index. MISMATCH = the two disagree and |r| \u2265 ",
        min_abs_r, "."
      )
    ) |>
    gt::tab_options(
      table.font.names         = "Arial",
      table.font.size          = 10,
      heading.title.font.size  = 13,
      column_labels.font.weight = "bold",
      data_row.padding         = gt::px(4)
    )

  dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
  gt::gtsave(tbl, filename = out_path, zoom = 2, expand = 15)
  message("Saved: ", out_path)
  invisible(tbl)
}

#' Convenience wrapper: render polarity audit PNGs for every country
#'
#' Loops over a sepi_results list and writes one PNG per country whose
#' data frame carries a "v3_polarity_audit" attribute.  Non-v3 countries
#' are silently skipped.
render_polarity_audits <- function(sepi_results, version) {
  for (country in names(sepi_results)) {
    audit <- attr(sepi_results[[country]], "v3_polarity_audit")
    if (is.null(audit)) next

    out_path <- versioned_output_path(version, "figures", "polarity_audit",
                                      paste0("polarity_audit_", country))
    render_polarity_audit_png(
      audit             = audit,
      country_label_str = country_label(country),
      out_path          = out_path
    )
  }
  invisible(NULL)
}

# ---- BoD (Benefit of the Doubt) weight computation -------------------------

#' Compute Benefit of the Doubt (BoD/DEA) scores for all districts
#'
#' For each district, solves a linear program that finds indicator weights
#' maximising that district's composite score, subject to no other district
#' exceeding 1 under those same weights.  Weight flexibility around equal
#' weights is controlled by flex (0.5 = ±50% of 1/n, Scenario I from JRC
#' handbook; 1.0 = weights can reach zero, Scenario II).
#'
#' Requires the lpSolve package.
#'
#' @param norm_mat  Numeric matrix — rows = districts, cols = indicators.
#'                  All values must be in [0,1] with higher = better
#'                  (bad_vars should be pre-flipped before calling).
#' @param flex      Flexibility around equal weight (default 0.5 = ±50%).
#' @return Numeric vector of BoD scores in [0,1], one per district.
compute_bod_sepi <- function(norm_mat, flex = 0.5) {
  if (!requireNamespace("lpSolve", quietly = TRUE)) {
    stop("Package 'lpSolve' is required for BoD weighting. ",
         "Install with: install.packages('lpSolve')")
  }

  n_units <- nrow(norm_mat)
  n_ind   <- ncol(norm_mat)
  eq_w    <- 1 / n_ind
  lb      <- max(0, eq_w * (1 - flex))
  ub      <- eq_w * (1 + flex)

  bod_scores <- numeric(n_units)

  # Build bound rows once — same for every district
  # Upper bounds: w_i <= ub  → one-hot row per indicator
  # Lower bounds: w_i >= lb  → -w_i <= -lb  → negative one-hot row per indicator
  ub_mat <- diag(n_ind)                   # w_i <= ub
  lb_mat <- -diag(n_ind)                  # -w_i <= -lb

  for (c_idx in seq_len(n_units)) {
    # Objective: maximise w'x_c  →  minimise -w'x_c
    obj <- -norm_mat[c_idx, ]

    # Constraints:
    #   w'x_d ≤ 1  for all districts d  (efficiency frontier)
    #   Σ w_i = 1                        (weights sum to 1)
    #   w_i ≤ ub                         (upper weight bounds)
    #  -w_i ≤ -lb                        (lower weight bounds)
    con_mat <- rbind(norm_mat, rep(1, n_ind), ub_mat, lb_mat)
    con_dir <- c(rep("<=", n_units), "=", rep("<=", n_ind), rep("<=", n_ind))
    con_rhs <- c(rep(1, n_units), 1, rep(ub, n_ind), rep(-lb, n_ind))

    sol <- lpSolve::lp(
      direction    = "min",
      objective.in = obj,
      const.mat    = con_mat,
      const.dir    = con_dir,
      const.rhs    = con_rhs
    )

    bod_scores[c_idx] <- if (sol$status == 0) -sol$objval else NA_real_
  }

  bod_scores
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

    # 2. Normalise SE vars using the configured method (no polarity flip for v3)
    norm_fn <- switch(version$normalisation,
      min_max = normalise_min_max,
      z_score = normalise_z_score,
      rank    = normalise_rank,
      normalise_min_max  # fallback
    )
    for (v in se_vars) {
      if (v %in% names(data)) {
        data[[paste0(v, "_norm")]] <- norm_fn(data[[v]])
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

    # 3b. Polarity audit — flag indicators where the empirical correlation
    #     sign disagrees with the normative polarity set via bad_vars.
    polarity_audit <- attr(eff_weights, "polarity_audit")
    min_abs_r      <- attr(eff_weights, "polarity_audit_min_abs_r") %||% 0.10

    if (!is.null(polarity_audit)) {
      n_total    <- nrow(polarity_audit)
      n_mismatch <- sum(polarity_audit$mismatch, na.rm = TRUE)

      if (n_mismatch > 0) {
        message("  [v3 polarity audit] ", n_mismatch, " / ", n_total,
                " indicator(s) with empirical \u2260 normative polarity ",
                "(|r| \u2265 ", min_abs_r, "):")
        mm <- polarity_audit[polarity_audit$mismatch, , drop = FALSE]
        for (i in seq_len(nrow(mm))) {
          nsign <- if (mm$normative_sign[i] > 0) "+" else "-"
          dsign <- if (mm$data_implied_sign[i] > 0) "+" else "-"
          message(sprintf(
            "    %-38s  r = %+.3f  data\u2192%s  bad_vars\u2192%s  |w| = %.3f",
            mm$indicator[i], mm$correlation[i],
            dsign, nsign, mm$weight_magnitude[i]
          ))
        }
      } else {
        message("  [v3 polarity audit] all ", n_total,
                " indicator(s) consistent (or |r| < ", min_abs_r, ")")
      }
    }

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

    # 5. Rescale to 0-1 (skipped when skip_final_rescale = TRUE, e.g. z-score robustness)
    data$sepi <- if (isTRUE(version$skip_final_rescale)) {
      data$sepi_raw
    } else {
      as.numeric(normalise_min_max(data$sepi_raw))
    }

    # 6. Rank (1 = best socio-economic conditions = highest SEPI)
    data$sepi_rank <- rank(-data$sepi, na.last = NA, ties.method = "min")

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
    attr(data, "v3_polarity_audit")    <- polarity_audit
    attr(data, "sepi_version") <- version$name

    return(data)
  }

  # ---- BoD weighting path ---------------------------------------------------
  if (isTRUE(version$bod_weighting)) {
    cfg        <- cfg_resolved
    bad_vars   <- cfg$bad_vars
    pillar_map <- cfg$pillar_map
    bod_flex   <- if (!is.null(version$bod_weight_flex)) version$bod_weight_flex else 0.5

    if (is.null(pillar_map) || length(pillar_map) == 0) {
      stop("BoD weighting requires 'pillar_map' in country config (one indicator per pillar).")
    }

    # BoD weights across pillars — one representative indicator per pillar,
    # as defined by pillar_map. This follows the standard BoD setup
    # (Cherchye et al., 2007): each pillar is a sub-dimension and BoD finds
    # optimal weights across them per district.
    pillar_vars <- unname(unlist(pillar_map))
    pillar_vars <- pillar_vars[pillar_vars %in% names(data)]

    # 1. Impute (same strategy as v3)
    data <- impute_missing_v3(data, pillar_vars, cfg$imputation)

    # 2. Normalise pillar representative indicators to [0,1] with min-max
    for (v in pillar_vars) {
      data[[paste0(v, "_norm")]] <- normalise_min_max(data[[v]])
    }

    # 3. Flip bad_vars so that higher = better for all indicators.
    #    BoD maximises composite score, so all inputs must point the same way.
    for (v in intersect(pillar_vars, bad_vars)) {
      nc <- paste0(v, "_norm")
      if (nc %in% names(data)) data[[nc]] <- 1 - data[[nc]]
    }

    # 4. Build normalised matrix and run BoD LP
    norm_cols <- paste0(pillar_vars, "_norm")
    norm_cols <- norm_cols[norm_cols %in% names(data)]
    norm_mat  <- as.matrix(data[, norm_cols, drop = FALSE])

    cat(sprintf("[BoD] Solving %d LP problems (%d pillars, flex=%.2f)...\n",
                nrow(norm_mat), ncol(norm_mat), bod_flex))

    data$sepi_raw <- compute_bod_sepi(norm_mat, flex = bod_flex)
    data$sepi     <- data$sepi_raw

    # 5. Rank (1 = best = highest BoD score)
    data$sepi_rank <- rank(-data$sepi, na.last = NA, ties.method = "min")

    # 6. Pillar columns (normalised representative indicator per pillar)
    if (!is.null(pillar_map)) {
      pillar_names <- names(pillar_map)
      for (p_name in pillar_names) {
        rep_var  <- pillar_map[[p_name]]
        norm_col <- paste0(rep_var, "_norm")
        data[[paste0("pillar_", p_name)]] <-
          if (norm_col %in% names(data)) data[[norm_col]] else NA_real_
      }
      pillar_cols   <- paste0("pillar_", pillar_names)
      data$n_pillars <- apply(
        data[, pillar_cols, drop = FALSE], 1,
        function(row) sum(!is.na(row))
      )
    } else {
      data$n_pillars <- NA_integer_
    }

    # 7. Normalise granular_vars for export (same as v3 path)
    granular_vars <- cfg$granular_vars
    if (!is.null(granular_vars)) {
      for (v in granular_vars) {
        norm_col <- paste0(v, "_norm")
        if (v %in% names(data) && !norm_col %in% names(data)) {
          data[[norm_col]] <- normalise_min_max(data[[v]])
        }
      }
    }

    attr(data, "sepi_version") <- version$name
    return(data)
  }

  # ---- V1/V2 standard path --------------------------------------------------

  cfg <- cfg_resolved

  # Fill pop_frac_3plus NAs with 0 before normalisation: NA means the county
  # is not monitored by IPC because it has no acute food crisis, so 0 (no
  # phase-3+ population) is the correct value, giving the best food security score.
  all_indicators <- unlist(lapply(cfg$pillars, `[[`, "indicators"))
  if ("pop_frac_3plus" %in% all_indicators && "pop_frac_3plus" %in% names(data)) {
    data[["pop_frac_3plus"]][is.na(data[["pop_frac_3plus"]])] <- 0
  }

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

  # 3b. Apply pillar-level NA fill (version$pillar_na_fill = list(food_security = 0, ...))
  if (!is.null(version$pillar_na_fill)) {
    for (pname in names(version$pillar_na_fill)) {
      col <- paste0("pillar_", pname)
      if (col %in% names(data)) {
        data[[col]][is.na(data[[col]])] <- version$pillar_na_fill[[pname]]
      }
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
  data$sepi_rank <- rank(-data$sepi, na.last = NA, ties.method = "min")

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

  # ---- BoD path: pillar_map instead of pillars or se_vars -------------------
  if (isTRUE(version$bod_weighting)) {
    pillar_map <- country_config$pillar_map
    if (is.null(pillar_map) || length(pillar_map) == 0) {
      return(data.frame())
    }

    pillar_vars <- unname(unlist(pillar_map))
    n_total     <- length(pillar_vars)

    for (ind in pillar_vars) {
      # Remove this pillar's indicator from pillar_map
      keep_names     <- names(pillar_map)[unlist(pillar_map) != ind]
      reduced_config <- country_config
      reduced_config$pillar_map <- pillar_map[keep_names]
      is_sole <- n_total == 1

      reduced_result <- tryCatch(
        suppressWarnings(compute_sepi(data, version, country_config = reduced_config)),
        error = function(e) NULL
      )

      # Find which pillar this indicator belongs to
      pillar_name <- names(pillar_map)[unlist(pillar_map) == ind]

      if (is.null(reduced_result)) {
        rows[[length(rows) + 1]] <- data.frame(
          pillar               = pillar_name,
          indicator            = ind,
          n_pillar_indicators  = n_total,
          is_sole_indicator    = is_sole,
          spearman_rho         = NA_real_,
          mean_abs_rank_shift  = NA_real_,
          max_abs_rank_shift   = NA_real_,
          interpretation       = "computation_failed",
          stringsAsFactors     = FALSE
        )
        next
      }

      reduced_ids <- reduced_result[[id_col]]
      common_ids  <- intersect(full_ids, reduced_ids)
      full_r      <- full_ranks[match(common_ids, full_ids)]
      reduced_r   <- reduced_result$sepi_rank[match(common_ids, reduced_ids)]
      valid       <- !is.na(full_r) & !is.na(reduced_r)

      if (sum(valid) < 3) {
        rho <- NA_real_; mean_shift <- NA_real_; max_shift <- NA_real_
      } else {
        rho        <- round(stats::cor(full_r[valid], reduced_r[valid], method = "spearman"), 3)
        rank_diffs <- abs(full_r[valid] - reduced_r[valid])
        mean_shift <- round(mean(rank_diffs), 2)
        max_shift  <- round(max(rank_diffs), 0)
      }

      interpretation <- if (is_sole)       "sole_indicator"
        else if (is.na(rho))               "insufficient_data"
        else if (rho > 0.95)               "redundant"
        else if (rho < 0.80)               "highly_influential"
        else                               "moderate_influence"

      rows[[length(rows) + 1]] <- data.frame(
        pillar               = pillar_name,
        indicator            = ind,
        n_pillar_indicators  = n_total,
        is_sole_indicator    = is_sole,
        spearman_rho         = rho,
        mean_abs_rank_shift  = mean_shift,
        max_abs_rank_shift   = max_shift,
        interpretation       = interpretation,
        stringsAsFactors     = FALSE
      )
    }

    result <- do.call(rbind, rows)
    rownames(result) <- NULL
    return(result)
  }

  # ---- V3 path: se_vars instead of pillars ----------------------------------
  if (isTRUE(version$conflict_weighting)) {
    se_vars <- unlist(country_config$se_vars)   # ensure character vector, not list
    n_total <- length(se_vars)

    for (ind in se_vars) {
      keep_idx       <- se_vars != ind
      reduced_config <- country_config
      reduced_config$se_vars <- se_vars[keep_idx]
      is_sole <- n_total == 1

      reduced_result <- tryCatch(
        suppressWarnings(compute_sepi(data, version, country_config = reduced_config)),
        error = function(e) NULL
      )

      if (is.null(reduced_result)) {
        rows[[length(rows) + 1]] <- data.frame(
          pillar               = NA_character_,
          indicator            = ind,
          n_pillar_indicators  = n_total,
          is_sole_indicator    = is_sole,
          spearman_rho         = NA_real_,
          mean_abs_rank_shift  = NA_real_,
          max_abs_rank_shift   = NA_real_,
          interpretation       = "computation_failed",
          stringsAsFactors     = FALSE
        )
        next
      }

      reduced_ids <- reduced_result[[id_col]]
      common_ids  <- intersect(full_ids, reduced_ids)
      full_r      <- full_ranks[match(common_ids, full_ids)]
      reduced_r   <- reduced_result$sepi_rank[match(common_ids, reduced_ids)]
      valid       <- !is.na(full_r) & !is.na(reduced_r)

      if (sum(valid) < 3) {
        rho <- NA_real_; mean_shift <- NA_real_; max_shift <- NA_real_
      } else {
        rho        <- round(stats::cor(full_r[valid], reduced_r[valid], method = "spearman"), 3)
        rank_diffs <- abs(full_r[valid] - reduced_r[valid])
        mean_shift <- round(mean(rank_diffs), 2)
        max_shift  <- round(max(rank_diffs), 0)
      }

      interpretation <- if (is_sole) "sole_indicator"
        else if (is.na(rho))  "insufficient_data"
        else if (rho > 0.95)  "redundant"
        else if (rho < 0.80)  "highly_influential"
        else                  "moderate_influence"

      rows[[length(rows) + 1]] <- data.frame(
        pillar               = NA_character_,
        indicator            = ind,
        n_pillar_indicators  = n_total,
        is_sole_indicator    = is_sole,
        spearman_rho         = rho,
        mean_abs_rank_shift  = mean_shift,
        max_abs_rank_shift   = max_shift,
        interpretation       = interpretation,
        stringsAsFactors     = FALSE
      )
    }

    result <- do.call(rbind, rows)
    rownames(result) <- NULL
    return(result)
  }

  # ---- V1/V2 path: pillars --------------------------------------------------
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
