# ============================================================================
# Candidate Indicator Exploration
# ============================================================================
# Purpose: Survey the full pool of variables available in each country's
# data and triage them as potential SEPI indicators, using:
#   - Metadata from the global metadata CSV (GLOBAL_DATA$metadata_file)
#   - Statistical quality checks (missingness, skewness, variance)
#   - Relevance signals (Spearman correlation with the current SEPI score)
#   - PCA feasibility flags (n/p ratio per pillar)
#
# Main entry points:
#   load_metadata(country, ...)               -> data frame of variable metadata
#   explore_country_candidates(country, ...)  -> candidate report data frame
#   explore_all_candidates(all_data, ...)     -> runs all countries
# ============================================================================

# ---- Load metadata from global metadata CSV --------------------------------

#' Load variable metadata for one country from the global metadata CSV
#'
#' Reads GLOBAL_DATA$metadata_file (all countries), filters to the requested
#' country, and returns a standardised data frame with columns:
#'   variable, description, source, type, category
#'
#' @param country        Country key ("kenya", "somalia", "south_sudan")
#' @param metadata_file  Path to the global metadata CSV
#'
#' @return data frame with columns: variable, description, source, type, category
load_metadata <- function(country,
                          metadata_file = GLOBAL_DATA$metadata_file) {
  if (!file.exists(metadata_file)) {
    message("  Metadata CSV not found: ", metadata_file, " — returning empty table.")
    return(.meta_empty())
  }

  all_meta <- tryCatch(
    readr::read_csv(metadata_file, show_col_types = FALSE, progress = FALSE),
    error = function(e) {
      message("  Could not read metadata CSV: ", conditionMessage(e))
      return(.meta_empty())
    }
  )

  meta <- all_meta |>
    dplyr::filter(country == !!country)

  # Map global metadata columns to the standardised names
  result <- data.frame(
    variable    = as.character(meta[["global_variable_name"]]),
    description = as.character(meta[["Description"]]),
    source      = as.character(meta[["Data source"]]),
    type        = as.character(meta[["Indicator type"]]),
    category    = as.character(meta[["Pillar"]]),
    stringsAsFactors = FALSE
  )

  # Replace "+" with "plus" to match column names after load_country_data()
  result$variable <- stringr::str_replace_all(result$variable, "\\+", "plus")

  result <- result[!duplicated(result$variable) &
                     !is.na(result$variable) &
                     nchar(trimws(result$variable)) > 0, ]
  rownames(result) <- NULL
  result
}

.meta_empty <- function() {
  data.frame(variable    = character(),
             description = character(),
             source      = character(),
             type        = character(),
             category    = character(),
             stringsAsFactors = FALSE)
}

# ---- Keyword-based pillar assignment ---------------------------------------

.pillar_keywords <- list(
  education    = c("school", "literacy", "enroll", "attendance", "dropout",
                   "education", "tertiary", "primary", "secondary", "learning",
                   "parity", "completion"),
  health       = c("health", "hospital", "clinic", "facility", "mortalit",
                   "immunis", "immuniz", "mch", "doctor"),
  food_security = c("ipc", "food", "famine", "hunger", "fcs", "rcsi",
                    "dietary", "meal", "consumption", "livelihood", "nutrition"),
  economic     = c("poverty", "income", "expenditure", "wage", "gdp", "gcp",
                   "wealth", "asset", "employ", "economic", "market",
                   "fuel", "sanitat", "drinking", "electric", "water", "cook"),
  climate      = c("ndvi", "rainfall", "precip", "temperature", "drought",
                   "flood", "vegetation", "climate", "soil", "moisture",
                   "fapar", "pdsi", "remote sensing", "environmental"),
  demographics = c("demograph", "youth", "bulge", "age structure",
                   "fertility", "population structure"),
  conflict     = c("conflict", "battle", "fatali", "violence", "acled",
                   "armed", "attack", "casualt")
)

suggest_pillar <- function(description) {
  if (is.na(description) || nchar(trimws(description)) == 0) return(NA_character_)
  desc_lower <- tolower(description)
  for (pillar in names(.pillar_keywords)) {
    if (any(sapply(.pillar_keywords[[pillar]], grepl, x = desc_lower, fixed = FALSE))) {
      return(pillar)
    }
  }
  NA_character_
}

# ---- Statistical profiling of a numeric vector ----------------------------

.profile_vector <- function(x, n_obs) {
  n_avail  <- sum(!is.na(x))
  miss_pct <- round((n_obs - n_avail) / n_obs * 100, 1)
  zero_pct <- if (n_avail > 0) round(sum(x == 0, na.rm = TRUE) / n_avail * 100, 1) else NA_real_

  vals <- x[!is.na(x)]

  if (length(vals) < 2) {
    return(list(
      n_available = n_avail, missing_pct = miss_pct, zero_pct = zero_pct,
      mean = NA_real_, sd = NA_real_, min = NA_real_, max = NA_real_,
      cv = NA_real_, skewness = NA_real_
    ))
  }

  mn   <- mean(vals)
  sdv  <- sd(vals)
  cv   <- if (abs(mn) > 1e-10) abs(sdv / mn) else NA_real_
  skew <- if (length(vals) >= 3) {
    m3 <- mean((vals - mn)^3)
    m2 <- mean((vals - mn)^2)
    if (m2 < 1e-20) 0 else m3 / m2^1.5
  } else NA_real_

  list(
    n_available = n_avail,
    missing_pct = miss_pct,
    zero_pct    = zero_pct,
    mean        = round(mn, 4),
    sd          = round(sdv, 4),
    min         = round(min(vals), 4),
    max         = round(max(vals), 4),
    cv          = round(cv, 4),
    skewness    = round(skew, 3)
  )
}

# ---- Identify columns to exclude from candidates --------------------------

.get_excluded_cols <- function(country_config) {
  used_indicators <- unlist(lapply(country_config$pillars,   `[[`, "indicators"))
  conflict_inds   <- country_config$conflict$indicators
  id_cols         <- country_config$id_cols
  pop_col         <- country_config$population_col
  c(used_indicators, conflict_inds, id_cols, pop_col,
    # common non-indicator admin/metadata columns
    "adm0_name", "adm0_pcode", "is_capital",
    "source_year", "source_year.x", "source_year.y",
    "population_total.x", "population_total.y")
}

# ---- Build PCA feasibility flag -------------------------------------------

.pca_feasible <- function(n_obs, current_pillar_sizes, max_n_p_ratio = 5) {
  # Returns TRUE if the country can support at least 2 indicators per pillar
  # given the n/p ≥ max_n_p_ratio rule
  max_per_pillar <- floor(n_obs / max_n_p_ratio)
  # A candidate is "pca_feasible" if the pillar it would go into would still
  # have n/p >= 5 after adding it.  Reported per candidate (current+1).
  max_per_pillar >= 2
}

# ---- Main exploration function --------------------------------------------

#' Explore candidate indicators for one country
#'
#' Profiles all numeric columns not already assigned to a SEPI pillar or
#' used as ID / population columns.  Each candidate is checked for
#' missingness, variance, skewness, PCA feasibility, and (optionally)
#' Spearman correlation with the current SEPI score.
#'
#' @param country        Country key, e.g. "kenya"
#' @param config         INDICATOR_CONFIG list (defaults to global INDICATOR_CONFIG)
#' @param sepi_data      Optional: data frame with a 'sepi' column and 'adm1_pcode'
#'                       from a previous SEPI computation, used to compute
#'                       Spearman correlations between candidates and the SEPI score.
#' @param metadata_file  Path to the global metadata CSV
#' @param out_dir        Directory to write the CSV report (defaults to "outputs/")
#' @param write_csv      If TRUE, writes candidate_report_{country}.csv to out_dir
#' @param country_data   Pre-loaded data frame for this country (from load_all_data())
#'
#' @return A data frame with one row per candidate variable
explore_country_candidates <- function(
    country,
    config        = VERSIONS$v1_equal_geometric$countries,
    sepi_data     = NULL,
    metadata_file = GLOBAL_DATA$metadata_file,
    out_dir       = "outputs",
    write_csv     = TRUE,
    country_data  = NULL
) {
  country_config <- config[[country]]
  if (is.null(country_config)) stop("Country '", country, "' not found in config.")

  raw_data <- if (!is.null(country_data)) {
    country_data
  } else {
    load_country_data(country)
  }
  n_obs <- nrow(raw_data)

  dict_df <- load_metadata(country, metadata_file = metadata_file)

  # ---- Identify candidate columns --------------------------------------------
  excluded   <- .get_excluded_cols(country_config)
  num_cols   <- names(raw_data)[sapply(raw_data, is.numeric)]
  candidates <- setdiff(num_cols, excluded)

  if (length(candidates) == 0) {
    message("  No candidate columns found for ", country)
    return(invisible(NULL))
  }

  # ---- Compute current pillar sizes (for PCA feasibility) -------------------
  pillar_sizes <- sapply(country_config$pillars,
                         function(p) length(p$indicators))

  # ---- Build profile for each candidate -------------------------------------
  rows <- lapply(candidates, function(col) {
    x    <- raw_data[[col]]
    prof <- .profile_vector(x, n_obs)

    # Spearman correlation with SEPI (if provided)
    r_sepi <- NA_real_
    if (!is.null(sepi_data) && "sepi" %in% names(sepi_data) &&
        "adm1_pcode" %in% names(sepi_data)) {
      merged <- merge(
        raw_data[, c(country_config$id_cols[1], col), drop = FALSE],
        sepi_data[, c("adm1_pcode", "sepi"),   drop = FALSE],
        by.x = country_config$id_cols[1], by.y = "adm1_pcode"
      )
      if (nrow(merged) >= 5) {
        r_sepi <- round(
          tryCatch(cor(merged[[col]], merged$sepi,
                       use = "pairwise.complete.obs", method = "spearman"),
                   error = function(e) NA_real_),
          3
        )
      }
    }

    # Look up metadata
    meta        <- dict_df[dict_df$variable == col, , drop = FALSE]
    description <- if (nrow(meta) > 0) meta$description[1] else NA_character_
    source      <- if (nrow(meta) > 0) meta$source[1]      else NA_character_
    type_str    <- if (nrow(meta) > 0) meta$type[1]        else NA_character_
    category    <- if (nrow(meta) > 0) meta$category[1]    else NA_character_

    suggested_pillar <- suggest_pillar(
      paste(col, description, category, sep = " ")
    )

    current_size <- if (!is.na(suggested_pillar) &&
                        suggested_pillar %in% names(pillar_sizes)) {
      pillar_sizes[suggested_pillar]
    } else {
      1L
    }
    pca_feasible <- (n_obs / (current_size + 1L)) >= 5

    # Build quality flags
    flags <- character(0)
    if (prof$missing_pct > 5)                             flags <- c(flags, "high_missingness")
    if (!is.na(prof$cv)       && prof$cv < 0.01)          flags <- c(flags, "near_zero_variance")
    if (!is.na(prof$skewness) && abs(prof$skewness) > 2)  flags <- c(flags, "high_skewness")
    if (!pca_feasible)                                    flags <- c(flags, "pca_n_p_too_low")
    if (!is.na(suggested_pillar) && suggested_pillar == "conflict")
                                                          flags <- c(flags, "conflict_exploratory_only")

    data.frame(
      variable         = col,
      category         = category,
      description      = description,
      source           = source,
      type             = type_str,
      suggested_pillar = suggested_pillar,
      n_obs            = n_obs,
      n_available      = prof$n_available,
      missing_pct      = prof$missing_pct,
      zero_pct         = prof$zero_pct,
      mean             = prof$mean,
      sd               = prof$sd,
      min              = prof$min,
      max              = prof$max,
      cv               = prof$cv,
      skewness         = prof$skewness,
      spearman_r_sepi  = r_sepi,
      pca_feasible     = pca_feasible,
      flags            = if (length(flags) == 0) "" else paste(flags, collapse = "; "),
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, rows)
  rownames(result) <- NULL

  result <- result[order(
    result$suggested_pillar,
    result$missing_pct,
    -abs(ifelse(is.na(result$spearman_r_sepi), 0, result$spearman_r_sepi))
  ), ]

  if (write_csv) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    out_file <- file.path(out_dir, paste0("candidate_report_", country, ".csv"))
    utils::write.csv(result, out_file, row.names = FALSE)
    message("  Written: ", out_file, " (", nrow(result), " candidates)")
  }

  invisible(result)
}

# ---- Wrapper for all countries --------------------------------------------

#' Explore candidate indicators for all countries
#'
#' @param all_data     Named list of loaded country data frames (from load_all_data())
#' @param sepi_results Optional: named list of SEPI result data frames
#'                     (from compute_all_countries()), used for correlation signal
#' @param ...          Additional arguments passed to explore_country_candidates()
#'
#' @return Named list of candidate report data frames (one per country)
explore_all_candidates <- function(all_data,
                                   sepi_results = NULL,
                                   ...) {
  countries <- names(all_data)
  cat("\n========================================\n")
  cat(" Candidate Indicator Exploration\n")
  cat(" Variable pool: all numeric columns (excluding configured indicators)\n")
  cat("========================================\n")

  results <- lapply(rlang::set_names(countries), function(country) {
    cat("\n--", country_label(country), "--\n")
    sepi_data <- if (!is.null(sepi_results)) sepi_results[[country]] else NULL
    explore_country_candidates(country,
                               sepi_data    = sepi_data,
                               country_data = all_data[[country]],
                               ...)
  })
  invisible(results)
}


# ============================================================================
# Candidate Correlation Matrices
# ============================================================================

# ---- Private helpers -------------------------------------------------------

# Shorten category labels for in-plot annotations
.corr_short_cat <- function(x) {
  x <- gsub("\\s*Indicators?\\s*(\\(.*?\\))?", "", x)
  x <- gsub("Environmental & Remote Sensing", "Environment", x)
  x <- trimws(x)
  # Fall back to full name when regex strips everything (e.g. bare "Other")
  ifelse(nchar(x) == 0, "Other", x)
}

# Consistent thematic order across all countries
.corr_theme_order <- c(
  "Education Indicators",
  "Health Indicators",
  "Food Security Indicators",
  "Economic Indicators",
  "Environmental & Remote Sensing Indicators",
  "Demographics",
  "Conflict Indicators (ACLED)",
  "Conflict Indicators",
  "Other"
)

# Fixed colour palette — one colour per thematic category
.corr_cat_cols <- c(
  "Education Indicators"                      = "#377eb8",
  "Health Indicators"                         = "#4daf4a",
  "Food Security Indicators"                  = "#ff7f00",
  "Economic Indicators"                       = "#984ea3",
  "Environmental & Remote Sensing Indicators" = "#1b9e77",
  "Demographics"                              = "#e7298a",
  "Conflict Indicators (ACLED)"               = "#e41a1c",
  "Conflict Indicators"                       = "#e41a1c",
  "Other"                                     = "#737373"
)

# ---- Main function ---------------------------------------------------------

#' Plot a Spearman correlation matrix for candidate indicators
#'
#' Produces a ggplot2 heatmap of pairwise Spearman correlations between all
#' numeric candidate variables for one country (excluding configured indicators).
#' Variables are sorted by thematic category; thin dividers and coloured
#' group labels make category boundaries immediately legible.
#' Correlation values are printed inside cells where |r| >= 0.40.
#'
#' @param country       Country key, e.g. "kenya"
#' @param config        INDICATOR_CONFIG (defaults to global)
#' @param metadata_file Path to the global metadata CSV
#' @param out_dir       Directory to save the PNG figure
#' @param save          If TRUE, writes a PNG to out_dir
#' @param country_data  Pre-loaded data frame for this country
#'
#' @return A ggplot object (invisibly)
plot_candidate_correlation_matrix <- function(
    country,
    config        = VERSIONS$v1_equal_geometric$countries,
    metadata_file = GLOBAL_DATA$metadata_file,
    out_dir       = "outputs/figures",
    save          = TRUE,
    country_data  = NULL
) {
  country_cfg <- config[[country]]
  if (is.null(country_cfg)) stop("Country '", country, "' not found in config.")
  label <- country_label(country)

  raw_data <- if (!is.null(country_data)) {
    country_data
  } else {
    load_country_data(country)
  }
  n_obs <- nrow(raw_data)

  # ---- Identify candidate variables ------------------------------------------
  excluded   <- .get_excluded_cols(country_cfg)
  candidates <- setdiff(names(raw_data)[sapply(raw_data, is.numeric)], excluded)

  # Keep only numeric columns that have at least 3 non-missing values
  candidates <- candidates[vapply(candidates, function(v) {
    is.numeric(raw_data[[v]]) && sum(!is.na(raw_data[[v]])) >= 3L
  }, logical(1L))]

  if (length(candidates) < 2L) {
    message("  Too few numeric candidates for correlation matrix: ", country)
    return(invisible(NULL))
  }

  # ---- Load metadata -----------------------------------------------------------
  dict_df <- load_metadata(country, metadata_file = metadata_file)

  # ---- Build per-variable metadata table -------------------------------------
  meta <- data.frame(variable = candidates, stringsAsFactors = FALSE)

  meta$category <- vapply(meta$variable, function(v) {
    row <- dict_df[dict_df$variable == v, , drop = FALSE]
    if (nrow(row) > 0L && !is.na(row$category[1L])) row$category[1L] else "Other"
  }, character(1L))

  meta$description <- vapply(meta$variable, function(v) {
    row <- dict_df[dict_df$variable == v, , drop = FALSE]
    if (nrow(row) > 0L && !is.na(row$description[1L])) row$description[1L]
    else NA_character_
  }, character(1L))

  # Display label: first 5 words of description, or cleaned variable name
  meta$disp_label <- vapply(seq_len(nrow(meta)), function(i) {
    dsc <- meta$description[i]
    lbl <- if (!is.na(dsc) && nchar(trimws(dsc)) > 0L) {
      words <- strsplit(trimws(dsc), "\\s+")[[1L]]
      paste(words[seq_len(min(5L, length(words)))], collapse = " ")
    } else {
      tools::toTitleCase(gsub("[_.]", " ", meta$variable[i]))
    }
    if (nchar(lbl) > 30L) paste0(substr(lbl, 1L, 28L), "\u2026") else lbl
  }, character(1L))

  # Ensure unique labels
  dups <- duplicated(meta$disp_label)
  if (any(dups)) {
    meta$disp_label[dups] <- paste0(meta$disp_label[dups],
                                     " (", seq_len(sum(dups)), ")")
  }

  # ---- Sort variables by thematic category, then name -----------------------
  meta$cat_ord <- match(meta$category, .corr_theme_order)
  meta$cat_ord[is.na(meta$cat_ord)] <- length(.corr_theme_order)
  meta <- meta[order(meta$cat_ord, meta$variable), ]

  ordered_vars   <- meta$variable
  ordered_labels <- meta$disp_label
  n              <- length(ordered_vars)

  # ---- Compute Spearman correlation matrix -----------------------------------
  cor_mat <- tryCatch(
    stats::cor(as.matrix(raw_data[, ordered_vars, drop = FALSE]),
               use = "pairwise.complete.obs", method = "spearman"),
    error = function(e) {
      message("  Correlation computation failed for ", country,
              ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(cor_mat)) return(invisible(NULL))

  rownames(cor_mat) <- ordered_labels
  colnames(cor_mat) <- ordered_labels

  # ---- Convert to long form --------------------------------------------------
  df_long           <- as.data.frame(as.table(cor_mat), stringsAsFactors = FALSE)
  names(df_long)    <- c("y_var", "x_var", "r")
  df_long$x_var     <- factor(df_long$x_var, levels = ordered_labels)
  # Reverse y so variable 1 appears at the top
  df_long$y_var     <- factor(df_long$y_var, levels = rev(ordered_labels))

  # Cell text: correlation value for off-diagonal cells where |r| >= 0.40
  df_long$r_text <- ifelse(
    df_long$x_var == df_long$y_var | abs(df_long$r) < 0.40,
    "",
    sprintf("%.2f", df_long$r)
  )
  # White text on dark tiles, dark text on light tiles
  df_long$txt_col <- ifelse(abs(df_long$r) >= 0.65, "white", "grey15")

  # ---- Category group geometry -----------------------------------------------
  cat_rle      <- rle(meta$category)
  group_ends   <- cumsum(cat_rle$lengths)
  sep_idx      <- group_ends[-length(group_ends)]   # boundary after each group

  # Midpoint position (x-axis) for each category group label
  group_starts <- c(0L, group_ends[-length(group_ends)])
  group_mids   <- (group_starts + group_ends) / 2 + 0.5

  # Separator positions:
  #   x-axis: between integer positions sep_idx and sep_idx + 1
  #   y-axis: y is reversed, so variable i (1-based) sits at y = n - i + 1
  #           => separator between group ending at k and starting at k+1
  #              lies at y = n - k + 0.5
  vline_pos <- sep_idx + 0.5
  hline_pos <- n - sep_idx + 0.5

  # Colours and short names for the group label annotations
  group_col  <- unname(.corr_cat_cols[cat_rle$values])
  group_col[is.na(group_col)] <- "#737373"
  group_name <- .corr_short_cat(cat_rle$values)

  # ---- Build plot ------------------------------------------------------------
  p <- ggplot2::ggplot(df_long, ggplot2::aes(x = x_var, y = y_var, fill = r)) +

    # Heatmap tiles
    ggplot2::geom_tile(colour = "white", linewidth = 0.2) +

    # Correlation value labels (only for |r| >= 0.40, off-diagonal)
    ggplot2::geom_text(
      ggplot2::aes(label = r_text, colour = txt_col),
      size = 2.1, show.legend = FALSE
    ) +
    ggplot2::scale_colour_identity() +

    # Diverging fill: blue (negative) — white (zero) — red (positive)
    ggplot2::scale_fill_gradient2(
      low      = "#2166ac",
      mid      = "white",
      high     = "#d6604d",
      midpoint = 0,
      limits   = c(-1, 1),
      name     = "Spearman r",
      guide    = ggplot2::guide_colorbar(
        barwidth       = 10,
        barheight      = 0.8,
        title.position = "top",
        title.hjust    = 0.5
      )
    ) +

    # Category separator lines
    ggplot2::geom_vline(xintercept = vline_pos,
                        colour = "grey25", linewidth = 0.7) +
    ggplot2::geom_hline(yintercept = hline_pos,
                        colour = "grey25", linewidth = 0.7) +

    # Category group labels above the matrix (rendered with clip = "off")
    ggplot2::annotate(
      "text",
      x        = group_mids,
      y        = n + 1.3,
      label    = group_name,
      colour   = group_col,
      size     = 2.8,
      fontface = "bold",
      hjust    = 0.5,
      vjust    = 0
    ) +

    ggplot2::coord_cartesian(clip = "off") +

    ggplot2::labs(
      title    = paste("Candidate Indicator Correlations:", label),
      subtitle = paste0(
        "Spearman rank correlations  \u00b7  n\u2009=\u2009", n_obs,
        " regions  \u00b7  values shown for |r|\u2009\u2265\u20090.40"
      ),
      x = NULL,
      y = NULL
    ) +

    theme_sepi(base_size = 9) +
    ggplot2::theme(
      axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1,
                                               vjust = 1, size = 6.5),
      axis.text.y     = ggplot2::element_text(hjust = 1, size = 6.5),
      panel.grid      = ggplot2::element_blank(),
      legend.position = "bottom",
      # Extra top margin so the category label annotations clear the plot border
      plot.margin     = ggplot2::margin(t = 18, r = 5, b = 5, l = 5,
                                        unit = "mm")
    )

  # ---- Save ------------------------------------------------------------------
  if (save) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    fname   <- file.path(out_dir, paste0("corr_matrix_", country, ".png"))
    fig_dim <- max(9, n * 0.42 + 2)
    ggplot2::ggsave(fname, p,
                    width  = fig_dim,
                    height = fig_dim * 0.88 + 2,
                    dpi    = 150)
    message("  Saved: ", fname)
  }

  invisible(p)
}

# ---- Wrapper: correlation matrices for all countries ----------------------

#' Plot candidate correlation matrices for all configured countries
#'
#' Calls plot_candidate_correlation_matrix() for every country in
#' INDICATOR_CONFIG and saves one PNG per country to outputs/figures/.
#'
#' @param config   INDICATOR_CONFIG (defaults to global)
#' @param all_data Named list of loaded country data frames
#' @param ...      Additional arguments forwarded to
#'                 plot_candidate_correlation_matrix()
#'
#' @return Named list of ggplot objects (invisibly)
plot_all_correlation_matrices <- function(
    config   = VERSIONS$v1_equal_geometric$countries,
    all_data = NULL,
    ...
) {
  countries <- if (!is.null(all_data)) names(all_data) else names(config)

  cat("\n========================================\n")
  cat(" Candidate Correlation Matrices\n")
  cat("========================================\n")

  results <- lapply(rlang::set_names(countries), function(country) {
    cat("\n--", country_label(country), "--\n")
    plot_candidate_correlation_matrix(
      country,
      config       = config,
      country_data = if (!is.null(all_data)) all_data[[country]] else NULL,
      ...
    )
  })
  invisible(results)
}

