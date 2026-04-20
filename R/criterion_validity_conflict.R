# ============================================================================
# Criterion Validity — Conflict Criterion (ACLED events per 1k population)
# ============================================================================
# Parallel criterion-validity test to the IDP displacement analysis in
# Section C–E of `04_evaluate.R`. The criterion variable here is the count of
# ACLED conflict events per 1,000 population, summed over a configurable time
# window:
#
#   "10y"  -> 2016–2025   (full available history)
#   "5y"   -> 2021–2025   (recent medium-term)
#   "2025" -> 2025 only   (circular for v3_conflict_weighted; still reported
#                          as a sanity check)
#
# Yearly per-capita event counts are already present in the merged country
# data as `count_conflicts_events_per_1k_YYYY`. Because population is fixed
# within the ADM1 unit, summing the per-1k yearly values is rank-equivalent
# to dividing the total event count over the window by population. Spearman
# rank correlation and ROC/AUC are therefore computed on the summed per-1k
# series directly.
# ============================================================================

# ---- Window resolver -------------------------------------------------------

#' Resolve a conflict-window label to the vector of years it covers.
#'
#' @param window One of "10y", "5y", "2025".
#' @return Integer vector of calendar years.
conflict_window_years <- function(window) {
  switch(window,
    "10y"  = 2016:2025,
    "5y"   = 2021:2025,
    "2025" = 2025L,
    stop("Unknown conflict window: ", window,
         " (expected one of '10y', '5y', '2025').")
  )
}

#' Human-readable label for a window.
conflict_window_label <- function(window) {
  switch(window,
    "10y"  = "Conflict (2016–2025)",
    "5y"   = "Conflict (2021–2025)",
    "2025" = "Conflict (2025)",
    window
  )
}

# ---- Criterion builder -----------------------------------------------------

#' Build the ACLED conflict criterion for a SEPI result frame.
#'
#' Sums the per-capita event columns across the requested window and adds a
#' within-country min-max normalised version for scatter display.
#'
#' @param sepi_df A country's SEPI result (must contain
#'   `count_conflicts_events_per_1k_YYYY` columns for the requested years
#'   and an `adm1_pcode` / `adm1_name` pair).
#' @param years Integer vector of years.
#' @return Data frame with columns `adm1_pcode`, `adm1_name`, `conflict_per_1k`,
#'   `conflict_norm`. Rows with entirely missing data are preserved so downstream
#'   joins can still match them but Spearman/ROC skip them via complete-cases.
build_conflict_criterion <- function(sepi_df, years) {
  cols <- paste0("count_conflicts_events_per_1k_", years)
  cols <- cols[cols %in% names(sepi_df)]

  if (length(cols) == 0) {
    return(tibble::tibble(
      adm1_pcode      = character(),
      adm1_name       = character(),
      conflict_per_1k = numeric(),
      conflict_norm   = numeric()
    ))
  }

  mat <- as.matrix(sepi_df[, cols, drop = FALSE])
  mat[is.na(mat)] <- 0  # missing year = 0 events (no ACLED match for that year)

  total <- rowSums(mat)

  rng   <- range(total, na.rm = TRUE)
  span  <- diff(rng)
  norm  <- if (is.finite(span) && span > 0) (total - rng[1]) / span else rep(0, length(total))

  tibble::tibble(
    adm1_pcode      = as.character(sepi_df$adm1_pcode),
    adm1_name       = as.character(sepi_df$adm1_name),
    conflict_per_1k = total,
    conflict_norm   = norm
  )
}

# ---- Spearman correlation --------------------------------------------------

#' Criterion validity (Spearman rho) of SEPI against the conflict criterion.
#'
#' Mirrors `criterion_validity()` in 05_compare_versions.R but sources the
#' criterion from `build_conflict_criterion()` rather than IDP data.
#'
#' @param sepi_results Named list of per-country SEPI data frames.
#' @param country     Country key (e.g. "kenya").
#' @param window      Conflict window label.
#' @return list(rho, p, n, verdict)
criterion_validity_conflict <- function(sepi_results, country, window) {
  sepi_df <- sepi_results[[country]]
  if (is.null(sepi_df)) {
    return(list(rho = NA_real_, p = NA_real_, n = 0L, verdict = "no data"))
  }

  years     <- conflict_window_years(window)
  criterion <- build_conflict_criterion(sepi_df, years)

  merged <- dplyr::inner_join(
    dplyr::select(sepi_df, adm1_pcode, sepi),
    dplyr::select(criterion, adm1_pcode, conflict_norm),
    by = "adm1_pcode"
  )

  if (nrow(merged) < 3) {
    return(list(rho = NA_real_, p = NA_real_, n = nrow(merged),
                verdict = "insufficient data"))
  }

  rho   <- stats::cor(merged$sepi, merged$conflict_norm,
                      method = "spearman", use = "complete.obs")
  p_val <- stats::cor.test(merged$sepi, merged$conflict_norm,
                           method = "spearman", exact = FALSE)$p.value

  verdict <- if (is.na(rho))      "insufficient data"
             else if (rho < -0.6) "SUPPORTED"
             else if (rho < 0)    "weak negative"
             else                 "NOT supported"

  list(rho = rho, p = p_val, n = nrow(merged), verdict = verdict)
}

# ---- Discriminatory capacity (ROC / AUC) -----------------------------------

#' AUC for SEPI predicting conflict hotspots (above-median events per 1k).
#'
#' Mirrors `auc_capacity()` in 05_compare_versions.R for the conflict criterion.
auc_capacity_conflict <- function(sepi_results, country, window, min_n = 8) {
  sepi_df <- sepi_results[[country]]
  if (is.null(sepi_df)) {
    return(list(auc = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                n = 0L, verdict = "no data"))
  }

  years     <- conflict_window_years(window)
  criterion <- build_conflict_criterion(sepi_df, years)

  merged <- dplyr::inner_join(
    dplyr::select(sepi_df, adm1_pcode, sepi),
    dplyr::select(criterion, adm1_pcode, conflict_per_1k),
    by = "adm1_pcode"
  )

  n_matched <- nrow(merged)
  if (n_matched < min_n) {
    return(list(auc = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                n = n_matched, verdict = "too few units"))
  }

  threshold      <- stats::median(merged$conflict_per_1k)
  merged$hotspot <- as.integer(merged$conflict_per_1k > threshold)
  n_hotspot      <- sum(merged$hotspot)

  if (n_hotspot < 2 || n_hotspot > (n_matched - 2)) {
    return(list(auc = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                n = n_matched, verdict = "class imbalance"))
  }

  roc_obj <- pROC::roc(merged$hotspot, merged$sepi,
                       direction = ">", quiet = TRUE,
                       ci = TRUE, ci.method = "delong")

  auc_val <- as.numeric(pROC::auc(roc_obj))
  ci_vals <- as.numeric(pROC::ci(roc_obj))

  verdict <- if (auc_val >= 0.80)      "GOOD (>=0.80)"
             else if (auc_val >= 0.70) "acceptable (>=0.70)"
             else if (auc_val >= 0.60) "poor (0.60-0.70)"
             else                      "no discrimination"

  list(auc = auc_val, ci_lo = ci_vals[1], ci_hi = ci_vals[3],
       n = n_matched, verdict = verdict)
}

# ---- Plot builders ---------------------------------------------------------

#' Merged per-country table used by both scatter and ROC builders.
#'
#' Returns a data frame of matched ADM1 units with SEPI, raw per-1k,
#' within-country min-max normalised per-1k, and hotspot flag.
prepare_conflict_match <- function(sepi_df, window) {
  years     <- conflict_window_years(window)
  criterion <- build_conflict_criterion(sepi_df, years)

  merged <- dplyr::inner_join(
    dplyr::select(sepi_df, adm1_pcode, adm1_name, sepi),
    dplyr::select(criterion, adm1_pcode, conflict_per_1k, conflict_norm),
    by = "adm1_pcode"
  )

  if (nrow(merged) > 0) {
    threshold <- stats::median(merged$conflict_per_1k, na.rm = TRUE)
    merged$hotspot <- as.integer(merged$conflict_per_1k > threshold)
  }
  merged
}

#' Single-country scatter panel (mirrors Section E1 of 04_evaluate.R).
build_conflict_scatter_panel <- function(country, merged) {
  if (nrow(merged) < 3) return(NULL)

  rho   <- round(stats::cor(merged$sepi, merged$conflict_norm,
                            method = "spearman", use = "complete.obs"), 3)
  p_val <- stats::cor.test(merged$sepi, merged$conflict_norm,
                           method = "spearman", exact = FALSE)$p.value
  p_lab <- if (p_val < 0.001) "p < 0.001" else sprintf("p = %.3f", p_val)

  ggplot2::ggplot(merged,
      ggplot2::aes(x = sepi, y = conflict_norm, label = adm1_name)) +
    ggplot2::geom_point(colour = "#d95f0e", size = 2.5, alpha = 0.8) +
    ggrepel::geom_text_repel(size = 2.8, colour = "grey30",
                             max.overlaps = 15, seed = 42) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, colour = "#2c7fb8",
                         linewidth = 0.8, alpha = 0.15) +
    ggplot2::annotate("text", x = Inf, y = Inf,
                      label = sprintf("Spearman \u03c1 = %s\n%s", rho, p_lab),
                      hjust = 1.1, vjust = 1.5, size = 3.2, colour = "grey20") +
    ggplot2::labs(
      title = country_label(country),
      x     = "SEPI score (higher = better)",
      y     = "Conflict events per 1k\n(within-country normalised)"
    ) +
    theme_sepi()
}

#' Single-country ROC panel (mirrors Section E2 of 04_evaluate.R).
build_conflict_roc_panel <- function(country, merged, min_n = 8) {
  n_matched <- nrow(merged)
  if (n_matched < min_n) return(NULL)

  n_hotspot <- sum(merged$hotspot)
  if (n_hotspot < 2 || n_hotspot > (n_matched - 2)) return(NULL)

  roc_obj <- pROC::roc(merged$hotspot, merged$sepi,
                       direction = ">", quiet = TRUE,
                       ci = TRUE, ci.method = "delong")
  auc_val <- as.numeric(pROC::auc(roc_obj))
  ci_vals <- as.numeric(pROC::ci(roc_obj))

  roc_df <- data.frame(
    specificity = roc_obj$specificities,
    sensitivity = roc_obj$sensitivities
  )

  threshold <- stats::median(merged$conflict_per_1k, na.rm = TRUE)

  ggplot2::ggplot(roc_df, ggplot2::aes(x = 1 - specificity, y = sensitivity)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         colour = "grey60", linewidth = 0.5) +
    ggplot2::geom_line(colour = "#d95f0e", linewidth = 1.1) +
    ggplot2::geom_area(fill = "#d95f0e", alpha = 0.1) +
    ggplot2::annotate("text", x = 0.65, y = 0.15,
                      label = sprintf("AUC = %.3f\n95%% CI: %.3f\u2013%.3f",
                                      auc_val, ci_vals[1], ci_vals[3]),
                      size = 3.3, colour = "grey20", hjust = 0) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::labs(
      title    = country_label(country),
      subtitle = sprintf("Hotspot = events/1k > %.4f (median)", threshold),
      x        = "1 - Specificity (False Positive Rate)",
      y        = "Sensitivity (True Positive Rate)"
    ) +
    theme_sepi()
}

#' Save a 3-panel scatter figure for a single window.
save_conflict_scatter <- function(sepi_results, window, out_dir) {
  panels <- list()
  for (country in names(sepi_results)) {
    merged <- prepare_conflict_match(sepi_results[[country]], window)
    p <- build_conflict_scatter_panel(country, merged)
    if (!is.null(p)) panels[[country]] <- p
  }
  if (length(panels) == 0) return(invisible(NULL))

  combined <- patchwork::wrap_plots(panels, ncol = length(panels)) +
    patchwork::plot_annotation(
      title    = sprintf("Criterion Validity: SEPI vs Conflict Intensity — %s",
                          conflict_window_label(window)),
      subtitle = "Spearman rank correlation | ACLED events per 1k population | matched ADM1 units",
      theme    = theme_sepi()
    )

  path <- file.path(out_dir,
                    sprintf("criterion_validity_scatter_conflict_%s.png", window))
  ggplot2::ggsave(path, combined,
                  width = length(panels) * 5, height = 6, dpi = 150)
  message("Saved: ", path)
  invisible(path)
}

#' Save a ROC figure for a single window (panels per country with n >= min_n).
save_conflict_roc <- function(sepi_results, window, out_dir, min_n = 8) {
  panels <- list()
  for (country in names(sepi_results)) {
    merged <- prepare_conflict_match(sepi_results[[country]], window)
    p <- build_conflict_roc_panel(country, merged, min_n = min_n)
    if (!is.null(p)) panels[[country]] <- p
  }
  if (length(panels) == 0) return(invisible(NULL))

  combined <- patchwork::wrap_plots(panels, ncol = length(panels)) +
    patchwork::plot_annotation(
      title    = sprintf("Discriminatory Capacity: ROC — %s",
                          conflict_window_label(window)),
      subtitle = "Can SEPI identify conflict hotspots? | Hotspot = above-median events per 1k",
      theme    = theme_sepi()
    )

  path <- file.path(out_dir,
                    sprintf("criterion_validity_roc_conflict_%s.png", window))
  ggplot2::ggsave(path, combined,
                  width = length(panels) * 5.5, height = 5.5, dpi = 150)
  message("Saved: ", path)
  invisible(path)
}
