# ============================================================================
# Criterion Validity — Conflict Criterion (ACLED events per 100k population)
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
# rank correlation is therefore computed on the summed per-1k series directly.
# ============================================================================

# ---- IDP data loader -------------------------------------------------------

#' Load and within-country normalise IDP displacement data.
load_idp_data <- function(path = "data/socio-economic/criterion_validity_data.csv") {
  idp_raw <- read.csv(path, stringsAsFactors = FALSE)
  idp_raw |>
    dplyr::group_by(country) |>
    dplyr::mutate(
      pop_frac_norm = (pop_frac_idps - min(pop_frac_idps)) /
                     (max(pop_frac_idps) - min(pop_frac_idps))
    ) |>
    dplyr::ungroup()
}

# ---- Criterion function builders -------------------------------------------

#' Returns a criterion_fn for IDP displacement for one country.
idp_criterion_fn <- function(country, idp_data) {
  cc <- COUNTRY_CODE_MAP[[country]]
  function(sepi_df) {
    idp_country <- dplyr::filter(idp_data, country_code == cc)
    if (nrow(idp_country) == 0) {
      return(tibble::tibble(adm1_pcode      = character(),
                            criterion_value = numeric(),
                            criterion_norm  = numeric()))
    }
    tibble::tibble(
      adm1_pcode      = as.character(idp_country$adm1_pcode),
      criterion_value = idp_country$pop_frac_idps,
      criterion_norm  = idp_country$pop_frac_norm
    )
  }
}

#' Returns a criterion_fn for ACLED conflict events per 1k over a window.
conflict_criterion_fn <- function(window) {
  function(sepi_df) {
    years <- conflict_window_years(window)
    c_df  <- build_conflict_criterion(sepi_df, years)
    tibble::tibble(
      adm1_pcode      = c_df$adm1_pcode,
      criterion_value = c_df$conflict_per_1k,
      criterion_norm  = c_df$conflict_norm
    )
  }
}

# ---- Generic criterion validity helpers ------------------------------------

#' Spearman rho between SEPI and any criterion (displacement or conflict).
#'
#' @param sepi_results Named list of per-country SEPI data frames.
#' @param country      Country key (e.g. "kenya").
#' @param criterion_fn Function that takes a SEPI data frame and returns a
#'   tibble with columns adm1_pcode, criterion_norm.
#' @return list(rho, p, n, verdict)
criterion_validity <- function(sepi_results, country, criterion_fn) {
  sepi_df <- sepi_results[[country]]
  if (is.null(sepi_df))
    return(list(rho = NA_real_, p = NA_real_, n = 0L, verdict = "no data"))

  criterion <- criterion_fn(sepi_df)
  if (nrow(criterion) == 0)
    return(list(rho = NA_real_, p = NA_real_, n = 0L, verdict = "no data"))

  merged <- dplyr::inner_join(
    dplyr::select(sepi_df, adm1_pcode, sepi),
    dplyr::select(criterion, adm1_pcode, criterion_norm),
    by = "adm1_pcode"
  )

  if (nrow(merged) < 3)
    return(list(rho = NA_real_, p = NA_real_, n = nrow(merged),
                verdict = "insufficient data"))

  rho   <- stats::cor(merged$sepi, merged$criterion_norm,
                      method = "spearman", use = "complete.obs")
  p_val <- stats::cor.test(merged$sepi, merged$criterion_norm,
                            method = "spearman", exact = FALSE)$p.value

  verdict <- if (is.na(rho))      "insufficient data"
             else if (rho < -0.6) "SUPPORTED"
             else if (rho < 0)    "weak negative"
             else                 "NOT supported"

  list(rho = rho, p = p_val, n = nrow(merged), verdict = verdict)
}

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
#'   joins can still match them but Spearman skips them via complete-cases.
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

# ---- Plot builders ---------------------------------------------------------

#' Merged per-country table used by scatter builders.
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

  merged$conflict_per_100k <- merged$conflict_per_1k * 100

  rho   <- round(stats::cor(merged$sepi, merged$conflict_per_100k,
                            method = "spearman", use = "complete.obs"), 3)
  p_val <- stats::cor.test(merged$sepi, merged$conflict_per_100k,
                           method = "spearman", exact = FALSE)$p.value
  p_lab <- if (p_val < 0.001) "p < 0.001" else sprintf("p = %.3f", p_val)

  ggplot2::ggplot(merged,
      ggplot2::aes(x = sepi, y = conflict_per_100k, label = adm1_name)) +
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
      y     = "Conflict events per 100,000 population"
    ) +
    theme_sepi()
}

#' Save a 3-panel scatter figure for a single window.
save_conflict_scatter <- function(sepi_results, window, version) {
  panels <- list()
  for (country in names(sepi_results)) {
    merged <- prepare_conflict_match(sepi_results[[country]], window)
    p <- build_conflict_scatter_panel(country, merged)
    if (!is.null(p)) panels[[country]] <- p
  }
  if (length(panels) == 0) return(invisible(NULL))

  footnote <- paste(
    "ρ (rho): Spearman rank correlation coefficient between SEPI and conflict intensity.",
    "A negative value indicates that higher socio-economic conditions are associated with fewer conflict events.",
    "Conflict data: ACLED events per 100,000 population | summed over the displayed time window."
  )

  combined <- patchwork::wrap_plots(panels, ncol = length(panels)) +
    patchwork::plot_annotation(
      title    = sprintf("Criterion Validity: SEPI vs Conflict Intensity — %s",
                          conflict_window_label(window)),
      subtitle = "Spearman rank correlation | ACLED events per 100k population | matched ADM1 units",
      caption  = footnote,
      theme    = theme_sepi() +
        ggplot2::theme(
          plot.caption          = ggplot2::element_text(size = 7, colour = "grey40", hjust = 0),
          plot.caption.position = "plot"
        )
    )

  path <- versioned_output_path(
    version, "figures", "criterion_validity",
    sprintf("criterion_validity_scatter_conflict_%s", window)
  )
  ggplot2::ggsave(path, combined,
                  width = length(panels) * 5, height = 6.5, dpi = 150)
  message("Saved: ", path)
  invisible(path)
}

#' Save per-country scatter figures, each with 3 window panels (10y, 5y, 2025) side by side.
save_conflict_scatter_by_country <- function(sepi_results, version,
                                              windows = c("10y", "5y", "2025")) {
  conflict_footnote <- paste(
    "ρ (rho): Spearman rank correlation coefficient between SEPI and conflict intensity.",
    "A negative value indicates that higher socio-economic conditions are associated with fewer conflict events.",
    "Conflict data: ACLED events per 100,000 population | summed over the displayed time window."
  )

  for (country in names(sepi_results)) {
    panels <- list()
    for (w in windows) {
      merged <- prepare_conflict_match(sepi_results[[country]], w)
      p <- build_conflict_scatter_panel(country, merged)
      if (!is.null(p)) {
        panels[[w]] <- p + ggplot2::labs(title = conflict_window_label(w))
      }
    }
    if (length(panels) == 0) next

    combined <- patchwork::wrap_plots(panels, ncol = length(panels)) +
      patchwork::plot_annotation(
        title    = paste("Criterion Validity: SEPI vs Conflict Intensity —", country_label(country)),
        subtitle = "Spearman rank correlation | ACLED events per 100,000 population | matched ADM1 units",
        caption  = conflict_footnote,
        theme    = theme_sepi() +
          ggplot2::theme(
            plot.caption          = ggplot2::element_text(size = 7, colour = "grey40", hjust = 0),
            plot.caption.position = "plot"
          )
      )

    path <- versioned_output_path(
      version, "figures", "criterion_validity",
      paste0("criterion_validity_scatter_conflict_", country)
    )
    ggplot2::ggsave(path, combined, width = length(panels) * 5, height = 6.5, dpi = 150)
    message("Saved: ", path)
  }
}
