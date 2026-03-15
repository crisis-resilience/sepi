# ============================================================================
# SEPI Visualisation
# ============================================================================

library(ggplot2)
library(sf)
library(patchwork)

# ---- 1. Rankings bar chart -------------------------------------------------

plot_sepi_rankings <- function(sepi_result, country_name,
                               version_name = NULL, save = TRUE) {
  label <- country_label(country_name)
  subtitle <- if (!is.null(version_name)) version_name else
    attr(sepi_result, "sepi_version")

  df <- sepi_result |>
    dplyr::select(adm1_name, sepi) |>
    dplyr::filter(!is.na(sepi)) |>
    dplyr::arrange(sepi) |>
    dplyr::mutate(adm1_name = factor(adm1_name, levels = adm1_name))

  p <- ggplot(df, aes(x = sepi, y = adm1_name)) +
    geom_col(fill = "#2c7fb8", width = 0.7) +
    labs(
      title    = paste("SEPI Scores:", label),
      subtitle = subtitle,
      x = "SEPI Score (higher = better socio-economic conditions)",
      y = NULL
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_sepi()

  if (save) {
    dir.create(file.path("outputs", "figures"), showWarnings = FALSE, recursive = TRUE)
    fname <- file.path("outputs", "figures", paste0("rankings_", country_name, ".png"))
    ggsave(fname, p, width = 8, height = max(5, nrow(df) * 0.25 + 1), dpi = 150)
    message("Saved: ", fname)
  }
  p
}

# ---- 2. Pillar heatmap ----------------------------------------------------

plot_pillar_heatmap <- function(sepi_result, country_name,
                                 country_config, conflict_data = NULL,
                                 save = TRUE) {
  label       <- country_label(country_name)
  pillar_cols <- paste0("pillar_", names(country_config$pillars))

  df_long <- sepi_result |>
    dplyr::select(adm1_name, dplyr::all_of(pillar_cols)) |>
    tidyr::pivot_longer(-adm1_name, names_to = "pillar", values_to = "score") |>
    dplyr::mutate(
      pillar    = gsub("^pillar_", "", pillar) |> pillar_label(),
      adm1_name = factor(adm1_name,
                          levels = sepi_result$adm1_name[order(sepi_result$sepi)])
    )

  conflict_col <- "count_conflict_events_per_100k"
  if (!is.null(conflict_data) && conflict_col %in% names(conflict_data)) {
    raw <- conflict_data[[conflict_col]]
    rng <- range(raw, na.rm = TRUE)
    normalised <- if (diff(rng) > 0) 1 - (raw - rng[1]) / diff(rng) else rep(NA_real_, length(raw))

    df_conflict <- dplyr::tibble(
      adm1_name = factor(conflict_data$adm1_name,
                         levels = levels(df_long$adm1_name)),
      pillar    = "Conflict\n(per 100k)",
      score     = normalised
    )
    df_long <- dplyr::bind_rows(df_long, df_conflict)
  }

  p <- ggplot(df_long, aes(x = pillar, y = adm1_name, fill = score)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    scale_fill_distiller(palette = "RdYlGn", direction = 1,
                         limits = c(0, 1), na.value = "grey80",
                         name = "Score\n(higher = better)") +
    labs(
      title = paste("Pillar Scores:", label),
      x = NULL, y = NULL
    ) +
    theme_sepi() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))

  if (save) {
    dir.create(file.path("outputs", "figures"), showWarnings = FALSE, recursive = TRUE)
    n_regions <- dplyr::n_distinct(df_long$adm1_name)
    fname <- file.path("outputs", "figures", paste0("pillars_", country_name, ".png"))
    ggsave(fname, p, width = 7, height = max(5, n_regions * 0.25 + 1), dpi = 150)
    message("Saved: ", fname)
  }
  p
}

# ---- 3. SEPI vs Conflict scatter ------------------------------------------

plot_sepi_vs_conflict <- function(conflict_result, country_name,
                                   conflict_var = "count_conflict_events",
                                   per_capita = TRUE, save = TRUE) {
  label <- country_label(country_name)
  data  <- conflict_result$data

  y_var <- if (per_capita) paste0(conflict_var, "_per_100k") else conflict_var
  if (!y_var %in% names(data)) {
    warning("Variable '", y_var, "' not found.")
    return(invisible(NULL))
  }

  df <- data |>
    dplyr::select(adm1_name, sepi, dplyr::all_of(y_var)) |>
    dplyr::filter(!is.na(sepi), !is.na(.data[[y_var]]))

  rho <- tryCatch(
    stats::cor(df$sepi, df[[y_var]], method = "spearman"),
    error = function(e) NA
  )
  rho_label <- if (!is.na(rho)) paste0("rho = ", round(rho, 2)) else ""

  p <- ggplot(df, aes(x = sepi, y = .data[[y_var]])) +
    geom_point(size = 3, alpha = 0.8, colour = "#d95f02") +
    ggrepel::geom_text_repel(aes(label = adm1_name), size = 3, max.overlaps = 15) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", colour = "grey40") +
    labs(
      title    = paste("SEPI vs Conflict:", label),
      subtitle = rho_label,
      x = "SEPI Score",
      y = gsub("_", " ", y_var)
    ) +
    theme_sepi()

  if (save) {
    dir.create(file.path("outputs", "figures"), showWarnings = FALSE, recursive = TRUE)
    fname <- file.path("outputs", "figures", paste0("sepi_conflict_", country_name, ".png"))
    ggsave(fname, p, width = 8, height = 6, dpi = 150)
    message("Saved: ", fname)
  }
  p
}

# ---- 4. Version comparison: slope chart -----------------------------------

plot_version_comparison <- function(comparison, country_name, save = TRUE) {
  label  <- country_label(country_name)
  scores <- comparison[[country_name]]$scores

  rank_cols <- grep("^rank_", names(scores), value = TRUE)
  if (length(rank_cols) < 2) {
    message("Need at least 2 versions to compare.")
    return(invisible(NULL))
  }

  df_long <- scores |>
    dplyr::select(adm1_name, dplyr::all_of(rank_cols)) |>
    tidyr::pivot_longer(-adm1_name, names_to = "version", values_to = "rank") |>
    dplyr::mutate(version = gsub("^rank_", "", version))

  p <- ggplot(df_long, aes(x = version, y = rank, group = adm1_name)) +
    geom_line(alpha = 0.5, colour = "grey50") +
    geom_point(size = 2) +
    ggrepel::geom_text_repel(
      data = dplyr::filter(df_long, version == rank_cols[1] |> gsub("^rank_", "", x = _)),
      aes(label = adm1_name), size = 2.5, nudge_x = -0.2
    ) +
    scale_y_reverse() +
    labs(
      title = paste("Rank Comparison:", label),
      x = "Version", y = "Rank (1 = best)"
    ) +
    theme_sepi()

  if (save) {
    dir.create(file.path("outputs", "figures"), showWarnings = FALSE, recursive = TRUE)
    fname <- file.path("outputs", "figures", paste0("version_comparison_", country_name, ".png"))
    ggsave(fname, p, width = 8, height = 7, dpi = 150)
    message("Saved: ", fname)
  }
  p
}

# ---- 5. Generate all standard plots for a run -----------------------------

generate_all_plots <- function(sepi_results, conflict_results,
                                config     = INDICATOR_CONFIG,
                                gis_config = GIS_CONFIG) {
  for (country in names(sepi_results)) {
    plot_sepi_rankings(sepi_results[[country]], country)
    plot_pillar_heatmap(sepi_results[[country]], country, config[[country]],
                        conflict_data = conflict_results[[country]]$data)
    plot_sepi_vs_conflict(conflict_results[[country]], country)

    if (!is.null(gis_config[[country]])) {
      plot_sepi_map(sepi_results[[country]], country,
                   conflict_data = conflict_results[[country]]$data,
                   gis_config    = gis_config)
      plot_pillar_maps(sepi_results[[country]], country,
                       config[[country]],
                       conflict_data = conflict_results[[country]]$data,
                       gis_config    = gis_config)
    }
  }
}

# ---- 6. GIS helpers --------------------------------------------------------

#' Load and normalise an ADM1 shapefile for a given country.
#' Renames the country-specific pcode/name columns to the standard
#' 'adm1_pcode' / 'adm1_name_shp' used by the join below.
load_adm1_sf <- function(country_name, gis_config = GIS_CONFIG) {
  cfg <- gis_config[[country_name]]
  if (is.null(cfg)) stop("No GIS config found for country: ", country_name)
  if (!file.exists(cfg$adm1_shp))
    stop("Shapefile not found: ", cfg$adm1_shp)

  shp <- sf::st_read(cfg$adm1_shp, quiet = TRUE)

  names(shp)[names(shp) == cfg$pcode_col] <- "adm1_pcode"
  names(shp)[names(shp) == cfg$name_col]  <- "adm1_name_shp"
  shp
}

# ---- 7. SEPI + Conflict side-by-side choropleth maps ----------------------
#
# Produces a two-panel plot:
#   Left  — SEPI score (0–1, RdYlGn, higher = better)
#   Right — Conflict events per 100k (RdYlGn inverted, lower = better)
#
# conflict_data: data frame with adm1_pcode + count_conflict_events_per_100k.
#   When NULL only the SEPI panel is drawn (single map, backward-compatible).
#
# conflict_trans: "log1p" (default) applies log(x+1) before mapping to colour,
#   which spreads the scale across all regions when the distribution is
#   right-skewed (typical for conflict counts). Use "none" for raw values.

plot_sepi_map <- function(sepi_result, country_name,
                           conflict_data  = NULL,
                           conflict_trans = c("log1p", "none"),
                           gis_config     = GIS_CONFIG,
                           save           = TRUE) {
  label          <- country_label(country_name)
  shp            <- load_adm1_sf(country_name, gis_config)
  subtitle       <- attr(sepi_result, "sepi_version")
  conflict_trans <- match.arg(conflict_trans)

  theme_map <- theme_sepi() +
    theme(
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      panel.grid.major = element_blank()
    )

  # --- SEPI panel ---
  sepi_data <- dplyr::left_join(
    shp,
    dplyr::select(sepi_result, adm1_pcode, adm1_name, sepi),
    by = "adm1_pcode"
  )

  p_sepi <- ggplot(sepi_data) +
    geom_sf(aes(fill = sepi), colour = "white", linewidth = 0.3) +
    scale_fill_distiller(
      palette  = "RdYlGn", direction = 1,
      limits   = c(0, 1),  na.value  = "grey80",
      name     = "Score\n(0–1)"
    ) +
    labs(title    = "SEPI Score",
         subtitle = "Higher = better conditions") +
    theme_map

  # --- Conflict panel (optional) ---
  conflict_col <- "count_conflict_events_per_100k"

  if (!is.null(conflict_data) && conflict_col %in% names(conflict_data)) {
    conf_df <- dplyr::left_join(
      shp,
      dplyr::select(conflict_data, adm1_pcode,
                    conflict = dplyr::all_of(conflict_col)),
      by = "adm1_pcode"
    )

    legend_label    <- "Events\nper 100k"
    conflict_subtitle <- "Conflict events per 100k population"

    if (conflict_trans == "log1p") {
      conf_df        <- dplyr::mutate(conf_df, conflict = log1p(conflict))
      legend_label   <- "Events per 100k\n(log scale)"
      conflict_subtitle <- paste(conflict_subtitle, "[log\u2081\u208a\u2081 transformed]")
    }

    p_conflict <- ggplot(conf_df) +
      geom_sf(aes(fill = conflict), colour = "white", linewidth = 0.3) +
      scale_fill_distiller(
        palette  = "RdYlGn", direction = -1,
        na.value = "grey80",
        name     = legend_label
      ) +
      labs(title    = "Conflict Events",
           subtitle = conflict_subtitle) +
      theme_map

    combined <- (p_sepi | p_conflict) +
      plot_annotation(
        title    = label,
        subtitle = subtitle,
        theme    = theme(
          plot.title    = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(colour = "grey40", size = 10)
        )
      )
  } else {
    combined <- p_sepi +
      plot_annotation(
        title    = paste("SEPI Scores:", label),
        subtitle = subtitle
      )
  }

  if (save) {
    dir.create(file.path("outputs", "maps"), showWarnings = FALSE, recursive = TRUE)
    fname <- file.path("outputs", "maps", paste0("map_sepi_", country_name, ".png"))
    ggsave(fname, combined, width = 14, height = 7, dpi = 150)
    message("Saved: ", fname)
  }
  combined
}

# ---- 8. Pillar choropleth maps + conflict panel ----------------------------
#
# Builds one map per pillar (RdYlGn 0–1, higher = better) plus an optional
# conflict map in the bottom-right corner (RdYlGn inverted, log1p-transformed).
# Panels are composed with patchwork::wrap_plots() so each can carry its own
# independent colour scale.

plot_pillar_maps <- function(sepi_result, country_name, country_config,
                              conflict_data  = NULL,
                              conflict_trans = c("log1p", "none"),
                              gis_config     = GIS_CONFIG,
                              save           = TRUE) {
  label          <- country_label(country_name)
  pillar_cols    <- paste0("pillar_", names(country_config$pillars))
  shp            <- load_adm1_sf(country_name, gis_config)
  conflict_trans <- match.arg(conflict_trans)

  theme_map <- theme_sepi() +
    theme(
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      panel.grid.major = element_blank(),
      plot.title       = element_text(size = 10, face = "bold"),
      plot.subtitle    = element_text(size = 8)
    )

  # --- One map per pillar ---
  pillar_plots <- lapply(pillar_cols, function(col) {
    pname <- gsub("^pillar_", "", col) |> pillar_label()
    df    <- dplyr::left_join(
      shp,
      dplyr::select(sepi_result, adm1_pcode, score = dplyr::all_of(col)),
      by = "adm1_pcode"
    )
    ggplot(df) +
      geom_sf(aes(fill = score), colour = "white", linewidth = 0.3) +
      scale_fill_distiller(
        palette = "RdYlGn", direction = 1,
        limits  = c(0, 1),  na.value  = "grey80",
        name    = "Score\n(0–1)"
      ) +
      labs(title = pname) +
      theme_map
  })

  # --- Conflict panel ---
  conflict_col <- "count_conflict_events_per_100k"
  has_conflict <- !is.null(conflict_data) &&
                  conflict_col %in% names(conflict_data)

  if (has_conflict) {
    conf_df <- dplyr::left_join(
      shp,
      dplyr::select(conflict_data, adm1_pcode,
                    conflict = dplyr::all_of(conflict_col)),
      by = "adm1_pcode"
    )
    legend_label <- "Events\nper 100k"
    if (conflict_trans == "log1p") {
      conf_df      <- dplyr::mutate(conf_df, conflict = log1p(conflict))
      legend_label <- "Events per 100k\n(log scale)"
    }
    p_conflict <- ggplot(conf_df) +
      geom_sf(aes(fill = conflict), colour = "white", linewidth = 0.3) +
      scale_fill_distiller(
        palette  = "RdYlGn", direction = -1,
        na.value = "grey80",
        name     = legend_label
      ) +
      labs(title = "Conflict Events") +
      theme_map

    all_panels <- c(pillar_plots, list(p_conflict))
  } else {
    all_panels <- pillar_plots
  }

  n_panels  <- length(all_panels)
  ncol_wrap <- min(3L, n_panels)

  combined <- patchwork::wrap_plots(all_panels, ncol = ncol_wrap) +
    plot_annotation(
      title = paste("Pillar Scores:", label),
      theme = theme(
        plot.title = element_text(face = "bold", size = 14)
      )
    )

  if (save) {
    dir.create(file.path("outputs", "maps"), showWarnings = FALSE, recursive = TRUE)
    fname <- file.path("outputs", "maps", paste0("map_pillars_", country_name, ".png"))
    w <- ncol_wrap * 3.5
    h <- ceiling(n_panels / ncol_wrap) * 3.5 + 1
    ggsave(fname, combined, width = w, height = max(h, 5), dpi = 150)
    message("Saved: ", fname)
  }
  combined
}
