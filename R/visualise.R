# ============================================================================
# SEPI Visualisation
# ============================================================================

library(ggplot2)
library(sf)
library(patchwork)

resolve_conflict_plot_var <- function(conflict_var, per_capita = TRUE) {
  if (!per_capita) {
    return(conflict_var)
  }

  # Strip any year suffix (e.g. _2025) to build the base name, then re-add suffix
  base <- sub("_\\d{4}$", "", conflict_var)
  suffix <- regmatches(conflict_var, regexpr("_\\d{4}$", conflict_var))
  suffix <- if (length(suffix) == 0) "" else suffix

  rate_lookup <- c(
    count_conflict_events_2025 = "count_conflicts_events_per_1k_2025",
    total_fatalities_2025 = "total_fatalities_per_1k_2025"
  )

  resolved_base <- unname(rate_lookup[base])
  if (length(resolved_base) == 0 || is.na(resolved_base)) {
    conflict_var
  } else {
    paste0(resolved_base, suffix)
  }
}

# Helper: find the per-1k conflict events column in a data frame,
# regardless of year suffix (e.g. _2025 or none).
.detect_conflict_col <- function(data) {
  cols <- names(data)
  # Prefer year-suffixed first (most specific), then unsuffixed fallback
  hit <- grep("^count_conflicts_events_per_1k", cols, value = TRUE)
  if (length(hit) > 0) hit[1] else NULL
}

# ---- 1. Rankings bar chart -------------------------------------------------

plot_sepi_rankings <- function(sepi_result, country_name,
                               version = NULL, version_name = NULL,
                               save = TRUE) {
  label <- country_label(country_name)

  df <- sepi_result |>
    dplyr::select(adm1_name, sepi) |>
    dplyr::filter(!is.na(sepi)) |>
    dplyr::arrange(sepi) |>
    dplyr::mutate(adm1_name = factor(adm1_name, levels = adm1_name))

  p <- ggplot(df, aes(x = sepi, y = adm1_name)) +
    geom_col(fill = "#2c7fb8", width = 0.7) +
    labs(
      title = paste("SEPI Scores:", label),
      x     = "SEPI Score (higher = better socio-economic conditions)",
      y     = NULL
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_sepi()

  if (save) {
    fname <- versioned_output_path(
      version, "figures", "rankings",
      paste0("rankings_", country_name)
    )
    ggsave(fname, p, width = 8, height = max(5, nrow(df) * 0.25 + 1), dpi = 150)
    message("Saved: ", fname)
  }
  p
}

# ---- 2. Pillar heatmap ----------------------------------------------------

plot_pillar_heatmap <- function(sepi_result, country_name,
                                country_config, conflict_data = NULL,
                                version = NULL, save = TRUE) {
  label <- country_label(country_name)
  # For v1/v2 use pillars definition; for v2 use pillar_* columns present in data
  if (!is.null(country_config$pillars)) {
    pillar_cols <- paste0("pillar_", names(country_config$pillars))
  } else {
    pillar_cols <- grep("^pillar_", names(sepi_result), value = TRUE)
  }

  df_long <- sepi_result |>
    dplyr::select(adm1_name, dplyr::all_of(pillar_cols)) |>
    tidyr::pivot_longer(-adm1_name, names_to = "pillar", values_to = "score") |>
    dplyr::mutate(
      pillar = gsub("^pillar_", "", pillar) |> pillar_label(),
      adm1_name = factor(adm1_name,
        levels = sepi_result$adm1_name[order(sepi_result$sepi)]
      )
    )

  p <- ggplot(df_long, aes(x = pillar, y = adm1_name, fill = score)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    scale_fill_distiller(
      palette = "RdYlGn", direction = 1,
      limits = c(0, 1), na.value = "grey80",
      name = "Score\n(higher = better)"
    ) +
    labs(
      title   = paste("Pillar Scores:", label),
      x       = NULL,
      y       = NULL,
      caption = "Scores are normalised to a 0–1 scale. Higher values indicate better socio-economic conditions."
    ) +
    theme_sepi() +
    theme(
      axis.text.x   = element_text(angle = 35, hjust = 1),
      plot.caption  = element_text(size = 7, colour = "grey40", hjust = 0),
      legend.title  = element_text(margin = margin(b = 16))
    )

  if (save) {
    n_regions <- dplyr::n_distinct(df_long$adm1_name)
    fname <- versioned_output_path(
      version, "figures", "pillars",
      paste0("pillars_", country_name)
    )
    ggsave(fname, p, width = 7, height = max(5, n_regions * 0.25 + 1), dpi = 150)
    message("Saved: ", fname)
  }
  p
}

# ---- 3. SEPI vs Conflict scatter ------------------------------------------

plot_sepi_vs_conflict <- function(conflict_result, country_name,
                                  conflict_var = "count_conflict_events_2025",
                                  per_capita = TRUE,
                                  version = NULL, save = TRUE) {
  label <- country_label(country_name)
  data <- conflict_result$data

  # Auto-detect conflict column when not specified
  if (is.null(conflict_var)) {
    conflict_var <- .detect_conflict_col(data)
    if (is.null(conflict_var)) {
      warning("No conflict events per 1k column found.")
      return(invisible(NULL))
    }
    per_capita <- FALSE # column is already per-1k
  }

  y_var <- resolve_conflict_plot_var(conflict_var, per_capita = per_capita)
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
      x        = "SEPI Score",
      y        = "Conflict Events 2025 (count)",
      caption  = "ρ (rho): Spearman rank correlation coefficient, measuring the monotonic relationship between SEPI scores and conflict events.\nValues range from -1 (perfect negative) to +1 (perfect positive); values closer to -1 indicate that higher socio-economic conditions are associated with fewer conflict events."
    ) +
    theme_sepi() +
    theme(plot.caption = element_text(size = 7, colour = "grey40", hjust = 0))

  if (save) {
    fname <- versioned_output_path(
      version, "figures", "sepi_conflict",
      paste0("sepi_conflict_", country_name)
    )
    ggsave(fname, p, width = 8, height = 6, dpi = 150)
    message("Saved: ", fname)
  }
  p
}

# ---- 4. Version comparison: slope chart -----------------------------------

plot_version_comparison <- function(comparison, country_name,
                                    version = NULL, save = TRUE) {
  label <- country_label(country_name)
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
    fname <- versioned_output_path(
      version, "figures", "version_comparison",
      paste0("version_comparison_", country_name)
    )
    ggsave(fname, p, width = 8, height = 7, dpi = 150)
    message("Saved: ", fname)
  }
  p
}

# ---- 5. Generate all standard plots for a run -----------------------------

generate_all_plots <- function(sepi_results, conflict_results,
                               version,
                               gis_config = GIS_CONFIG) {
  for (country in names(sepi_results)) {
    country_config <- version$countries[[country]]
    plot_sepi_rankings(sepi_results[[country]], country, version = version)
    plot_pillar_heatmap(sepi_results[[country]], country, country_config,
      conflict_data = conflict_results[[country]]$data,
      version       = version
    )
    plot_sepi_vs_conflict(conflict_results[[country]], country,
      version = version
    )

    if (!is.null(gis_config[[country]])) {
      plot_sepi_map(sepi_results[[country]], country,
        conflict_data = conflict_results[[country]]$data,
        gis_config    = gis_config,
        version       = version
      )
      plot_pillar_maps(sepi_results[[country]], country,
        country_config,
        conflict_data = conflict_results[[country]]$data,
        gis_config    = gis_config,
        version       = version
      )
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
  if (!file.exists(cfg$adm1_shp)) {
    stop("Shapefile not found: ", cfg$adm1_shp)
  }

  shp <- sf::st_read(cfg$adm1_shp, quiet = TRUE)

  names(shp)[names(shp) == cfg$pcode_col] <- "adm1_pcode"
  names(shp)[names(shp) == cfg$name_col] <- "adm1_name_shp"
  shp
}

#' Load the ADM0 (national boundary) shapefile for a given country.
load_adm0_sf <- function(country_name, gis_config = GIS_CONFIG) {
  cfg <- gis_config[[country_name]]
  if (is.null(cfg)) stop("No GIS config found for country: ", country_name)
  if (!file.exists(cfg$adm0_shp)) {
    stop("Shapefile not found: ", cfg$adm0_shp)
  }
  sf::st_read(cfg$adm0_shp, quiet = TRUE)
}

# ---- 7. SEPI + Conflict side-by-side choropleth maps ----------------------
#
# Produces a two-panel plot:
#   Left  — SEPI score, 5-class dashboard legend (Very Low..Very High + No data)
#   Right — Conflict events, 5-class dashboard legend (same style)
#
# conflict_data: data frame with adm1_pcode + count_conflict_events_2025.
#   When NULL only the SEPI panel is drawn (single map, backward-compatible).

#' A custom colour-ramp legend strip: each break gets a category name
#' ("Low" / "Moderate" / "Very High") with its numeric range as a smaller
#' second line directly underneath -- category and range are one plotmath
#' label (built with atop()), not two separately-positioned text layers, so
#' rotating it is just rotating that whole two-line block as a single rigid
#' unit. That guarantees the range always sits exactly under its category
#' regardless of either string's length, the same way a two-line paragraph
#' keeps its second line under its first no matter how you tilt the page.
#'
#' Not achievable with guide_colorbar()'s native tick labels -- those support
#' only a single label per break. This builds the ramp + labels as plain
#' layers instead, then gets stacked under the map panel with patchwork
#' (see plot_sepi_map()) rather than relying on ggplot2's automatic legend.
build_score_legend_strip <- function(colours, positions, category_labels, range_labels,
                                     text_size = 4, line_spacing = 0.85, side_pad = 0.05) {
  ramp_colours <- grDevices::colorRampPalette(colours)(200)
  xs <- seq(0, 1, length.out = 200)
  step <- xs[2] - xs[1]
  bar_df <- data.frame(x = xs, xend = xs + step, colour = ramp_colours)

  # atop("Very Low", scriptstyle("(0.0-0.2)")) renders as one two-line grob:
  # bigger category name on top, smaller range underneath, both part of the
  # same label -- line_spacing (passed through as lineheight) sets the gap
  # between those two lines so they read clearly without touching.
  label_expr <- do.call(expression, Map(
    function(cat, rng) parse(text = sprintf('atop("%s", scriptstyle("%s"))', cat, rng))[[1]],
    category_labels, range_labels
  ))
  label_df <- data.frame(x = positions)
  label_df$label <- label_expr

  ggplot() +
    geom_rect(
      data = bar_df, aes(xmin = x, xmax = xend, ymin = 0.55, ymax = 1, fill = colour),
      colour = NA
    ) +
    scale_fill_identity() +
    geom_text(
      # y = 0.40 (rather than sitting flush on the bar's 0.55 bottom edge)
      # leaves a visible gap so the label's top doesn't brush the ramp.
      data = label_df, aes(x = x, y = 0.40, label = label),
      size = text_size, angle = 70, hjust = 1, vjust = 0.5, lineheight = line_spacing
    ) +
    # side_pad adds blank margin on both sides of the 0-1 bar, in bar-width
    # units -- since this strip's panel is width-matched to the map panel
    # above it (see plot_sepi_map()), a wide-aspect country (e.g. South
    # Sudan) would otherwise stretch the bar itself very wide; a bigger
    # side_pad keeps the bar's rendered width comparable across countries by
    # padding it with more surrounding blank space instead.
    coord_cartesian(xlim = c(-side_pad, 1 + side_pad), ylim = c(-0.85, 1.05), clip = "off") +
    theme_void()
}

plot_sepi_map <- function(sepi_result, country_name,
                          conflict_data = NULL,
                          gis_config = GIS_CONFIG,
                          version = NULL,
                          save = TRUE,
                          dpi = 300) {
  shp <- load_adm1_sf(country_name, gis_config)
  bbox <- sf::st_bbox(shp)
  map_xlim <- c(bbox[["xmin"]], bbox[["xmax"]])
  map_ylim <- c(bbox[["ymin"]], bbox[["ymax"]])

  # coord_sf() locks the true geographic aspect ratio, so a narrow/tall
  # country (e.g. Somalia) rendered into a wide fixed panel leaves large
  # blank margins on both sides — visually reading as "shifted right"
  # relative to the left-aligned title. Size the figure width to each
  # country's actual aspect ratio (accounting for the latitude-dependent
  # longitude scaling coord_sf itself uses) so the map fills its panel
  # instead of floating centred in extra whitespace.
  mean_lat <- mean(map_ylim)
  map_aspect <- (diff(map_xlim) * cos(mean_lat * pi / 180)) / diff(map_ylim)

  capital <- CAPITAL_CITIES[[country_name]]
  capital_df <- data.frame(
    lon = capital$lon, lat = capital$lat,
    marker_label = "National Capital", city_name = capital$name
  )

  # Secondary (non-capital) reference cities — same shape as the capital's
  # star/label, just smaller, giving readers extra anchor points without
  # competing with the capital for attention.
  secondary <- SECONDARY_CITIES[[country_name]]
  secondary_df <- if (length(secondary) > 0) {
    do.call(rbind, lapply(secondary, function(city) {
      data.frame(
        lon = city$lon, lat = city$lat,
        marker_label = "Major City", city_name = city$name
      )
    }))
  } else {
    capital_df[0, ]
  }

  city_label <- function(df, size, fontface) {
    geom_text(
      data = df, aes(x = lon, y = lat, label = city_name),
      inherit.aes = FALSE, size = size, fontface = fontface, colour = "black",
      hjust = 0.5, vjust = -0.8
    )
  }
  # South Sudan's panel renders proportionally wider (its wide/short aspect
  # ratio inflates fig_width relative to fig_height -- see map_aspect below),
  # which shrinks these labels relative to the map compared to Kenya/Somalia;
  # bump their size there so city names stay readable.
  city_size_boost <- if (country_name == "south_sudan") 1.3 else 1

  # South Sudan's wide/short bbox inflates fig_width (see map_aspect below),
  # and since the legend strips are width-matched to the map panel above
  # them, their colour bars stretch out much wider than Kenya's/Somalia's at
  # the same relative scale -- pad them with more surrounding blank space to
  # keep the bar itself a comparable rendered width across countries.
  legend_side_pad <- if (country_name == "south_sudan") 0.55 else 0.05
  capital_label <- city_label(capital_df, size = 2.8 * city_size_boost, fontface = "bold")
  secondary_label <- if (nrow(secondary_df) > 0) {
    city_label(secondary_df, size = 2.2 * city_size_boost, fontface = "plain")
  } else {
    NULL
  }

  # `with_legend = FALSE` draws the same markers but with the shape scale's
  # guide suppressed rather than dropped, so it registers no legend at all —
  # the cleanest way to keep "National Capital"/"Major City" appearing in
  # only one panel's legend (mapping-then-show.legend=FALSE left a stray
  # empty legend entry, a ggplot2 4.0.2 quirk, same family as the
  # phantom-legend-key bug worked around elsewhere in this file).
  city_marker <- function(with_legend = TRUE) {
    cities_df <- rbind(capital_df, secondary_df)
    cities_df$marker_label <- factor(cities_df$marker_label,
      levels = c("National Capital", "Major City")
    )
    point <- geom_point(
      data = cities_df, aes(x = lon, y = lat, shape = marker_label, size = marker_label),
      colour = "black", fill = "white", stroke = 1.1,
      inherit.aes = FALSE
    )
    scales <- list(
      # Stacking the two keys vertically (one entry per row) keeps each
      # label on a single line and reads far more clearly than squeezing
      # "National Capital"/"Major City" side by side with wrapped two-line
      # text, while still keeping the legend narrow enough, alongside the
      # SEPI/Conflict colorbars, not to overflow the saved figure width.
      scale_shape_manual(
        name = NULL, values = c("National Capital" = 8, "Major City" = 16),
        guide = if (with_legend) {
          # order = 2 (paired with the fill scale's order = 1
          # below, plus legend.box = "vertical" in the theme)
          # stacks this legend directly under the SEPI/Conflict
          # colorbar instead of beside it as its own column.
          guide_legend(direction = "vertical", ncol = 1, order = 2)
        } else {
          "none"
        }
      ),
      scale_size_manual(values = c("National Capital" = 2.2, "Major City" = 1.4), guide = "none")
    )
    c(list(point, capital_label), if (!is.null(secondary_label)) list(secondary_label), scales)
  }

  theme_map <- theme_sepi() +
    theme(
      # theme_sepi() left-aligns titles (hjust = 0) by default; centering
      # here lines each panel's title up with its map instead of the far
      # left edge of the (wider, legend-driven) plot area.
      plot.title       = element_text(face = "bold", hjust = 0.5),
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      axis.title       = element_blank(),
      panel.grid.major = element_blank()
    )

  # --- SEPI panel ---
  sepi_data <- dplyr::left_join(
    shp,
    dplyr::select(sepi_result, adm1_pcode, adm1_name, sepi),
    by = "adm1_pcode"
  )
  # Middle Juba (SO27) has insufficient data for a reliable SEPI score —
  # render it as "No data" here too, matching the dashboard GeoJSON export
  # (see export_sepi_geojson()).
  if (country_name == "somalia") {
    sepi_data$sepi[sepi_data$adm1_pcode == "SO27"] <- NA_real_
  }

  p_sepi <- ggplot(sepi_data) +
    geom_sf(aes(fill = sepi), colour = "white", linewidth = 0.3) +
    # Continuous version of the same 5-colour SEPI_SCORE_COLOURS ramp — only
    # the min/mid/max points are labelled, which is far more compact than a
    # discrete legend with a swatch per range. guide = "none": the ramp's own
    # labels are drawn separately below via build_score_legend_strip(), since
    # guide_colorbar can't keep "Low" horizontal while rotating just its
    # range sub-line (see that function's comment).
    scale_fill_gradientn(
      colours = unname(SEPI_SCORE_COLOURS[1:5]),
      limits = c(0, 1),
      na.value = unname(SEPI_SCORE_COLOURS[["No data"]]),
      guide = "none"
    ) +
    city_marker(with_legend = TRUE) +
    coord_sf(xlim = map_xlim, ylim = map_ylim) +
    labs(title = "SEPI Score") +
    theme_map +
    theme(
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 10),
      legend.justification = "top"
    )

  sepi_labels <- split_category_range_names(names(SEPI_SCORE_COLOURS)[1:5])
  sepi_legend_strip <- build_score_legend_strip(
    colours = unname(SEPI_SCORE_COLOURS[1:5]),
    positions = c(0.1, 0.3, 0.5, 0.7, 0.9),
    category_labels = sepi_labels$category,
    range_labels = sepi_labels$range,
    side_pad = legend_side_pad
  )
  p_sepi <- p_sepi / sepi_legend_strip + patchwork::plot_layout(heights = c(1, 0.3))

  # --- Conflict panel (optional) ---
  conflict_col <- "count_conflict_events_2025"
  has_conflict <- !is.null(conflict_data) && !is.null(conflict_col) &&
    conflict_col %in% names(conflict_data)

  if (has_conflict) {
    conf_df <- dplyr::left_join(
      shp,
      dplyr::select(conflict_data, adm1_pcode,
        conflict = dplyr::all_of(conflict_col)
      ),
      by = "adm1_pcode"
    )
    # Fixed anchor points (0 / Low-Moderate boundary / Very High boundary),
    # matching CONFLICT_EVENT_BREAKS — the colour scale stays comparable
    # across countries rather than rescaling to each one's own max.
    conflict_upper <- max(c(CONFLICT_EVENT_BREAKS[5], conf_df$conflict), na.rm = TRUE)

    p_conflict <- ggplot(conf_df) +
      geom_sf(aes(fill = conflict), colour = "white", linewidth = 0.3) +
      # Continuous version of the same 5-colour CONFLICT_EVENT_COLOURS ramp,
      # log1p-transformed since event counts are heavily right-skewed —
      # only the min/mid/max points are labelled to save space.
      scale_fill_gradientn(
        colours   = unname(CONFLICT_EVENT_COLOURS[1:5]),
        limits    = c(0, conflict_upper),
        transform = "log1p",
        na.value  = unname(CONFLICT_EVENT_COLOURS[["No data"]]),
        guide     = "none"
      ) +
      city_marker(with_legend = FALSE) +
      coord_sf(xlim = map_xlim, ylim = map_ylim) +
      # "(ACLED 2025)" left un-bold (plotmath plain()) against the bold
      # "Conflict Events", unlike the rest of the (uniformly bold) title --
      # theme's plot.title face="bold" would otherwise apply to the whole
      # string.
      labs(title = expression(bold("Conflict Events") ~ plain("(ACLED 2025)"))) +
      theme_map +
      theme(
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        # Matches p_sepi's top-justified legend so its colorbar lines up with
        # the SEPI colorbar rather than centring in the (patchwork-padded)
        # taller legend row shared with the SEPI panel's 2-guide stack.
        legend.justification = "top"
      )

    # Conflict's ramp is log1p-transformed, so the break positions along the
    # legend strip's 0-1 bar are NOT evenly spaced like SEPI's -- convert
    # through the same transform used by the fill scale above.
    conflict_breaks <- CONFLICT_EVENT_BREAKS[1:5]
    conflict_labels <- split_category_range_names(names(CONFLICT_EVENT_COLOURS)[1:5])
    # +0.04: with hjust = 1 (see build_score_legend_strip()), each rotated
    # label hangs down-LEFT from its tick -- fine when the leftmost tick has
    # room to its left, but conflict's first break sits at position 0 (the
    # bar's own left edge), so "Very Low" hung off the start of the bar
    # instead of sitting flush with it. Nudging every position right brings
    # the whole row back into alignment with the bar's left corner.
    conflict_legend_strip <- build_score_legend_strip(
      colours = unname(CONFLICT_EVENT_COLOURS[1:5]),
      positions = log1p(conflict_breaks) / log1p(conflict_upper) + 0.04,
      category_labels = conflict_labels$category,
      range_labels = conflict_labels$range,
      side_pad = legend_side_pad
    )
    p_conflict <- p_conflict / conflict_legend_strip +
      patchwork::plot_layout(heights = c(1, 0.3))

    # A spacer column between the two panels — otherwise long subtitle text
    # (e.g. "Higher SEPI score = Better structural conditions") can overflow
    # its panel's allotted width and visually run into the neighbour's title.
    combined <- (p_sepi | patchwork::plot_spacer() | p_conflict) +
      patchwork::plot_layout(widths = c(1, 0.06, 1))
  } else {
    combined <- p_sepi
  }

  if (save) {
    fname_png <- versioned_output_path(version, "maps", "sepi",
      paste0("map_sepi_", country_name),
      ext = "png"
    )
    fname_pdf <- versioned_output_path(version, "maps", "sepi",
      paste0("map_sepi_", country_name),
      ext = "pdf"
    )

    # ~72% of the figure height is the actual map panel (rest is
    # title/legend chrome); size each panel's width to that at the
    # country's true aspect ratio, so the map fills its column rather than
    # floating centred in blank space.
    fig_height <- 7
    panel_width <- fig_height * 0.72 * map_aspect
    fig_width <- if (has_conflict) panel_width * 2 * 1.08 else panel_width * 1.15
    fig_width <- max(min(fig_width, 20), 6)

    ggsave(fname_png, combined, width = fig_width, height = fig_height, dpi = dpi)
    ggsave(fname_pdf, combined, width = fig_width, height = fig_height, device = cairo_pdf)
    message("Saved: ", fname_png)
    message("Saved: ", fname_pdf)
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
                             conflict_data = NULL,
                             gis_config = GIS_CONFIG,
                             version = NULL,
                             save = TRUE,
                             dpi = 300) {
  label <- country_label(country_name)
  # For v1/v2 use pillars definition; for v2 use pillar_* columns present in data
  if (!is.null(country_config$pillars)) {
    pillar_cols <- paste0("pillar_", names(country_config$pillars))
  } else {
    pillar_cols <- grep("^pillar_", names(sepi_result), value = TRUE)
  }
  shp <- load_adm1_sf(country_name, gis_config)
  bbox <- sf::st_bbox(shp)
  map_xlim <- c(bbox[["xmin"]], bbox[["xmax"]])
  map_ylim <- c(bbox[["ymin"]], bbox[["ymax"]])

  theme_map <- theme_sepi() +
    theme(
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      axis.title       = element_blank(),
      panel.grid.major = element_blank(),
      plot.title       = element_text(size = 10, face = "bold"),
      plot.subtitle    = element_text(size = 8)
    )

  # --- One map per pillar ---
  pillar_plots <- lapply(pillar_cols, function(col) {
    pname <- gsub("^pillar_", "", col) |> pillar_label()
    df <- dplyr::left_join(
      shp,
      dplyr::select(sepi_result, adm1_pcode, score = dplyr::all_of(col)),
      by = "adm1_pcode"
    )
    df <- dplyr::mutate(df, score_class = sepi_score_bin(score))
    ggplot(df) +
      # Off-canvas points guaranteeing every category has real (though
      # invisible) data behind it — see phantom_legend_points().
      geom_point(
        data = phantom_legend_points(bbox, SEPI_SCORE_COLOURS, "score_class"),
        aes(x = lon, y = lat, fill = score_class),
        shape = 22, size = 3, colour = NA, inherit.aes = FALSE,
        show.legend = TRUE, key_glyph = "rect"
      ) +
      geom_sf(aes(fill = score_class), colour = "white", linewidth = 0.3) +
      scale_fill_manual(
        values = SEPI_SCORE_COLOURS,
        name   = "SEPI Score Ranges",
        # Single column with the title stacked on top (not beside the keys):
        # each panel here is narrow (~3.5in), and a title-beside-keys layout
        # is wide enough to bleed into neighbouring panels.
        guide  = guide_legend(ncol = 1, title.position = "top")
      ) +
      coord_sf(xlim = map_xlim, ylim = map_ylim) +
      labs(title = pname) +
      theme_map +
      theme(
        legend.text     = element_text(size = 7),
        legend.title    = element_text(size = 8),
        legend.key.size = unit(0.35, "cm")
      )
  })

  # --- Conflict panel ---
  conflict_col <- "count_conflict_events_2025"
  has_conflict <- !is.null(conflict_data) &&
    conflict_col %in% names(conflict_data)

  if (has_conflict) {
    conf_df <- dplyr::left_join(
      shp,
      dplyr::select(conflict_data, adm1_pcode,
        conflict = dplyr::all_of(conflict_col)
      ),
      by = "adm1_pcode"
    )
    conf_df <- dplyr::mutate(conf_df, conflict_class = conflict_event_bin(conflict))

    p_conflict <- ggplot(conf_df) +
      geom_point(
        data = phantom_legend_points(bbox, CONFLICT_EVENT_COLOURS, "conflict_class"),
        aes(x = lon, y = lat, fill = conflict_class),
        shape = 22, size = 3, colour = NA, inherit.aes = FALSE,
        show.legend = TRUE, key_glyph = "rect"
      ) +
      geom_sf(aes(fill = conflict_class), colour = "white", linewidth = 0.3) +
      scale_fill_manual(
        values = CONFLICT_EVENT_COLOURS,
        name   = "Conflict Events Color Scheme",
        guide  = guide_legend(ncol = 1, title.position = "top")
      ) +
      coord_sf(xlim = map_xlim, ylim = map_ylim) +
      labs(title = "Conflict Events") +
      theme_map +
      theme(
        legend.text     = element_text(size = 7),
        legend.title    = element_text(size = 8),
        legend.key.size = unit(0.35, "cm")
      )

    all_panels <- c(pillar_plots, list(p_conflict))
  } else {
    all_panels <- pillar_plots
  }

  n_panels <- length(all_panels)
  ncol_wrap <- min(3L, n_panels)

  combined <- patchwork::wrap_plots(all_panels, ncol = ncol_wrap) +
    plot_annotation(
      title = paste("Pillar Scores:", label),
      theme = theme(
        plot.title = element_text(face = "bold", size = 14)
      )
    )

  if (save) {
    fname_png <- versioned_output_path(version, "maps", "pillars",
      paste0("map_pillars_", country_name),
      ext = "png"
    )
    fname_pdf <- versioned_output_path(version, "maps", "pillars",
      paste0("map_pillars_", country_name),
      ext = "pdf"
    )
    w <- ncol_wrap * 3.5
    h <- ceiling(n_panels / ncol_wrap) * 3.5 + 1
    ggsave(fname_png, combined, width = w, height = max(h, 5), dpi = dpi)
    ggsave(fname_pdf, combined, width = w, height = max(h, 5), device = cairo_pdf)
    message("Saved: ", fname_png)
    message("Saved: ", fname_pdf)
  }
  combined
}
