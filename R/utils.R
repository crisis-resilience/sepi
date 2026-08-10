# ============================================================================
# SEPI Utility Functions
# ============================================================================

# ---- Aggregation helpers ---------------------------------------------------

aggregate_scores <- function(x, w = NULL, method = "arithmetic", floor = 0.001) {
  non_na <- !is.na(x)
  if (sum(non_na) == 0) return(NA_real_)

  x <- x[non_na]
  if (is.null(w)) {
    w <- rep(1, length(x))
  } else {
    w <- w[non_na]
  }
  w <- w / sum(w)

  switch(method,
    arithmetic = sum(x * w),
    geometric  = {
      x_f <- pmax(x, floor)
      exp(sum(w * log(x_f)))
    },
    stop("Unknown aggregation method: ", method)
  )
}

# ---- Missingness -----------------------------------------------------------

missingness_report <- function(data, indicators) {
  tibble(
    indicator   = indicators,
    n_total     = nrow(data),
    n_available = vapply(indicators, function(v) sum(!is.na(data[[v]])), integer(1)),
    n_missing   = n_total - n_available,
    pct_missing = round(n_missing / n_total * 100, 1)
  )
}

# ---- Output path helpers ---------------------------------------------------

# Build a versioned output path under outputs/{category}/{subdir}/ and ensure
# the directory exists. Filenames are suffixed with the version name so reruns
# of different versions do not overwrite each other.
#
# For cross-version outputs (e.g. comparators that operate on several versions
# at once), pass version = NULL to omit the suffix; the subdir still provides
# grouping.
versioned_output_path <- function(version, category, subdir, base_name, ext = "png") {
  dir <- file.path("outputs", category, subdir)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  suffix <- if (is.null(version)) "" else paste0("_", version$name)
  file.path(dir, sprintf("%s%s.%s", base_name, suffix, ext))
}

# ---- Pretty helpers --------------------------------------------------------

pillar_label <- function(name) {
  gsub("_", " ", name) |> tools::toTitleCase()
}

country_label <- function(name) {
  labels <- c(kenya = "Kenya", somalia = "Somalia", south_sudan = "South Sudan")
  unname(labels[name])
}

# ---- Shared constants ------------------------------------------------------

COUNTRY_CODE_MAP <- c(south_sudan = "SSD", kenya = "KEN", somalia = "SOM")
COUNTRIES        <- c("kenya", "somalia", "south_sudan")

# National capitals, used to mark them on the SEPI/Conflict maps.
CAPITAL_CITIES <- list(
  kenya       = list(name = "Nairobi",   lon = 36.8219, lat = -1.2921),
  somalia     = list(name = "Mogadishu", lon = 45.3182, lat = 2.0469),
  south_sudan = list(name = "Juba",      lon = 31.5825, lat = 4.8517)
)

# Secondary (non-capital) reference cities, marked on the SEPI/Conflict maps
# alongside the capital so readers have more than one anchor point to orient
# themselves against.
SECONDARY_CITIES <- list(
  kenya = list(
    list(name = "Mombasa", lon = 39.6682, lat = -4.0435),
    list(name = "Kisumu",  lon = 34.7680, lat = -0.0917)
  ),
  somalia = list(
    list(name = "Hargeisa", lon = 44.0770, lat = 9.5624),
    list(name = "Kismayo",  lon = 42.5454, lat = -0.3582)
  ),
  south_sudan = list(
    list(name = "Wau",     lon = 27.9898, lat = 7.7024),
    list(name = "Malakal", lon = 31.6605, lat = 9.5334)
  )
)

# ---- SEPI score classification (dashboard-matching legend) -----------------
# Fixed 5-class breakdown of any 0-1 SEPI/pillar score, plus a "No data"
# category — matches the legend used on the dashboard so static map outputs
# stay visually consistent with it.

SEPI_SCORE_BREAKS <- c(0, 0.2, 0.4, 0.6, 0.8, 1)

SEPI_SCORE_COLOURS <- c(
  "Very Low (0.0-0.2)"  = "#E31A1C",
  "Low (0.2-0.4)"       = "#F7941D",
  "Moderate (0.4-0.6)"  = "#FFC20E",
  "High (0.6-0.8)"      = "#4CAF50",
  "Very High (0.8-1.0)" = "#1B7837",
  "No data"             = "#D9D9D9"
)

# Bins a numeric 0-1 score vector into the SEPI_SCORE_COLOURS categories.
# NA scores (e.g. excluded regions) are mapped to "No data" rather than
# dropped, so they still render (in grey) instead of leaving gaps.
sepi_score_bin <- function(x) {
  labels <- setdiff(names(SEPI_SCORE_COLOURS), "No data")
  binned <- cut(x, breaks = SEPI_SCORE_BREAKS, labels = labels,
               include.lowest = TRUE, right = TRUE)
  binned <- factor(binned, levels = names(SEPI_SCORE_COLOURS))
  binned[is.na(x)] <- "No data"
  binned
}

# ---- Conflict event classification (dashboard-matching legend) -------------
# Fixed 5-class breakdown of raw conflict event counts, plus a "No data"
# category — matches the dashboard's Conflict Events legend.

CONFLICT_EVENT_BREAKS <- c(0, 3, 14, 56, 221, Inf)

CONFLICT_EVENT_COLOURS <- c(
  "Very Low (0-3)"   = "#FFFFB2",
  "Low (3-14)"       = "#FECC5C",
  "Moderate (14-56)" = "#FD8D3C",
  "High (56-221)"    = "#F03B20",
  "Very High (221+)" = "#BD0026",
  "No data"          = "#D9D9D9"
)

# Bins a numeric conflict-event-count vector into the CONFLICT_EVENT_COLOURS
# categories. NA counts are mapped to "No data" rather than dropped, so they
# still render (in grey) instead of leaving gaps.
conflict_event_bin <- function(x) {
  labels <- setdiff(names(CONFLICT_EVENT_COLOURS), "No data")
  binned <- cut(x, breaks = CONFLICT_EVENT_BREAKS, labels = labels,
               include.lowest = TRUE, right = TRUE)
  binned <- factor(binned, levels = names(CONFLICT_EVENT_COLOURS))
  binned[is.na(x)] <- "No data"
  binned
}

# Splits SEPI_SCORE_COLOURS/CONFLICT_EVENT_COLOURS-style names (e.g.
# "Very Low (0.0-0.2)") into parallel category/range character vectors, for
# build_score_legend_strip() (see visualise.R) which draws its own labels
# rather than using a ggplot2 guide.
split_category_range_names <- function(colour_names) {
  m <- regmatches(colour_names, regexec("^(.*) (\\([^)]*\\))$", colour_names))
  list(
    category = vapply(m, `[`, character(1), 2),
    range    = vapply(m, `[`, character(1), 3)
  )
}

# Builds an off-canvas data frame with one row per legend category, used with
# geom_point(inherit.aes = FALSE) + coord_sf(xlim/ylim = bbox) so that every
# category has a real (though invisible, clipped-out) data point behind its
# legend key. Works around a ggplot2 bug (still present as of 4.0.2) where
# scale_fill_manual(drop = FALSE) renders a blank/grey key for a factor level
# that has zero rows in that specific panel's actual plotted data — giving
# every map a consistent, fully-populated legend regardless of which
# categories that country/pillar's data actually contains.
phantom_legend_points <- function(bbox, colours, class_col = "class") {
  n <- length(colours)
  df <- data.frame(
    lon = rep(bbox[["xmin"]] - 1, n),
    lat = rep(bbox[["ymin"]] - 1, n)
  )
  df[[class_col]] <- factor(names(colours), levels = names(colours))
  df
}

# ---- Theme for plots -------------------------------------------------------

theme_sepi <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", hjust = 0),
      plot.subtitle    = ggplot2::element_text(colour = "grey40"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "bottom"
    )
}
