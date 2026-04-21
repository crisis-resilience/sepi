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
MIN_N_ROC        <- 8L

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
