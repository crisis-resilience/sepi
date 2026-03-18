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

# ---- Pretty helpers --------------------------------------------------------

pillar_label <- function(name) {
  gsub("_", " ", name) |> tools::toTitleCase()
}

country_label <- function(name) {
  labels <- c(kenya = "Kenya", somalia = "Somalia", south_sudan = "South Sudan")
  unname(labels[name])
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
