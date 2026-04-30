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

# ---- Criterion validity helpers --------------------------------------------

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

spearman_cor <- function(x, y) {
  rho   <- stats::cor(x, y, method = "spearman", use = "complete.obs")
  p_val <- stats::cor.test(x, y, method = "spearman", exact = FALSE)$p.value
  list(rho = rho, p = p_val)
}

spearman_verdict <- function(rho) {
  if (is.na(rho))      "insufficient data"
  else if (rho < -0.6) "SUPPORTED (rho < -0.6)"
  else if (rho < 0)    "weak negative — not conclusive"
  else                 "NOT supported (positive or near-zero)"
}

auc_verdict <- function(auc_val) {
  if (auc_val >= 0.80)      "GOOD discrimination (AUC >= 0.80)"
  else if (auc_val >= 0.70) "ACCEPTABLE discrimination (AUC >= 0.70)"
  else if (auc_val >= 0.60) "poor — weak discrimination"
  else                      "NO discrimination (near random)"
}

hotspot_threshold <- function(values) {
  threshold <- median(values)
  list(
    threshold = threshold,
    hotspot   = as.integer(values > threshold)
  )
}

compute_roc <- function(hotspot, sepi) {
  roc_obj <- pROC::roc(hotspot, sepi,
                        direction = ">", quiet = TRUE,
                        ci = TRUE, ci.method = "delong")
  auc_val <- as.numeric(pROC::auc(roc_obj))
  ci_vals  <- as.numeric(pROC::ci(roc_obj))
  list(roc_obj = roc_obj, auc = auc_val, ci_lo = ci_vals[1], ci_hi = ci_vals[3])
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
