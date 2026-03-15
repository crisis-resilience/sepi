# ============================================================================
# Normalisation Functions
# ============================================================================
# All normalisers map indicators to a common scale where higher = better.
# Polarity is applied *before* normalisation so that negative-polarity
# indicators (higher raw value = worse outcome) are flipped.
# ============================================================================

# ---- Normalisation methods -------------------------------------------------

normalise_min_max <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[2] == rng[1]) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

normalise_z_score <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - m) / s
}

normalise_rank <- function(x) {
  r <- rank(x, na.last = "keep", ties.method = "average")
  n <- sum(!is.na(x))
  if (n <= 1) return(rep(0.5, length(x)))
  (r - 1) / (n - 1)
}

# ---- Polarity alignment + normalisation ------------------------------------

align_and_normalise <- function(x, polarity, method = "min_max") {
  if (polarity == -1) x <- -x

  switch(method,
    min_max = normalise_min_max(x),
    z_score = normalise_z_score(x),
    rank    = normalise_rank(x),
    stop("Unknown normalisation method: ", method)
  )
}

# ---- Apply to all indicators in a country ----------------------------------

normalise_country <- function(data, country_config, method = "min_max") {
  for (pillar in country_config$pillars) {
    for (i in seq_along(pillar$indicators)) {
      ind <- pillar$indicators[i]
      pol <- pillar$polarity[i]

      if (!ind %in% names(data)) {
        warning("Indicator '", ind, "' not found in data — skipping.")
        next
      }

      norm_col <- paste0(ind, "_norm")
      data[[norm_col]] <- align_and_normalise(data[[ind]], pol, method)
    }
  }
  data
}
