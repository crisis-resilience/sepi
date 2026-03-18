# ============================================================================
# Diagnostic Analysis
# ============================================================================
# Run before or alongside index computation to assess data quality, internal
# consistency, and indicator structure.
# ============================================================================

# ---- Missingness assessment ------------------------------------------------

diagnose_missingness <- function(data, country_config) {
  all_indicators <- unlist(lapply(country_config$pillars, `[[`, "indicators"))

  report <- missingness_report(data, all_indicators)
  report$pillar <- unlist(mapply(
    function(p, cfg) rep(p, length(cfg$indicators)),
    names(country_config$pillars),
    country_config$pillars,
    SIMPLIFY = FALSE
  ))

  report <- dplyr::select(report, pillar, dplyr::everything())
  report
}

# ---- Within-pillar correlations --------------------------------------------

pillar_correlations <- function(data, country_config) {
  purrr::imap(country_config$pillars, function(pillar, p_name) {
    inds <- pillar$indicators
    inds <- inds[inds %in% names(data)]

    if (length(inds) < 2) {
      return(NULL)
    }

    mat <- as.matrix(data[, inds, drop = FALSE])
    stats::cor(mat, use = "pairwise.complete.obs", method = "spearman")
  })
}

# ---- Cronbach's alpha per pillar -------------------------------------------

pillar_alpha <- function(data, country_config) {
  if (!requireNamespace("psych", quietly = TRUE)) {
    warning("Package 'psych' needed for Cronbach's alpha. Skipping.")
    return(NULL)
  }

  purrr::imap(country_config$pillars, function(pillar, p_name) {
    inds <- pillar$indicators
    inds <- inds[inds %in% names(data)]

    if (length(inds) < 2) {
      return(list(pillar = p_name, std_alpha = NA_real_, n_items = length(inds),
                  note = "Fewer than 2 indicators; alpha not computable."))
    }

    mat <- data[, inds, drop = FALSE] |> stats::na.omit()
    if (nrow(mat) < 3) {
      return(list(pillar = p_name, std_alpha = NA_real_, n_items = length(inds),
                  note = "Too few complete observations."))
    }

    a <- tryCatch(
      psych::alpha(mat, check.keys = TRUE, warnings = FALSE),
      error = function(e) NULL
    )

    if (is.null(a)) {
      return(list(pillar = p_name, std_alpha = NA_real_, n_items = length(inds),
                  note = "alpha() failed."))
    }

    list(pillar = p_name, std_alpha = a$total$std.alpha, n_items = length(inds), note = "")
  }) |>
    purrr::map_dfr(tibble::as_tibble)
}

# ---- Full diagnostic report for one country --------------------------------

run_diagnostics <- function(data, country_config, country_name = NULL) {
  label <- if (!is.null(country_name)) country_label(country_name) else "Country"

  cat("\n========================================\n")
  cat(" Diagnostics:", label, "\n")
  cat("========================================\n")

  if (is.null(country_config$pillars)) {
    cat("  (No pillars configured for this version — pillar diagnostics skipped)\n\n")
    return(invisible(list(missingness = NULL, correlations = NULL, alphas = NULL)))
  }

  # Missingness
  miss <- diagnose_missingness(data, country_config)
  cat("\n-- Missingness --\n")
  print(as.data.frame(miss), row.names = FALSE)

  # Within-pillar correlations
  cors <- pillar_correlations(data, country_config)
  cat("\n-- Within-pillar Spearman correlations --\n")
  for (p_name in names(cors)) {
    if (is.null(cors[[p_name]])) {
      cat(pillar_label(p_name), ": single indicator (no correlation)\n")
    } else {
      cat(pillar_label(p_name), ":\n")
      print(round(cors[[p_name]], 3))
    }
  }

  # Cronbach's alpha
  alphas <- pillar_alpha(data, country_config)
  if (!is.null(alphas)) {
    cat("\n-- Cronbach's alpha --\n")
    print(as.data.frame(alphas), row.names = FALSE)
  }

  cat("\n")

  invisible(list(missingness = miss, correlations = cors, alphas = alphas))
}

# ---- Wrapper for all countries ---------------------------------------------

run_all_diagnostics <- function(all_data, version) {
  purrr::imap(all_data, function(data, country) {
    run_diagnostics(data, version$countries[[country]], country)
  })
}
