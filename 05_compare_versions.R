# ============================================================================
# 05_compare_versions.R — V1 vs v2 Head-to-Head Comparison
# ============================================================================
# Compares v1_equal_geometric and v2_conflict_weighted on:
#
#   A.  Rank stability — Spearman rho between each version's primary results
#       and its robustness variants (z-score, BoD). Higher mean rho = more
#       stable rankings under methodology changes.
#   A2. Extended robustness — MARS (Mean Absolute Rank Shift) and Top-5
#       stability (% of worst-off ADM1s retained across variants).
#   B.  Criterion validity — Spearman rho between SEPI and 4 external
#       criteria: IDP displacement density + ACLED conflict (10y / 5y / 2025).
#       Target: rho < -0.6.
#   C.  Summary scorecard — colour-coded PNG table of all metrics.
#   D.  Unit-level rank tables — ADM1 rankings across primary version and
#       both variants, with shift (Delta) highlighted where |Delta| >= 3.
#
# No inputs required beyond the data files already used by the pipeline.
# ============================================================================

source("R/setup.R")

source("R/criterion_validity_conflict.R")

# COUNTRY_CODE_MAP, COUNTRIES defined in R/utils.R

# ── Load data ──────────────────────────────────────────────────────────────────
# v1 and v2 have different country configs (different indicator sets),
# so each needs its own data load.
cat("Loading data...\n")
all_data_v1 <- load_all_data(version = VERSIONS$v1_equal_geometric)
all_data_v2 <- load_all_data(version = VERSIONS$v2_conflict_weighted)

# ── Compute all six versions ───────────────────────────────────────────────────
cat("Computing all versions...\n")
results_v1        <- compute_all_countries(all_data_v1, VERSIONS$v1_equal_geometric)
results_v1_zscore <- compute_all_countries(all_data_v1, VERSIONS$v1_zscore)
results_v1_bod    <- compute_all_countries(all_data_v1, VERSIONS$v1_bod)

results_v2        <- compute_all_countries(all_data_v2, VERSIONS$v2_conflict_weighted)
results_v2_zscore <- compute_all_countries(all_data_v2, VERSIONS$v2_zscore)
results_v2_bod    <- compute_all_countries(all_data_v2, VERSIONS$v2_bod)
cat("Done.\n\n")

# ── IDP data (shared across B and C) ──────────────────────────────────────────
idp_data <- load_idp_data()

# criterion_validity(), idp_criterion_fn(), conflict_criterion_fn()
# are defined in R/criterion_validity_conflict.R.

# Registry of all criterion sources used in the scorecard.
# Order controls the row-group order inside the PNG.
CRITERION_SOURCES <- list(
  displacement  = list(label      = "Displacement",
                       fn_builder = function(country) idp_criterion_fn(country, idp_data)),
  conflict_10y  = list(label      = "Conflict (2016\u20132025)",
                       fn_builder = function(country) conflict_criterion_fn("10y")),
  conflict_5y   = list(label      = "Conflict (2021\u20132025)",
                       fn_builder = function(country) conflict_criterion_fn("5y")),
  conflict_2025 = list(label      = "Conflict (2025)",
                       fn_builder = function(country) conflict_criterion_fn("2025"))
)

# ── MARS: Mean Absolute Rank Shift ─────────────────────────────────────────────
# rank 1 = worst-off (lowest SEPI).  MARS = mean(|rank_primary - rank_variant|).
# Lower MARS = rankings barely move when methodology changes = more stable.
mars_country <- function(res_primary, res_variant, country) {
  make_r <- function(res) {
    res[[country]] |>
      dplyr::select(adm1_pcode, sepi) |>
      dplyr::mutate(r = rank(sepi, ties.method = "min"))  # 1 = worst-off
  }
  merged <- dplyr::inner_join(
    make_r(res_primary) |> dplyr::rename(r_p = r),
    make_r(res_variant)  |> dplyr::rename(r_v = r),
    by = "adm1_pcode"
  )
  mean(abs(merged$r_p - merged$r_v), na.rm = TRUE)
}

# ── Top-k stability ─────────────────────────────────────────────────────────────
# What % of the k worst-off ADM1s in the primary version remain in the bottom-k
# in the variant?  100 % = targeting list unchanged; 0 % = completely different.
topk_country <- function(res_primary, res_variant, country, k = 5) {
  make_r <- function(res) {
    res[[country]] |>
      dplyr::select(adm1_pcode, sepi) |>
      dplyr::mutate(r = rank(sepi, ties.method = "min"))
  }
  merged <- dplyr::inner_join(
    make_r(res_primary) |> dplyr::rename(r_p = r),
    make_r(res_variant)  |> dplyr::rename(r_v = r),
    by = "adm1_pcode"
  )
  k_use    <- min(k, nrow(merged))
  bottom_p <- merged$adm1_pcode[merged$r_p <= k_use]
  bottom_v <- merged$adm1_pcode[merged$r_v <= k_use]
  round(sum(bottom_p %in% bottom_v) / k_use * 100, 1)
}

# ── Unit-level rank table builder ──────────────────────────────────────────────
build_rank_table <- function(res_primary, res_zscore, res_bod, country) {
  make_r <- function(res) {
    res[[country]] |>
      dplyr::select(adm1_pcode, adm1_name, sepi) |>
      dplyr::mutate(rank = rank(-sepi, ties.method = "min"))   # 1 = best-off (highest SEPI)
  }
  df_p <- make_r(res_primary) |> dplyr::rename(sepi_p = sepi, rank_p = rank)
  df_z <- make_r(res_zscore)  |> dplyr::select(adm1_pcode, rank_z = rank)
  df_b <- make_r(res_bod)     |> dplyr::select(adm1_pcode, rank_b = rank)

  dplyr::left_join(df_p, df_z, by = "adm1_pcode") |>
    dplyr::left_join(df_b, by = "adm1_pcode")      |>
    dplyr::mutate(shift_z = rank_z - rank_p,
                  shift_b = rank_b - rank_p,
                  worst5  = rank_p >= (max(rank_p, na.rm = TRUE) - 4)) |>
    dplyr::arrange(rank_p)                          |>
    dplyr::select(adm1_name, sepi_p,
                  rank_p, rank_z, shift_z, rank_b, shift_b, worst5)
}

save_rank_table_png <- function(rt, title_str, primary_lbl, zscore_lbl, bod_lbl, fname) {
  gt_rt <- rt |>
    gt::gt(rowname_col = "adm1_name") |>
    gt::tab_header(title = gt::md(paste0("**", title_str, "**"))) |>
    gt::cols_label(sepi_p  = "SEPI",
                   rank_p  = primary_lbl,
                   rank_z  = zscore_lbl,
                   shift_z = "\u0394",
                   rank_b  = bod_lbl,
                   shift_b = "\u0394") |>
    gt::tab_spanner(label = "Primary",         columns = c(sepi_p, rank_p)) |>
    gt::tab_spanner(label = "Z-score variant", columns = c(rank_z, shift_z)) |>
    gt::tab_spanner(label = "BoD variant",     columns = c(rank_b, shift_b)) |>
    gt::fmt_number(columns = sepi_p, decimals = 3) |>
    # Large shifts (|shift| >= 3) in amber
    gt::tab_style(
      style     = gt::cell_fill(color = "#fff3cd"),
      locations = gt::cells_body(columns = shift_z, rows = abs(shift_z) >= 3)
    ) |>
    gt::tab_style(
      style     = gt::cell_fill(color = "#fff3cd"),
      locations = gt::cells_body(columns = shift_b, rows = abs(shift_b) >= 3)
    ) |>
    # Bold the 5 worst-off rows (prime targeting targets)
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(rows = worst5)
    ) |>
    gt::tab_footnote(
      footnote = "\u0394 = variant rank \u2212 primary rank. Positive = region moved up (looks better in variant); negative = moved down. Amber = |\u0394| \u2265 3. Bold rows = bottom-5 worst-off in primary version. Rank 1 = highest SEPI (best-off)."
    ) |>
    gt::cols_hide(columns = worst5) |>
    gt::tab_options(
      table.font.names             = "Arial",
      table.font.size              = 10,
      heading.title.font.size      = 12,
      column_labels.font.weight    = "bold",
      data_row.padding             = gt::px(4),
      table.border.top.color       = "#333333",
      table.border.bottom.color    = "#333333"
    )
  gt::gtsave(gt_rt, filename = fname, zoom = 2, expand = 15)
  message("Saved: ", fname)
}

# ============================================================================
# A. Rank Stability
# ============================================================================
cat("========================================\n")
cat(" A. Rank Stability\n")
cat("========================================\n")
cat(" Spearman rho between primary version and robustness variants.\n")
cat(" Higher mean rho = rankings more stable under method changes.\n\n")

comparison_v1 <- compare_versions(list(
  v1_base   = results_v1,
  v1_zscore = results_v1_zscore,
  v1_bod    = results_v1_bod
))

comparison_v2 <- compare_versions(list(
  v2_base   = results_v2,
  v2_zscore = results_v2_zscore,
  v2_bod    = results_v2_bod
))

stability_rows <- list()

for (country in COUNTRIES) {
  mat_v1 <- comparison_v1[[country]]$rank_correlation
  mat_v2 <- comparison_v2[[country]]$rank_correlation

  rho_v1_zscore <- mat_v1["rank_v1_base", "rank_v1_zscore"]
  rho_v1_bod    <- mat_v1["rank_v1_base", "rank_v1_bod"]
  rho_v2_zscore <- mat_v2["rank_v2_base", "rank_v2_zscore"]
  rho_v2_bod    <- mat_v2["rank_v2_base", "rank_v2_bod"]

  mean_v1 <- mean(c(rho_v1_zscore, rho_v1_bod), na.rm = TRUE)
  mean_v2 <- mean(c(rho_v2_zscore, rho_v2_bod), na.rm = TRUE)

  stability_rows[[country]] <- data.frame(
    country      = country_label(country),
    v1_vs_zscore = round(rho_v1_zscore, 3),
    v1_vs_bod    = round(rho_v1_bod, 3),
    v1_mean      = round(mean_v1, 3),
    v2_vs_zscore = round(rho_v2_zscore, 3),
    v2_vs_bod    = round(rho_v2_bod, 3),
    v2_mean      = round(mean_v2, 3),
    stringsAsFactors = FALSE
  )

  cat(country_label(country), "\n")
  cat("  v1: rho vs z-score =", round(rho_v1_zscore, 3),
      "| rho vs BoD =", round(rho_v1_bod, 3),
      "| mean =", round(mean_v1, 3), "\n")
  cat("  v2: rho vs z-score =", round(rho_v2_zscore, 3),
      "| rho vs BoD =", round(rho_v2_bod, 3),
      "| mean =", round(mean_v2, 3), "\n\n")
}

stability_tbl <- dplyr::bind_rows(stability_rows)

cat("Full rank correlation matrices:\n")
for (country in COUNTRIES) {
  cat("\n", country_label(country), "— v1 family:\n")
  print(round(comparison_v1[[country]]$rank_correlation, 3))
  cat("\n", country_label(country), "— v2 family:\n")
  print(round(comparison_v2[[country]]$rank_correlation, 3))
}

# ============================================================================
# A2. Extended Robustness Metrics — MARS and Top-5 Stability
# ============================================================================
# MARS = Mean Absolute Rank Shift.  Spearman rho says "are the shapes similar?"
# MARS says "on average, how many places does an ADM1 move?"  A region could
# sit at rank 5 in one version and rank 8 in another — that is a MARS of 3.
# Top-5 stability asks: "do the same 5 worst-off regions stay in the bottom-5?"
# This directly measures whether targeting decisions would change.
# ============================================================================
TOPK <- 5

cat("\n========================================\n")
cat(" A2. Extended Robustness Metrics\n")
cat("========================================\n")
cat(sprintf(" MARS: Mean Absolute Rank Shift. Lower is more stable.\n"))
cat(sprintf(" Top-%d: %% of bottom-%d worst-off ADM1s retained across variants.\n\n",
            TOPK, TOPK))

mars_rows <- list()
topk_rows <- list()

for (country in COUNTRIES) {
  mars_v1_z <- mars_country(results_v1, results_v1_zscore, country)
  mars_v1_b <- mars_country(results_v1, results_v1_bod,    country)
  mars_v2_z <- mars_country(results_v2, results_v2_zscore, country)
  mars_v2_b <- mars_country(results_v2, results_v2_bod,    country)

  mean_mars_v1 <- mean(c(mars_v1_z, mars_v1_b), na.rm = TRUE)
  mean_mars_v2 <- mean(c(mars_v2_z, mars_v2_b), na.rm = TRUE)

  topk_v1_z <- topk_country(results_v1, results_v1_zscore, country, k = TOPK)
  topk_v1_b <- topk_country(results_v1, results_v1_bod,    country, k = TOPK)
  topk_v2_z <- topk_country(results_v2, results_v2_zscore, country, k = TOPK)
  topk_v2_b <- topk_country(results_v2, results_v2_bod,    country, k = TOPK)

  mean_topk_v1 <- mean(c(topk_v1_z, topk_v1_b), na.rm = TRUE)
  mean_topk_v2 <- mean(c(topk_v2_z, topk_v2_b), na.rm = TRUE)

  cat(country_label(country), "\n")
  cat(sprintf("  v1 MARS  : z-score = %.2f | BoD = %.2f | mean = %.2f\n",
              mars_v1_z, mars_v1_b, mean_mars_v1))
  cat(sprintf("  v2 MARS  : z-score = %.2f | BoD = %.2f | mean = %.2f\n",
              mars_v2_z, mars_v2_b, mean_mars_v2))
  cat(sprintf("  v1 Top-%d : z-score = %.0f%% | BoD = %.0f%% | mean = %.0f%%\n",
              TOPK, topk_v1_z, topk_v1_b, mean_topk_v1))
  cat(sprintf("  v2 Top-%d : z-score = %.0f%% | BoD = %.0f%% | mean = %.0f%%\n\n",
              TOPK, topk_v2_z, topk_v2_b, mean_topk_v2))

  mars_rows[[country]] <- data.frame(
    dimension  = "MARS (lower = more stable)",
    country    = country_label(country),
    v1_value   = round(mean_mars_v1, 2),
    v1_detail  = sprintf("z-score: %.2f  |  BoD: %.2f", mars_v1_z, mars_v1_b),
    v1_verdict = dplyr::case_when(
      mean_mars_v1 <= 1.5 ~ "stable",
      mean_mars_v1 <= 3.0 ~ "moderate",
      TRUE                ~ "unstable"
    ),
    v2_value   = round(mean_mars_v2, 2),
    v2_detail  = sprintf("z-score: %.2f  |  BoD: %.2f", mars_v2_z, mars_v2_b),
    v2_verdict = dplyr::case_when(
      mean_mars_v2 <= 1.5 ~ "stable",
      mean_mars_v2 <= 3.0 ~ "moderate",
      TRUE                ~ "unstable"
    ),
    stringsAsFactors = FALSE
  )

  topk_rows[[country]] <- data.frame(
    dimension  = sprintf("Top-%d Stability (%%)", TOPK),
    country    = country_label(country),
    v1_value   = round(mean_topk_v1, 1),
    v1_detail  = sprintf("z-score: %.0f%%  |  BoD: %.0f%%", topk_v1_z, topk_v1_b),
    v1_verdict = dplyr::case_when(
      mean_topk_v1 >= 80 ~ "stable",
      mean_topk_v1 >= 60 ~ "moderate",
      TRUE               ~ "unstable"
    ),
    v2_value   = round(mean_topk_v2, 1),
    v2_detail  = sprintf("z-score: %.0f%%  |  BoD: %.0f%%", topk_v2_z, topk_v2_b),
    v2_verdict = dplyr::case_when(
      mean_topk_v2 >= 80 ~ "stable",
      mean_topk_v2 >= 60 ~ "moderate",
      TRUE               ~ "unstable"
    ),
    stringsAsFactors = FALSE
  )
}

mars_tbl <- dplyr::bind_rows(mars_rows)
topk_tbl <- dplyr::bind_rows(topk_rows)

# ============================================================================
# B. Criterion Validity — 4 sources: displacement + conflict (10y / 5y / 2025)
# ============================================================================
cat("\n========================================\n")
cat(" B. Criterion Validity\n")
cat("========================================\n")
cat(" H1: lower SEPI -> higher criterion (rho < 0)\n")
cat(" Target: rho < -0.6 (strong negative)\n")
cat(" Sources: IDP displacement, ACLED conflict (10y / 5y / 2025)\n")
cat(" Note: 2025 conflict is circular for v2_conflict_weighted\n\n")

cv_tables <- list()

for (src_key in names(CRITERION_SOURCES)) {
  src <- CRITERION_SOURCES[[src_key]]
  cat("---- ", src$label, " ----\n", sep = "")

  rows <- list()
  for (country in COUNTRIES) {
    fn    <- src$fn_builder(country)
    cv_v1 <- criterion_validity(results_v1, country, fn)
    cv_v2 <- criterion_validity(results_v2, country, fn)

    rows[[country]] <- data.frame(
      country    = country_label(country),
      v1_rho     = round(cv_v1$rho, 3),
      v1_p       = round(cv_v1$p, 3),
      v1_n       = cv_v1$n,
      v1_verdict = cv_v1$verdict,
      v2_rho     = round(cv_v2$rho, 3),
      v2_p       = round(cv_v2$p, 3),
      v2_n       = cv_v2$n,
      v2_verdict = cv_v2$verdict,
      stringsAsFactors = FALSE
    )

    cat(country_label(country), "\n")
    cat(sprintf("  v1: rho = %6.3f  p = %.3f  n = %d  [%s]\n",
                cv_v1$rho, cv_v1$p, cv_v1$n, cv_v1$verdict))
    cat(sprintf("  v2: rho = %6.3f  p = %.3f  n = %d  [%s]\n",
                cv_v2$rho, cv_v2$p, cv_v2$n, cv_v2$verdict))
  }
  cat("\n")
  cv_tables[[src_key]] <- dplyr::bind_rows(rows)
}


# ============================================================================
# C. Summary Scorecard — PNG table
# ============================================================================
for (pkg in c("gt", "webshot2")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

cat("\n")
cat(strrep("=", 72), "\n")
cat(" C. SUMMARY SCORECARD — v1_equal_geometric vs v2_conflict_weighted\n")
cat(strrep("=", 72), "\n\n")

avg_stability_v1 <- mean(stability_tbl$v1_mean, na.rm = TRUE)
avg_stability_v2 <- mean(stability_tbl$v2_mean, na.rm = TRUE)

# ── Build unified long table ───────────────────────────────────────────────────

# Section 1: Rank Stability — one row per country + average
rank_rows <- stability_tbl |>
  dplyr::transmute(
    dimension  = "Rank Stability",
    country    = country,
    v1_value   = v1_mean,
    v1_detail  = sprintf("z-score: %.3f  |  BoD: %.3f", v1_vs_zscore, v1_vs_bod),
    v1_verdict = dplyr::case_when(
      v1_mean >= 0.90 ~ "stable",
      v1_mean >= 0.75 ~ "moderate",
      TRUE            ~ "unstable"
    ),
    v2_value   = v2_mean,
    v2_detail  = sprintf("z-score: %.3f  |  BoD: %.3f", v2_vs_zscore, v2_vs_bod),
    v2_verdict = dplyr::case_when(
      v2_mean >= 0.90 ~ "stable",
      v2_mean >= 0.75 ~ "moderate",
      TRUE            ~ "unstable"
    )
  ) |>
  dplyr::bind_rows(data.frame(
    dimension  = "Rank Stability",
    country    = "Average",
    v1_value   = round(avg_stability_v1, 3),
    v1_detail  = "",
    v1_verdict = dplyr::case_when(
      avg_stability_v1 >= 0.90 ~ "stable",
      avg_stability_v1 >= 0.75 ~ "moderate",
      TRUE                     ~ "unstable"
    ),
    v2_value   = round(avg_stability_v2, 3),
    v2_detail  = "",
    v2_verdict = dplyr::case_when(
      avg_stability_v2 >= 0.90 ~ "stable",
      avg_stability_v2 >= 0.75 ~ "moderate",
      TRUE                     ~ "unstable"
    ),
    stringsAsFactors = FALSE
  ))

# Section 2: Criterion Validity — one row group per criterion source
cv_rows_gt <- purrr::imap_dfr(cv_tables, function(tbl, src_key) {
  lbl <- CRITERION_SOURCES[[src_key]]$label
  tbl |>
    dplyr::transmute(
      dimension  = paste0("Criterion Validity \u2014 ", lbl),
      country    = country,
      v1_value   = v1_rho,
      v1_detail  = sprintf("p = %.3f  |  n = %d", v1_p, v1_n),
      v1_verdict = v1_verdict,
      v2_value   = v2_rho,
      v2_detail  = sprintf("p = %.3f  |  n = %d", v2_p, v2_n),
      v2_verdict = v2_verdict
    )
})

scorecard_long <- dplyr::bind_rows(rank_rows, mars_tbl, topk_tbl, cv_rows_gt)

# ── Build gt table ─────────────────────────────────────────────────────────────

# Colour helpers
verdict_bg <- function(verdict) {
  dplyr::case_when(
    verdict %in% c("stable", "SUPPORTED", "GOOD (>=0.80)")             ~ "#d4edda",
    verdict %in% c("moderate", "acceptable (>=0.70)", "weak negative") ~ "#fff3cd",
    verdict %in% c("unstable", "NOT supported", "poor (0.60-0.70)",
                   "no discrimination", "too few units",
                   "class imbalance", "no data")                       ~ "#f8d7da",
    TRUE ~ "#ffffff"
  )
}

gt_tbl <- scorecard_long |>
  gt::gt(groupname_col = "dimension", rowname_col = "country") |>
  gt::tab_header(
    title    = gt::md("**SEPI Version Comparison Scorecard**"),
    subtitle = "v1 Equal-Weighted Geometric  vs  v2 Conflict-Weighted"
  ) |>
  gt::cols_label(
    v1_value   = "Score",
    v1_detail  = "Detail",
    v1_verdict = "Verdict",
    v2_value   = "Score",
    v2_detail  = "Detail",
    v2_verdict = "Verdict"
  ) |>
  gt::tab_spanner(label = "v1 — Equal Geometric",  columns = c(v1_value, v1_detail, v1_verdict)) |>
  gt::tab_spanner(label = "v2 — Conflict Weighted", columns = c(v2_value, v2_detail, v2_verdict)) |>
  gt::fmt_number(columns = c(v1_value, v2_value), decimals = 3, use_seps = FALSE) |>
  gt::sub_missing(columns = c(v1_value, v2_value), missing_text = "n/a") |>
  # Colour verdict cells
  gt::tab_style(
    style     = gt::cell_fill(color = "#d4edda"),
    locations = gt::cells_body(
      columns = v1_verdict,
      rows    = v1_verdict %in% c("stable", "SUPPORTED", "GOOD (>=0.80)")
    )
  ) |>
  gt::tab_style(
    style     = gt::cell_fill(color = "#fff3cd"),
    locations = gt::cells_body(
      columns = v1_verdict,
      rows    = v1_verdict %in% c("moderate", "acceptable (>=0.70)", "weak negative")
    )
  ) |>
  gt::tab_style(
    style     = gt::cell_fill(color = "#f8d7da"),
    locations = gt::cells_body(
      columns = v1_verdict,
      rows    = !v1_verdict %in% c("stable", "SUPPORTED", "GOOD (>=0.80)",
                                   "moderate", "acceptable (>=0.70)", "weak negative")
    )
  ) |>
  gt::tab_style(
    style     = gt::cell_fill(color = "#d4edda"),
    locations = gt::cells_body(
      columns = v2_verdict,
      rows    = v2_verdict %in% c("stable", "SUPPORTED", "GOOD (>=0.80)")
    )
  ) |>
  gt::tab_style(
    style     = gt::cell_fill(color = "#fff3cd"),
    locations = gt::cells_body(
      columns = v2_verdict,
      rows    = v2_verdict %in% c("moderate", "acceptable (>=0.70)", "weak negative")
    )
  ) |>
  gt::tab_style(
    style     = gt::cell_fill(color = "#f8d7da"),
    locations = gt::cells_body(
      columns = v2_verdict,
      rows    = !v2_verdict %in% c("stable", "SUPPORTED", "GOOD (>=0.80)",
                                   "moderate", "acceptable (>=0.70)", "weak negative")
    )
  ) |>
  # Bold average row
  gt::tab_style(
    style     = gt::cell_text(weight = "bold"),
    locations = gt::cells_body(rows = country == "Average")
  ) |>
  gt::tab_style(
    style     = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups()
  ) |>
  gt::tab_footnote(
    footnote = "Rank stability (Spearman rho): >= 0.90 stable, 0.75-0.90 moderate, <0.75 unstable"
  ) |>
  gt::tab_footnote(
    footnote = "MARS (Mean Abs. Rank Shift): <= 1.5 stable, 1.5-3.0 moderate, >3.0 unstable"
  ) |>
  gt::tab_footnote(
    footnote = sprintf("Top-%d stability: >= 80%% stable, 60-80%% moderate, <60%% unstable", TOPK)
  ) |>
  gt::tab_footnote(
    footnote = "Criterion validity: rho < -0.60 SUPPORTED"
  ) |>
  gt::tab_footnote(
    footnote = paste(
      "Conflict (2025) is circular for v2_conflict_weighted, which derives its",
      "weights from 2025 ACLED indicators; read as a sanity check, not external validation.",
      "Conflict (2021\u20132025) and Conflict (2016\u20132025) include 2025 and are therefore",
      "partially endogenous for v2. v1 is independent of all conflict windows."
    )
  ) |>
  gt::tab_options(
    table.font.names       = "Arial",
    table.font.size        = 11,
    heading.title.font.size = 14,
    row_group.font.weight  = "bold",
    column_labels.font.weight = "bold",
    table.border.top.color = "#333333",
    table.border.bottom.color = "#333333",
    row_group.border.bottom.color = "#aaaaaa",
    stub.border.color      = "#dddddd",
    data_row.padding       = gt::px(5)
  )

# ── Save as PNG ────────────────────────────────────────────────────────────────
png_path <- versioned_output_path(NULL, "figures", "compare_versions", "scorecard")
gt::gtsave(gt_tbl, filename = png_path, zoom = 2, expand = 20)
message("Saved: ", png_path)

cat("\nComparison complete.\n")

# ============================================================================
# D. Unit-Level Rank Tables — side-by-side ADM1 rankings across variants
# ============================================================================
# For each country and each version family (v1 / v2):
#   Region | SEPI | Primary Rank | Z-score Rank | Δ | BoD Rank | Δ
#   Sorted by primary rank (rank 1 = best-off, highest SEPI).
#   Amber cells = |Δ| >= 3 (large enough to change a targeting decision).
#   Bold rows   = bottom-5 worst-off in primary (most policy-relevant).
#
# This is the answer to "can I trust this ranking for targeting?"
# High Spearman rho with amber cells in the bottom-5 rows = misleading stability.
# ============================================================================
cat("\n")
cat(strrep("=", 72), "\n")
cat(" D. Unit-Level Rank Tables — per country, per version family\n")
cat(strrep("=", 72), "\n")
cat(" Rank 1 = best-off (highest SEPI).  \u0394 = variant rank \u2212 primary rank.\n")
cat(" Amber = |\u0394| >= 3.  Bold = bottom-5 worst-off in primary version.\n\n")

for (country in COUNTRIES) {
  cat(country_label(country), "— v1 family:\n")
  rt_v1 <- build_rank_table(results_v1, results_v1_zscore, results_v1_bod, country)
  print(as.data.frame(rt_v1) |>
        dplyr::select(-worst5) |>
        dplyr::rename(Region = adm1_name, SEPI = sepi_p,
                      `v1 Rank` = rank_p, `z-score Rank` = rank_z, `Shift.z` = shift_z,
                      `BoD Rank` = rank_b, `Shift.b` = shift_b),
        row.names = FALSE)

  cat(country_label(country), "— v2 family:\n")
  rt_v2 <- build_rank_table(results_v2, results_v2_zscore, results_v2_bod, country)
  print(as.data.frame(rt_v2) |>
        dplyr::select(-worst5) |>
        dplyr::rename(Region = adm1_name, SEPI = sepi_p,
                      `v2 Rank` = rank_p, `z-score Rank` = rank_z, `Shift.z` = shift_z,
                      `BoD Rank` = rank_b, `Shift.b` = shift_b),
        row.names = FALSE)
  cat("\n")

  save_rank_table_png(
    rt_v1,
    title_str   = paste0(country_label(country), " — v1 Rank Stability"),
    primary_lbl = "v1 Rank",
    zscore_lbl  = "z-score Rank",
    bod_lbl     = "BoD Rank",
    fname       = versioned_output_path(NULL, "figures", "compare_versions",
                                        paste0("ranks_v1_", country))
  )
  save_rank_table_png(
    rt_v2,
    title_str   = paste0(country_label(country), " — v2 Rank Stability"),
    primary_lbl = "v2 Rank",
    zscore_lbl  = "z-score Rank",
    bod_lbl     = "BoD Rank",
    fname       = versioned_output_path(NULL, "figures", "compare_versions",
                                        paste0("ranks_v2_", country))
  )
}

cat("\nAll outputs saved to outputs/figures/compare_versions/\n")
