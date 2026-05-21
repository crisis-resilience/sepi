# ============================================================================
# run_all.R — Master pipeline runner
# ============================================================================
# Executes the full SEPI pipeline in sequence:
#   01  Build data
#   02  Explore and screen indicators
#   03  Compute SEPI, visualise, export
#   04  Version comparison and criterion validity evaluation
#   05  Version comparison (v1 vs v3, always both)
#   06  Sensitivity analysis (SA1 + SA2, always both v1 and v3)
#
# Change `active_version` below to run the full pipeline for a different
# version. Scripts 02, 03, and 04 will pick it up automatically.
# Scripts 05 and 06 always compare v1_aligned_equal_geometric against
# v3_aligned_conflict_weighted regardless of this setting.
#
# Usage:
#   source("run_all.R")
#   — or —
#   Rscript run_all.R
# ============================================================================

source("R/config.R")

# ── Version ───────────────────────────────────────────────────────────────────
active_version <- VERSIONS$v1_aligned_equal_geometric  # ← change here

# Expose to sub-scripts (02_explore, 03_run_sepi, 04_evaluate check for this)
.sepi_run_version <- active_version

# ── Helpers ───────────────────────────────────────────────────────────────────
.banner <- function(step, title, detail) {
  cat("\n")
  cat(strrep("-", 64), "\n")
  cat(sprintf(" [%d/6]  %s\n", step, title))
  cat(sprintf("        %s\n", detail))
  cat(strrep("-", 64), "\n\n")
}

.elapsed <- function(t0) {
  s <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (s < 60) sprintf("%.1f s", s) else sprintf("%.1f min", s / 60)
}

pipeline_start <- Sys.time()
cat(sprintf("\nSEPI pipeline starting — version: %s\n", active_version$name))

# ── [1/6] Build data ──────────────────────────────────────────────────────────
.banner(1, "Build data",
  "Merging per-country source CSVs and ACLED data into a single global dataset.")
t0 <- Sys.time()
source("01_build_data.R")
cat(sprintf("  Done (%s)\n", .elapsed(t0)))

# ── [2/6] Explore and screen indicators ───────────────────────────────────────
.banner(2, "Explore and screen indicators",
  sprintf("Candidate exploration, collinearity screening for: %s", active_version$name))
t0 <- Sys.time()
source("02_explore.R")
cat(sprintf("  Done (%s)\n", .elapsed(t0)))

# ── [3/6] Compute SEPI ────────────────────────────────────────────────────────
.banner(3, "Compute SEPI",
  sprintf("Normalise, aggregate pillars, rank regions, export results for: %s",
          active_version$name))
t0 <- Sys.time()
source("03_run_sepi.R")
cat(sprintf("  Done (%s)\n", .elapsed(t0)))

# ── [4/6] Criterion validity ──────────────────────────────────────────────────
.banner(4, "Criterion validity evaluation",
  sprintf("Spearman correlations with IDP displacement and ACLED conflict for: %s",
          active_version$name))
t0 <- Sys.time()
source("04_evaluate.R")
cat(sprintf("  Done (%s)\n", .elapsed(t0)))

# ── [5/6] Version comparison ──────────────────────────────────────────────────
.banner(5, "Version comparison",
  paste("Head-to-head: v1_aligned_equal_geometric vs v3_aligned_conflict_weighted",
        "\n        Rank stability, criterion validity (IDP + conflict)"))
t0 <- Sys.time()
source("05_compare_versions.R")
cat(sprintf("  Done (%s)\n", .elapsed(t0)))

# ── [6/6] Sensitivity analysis ────────────────────────────────────────────────
.banner(6, "Sensitivity analysis",
  paste("Leave-one-indicator (SA1) and leave-one-pillar (SA2)",
        "\n        for v1_aligned_equal_geometric and v3_aligned_conflict_weighted"))
t0 <- Sys.time()
source("06_sensitivity_analysis.R")
cat(sprintf("  Done (%s)\n", .elapsed(t0)))

# ── Summary ───────────────────────────────────────────────────────────────────
cat("\n", strrep("=", 64), "\n", sep = "")
cat(sprintf("  Pipeline complete in %s\n", .elapsed(pipeline_start)))
cat(sprintf("  Active version : %s\n", active_version$name))
cat(sprintf("  Outputs        : outputs/%s/\n", active_version$name))
cat(strrep("=", 64), "\n", sep = "")

# Clean up sentinel
rm(.sepi_run_version)
