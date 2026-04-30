#!/usr/bin/env Rscript
# precompute_all.R — run once at Docker build time (or locally before deploy).
#
# Does two things:
#   1. Converts .rds data files → .qs (3-5x faster to read at session start)
#   2. Pre-bakes HDBSCAN best-defaults for P6–P9 so do_load_best() is instant
#
# Usage (local):  Rscript precompute_all.R
# Docker:         RUN cd /srv/shiny-server && Rscript precompute_all.R
#
# Output files written to data/:
#   P6_umap.qs, P7_umap.qs, P8_umap.qs, P9_umap.qs
#   EP6_9_Voted.qs
#   P6_best_clusters.rds, P7_best_clusters.rds,
#   P8_best_clusters.rds, P9_best_clusters.rds

suppressPackageStartupMessages({
  library(dbscan)
  library(dplyr)
})

cat("=== precompute_all.R starting ===\n")

# ── 1. Convert .rds → .qs ──────────────────────────────────────────────────
if (requireNamespace("qs", quietly = TRUE)) {
  rds_files <- list(
    "data/P6_umap.rds"       = "data/P6_umap.qs",
    "data/P7_umap.rds"       = "data/P7_umap.qs",
    "data/P8_umap.rds"       = "data/P8_umap.qs",
    "data/P9_umap.rds"       = "data/P9_umap.qs",
    "data/EP6_9_Voted.rds"   = "data/EP6_9_Voted.qs"
  )
  for (src in names(rds_files)) {
    dst <- rds_files[[src]]
    if (file.exists(src) && !file.exists(dst)) {
      cat(sprintf("  qs: converting %s → %s\n", src, dst))
      obj <- readRDS(src)
      qs::qsave(obj, dst)
    } else if (file.exists(dst)) {
      cat(sprintf("  qs: %s already exists, skipping\n", dst))
    } else {
      cat(sprintf("  qs: source %s not found, skipping\n", src))
    }
  }
} else {
  cat("  qs package not available — skipping .qs conversion\n")
}

# ── 2. Precompute HDBSCAN best-defaults ───────────────────────────────────
best_defaults <- list(
  P6 = list(minPts = 25, umap_x = "UMAP1", umap_y = "UMAP2"),
  P7 = list(minPts = 25, umap_x = "UMAP1", umap_y = "UMAP2"),
  P8 = list(minPts = 20, umap_x = "UMAP1", umap_y = "UMAP2"),
  P9 = list(minPts = 20, umap_x = "UMAP1", umap_y = "UMAP2")
)

needed_base <- c(
  "EPG", "FullName", "Country", "Party", "Photo",
  "Age_At_Start", "Experience_at_Start", "Gender",
  "Winning_Score", "Attendance_Score", "loyalty_score", "Activity_Index",
  "economic_votesScore", "social_votesScore", "foreign_policy_votesScore",
  "industry_votesScore", "education_votesScore", "budget_votesScore",
  "coord1D_red", "coord2D_red"
)

for (p in c("P6", "P7", "P8", "P9")) {
  out_path <- sprintf("data/%s_best_clusters.rds", p)
  if (file.exists(out_path)) {
    cat(sprintf("[%s] %s already exists, skipping\n", p, out_path))
    next
  }

  cat(sprintf("[%s] Loading data...\n", p))
  qs_path  <- sprintf("data/%s_umap.qs", p)
  rds_path <- sprintf("data/%s_umap.rds", p)
  if (requireNamespace("qs", quietly = TRUE) && file.exists(qs_path)) {
    data <- qs::qread(qs_path)
  } else if (file.exists(rds_path)) {
    data <- readRDS(rds_path)
  } else {
    cat(sprintf("[%s] Neither .qs nor .rds found, skipping\n", p))
    next
  }

  defs     <- best_defaults[[p]]
  needed   <- c(defs$umap_x, defs$umap_y, needed_base)
  needed   <- needed[needed %in% colnames(data)]
  umap_data <- data[, needed, drop = FALSE]

  # Rename UMAP coordinate columns to canonical names
  if (defs$umap_x %in% colnames(umap_data))
    colnames(umap_data)[colnames(umap_data) == defs$umap_x] <- "UMAP1"
  if (defs$umap_y %in% colnames(umap_data))
    colnames(umap_data)[colnames(umap_data) == defs$umap_y] <- "UMAP2"

  # Column aliases (same as do_load_best in server.R)
  umap_data$Name                 <- umap_data$FullName
  umap_data$Age                  <- umap_data$Age_At_Start
  umap_data$Experience           <- umap_data$Experience_at_Start
  umap_data$Sex                  <- umap_data$Gender
  umap_data$Winning_Index        <- umap_data$Winning_Score
  umap_data$Attendance_Index     <- umap_data$Attendance_Score
  umap_data$Loyalty_Index        <- umap_data$loyalty_score
  umap_data$Economy_Score        <- umap_data$economic_votesScore
  umap_data$Social_Score         <- umap_data$social_votesScore
  umap_data$Foreign_Policy_Score <- umap_data$foreign_policy_votesScore
  umap_data$Industry_Score       <- umap_data$industry_votesScore
  umap_data$Education_Score      <- umap_data$education_votesScore
  umap_data$Budget_Score         <- umap_data$budget_votesScore

  # Drop NA rows before HDBSCAN (same guard as server.R)
  valid_rows <- complete.cases(umap_data[, c("UMAP1", "UMAP2")])
  umap_data  <- umap_data[valid_rows, , drop = FALSE]

  if (nrow(umap_data) < defs$minPts) {
    cat(sprintf("[%s] Too few valid rows (%d) for minPts=%d — skipping\n",
                p, nrow(umap_data), defs$minPts))
    next
  }

  cat(sprintf("[%s] Running HDBSCAN (minPts=%d, n=%d)...\n",
              p, defs$minPts, nrow(umap_data)))
  hdb <- dbscan::hdbscan(umap_data[, c("UMAP1", "UMAP2")], minPts = defs$minPts)
  umap_data$Cluster <- as.factor(hdb$cluster)

  saveRDS(umap_data, out_path)
  cat(sprintf("[%s] Saved → %s  (clusters: %s)\n",
              p, out_path,
              paste(sort(unique(hdb$cluster)), collapse = ",")))
}

cat("=== precompute_all.R done ===\n")
