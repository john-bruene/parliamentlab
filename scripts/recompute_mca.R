#!/usr/bin/env Rscript
# recompute_mca.R — redo only the MCA columns of P10.
#
#   Rscript scripts/recompute_mca.R
#
# The file was first built with plain MCA, where "not in office" (code 0) and
# "absent" (4) count as answers. MEPs who left early then pull the first axis
# to themselves; see R/mca_specific.R. This rewrites MCA1, MCA2, MCA1_red and
# MCA2_red with those codes set aside and leaves every other column, the vote
# matrix and the W-NOMINATE and UMAP coordinates included, exactly as it is.
# build_p10.R now does the same, so a full rebuild gives the same result.
#
# P6-P9 are not touched: their coordinates back the published article.
# Takes about two minutes.

suppressPackageStartupMessages(library(FactoMineR))
source("R/mca_specific.R")

# Final-vote subsets, defined exactly as in the build scripts: run each script
# up to the point where its vote metadata exists, and take what it built.
prefix <- function(script, stop_at) {
  e <- new.env()
  l <- readLines(script, warn = FALSE)
  eval(parse(text = l[seq_len(grep(stop_at, l, fixed = TRUE)[1] - 1)]), envir = e)
  e
}

redo <- function(file, red_cols) {
  d  <- suppressWarnings(readRDS(file))
  xc <- grep("^X[0-9]+$", names(d), value = TRUE)
  red_cols <- intersect(red_cols, xc)
  stopifnot(length(red_cols) > 0)
  for (s in list(list(cols = xc, sfx = ""), list(cols = red_cols, sfx = "_red"))) {
    f  <- as.data.frame(lapply(d[s$cols], factor))
    mc <- mca_votes(f, ncp = 2)
    d[[paste0("MCA1", s$sfx)]] <- mc$ind$coord[, 1]
    d[[paste0("MCA2", s$sfx)]] <- mc$ind$coord[, 2]
    cat(sprintf("%s: MCA%s on %d votes, range dim1 [%.2f, %.2f], dim2 [%.2f, %.2f]\n",
                basename(file), s$sfx, length(s$cols),
                min(mc$ind$coord[, 1]), max(mc$ind$coord[, 1]),
                min(mc$ind$coord[, 2]), max(mc$ind$coord[, 2])))
  }
  saveRDS(d, file, compress = "gzip")
}

# P10: final votes are flagged from the vote title (FINAL_RX in build_p10.R).
e10 <- prefix("scripts/scrape/build_p10.R", "# ── Activity columns")
ti  <- tolower(trimws(enc2utf8(as.character(e10$m$title)))); ti[is.na(ti)] <- ""
redo("data/P10_umap.rds", paste0("X", e10$m$Vote_ID[grepl(e10$FINAL_RX, ti, useBytes = TRUE)]))

