#!/usr/bin/env Rscript
# scrape_activities.R — MEP activity counts (speeches, reports, questions ...)
# for the extended dataset, from the Parltrack ep_mep_activities dump.
#
# These are the inputs behind Activity_Index. The Parliament's own open data
# API does not expose per-MEP authorship (its document endpoints carry no
# author, and the author filter is silently ignored), so Parltrack remains the
# only source, as it was for the original EP6-EP9 data.
#
# Usage (from the project root):
#   Rscript scripts/scrape/scrape_activities.R                    # both terms
#   Rscript scripts/scrape/scrape_activities.R 2022-06-10 2024-07-15 9
#
# The dump is lzip-compressed, which R cannot read, and expands to about
# 400 MB. scripts/scrape/extract_activities.py does the decompression and
# counting in one streaming pass and hands back a small CSV.

DUMP_URL <- "https://parltrack.org/dumps/ep_mep_activities.json.lz"
dump_path <- file.path("data", "api_cache", "ep_mep_activities.json.lz")
out_dir   <- file.path("data", "scraped")
dir.create(dirname(dump_path), showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

CATEGORIES <- c("CRE", "WDECL", "COMPARL", "REPORT", "REPORT_SHADOW",
                "COMPARL_SHADOW", "MOTION", "OQ", "WEXP", "WQ",
                "MINT", "IMOTION", "PRUNACT")

args <- commandArgs(trailingOnly = TRUE)
windows <- if (length(args) >= 3) {
  list(list(start = args[1], end = args[2], leg = as.integer(args[3])))
} else {
  list(list(start = "2022-06-10", end = "2024-07-15", leg = 9L),
       list(start = "2024-07-16", end = as.character(Sys.Date()), leg = 10L))
}

# ── Fetch the dump once ─────────────────────────────────────────────────────
if (!file.exists(dump_path) || file.info(dump_path)$size < 1e6) {
  cat("Downloading Parltrack activities dump (~35 MB)...\n")
  options(timeout = 600)
  ok <- tryCatch(download.file(DUMP_URL, dump_path, quiet = TRUE, mode = "wb"),
                 error = function(e) -1L, warning = function(w) -1L)
  if (!identical(as.integer(ok), 0L) || file.info(dump_path)$size < 1e6) {
    stop("Could not download ", DUMP_URL)
  }
}
cat(sprintf("Dump: %s (%.1f MB)\n", dump_path, file.info(dump_path)$size / 1048576))

helper <- file.path("scripts", "scrape", "extract_activities.py")
if (!file.exists(helper)) stop("Missing helper: ", helper)

for (w in windows) {
  cat(sprintf("\n=== EP%d: %s to %s ===\n", w$leg, w$start, w$end))
  csv <- tempfile(fileext = ".csv")
  status <- system2("python3", c(shQuote(helper), shQuote(dump_path), shQuote(csv),
                                 w$start, w$end), stdout = TRUE, stderr = TRUE)
  cat(paste0("  ", status, collapse = "\n"), "\n")
  if (!file.exists(csv)) { warning("extraction failed for EP", w$leg); next }

  acts <- utils::read.csv(csv, stringsAsFactors = FALSE)
  unlink(csv)

  # Restrict to the MEPs in the scraped vote file for this term, so the
  # activity table lines up row for row with the voting data.
  votes_file <- file.path(out_dir, sprintf("P%d_votes.rds", w$leg))
  if (file.exists(votes_file)) {
    v <- suppressWarnings(readRDS(votes_file))
    keep <- as.integer(v$WebisteEpID)
    acts <- acts[acts$mep_id %in% keep, , drop = FALSE]
    missing <- setdiff(keep, acts$mep_id)
    if (length(missing)) {
      pad <- data.frame(mep_id = missing)
      for (cl in CATEGORIES) pad[[cl]] <- 0L
      acts <- rbind(acts, pad)
    }
    acts <- acts[match(keep, acts$mep_id), , drop = FALSE]
    acts$FullName <- v$FullName
  }

  # Activity_Index, as described in the article: the (weighted) average of an
  # MEP's activity counts, weights defaulting to 1. The counts live on wildly
  # different scales (written explanations run to hundreds, reports to a
  # handful), so they are standardised first and the index is centred on the
  # cohort. This reproduces the original EP9 index at r = 0.98; the exact
  # weights behind the published figures are not recorded in the data.
  A <- as.matrix(acts[, CATEGORIES])
  A[!is.finite(A)] <- 0
  keep_cols <- apply(A, 2, function(x) stats::sd(x, na.rm = TRUE) > 0)
  acts$Activity_Index <- if (any(keep_cols)) {
    round(as.numeric(scale(rowMeans(scale(A[, keep_cols, drop = FALSE]), na.rm = TRUE))), 4)
  } else NA_real_

  f <- file.path(out_dir, sprintf("P%d_activities.rds", w$leg))
  saveRDS(acts, f, compress = "gzip")
  active <- sum(rowSums(A) > 0)
  cat(sprintf("  %d MEPs (%d with recorded activity) -> %s\n", nrow(acts), active, f))
}

cat("\nNote: the Parltrack dump was last refreshed on 7 November 2024, so EP10\n")
cat("coverage stops there. Re-run once Parltrack publishes a newer dump.\n")
