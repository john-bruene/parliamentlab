#!/usr/bin/env Rscript
# scrape_topic_scores.R — per-policy voting scores for the extended dataset.
#
# This is a port of the topic-score code from the original data preparation
# (archive/Data_Prep.Rmd, "Topic Scores"). The block definitions are kept
# verbatim, including the spelling variants VoteWatch used for the same policy
# area ("Foreign & security policy", "Foreign& security policy", "Foreign and
# security policy"), so the resulting columns line up with the published data.
#
# Score for one block = (# votes in favour) - (# votes against) across all
# roll-calls classified into that block. The published EP6-EP9 columns are raw
# sums on this scale, not rescaled.
#
# Usage (from the project root):
#   Rscript scripts/scrape/scrape_topic_scores.R            # score scraped data
#   Rscript scripts/scrape/scrape_topic_scores.R --validate # check the port
#
# --validate re-runs the port on the ORIGINAL EP6-EP9 data and compares the
# result with the stored *_votesScore columns, which is what proves the port
# is faithful before it is applied to anything new.

out_dir <- file.path("data", "scraped")
drop_amendments <- "--exclude-amendments" %in% commandArgs(trailingOnly = TRUE)

# ── Block definitions, verbatim from archive/Data_Prep.Rmd ──────────────────
topic_blocks <- list(
  economic_votes = c("Economics", "Economic & monetary affairs", "Economic and monetary affairs"),
  social_votes = c("Employment & social affairs", "Employment & Social affairs"),
  foreign_policy_votes = c("Foreign & security policy", "Foreign& security policy", "Foreign and security policy"),
  industry_votes = c("Industry, research & energy", "Industry, Research & Energy"),
  health_votes = c("Environment & public health"),
  gender_votes = c("Gender equality"),
  law_votes = c("Judicial affairs", "Juridical Affairs", "Legal Affairs", "Legal affairs",
                "Constitutional and interconstitutional affairs", "International regulations of the EP",
                "Constitutional and inter-institutional affairs"),
  agriculture_fisheries_votes = c("Agriculture", "Fisheries"),
  budget_votes = c("Budget", "Budgetary Control"),
  civil_liberties_votes = c("Civil liberties, justice & home affairs"),
  education_votes = c("Culture & education"),
  petitions_votes = c("Petitions"),
  internal_market_votes = c("Internal market & consumer protection"),
  international_trade_votes = c("International trade"),
  regional_development_votes = c("Regional development", "Regioanal development", "Development"),
  transport_tourism_votes = c("Transport & tourism")
)

# Case-insensitive matching. The original list already carries VoteWatch's
# spelling variants; the recovered labels differ from it only in capitalisation
# ("Budgetary control" vs "Budgetary Control"), so folding case makes the two
# vocabularies line up exactly instead of silently dropping those votes.
block_of <- function(policy, fold_case = TRUE) {
  p <- trimws(as.character(policy))
  if (fold_case) p <- tolower(p)
  out <- rep(NA_character_, length(p))
  for (b in names(topic_blocks)) {
    lv <- if (fold_case) tolower(topic_blocks[[b]]) else topic_blocks[[b]]
    out[is.na(out) & p %in% lv] <- b
  }
  out
}

# Score every block at once: +1 for a vote in favour, -1 against, 0 otherwise.
# The original looped over votes one at a time; this is the same arithmetic.
score_blocks <- function(wide, vote_ids, policy, fold_case = TRUE) {
  blk <- block_of(policy, fold_case)
  res <- vector("list", length(topic_blocks))
  names(res) <- paste0(names(topic_blocks), "Score")
  for (b in names(topic_blocks)) {
    cols <- paste0("X", vote_ids[!is.na(blk) & blk == b])
    cols <- cols[cols %in% names(wide)]
    sc <- if (length(cols)) {
      m <- as.matrix(wide[, cols, drop = FALSE])
      rowSums(m == 1L, na.rm = TRUE) - rowSums(m == 2L, na.rm = TRUE)
    } else rep(0L, nrow(wide))
    res[[paste0(b, "Score")]] <- as.integer(sc)
  }
  as.data.frame(res, stringsAsFactors = FALSE)
}

# ── Validation: reproduce the published columns from the original data ──────
if ("--validate" %in% commandArgs(trailingOnly = TRUE)) {
  votes <- suppressWarnings(readRDS(file.path("data", "EP6_9_Voted.rds")))
  ok <- TRUE
  for (leg in 6:9) {
    f <- file.path("data", sprintf("P%d_umap.rds", leg))
    if (!file.exists(f)) next
    wide <- suppressWarnings(readRDS(f))
    v <- votes[votes$Legislature == leg & votes$final_vote == 1, ]
    got <- score_blocks(wide, v$Vote_ID, v$main_policy_name, fold_case = FALSE)
    cmp <- intersect(names(got), names(wide))
    same <- vapply(cmp, function(cl)
      isTRUE(all.equal(as.numeric(got[[cl]]), as.numeric(wide[[cl]]))), logical(1))
    cat(sprintf("EP%d: %d of %d topic columns reproduced exactly\n", leg, sum(same), length(cmp)))
    if (!all(same)) {
      ok <- FALSE
      cat("   differing:", paste(cmp[!same], collapse = ", "), "\n")
    }
  }
  cat(if (ok) "\nPort is faithful.\n" else "\nPort differs from the published columns.\n")
  quit(save = "no")
}

# ── Apply to the scraped data ───────────────────────────────────────────────
for (leg in c(9, 10)) {
  wf <- file.path(out_dir, sprintf("P%d_votes.rds", leg))
  mf <- file.path(out_dir, sprintf("P%d_votes_meta.rds", leg))
  if (!file.exists(wf) || !file.exists(mf)) next
  wide <- suppressWarnings(readRDS(wf))
  meta <- suppressWarnings(readRDS(mf))

  if (!"main_policy_name" %in% names(meta) || all(is.na(meta$main_policy_name))) {
    cat(sprintf("EP%d: no policy areas yet, run scrape_policy.R first. Skipped.\n", leg))
    next
  }

  # The original restricted this to final votes (final_vote == 1). The
  # Parliament's API publishes no final-vote flag, and neither vote titles nor
  # the sitting order reconstruct it reliably, so by default every classified
  # vote is scored. Values are therefore on a different scale to the published
  # EP6-EP9 columns and should not be compared with them directly.
  #
  # --exclude-amendments drops votes whose title marks them as an amendment or
  # a paragraph-by-paragraph vote. That is a heuristic, not the original flag:
  # it keeps about a third of classified votes where the original kept under a
  # tenth. It gets closer to measuring a policy stance rather than sheer voting
  # volume, which is why it is offered, but it is not the published method.
  keep <- !is.na(meta$main_policy_name)
  if (drop_amendments) {
    if (!"vote_title" %in% names(meta)) {
      cat("   (no vote titles stored; re-run scrape_policy.R to use --exclude-amendments)\n")
    } else {
      am <- grepl(" - Am |Am [0-9]|Paragraph|paragraphe", enc2utf8(as.character(meta$vote_title)),
                  useBytes = TRUE)
      am[is.na(am)] <- FALSE
      keep <- keep & !am
    }
  }
  sc <- score_blocks(wide, meta$Vote_ID[keep], meta$main_policy_name[keep])

  wide[names(sc)] <- sc
  saveRDS(wide, wf, compress = "gzip")
  nz <- sum(rowSums(abs(as.matrix(sc))) > 0)
  cat(sprintf("EP%d: %d topic scores added for %d MEPs (%d with any non-zero score), from %d classified votes\n",
              leg, ncol(sc), nrow(wide), nz, sum(keep)))
}

cat(if (drop_amendments)
      "\nNote: amendment votes excluded. This approximates, but is not, the\npublished final-vote restriction. See the README.\n"
    else
      "\nNote: unlike the published EP6-EP9 columns, these cover all classified\nvotes rather than final votes only. See the README.\n")
