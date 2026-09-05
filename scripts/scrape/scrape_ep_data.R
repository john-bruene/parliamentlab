#!/usr/bin/env Rscript
# scrape_ep_data.R — extend the ParliamentLab dataset to the present day.
#
# The EP6-EP9 data came from VoteWatch Europe, which closed in 2022, leaving
# the dataset ending 9 June 2022. This script rebuilds the same structure from
# the European Parliament's own open data API, which publishes roll-call votes
# with the individual MEP breakdown.
#
# Usage (from the project root):
#   Rscript scripts/scrape/scrape_ep_data.R                # 2022-06-10 -> today
#   Rscript scripts/scrape/scrape_ep_data.R 2024-07-16 2025-12-31
#
# Every API response is cached under data/api_cache/, so the script is
# resumable and a second run is nearly free. Output goes to data/scraped/.
#
# Vote coding matches the existing files exactly:
#   1 = for, 2 = against, 3 = abstention,
#   4 = in office but did not vote, 0 = not an MEP at that time.

suppressPackageStartupMessages({ library(jsonlite) })
source(file.path("scripts", "scrape", "ep_api.R"))

args       <- commandArgs(trailingOnly = TRUE)
START_DATE <- as.Date(if (length(args) >= 1) args[1] else "2022-06-10")
END_DATE   <- as.Date(if (length(args) >= 2) args[2] else Sys.Date())

# Parliamentary terms. EP9 ran to the 2024 election; EP10 began 16 July 2024.
TERMS <- list(
  list(term = 9L, start = as.Date("2019-07-02"), end = as.Date("2024-07-15")),
  list(term = 10L, start = as.Date("2024-07-16"), end = as.Date("2099-12-31"))
)
term_of <- function(d) {
  for (t in TERMS) if (d >= t$start && d <= t$end) return(as.integer(t$term))
  NA_integer_
}

out_dir <- file.path("data", "scraped")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("=== ParliamentLab data extension: %s to %s ===\n", START_DATE, END_DATE))

# ── 1. Roll-call votes across every plenary sitting in range ────────────────
years <- seq(as.integer(format(START_DATE, "%Y")), as.integer(format(END_DATE, "%Y")))
meeting_ids <- unlist(lapply(years, function(y) {
  ids <- ep_meeting_ids(y)
  cat(sprintf("  %d: %d sitting days\n", y, length(ids)))
  ids
}))

# Keep only sittings inside the requested window
mdates <- as.Date(sub("^MTG-PL-", "", meeting_ids))
keep   <- !is.na(mdates) & mdates >= START_DATE & mdates <= END_DATE
meeting_ids <- meeting_ids[keep]
cat(sprintf("Sitting days in range: %d\n", length(meeting_ids)))

all_meta <- list(); all_voters <- list()
for (i in seq_along(meeting_ids)) {
  rc <- ep_rollcall_votes(meeting_ids[i])
  if (!is.null(rc)) {
    all_meta[[length(all_meta) + 1L]]     <- rc$meta
    all_voters[[length(all_voters) + 1L]] <- rc$voters
  }
  if (i %% 25 == 0) cat(sprintf("  fetched %d/%d sittings\n", i, length(meeting_ids)))
}
if (!length(all_meta)) stop("No roll-call votes found in this date range.")

meta   <- do.call(rbind, all_meta)
voters <- do.call(c, all_voters)
meta   <- meta[!duplicated(meta$voting_id), ]
voters <- voters[meta$voting_id]

meta$Date        <- as.Date(meta$date)
meta$Legislature <- vapply(meta$Date, term_of, integer(1))
meta <- meta[!is.na(meta$Legislature), ]
voters <- voters[meta$voting_id]

# Order chronologically, then by position within the sitting
meta <- meta[order(meta$Legislature, meta$Date, suppressWarnings(as.numeric(meta$order))), ]
voters <- voters[meta$voting_id]
cat(sprintf("Roll-call votes collected: %d (%s)\n", nrow(meta),
            paste(sprintf("EP%d: %d", sort(unique(meta$Legislature)),
                          as.integer(table(meta$Legislature))), collapse = ", ")))

# ── 2. MEPs who appear in those votes ───────────────────────────────────────
mep_ids <- sort(unique(unlist(lapply(voters, function(v) c(v$favor, v$against, v$abstention)))))
cat(sprintf("Distinct MEPs appearing in votes: %d\n", length(mep_ids)))

cat("Fetching MEP details (cached after the first run)...\n")
meps <- list()
for (i in seq_along(mep_ids)) {
  d <- ep_mep_detail(mep_ids[i])
  if (!is.null(d)) meps[[mep_ids[i]]] <- d
  if (i %% 100 == 0) cat(sprintf("  %d/%d\n", i, length(mep_ids)))
}

# Political group / national party / term window, as at a given date
# Political group / national party held at `on_date`. Candidates are first
# restricted to the term window: MEPs often sat in earlier parliaments, and
# without that filter a long-serving member could be labelled with a group
# that was dissolved years earlier.
mem_pick <- function(md, role, on_date, win_start = NULL, win_end = NULL) {
  if (is.null(md)) return(NA_character_)
  r <- md[!is.na(md$role) & md$role == role, , drop = FALSE]
  if (!nrow(r)) return(NA_character_)
  s <- as.Date(r$start, "%Y%m%d"); e <- as.Date(r$end, "%Y%m%d")
  e[is.na(e)] <- as.Date("2099-12-31")
  ok <- !is.na(s)
  if (!is.null(win_start) && !is.null(win_end)) ok <- ok & s <= win_end & e >= win_start
  if (!any(ok)) ok <- !is.na(s)                       # nothing in window: use all
  hit <- which(ok & s <= on_date & on_date <= e)
  if (!length(hit)) hit <- which(ok)[which.max(s[ok])]  # else the latest one that fits
  ep_org_label(r$org[hit[1]])
}
term_window <- function(md, term) {
  if (is.null(md)) return(c(NA, NA))
  r <- md[!is.na(md$org) & md$org == paste0("org/ep-", term), , drop = FALSE]
  if (!nrow(r)) return(c(NA, NA))
  c(as.Date(r$start[1], "%Y%m%d"), as.Date(r$end[1], "%Y%m%d"))
}

# ── 3. Build one wide matrix per legislature ────────────────────────────────
for (leg in sort(unique(meta$Legislature))) {
  m_leg <- meta[meta$Legislature == leg, ]
  v_leg <- voters[m_leg$voting_id]
  m_leg$Vote_ID <- seq_len(nrow(m_leg))

  ids_leg <- sort(unique(unlist(lapply(v_leg, function(v) c(v$favor, v$against, v$abstention)))))
  mat <- matrix(0L, nrow = length(ids_leg), ncol = nrow(m_leg),
                dimnames = list(ids_leg, paste0("X", m_leg$Vote_ID)))

  # In-office window per MEP, so absence (4) is distinguished from not-yet-
  # elected / already-left (0) exactly as in the VoteWatch-derived files.
  tw <- t(vapply(ids_leg, function(id) {
    w <- term_window(meps[[id]]$memberships, leg)
    c(if (is.na(w[1])) -Inf else as.numeric(w[1]),
      if (is.na(w[2]))  Inf else as.numeric(w[2]))
  }, numeric(2)))
  vdates <- as.numeric(m_leg$Date)
  for (j in seq_len(nrow(m_leg))) {
    in_office <- tw[, 1] <= vdates[j] & vdates[j] <= tw[, 2]
    mat[in_office, j] <- 4L
  }
  for (j in seq_len(nrow(m_leg))) {
    v <- v_leg[[j]]
    mat[rownames(mat) %in% v$favor,      j] <- 1L
    mat[rownames(mat) %in% v$against,    j] <- 2L
    mat[rownames(mat) %in% v$abstention, j] <- 3L
  }

  # ── MEP metadata + derived indices ────────────────────────────────────────
  ref_date  <- min(m_leg$Date)
  win_start <- min(m_leg$Date); win_end <- max(m_leg$Date)
  info <- do.call(rbind, lapply(ids_leg, function(id) {
    p <- meps[[id]]
    if (is.null(p)) p <- list()
    bd <- suppressWarnings(as.Date(p$birthdate %||% NA))
    data.frame(
      WebisteEpID = id,
      # Existing files name MEPs "SURNAME, Given"; the API returns
      # "Given SURNAME", so rebuild the house format for a clean join.
      FullName    = if (!is.null(p$LnameUpper) && !is.na(p$LnameUpper))
                      paste0(p$LnameUpper, ", ", p$Fname) else (p$FullName %||% NA_character_),
      Fname       = p$Fname %||% NA_character_,
      Lname       = p$Lname %||% NA_character_,
      Country     = p$Country %||% NA_character_,
      EPG         = mem_pick(p$memberships, "EU_POLITICAL_GROUP", ref_date, win_start, win_end),
      Party       = mem_pick(p$memberships, "NATIONAL_POLITICAL_GROUP", ref_date, win_start, win_end),
      birthdate   = as.character(bd),
      birthplace  = p$birthplace %||% NA_character_,
      Gender      = p$Gender %||% NA_character_,
      Photo       = p$Photo %||% NA_character_,
      Age_At_Start = if (is.na(bd)) NA_real_ else
        round(as.numeric(difftime(ref_date, bd, units = "days")) / 365.25, 1),
      stringsAsFactors = FALSE
    )
  }))
  info$full <- info$FullName

  # The three indices below follow the definitions used for the published
  # EP6-EP9 data (archive/Data_Prep.Rmd), which were checked by recomputing
  # them from the original files and matching the stored columns exactly.
  # They are not the obvious definitions: attendance counts any code other
  # than 0 (so recorded absences count as present), loyalty compares the
  # MEP's code with their group's most common code over all non-zero codes,
  # and winning is a NET score in [-1, 1] rather than a share.
  present <- mat != 0L

  # Attendance: share of the term's roll-calls where the MEP was an MEP.
  info$Attendance_Score <- round(rowSums(present) / ncol(mat), 4)

  # Winning: +1 for siding with the outcome, -1 against, over all non-zero
  # codes. The Parliament's API leaves the outcome empty on most votes, so it
  # is taken from the tally; that rule reproduces the official result_code on
  # 100% of the 13,459 original EP9 votes.
  adopted <- m_leg$yes > m_leg$no
  agree    <- sweep(mat == 1L, 2, adopted, "&") | sweep(mat == 2L, 2, !adopted, "&")
  disagree <- sweep(mat == 1L, 2, !adopted, "&") | sweep(mat == 2L, 2, adopted, "&")
  tot_w <- rowSums(present)
  info$Winning_Score <- round(ifelse(tot_w > 0,
                                     (rowSums(agree) - rowSums(disagree)) / tot_w, NA_real_), 4)

  # Loyalty: agreement with the group's most common code, over non-zero codes.
  info$loyalty_score <- NA_real_
  for (g in unique(info$EPG[!is.na(info$EPG)])) {
    rows <- which(info$EPG == g)
    sub  <- mat[rows, , drop = FALSE]
    maj  <- apply(sub, 2, function(col) {
      col <- col[col != 0L]
      if (!length(col)) return(NA_integer_)
      as.integer(names(sort(table(col), decreasing = TRUE))[1])
    })
    ok <- !is.na(maj)
    info$loyalty_score[rows] <- round(vapply(seq_along(rows), function(i) {
      keep <- sub[i, ] != 0L & ok
      if (!any(keep)) return(NA_real_)
      mean(sub[i, keep] == maj[keep])
    }, numeric(1)), 4)
  }

  wide <- cbind(info, as.data.frame(mat))
  saveRDS(wide,   file.path(out_dir, sprintf("P%d_votes.rds", leg)), compress = "gzip")
  saveRDS(m_leg,  file.path(out_dir, sprintf("P%d_votes_meta.rds", leg)), compress = "gzip")
  cat(sprintf("EP%d: %d MEPs x %d votes -> %s\n", leg, nrow(wide), nrow(m_leg),
              file.path(out_dir, sprintf("P%d_votes.rds", leg))))
}

cat("\nDone. Files written to ", out_dir, "\n", sep = "")
cat("Note: Activity_Index inputs (reports, questions, speeches) and the policy\n")
cat("area behind the topic scores are not in this API; see the README.\n")
