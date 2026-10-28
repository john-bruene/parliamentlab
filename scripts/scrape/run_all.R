#!/usr/bin/env Rscript
# run_all.R — run the whole data-extension pipeline in the right order.
#
# The stages are not independent. scrape_ep_data.R rewrites the vote files
# from scratch, which drops the policy areas and topic scores that the later
# stages add to them. Running the stages out of order, or re-running only the
# first one, therefore leaves the dataset silently incomplete. This script
# runs all four in sequence so that cannot happen.
#
#   Rscript scripts/scrape/run_all.R                     # 2022-06-10 to today
#   Rscript scripts/scrape/run_all.R 2024-07-16 2025-12-31
#
# Everything is cached under data/api_cache/, so a repeat run is cheap.

args  <- commandArgs(trailingOnly = TRUE)
start <- if (length(args) >= 1) args[1] else "2022-06-10"
end   <- if (length(args) >= 2) args[2] else as.character(Sys.Date())

stages <- list(
  list(script = "scrape_ep_data.R",     args = c(start, end),
       what = "roll-call votes, MEPs and the derived indices"),
  list(script = "scrape_activities.R",  args = character(0),
       what = "activity counts (speeches, reports, questions)"),
  list(script = "scrape_policy.R",      args = character(0),
       what = "policy area per vote"),
  list(script = "scrape_topic_scores.R", args = character(0),
       what = "per-topic voting scores")
)

for (i in seq_along(stages)) {
  s <- stages[[i]]
  f <- file.path("scripts", "scrape", s$script)
  if (!file.exists(f)) stop("missing stage script: ", f)
  cat(sprintf("\n=== stage %d/%d: %s ===\n    %s\n", i, length(stages), s$script, s$what))
  status <- system2("Rscript", c(shQuote(f), s$args))
  if (!identical(as.integer(status), 0L)) {
    stop("stage ", s$script, " failed (exit ", status, "); later stages not run")
  }
}

cat("\n=== pipeline complete ===\n")
for (leg in c(9, 10)) {
  f <- file.path("data", "scraped", sprintf("P%d_votes.rds", leg))
  m <- file.path("data", "scraped", sprintf("P%d_votes_meta.rds", leg))
  if (!file.exists(f)) next
  w <- suppressWarnings(readRDS(f)); mm <- suppressWarnings(readRDS(m))
  cat(sprintf("EP%d: %d MEPs, %d votes, %d with a policy area, %d topic columns\n",
              leg, nrow(w), nrow(mm),
              sum(!is.na(mm$main_policy_name)), sum(grepl("votesScore$", names(w)))))
}
