#!/usr/bin/env Rscript
# scrape_policy.R — attach main_policy_name to the scraped roll-call votes.
#
# The Parliament's API publishes votes without any committee or procedure
# reference, so the policy area is recovered through Parltrack:
#
#   our vote id -> Parltrack voteid -> procedure reference -> dossier ->
#   responsible committee -> main_policy_name
#
# The last step reuses the committee-to-policy correspondence already present
# in the original EP6-EP9 data, so the vocabulary stays identical instead of
# inventing new labels.
#
# Usage (from the project root):
#   Rscript scripts/scrape/scrape_policy.R
#
# Requires the two Parltrack dumps; they are downloaded on first use.

DUMPS <- list(
  votes    = list(url = "https://parltrack.org/dumps/ep_votes.json.lz",
                  path = file.path("data", "api_cache", "ep_votes.json.lz")),
  dossiers = list(url = "https://parltrack.org/dumps/ep_dossiers.json.lz",
                  path = file.path("data", "api_cache", "ep_dossiers.json.lz"))
)
out_dir <- file.path("data", "scraped")
helper  <- file.path("scripts", "scrape", "extract_policy.py")

for (d in DUMPS) {
  if (!file.exists(d$path) || file.info(d$path)$size < 1e6) {
    cat("Downloading ", basename(d$path), " ...\n", sep = "")
    options(timeout = 900)
    ok <- tryCatch(download.file(d$url, d$path, quiet = TRUE, mode = "wb"),
                   error = function(e) -1L, warning = function(w) -1L)
    if (!identical(as.integer(ok), 0L)) stop("Could not download ", d$url)
  }
}
if (!file.exists(helper)) stop("Missing helper: ", helper)

# ── 1. vote -> committee, via Parltrack ─────────────────────────────────────
csv <- tempfile(fileext = ".csv")
cat("Resolving procedure references (this takes a few minutes)...\n")
log <- system2("python3", c(shQuote(helper), shQuote(DUMPS$votes$path),
                            shQuote(DUMPS$dossiers$path), shQuote(csv), "2022-06-01"),
               stdout = TRUE, stderr = TRUE)
cat(paste0("  ", log, collapse = "\n"), "\n")
if (!file.exists(csv)) stop("policy extraction failed")
pol <- utils::read.csv(csv, stringsAsFactors = FALSE, colClasses = "character")
unlink(csv)

# ── 2. committee -> main_policy_name, learned from the original data ────────
# committee_code in EP6_9_Voted appears both as a code (ENVI) and as a full
# name (Committee on the Environment...), so both forms are normalised to the
# same key and each committee takes its modal policy area.
norm <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- gsub("^COMMITTEE ON( THE)? ", "", x)
  gsub("[^A-Z0-9]+", "", x)
}
ref <- suppressWarnings(readRDS(file.path("data", "EP6_9_Voted.rds")))
ref <- ref[!is.na(ref$committee_code) & ref$committee_code != "0" &
             !is.na(ref$main_policy_name) & ref$main_policy_name != "", ]
key <- norm(ref$committee_code)
crosswalk <- tapply(ref$main_policy_name, key, function(v) names(sort(table(v), decreasing = TRUE))[1])
cat(sprintf("Committee-to-policy crosswalk learned from %d original votes, %d committees\n",
            nrow(ref), length(crosswalk)))

lookup <- function(code, full) {
  out <- rep(NA_character_, length(code))
  for (k in list(norm(code), norm(full))) {
    miss <- is.na(out) & nzchar(k)
    out[miss] <- crosswalk[k[miss]]
  }
  unname(out)
}
pol$main_policy_name <- lookup(pol$committee_code, pol$committee_full)

# ── 3. Attach to the scraped vote metadata ──────────────────────────────────
for (leg in c(9, 10)) {
  f <- file.path(out_dir, sprintf("P%d_votes_meta.rds", leg))
  if (!file.exists(f)) next
  m <- suppressWarnings(readRDS(f))
  i <- match(as.character(m$voting_id), pol$voting_id)
  m$epref            <- pol$epref[i]
  m$committee_code   <- pol$committee_full[i]
  m$main_policy_name <- pol$main_policy_name[i]
  m$procedure_title  <- pol$procedure_title[i]
  m$vote_title       <- pol$vote_title[i]
  saveRDS(m, f, compress = "gzip")
  cat(sprintf("EP%d: %d votes, %d with a policy area (%.1f%%) -> %s\n",
              leg, nrow(m), sum(!is.na(m$main_policy_name)),
              100 * mean(!is.na(m$main_policy_name)), f))
}

cat("\nParltrack's vote dump currently ends on 25 April 2024, so EP10 votes\n")
cat("have no policy area yet. Re-run once a newer dump is published.\n")
