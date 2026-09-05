#!/usr/bin/env Rscript
# fill_birthdates.R — fill missing MEP birth dates from Wikidata.
#
# The Parliament's API does not publish a birth date for every MEP (123 of 738
# in EP10), which leaves Age_At_Start empty for those rows. Wikidata carries
# the EP person id as property P1186, so the join is exact rather than by name.
#
#   Rscript scripts/scrape/fill_birthdates.R            # data/P10_umap.rds
#   Rscript scripts/scrape/fill_birthdates.R data/P10_umap.rds 2024-07-17
#
# Existing dates are never overwritten. Checked against 40 MEPs whose date the
# API does publish, Wikidata agreed on 39; the one difference was a month, so
# treat the filled values as good but not authoritative.

suppressPackageStartupMessages(library(jsonlite))

args      <- commandArgs(trailingOnly = TRUE)
target    <- if (length(args) >= 1) args[1] else file.path("data", "P10_umap.rds")
ref_date  <- as.Date(if (length(args) >= 2) args[2] else "2024-07-17")
ENDPOINT  <- "https://query.wikidata.org/sparql"
UA        <- "ParliamentLab/1.0 (https://parliamentlab.eu; research use)"

d <- suppressWarnings(readRDS(target))
need <- which(is.na(d$birthdate) | !nzchar(as.character(d$birthdate)))
cat(sprintf("%s: %d of %d MEPs have no birth date\n", basename(target), length(need), nrow(d)))
if (!length(need)) quit(save = "no")

# Query in batches so one long URL cannot take the whole run down.
fetch <- function(ids) {
  vals <- paste(sprintf('"%s"', ids), collapse = " ")
  q <- sprintf('SELECT ?mepid ?dob WHERE { VALUES ?mepid { %s } ?p wdt:P1186 ?mepid . ?p wdt:P569 ?dob . }', vals)
  url <- paste0(ENDPOINT, "?", "format=json&query=", utils::URLencode(q, reserved = TRUE))
  tmp <- tempfile(fileext = ".json")
  ok <- FALSE
  for (attempt in 1:3) {
    res <- tryCatch(
      utils::download.file(url, tmp, quiet = TRUE, mode = "wb",
                           headers = c("User-Agent" = UA, "Accept" = "application/sparql-results+json")),
      error = function(e) -1L, warning = function(w) -1L)
    if (identical(as.integer(res), 0L) && file.exists(tmp) && file.info(tmp)$size > 10L) { ok <- TRUE; break }
    Sys.sleep(2 ^ attempt)
  }
  if (!ok) return(NULL)
  j <- tryCatch(jsonlite::fromJSON(tmp, simplifyVector = FALSE), error = function(e) NULL)
  unlink(tmp)
  if (is.null(j)) return(NULL)
  b <- j$results$bindings
  if (!length(b)) return(NULL)
  data.frame(
    id  = vapply(b, function(x) x$mepid$value, ""),
    dob = substr(vapply(b, function(x) x$dob$value, ""), 1, 10),
    stringsAsFactors = FALSE
  )
}

ids <- as.character(d$WebisteEpID[need])
found <- NULL
batches <- split(ids, ceiling(seq_along(ids) / 60))
for (i in seq_along(batches)) {
  r <- fetch(batches[[i]])
  if (!is.null(r)) found <- rbind(found, r)
  cat(sprintf("  batch %d/%d: %d ids -> %d dates so far\n", i, length(batches),
              length(batches[[i]]), if (is.null(found)) 0 else nrow(found)))
  Sys.sleep(1)   # be polite to the public endpoint
}
if (is.null(found) || !nrow(found)) { cat("nothing returned\n"); quit(save = "no") }

# Wikidata can hold several dates for one person (differing sources); keep the
# first and drop anything that is not a plausible birth year for a sitting MEP.
found <- found[!duplicated(found$id), ]
yr <- suppressWarnings(as.integer(substr(found$dob, 1, 4)))
found <- found[!is.na(yr) & yr >= 1920 & yr <= as.integer(format(Sys.Date(), "%Y")) - 17, ]

hit <- match(as.character(d$WebisteEpID), found$id)
fillable <- is.na(d$birthdate) & !is.na(hit)
d$birthdate[fillable] <- found$dob[hit[fillable]]

# Recompute Age_At_Start for exactly those rows, same definition as elsewhere.
bd <- suppressWarnings(as.Date(d$birthdate[fillable]))
d$Age_At_Start[fillable] <- round(as.numeric(difftime(ref_date, bd, units = "days")) / 365.25, 1)

saveRDS(d, target, compress = "gzip")
cat(sprintf("\nfilled %d birth dates (%d still missing)\n", sum(fillable), sum(is.na(d$birthdate))))
cat(sprintf("Age_At_Start now known for %d of %d MEPs\n", sum(!is.na(d$Age_At_Start)), nrow(d)))
