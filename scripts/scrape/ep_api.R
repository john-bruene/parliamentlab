# ep_api.R — client for the European Parliament open data API
# (https://data.europarl.europa.eu/api/v2)
#
# VoteWatch Europe, the source behind the EP6-EP9 data, closed in 2022. This
# client replaces it with the Parliament's own open data service, which carries
# roll-call votes with the individual MEP breakdown we need.
#
# Sourced by scrape_ep_data.R. Every response is cached on disk, so re-running
# a scrape costs nothing and an interrupted run resumes where it stopped.

suppressPackageStartupMessages(library(jsonlite))

EP_API    <- "https://data.europarl.europa.eu/api/v2"
EP_FORMAT <- "application/ld+json"
EP_PAUSE  <- 0.15   # seconds between live requests, to stay polite

ep_cache_dir <- function() {
  d <- file.path("data", "api_cache")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

# GET one endpoint and return parsed JSON. `key` names the cache file; a cached
# response is reused verbatim, which is what makes the scrape resumable.
ep_get <- function(path, params = list(), key = NULL, quiet = TRUE) {
  if (is.null(key)) key <- gsub("[^A-Za-z0-9]+", "_", paste(path, unlist(params), collapse = "_"))
  cache_file <- file.path(ep_cache_dir(), paste0(key, ".json"))

  if (file.exists(cache_file) && file.info(cache_file)$size > 2L) {
    return(tryCatch(fromJSON(cache_file, simplifyVector = FALSE),
                    error = function(e) NULL))
  }

  params$format <- EP_FORMAT
  qs  <- paste(sprintf("%s=%s", names(params),
                       vapply(params, function(v) utils::URLencode(as.character(v), reserved = TRUE), "")),
               collapse = "&")
  url <- sprintf("%s/%s?%s", EP_API, path, qs)

  tmp <- tempfile(fileext = ".json")
  ok  <- FALSE
  for (attempt in 1:5) {
    res <- tryCatch(download.file(url, tmp, quiet = TRUE, mode = "wb"),
                    error = function(e) -1L, warning = function(w) -1L)
    if (identical(as.integer(res), 0L) && file.exists(tmp) && file.info(tmp)$size > 2L) {
      ok <- TRUE; break
    }
    Sys.sleep(2 ^ (attempt - 1))   # back off 1, 2, 4, 8s: the API throttles
  }
  Sys.sleep(EP_PAUSE)

  # A failure is NOT cached. Writing an empty placeholder here would turn a
  # passing rate-limit into permanent missing data: whole sitting days would
  # silently carry no votes on every later run. Failures are re-tried instead.
  if (!ok) {
    message("  [api] request failed, will retry on the next run: ", path)
    return(NULL)
  }
  file.rename(tmp, cache_file)
  tryCatch(fromJSON(cache_file, simplifyVector = FALSE), error = function(e) NULL)
}

# ── Plenary sitting days ────────────────────────────────────────────────────
ep_meeting_ids <- function(year) {
  d <- ep_get("meetings", list(year = year, limit = 400), key = paste0("meetings_", year))
  items <- d$data
  if (is.null(items) || !length(items)) return(character(0))
  ids <- vapply(items, function(x) x$activity_id %||% NA_character_, "")
  sort(unique(ids[!is.na(ids)]))
}

# ── Roll-call votes for one sitting day ─────────────────────────────────────
# Returns a list with `meta` (one row per vote) and `voters` (per-vote id lists).
# Only VOTE_ELECTRONIC_ROLLCALL decisions carry individual votes; everything
# else (show of hands, electronic without roll call) is skipped.
ep_rollcall_votes <- function(meeting_id) {
  d <- ep_get(paste0("meetings/", meeting_id, "/decisions"),
              key = paste0("dec_", meeting_id))
  items <- d$data
  if (is.null(items) || !length(items)) return(NULL)

  keep <- Filter(function(r) {
    !is.null(r$had_voter_favor) || !is.null(r$had_voter_against) ||
      !is.null(r$had_voter_abstention)
  }, items)
  if (!length(keep)) return(NULL)

  chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)[1]
  num <- function(x) if (is.null(x)) NA_real_    else as.numeric(x)[1]
  ids <- function(x) if (is.null(x)) character(0) else
    sub("^person/", "", unlist(x, use.names = FALSE))

  meta <- do.call(rbind, lapply(keep, function(r) {
    lab <- r$referenceText %||% r$activity_label
    data.frame(
      voting_id   = chr(r$notation_votingId),
      activity_id = chr(r$activity_id),
      date        = chr(r$activity_date),
      title       = chr(lab$en %||% lab$mul %||% NA_character_),
      method      = sub(".*/", "", chr(r$decision_method)),
      outcome     = sub(".*/", "", chr(r$decision_outcome)),
      order       = chr(r$activity_order),
      yes         = num(r$number_of_votes_favor),
      no          = num(r$number_of_votes_against),
      abstain     = num(r$number_of_votes_abstention),
      attendees   = num(r$number_of_attendees),
      document    = chr(unlist(r$recorded_in_a_realization_of)[1]),
      stringsAsFactors = FALSE
    )
  }))

  voters <- lapply(keep, function(r) list(
    favor      = ids(r$had_voter_favor),
    against    = ids(r$had_voter_against),
    abstention = ids(r$had_voter_abstention)
  ))
  names(voters) <- meta$voting_id

  list(meta = meta, voters = voters)
}

# ── MEPs of one parliamentary term ──────────────────────────────────────────
ep_term_meps <- function(term) {
  d <- ep_get("meps", list(`parliamentary-term` = term, limit = 1500),
              key = paste0("meps_term_", term))
  items <- d$data
  if (is.null(items) || !length(items)) return(character(0))
  unique(vapply(items, function(x) as.character(x$identifier %||% NA), ""))
}

# One MEP's biography plus the memberships we need for group and party.
ep_mep_detail <- function(id) {
  d <- ep_get(paste0("meps/", id), key = paste0("mep_", id))
  r <- if (!is.null(d$data) && length(d$data)) d$data[[1]] else NULL
  if (is.null(r)) return(NULL)
  chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)[1]

  country <- sub(".*/", "", chr(r$citizenship))
  gender  <- sub(".*/", "", chr(r$hasGender))

  # hasMembership holds political group, national party and term memberships,
  # each with its own validity window.
  mems <- r$hasMembership
  parse_mem <- function(m) {
    org  <- chr(m$organization)
    role <- sub(".*/", "", chr(m$membershipClassification))
    per  <- m$memberDuring$id %||% ""
    per  <- sub("^time-period/", "", as.character(per)[1])
    parts <- strsplit(per, "-", fixed = TRUE)[[1]]
    data.frame(org = org, role = role,
               start = if (length(parts) >= 1) parts[1] else NA_character_,
               end   = if (length(parts) >= 2) parts[2] else NA_character_,
               stringsAsFactors = FALSE)
  }
  mem_df <- if (!is.null(mems) && length(mems)) do.call(rbind, lapply(mems, parse_mem)) else NULL

  list(
    id         = chr(r$identifier),
    FullName   = chr(r$label),
    Fname      = chr(r$givenName),
    Lname      = chr(r$familyName),
    LnameUpper = chr(r$upperFamilyName %||% r$familyName),
    Country    = country,
    birthdate  = chr(r$bday),
    birthplace = chr(r$placeOfBirth),
    Gender     = gender,
    Photo      = chr(r$img),
    memberships = mem_df
  )
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

# ── Organisation names (political groups, national parties) ─────────────────
# Membership records reference organisations as "org/5191"; this resolves the
# id to its English label. Results are cached, and the same handful of groups
# recurs across thousands of MEPs, so this costs very few live requests.
ep_org_label <- function(org_ref) {
  if (is.na(org_ref) || !nzchar(org_ref)) return(NA_character_)
  oid <- sub("^org/", "", org_ref)
  if (!grepl("^[0-9]+$", oid)) return(NA_character_)   # e.g. "ep-9", not a body
  d <- ep_get(paste0("corporate-bodies/", oid), key = paste0("org_", oid))
  r <- if (!is.null(d$data) && length(d$data)) d$data[[1]] else NULL
  if (is.null(r)) return(NA_character_)
  lab <- r$prefLabel %||% r$label
  if (is.list(lab)) as.character(lab$en %||% lab$mul %||% NA_character_) else as.character(lab)[1]
}
