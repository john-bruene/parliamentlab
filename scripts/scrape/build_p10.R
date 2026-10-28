#!/usr/bin/env Rscript
# build_p10.R — assemble data/P10_umap.rds in the schema the app expects.
#
# Takes the scraped EP10 files, adds the columns the app selects but the
# scrape cannot provide directly, and computes the "_red" coordinates.
#
#   Rscript scripts/scrape/build_p10.R
#
# Prerequisites: run_all.R has produced data/scraped/P10_votes.rds and
# P10_activities.rds. Needs the packages the app itself uses (wnominate,
# FactoMineR, umap, cluster).
#
# EP9 is deliberately left alone: the published EP9 file backs a published
# paper, and its group names use short codes that would clash with the long
# names the Parliament's API returns.

suppressPackageStartupMessages({
  library(cluster); library(umap); library(FactoMineR); library(wnominate)
})

out_file <- file.path("data", "P10_umap.rds")
TERM_START <- as.Date("2024-07-16")

# Votes on the text as a whole, as opposed to amendments and paragraphs. The
# app's "Use Only Final Votes" switch selects the _red coordinates, which the
# published data derived from a final_vote flag. The Parliament's API does not
# publish that flag, so it is reconstructed from the vote title using the
# vocabulary that only ever carried final_vote == 1 in the published data
# (single vote, legislative resolution, ... as a whole, consent, Commission
# proposal, discharge, referral back). On EP10 this flags 7.8% of roll-calls
# where the published EP9 flag covered 9.3%, so it is close but approximate.
FINAL_RX <- paste0("as a whole|single vote|legislative resolution|",
                   "^motion for a resolution$|^commission proposal$|^decision$|",
                   "consent|approbation|discharge|referral back|",
                   "^recommendation$|^draft decision$")

d <- suppressWarnings(readRDS(file.path("data", "scraped", "P10_votes.rds")))
a <- suppressWarnings(readRDS(file.path("data", "scraped", "P10_activities.rds")))
m <- suppressWarnings(readRDS(file.path("data", "scraped", "P10_votes_meta.rds")))
stopifnot(identical(as.character(d$WebisteEpID), as.character(a$mep_id)))

# ── Activity columns ────────────────────────────────────────────────────────
acols <- setdiff(names(a), c("mep_id", "FullName"))
d[acols] <- a[acols]
cat(sprintf("activities merged: %d columns\n", length(acols)))

# ── Full-matrix coordinates (coord1D/2D, MCA1/2, UMAP1/2) ──────────────────
# W-NOMINATE over all 5,568 roll-calls takes about 25 minutes, so a previous
# result is reused when one is on disk. Delete the candidate file to force a
# recomputation.
cand <- file.path("data", "scraped", "P10_umap_candidate.rds")
full_cols <- c("coord1D", "coord2D", "MCA1", "MCA2", "UMAP1", "UMAP2")
xc_all <- grep("^X[0-9]+$", names(d), value = TRUE)
if (file.exists(cand)) {
  cc <- suppressWarnings(readRDS(cand))
  if (all(full_cols %in% names(cc)) &&
      identical(as.character(cc$WebisteEpID), as.character(d$WebisteEpID))) {
    d[full_cols] <- cc[full_cols]
    cat("full-matrix coordinates reused from the candidate file\n")
  }
}
if (!all(full_cols %in% names(d))) {
  cat("computing full-matrix coordinates (about 25 minutes)...\n")
  Ma <- as.matrix(d[, xc_all])
  rca <- pscl::rollcall(Ma, yea = 1, nay = 2, missing = c(3, 4), notInLegis = 0,
                        legis.names = make.unique(as.character(d$FullName)))
  pa <- which(d$EPG == "European Conservatives and Reformists Group")[1]
  wa <- wnominate::wnominate(rca, dims = 2, polarity = c(pa, pa),
                             minvotes = 20, lop = 0.025, verbose = FALSE)
  d$coord1D <- wa$legislators$coord1D; d$coord2D <- wa$legislators$coord2D
  fa <- as.data.frame(lapply(as.data.frame(Ma), factor))
  mca <- FactoMineR::MCA(fa, ncp = 2, graph = FALSE)
  d$MCA1 <- mca$ind$coord[, 1]; d$MCA2 <- mca$ind$coord[, 2]
  ga <- cluster::daisy(fa, metric = "gower")
  cfa <- umap::umap.defaults; cfa$input <- "dist"; cfa$n_components <- 2
  set.seed(123)
  ua <- umap::umap(as.matrix(ga), config = cfa)
  d$UMAP1 <- ua$layout[, 1]; d$UMAP2 <- ua$layout[, 2]
}

# ── Term dates and experience, from the cached membership records ───────────
source(file.path("scripts", "scrape", "ep_api.R"))
starts <- ends <- rep(NA_character_, nrow(d)); exper <- rep(NA_real_, nrow(d))
for (i in seq_len(nrow(d))) {
  p <- ep_mep_detail(d$WebisteEpID[i])
  md <- if (is.null(p)) NULL else p$memberships
  if (is.null(md)) next
  terms <- md[!is.na(md$org) & grepl("^org/ep-", md$org), , drop = FALSE]
  # The API lists each term membership twice, once tagged EU_INSTITUTION and
  # once untagged, so it must be deduplicated on the term and its dates only.
  # Including `role` in the key leaves both copies and doubles the experience
  # (a 30-year member would read as 60).
  terms <- unique(terms[, c("org", "start", "end")])
  if (!nrow(terms)) next
  s <- as.Date(terms$start, "%Y%m%d"); e <- as.Date(terms$end, "%Y%m%d")
  cur <- which(terms$org == "org/ep-10")
  if (length(cur)) {
    starts[i] <- as.character(s[cur[1]])
    ends[i]   <- as.character(e[cur[1]])
  }
  # Experience: days served in earlier terms before this one began.
  prev <- which(terms$org != "org/ep-10" & !is.na(s) & s < TERM_START)
  if (length(prev)) {
    ee <- e[prev]                       # ifelse() would drop the Date class
    ee[is.na(ee)] <- TERM_START
    ee <- pmin(ee, TERM_START)
    exper[i] <- sum(pmax(as.numeric(ee - s[prev]), 0), na.rm = TRUE)
  } else exper[i] <- 0
}
d$Start <- starts; d$End <- ends
d$Experience_at_Start <- exper
# The API returns ISO-3166 alpha-3 codes ("DEU"); the app keys its flag and
# country-map lookups on full English names ("Germany"), as the published
# files do. Translate so the hemicycle profile and the country map work.
ISO3 <- c(AUT="Austria", BEL="Belgium", BGR="Bulgaria", HRV="Croatia", CYP="Cyprus",
          CZE="Czech Republic", DNK="Denmark", EST="Estonia", FIN="Finland",
          FRA="France", DEU="Germany", GRC="Greece", HUN="Hungary", IRL="Ireland",
          ITA="Italy", LVA="Latvia", LTU="Lithuania", LUX="Luxembourg", MLT="Malta",
          NLD="Netherlands", POL="Poland", PRT="Portugal", ROU="Romania",
          SVK="Slovakia", SVN="Slovenia", ESP="Spain", SWE="Sweden",
          GBR="United Kingdom")
unmapped <- setdiff(unique(d$Country), names(ISO3))
if (length(unmapped)) cat("WARNING unmapped country codes:", paste(unmapped, collapse=", "), "\n")
d$Country <- unname(ifelse(d$Country %in% names(ISO3), ISO3[d$Country], d$Country))

d$title  <- NA_character_   # honorific; not published by the API
d$CV     <- NA_character_   # ~80% missing in the original and excluded there
d$active <- is.na(ends)
cat(sprintf("experience computed: %d of %d MEPs (median %.0f days)\n",
            sum(!is.na(exper)), nrow(d), median(exper, na.rm = TRUE)))

# ── "_red" coordinates on the final-vote subset ─────────────────────────────
xc <- grep("^X[0-9]+$", names(d), value = TRUE)
ti <- tolower(trimws(enc2utf8(as.character(m$title)))); ti[is.na(ti)] <- ""
fin_ids <- m$Vote_ID[grepl(FINAL_RX, ti, useBytes = TRUE)]
red_cols <- intersect(paste0("X", fin_ids), xc)
cat(sprintf("final-vote subset: %d of %d roll-calls (%.1f%%)\n",
            length(red_cols), length(xc), 100 * length(red_cols) / length(xc)))

Mr <- as.matrix(d[, red_cols])
rc <- pscl::rollcall(Mr, yea = 1, nay = 2, missing = c(3, 4), notInLegis = 0,
                     legis.names = make.unique(as.character(d$FullName)))
pol <- which(d$EPG == "European Conservatives and Reformists Group")[1]
w <- wnominate::wnominate(rc, dims = 2, polarity = c(pol, pol),
                          minvotes = 10, lop = 0.025, verbose = FALSE)
d$coord1D_red <- w$legislators$coord1D
d$coord2D_red <- w$legislators$coord2D

fr <- as.data.frame(lapply(as.data.frame(Mr), factor))
mc <- FactoMineR::MCA(fr, ncp = 2, graph = FALSE)
d$MCA1_red <- mc$ind$coord[, 1]; d$MCA2_red <- mc$ind$coord[, 2]

g <- cluster::daisy(fr, metric = "gower")
cfg <- umap::umap.defaults; cfg$input <- "dist"; cfg$n_components <- 2
set.seed(123)
u <- umap::umap(as.matrix(g), config = cfg)
d$UMAP1_red <- u$layout[, 1]; d$UMAP2_red <- u$layout[, 2]
cat("reduced coordinates computed\n")

# ── Topic scores: not available for EP10 ────────────────────────────────────
# Parltrack's dossier dump stops on 25 April 2024, so EP10 votes have no
# policy area and the per-topic scores cannot be built. The columns are
# created as NA so the app's select() calls still resolve.
topic_cols <- c("economic_votesScore", "social_votesScore", "foreign_policy_votesScore",
                "industry_votesScore", "health_votesScore", "gender_votesScore",
                "law_votesScore", "agriculture_fisheries_votesScore", "budget_votesScore",
                "civil_liberties_votesScore", "education_votesScore", "petitions_votesScore",
                "internal_market_votesScore", "international_trade_votesScore",
                "regional_development_votesScore", "transport_tourism_votesScore")
for (cl in topic_cols) if (!cl %in% names(d)) d[[cl]] <- NA_real_

# ── Check against what the app selects, then write ──────────────────────────
ref <- suppressWarnings(readRDS(file.path("data", "P9_umap.rds")))
src <- readLines("server.R", warn = FALSE)
sel <- unlist(regmatches(src, gregexpr("(?<=select\\()[^)]*", src, perl = TRUE)))
tok <- unique(unlist(strsplit(gsub("[\"']", "", paste(sel, collapse = ",")), "[,[:space:]]+")))
need <- intersect(tok[grepl("^[A-Za-z][A-Za-z0-9_]*$", tok)], names(ref))
miss <- setdiff(need, names(d))
if (length(miss)) {
  cat("STILL MISSING:", paste(miss, collapse = ", "), "\n")
} else {
  cat("all columns the app selects are present\n")
}

saveRDS(d, out_file, compress = "gzip")
cat(sprintf("wrote %s: %d MEPs x %d columns (%.1f MB)\n", out_file, nrow(d), ncol(d),
            file.info(out_file)$size / 1048576))
