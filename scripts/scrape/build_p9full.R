#!/usr/bin/env Rscript
# build_p9full.R — EP9 over its full term (2019-2024) as an ADDITIONAL file.
#
# data/P9_umap.rds is left exactly as published: it backs the companion
# article and its figures have to stay reproducible. This script writes a
# separate data/P9full_umap.rds that joins the published 2019-2022 records to
# the 2022-2024 remainder scraped from the Parliament's API, so the complete
# term can be explored alongside the published one.
#
#   Rscript scripts/scrape/build_p9full.R
#
# The two halves join exactly on the MEP id (696 of 758 scraped MEPs are
# already in the published file, 62 are genuine replacements), so no name
# matching is involved.

suppressPackageStartupMessages({
  library(cluster); library(umap); library(FactoMineR); library(wnominate)
})

TERM_START <- as.Date("2019-07-02")
TERM_END   <- as.Date("2024-07-15")
out_file   <- file.path("data", "P9full_umap.rds")

pub      <- suppressWarnings(readRDS(file.path("data", "P9_umap.rds")))
pub_meta <- suppressWarnings(readRDS(file.path("data", "EP6_9_Voted.rds")))
pub_meta <- pub_meta[pub_meta$Legislature == 9, ]
new      <- suppressWarnings(readRDS(file.path("data", "scraped", "P9_votes.rds")))
new_meta <- suppressWarnings(readRDS(file.path("data", "scraped", "P9_votes_meta.rds")))

# ── 1. MEP union, joined on the Parliament's person id ──────────────────────
pid <- as.character(pub$WebisteEpID); nid <- as.character(new$WebisteEpID)
ids <- union(pid, nid)
ip  <- match(ids, pid); iN <- match(ids, nid)
cat(sprintf("MEPs: %d published + %d scraped -> %d over the full term\n",
            length(pid), length(nid), length(ids)))

pick <- function(col, prefer_published = TRUE) {
  a <- if (col %in% names(pub)) as.character(pub[[col]])[ip] else rep(NA_character_, length(ids))
  b <- if (col %in% names(new)) as.character(new[[col]])[iN] else rep(NA_character_, length(ids))
  if (prefer_published) ifelse(!is.na(a), a, b) else ifelse(!is.na(b), b, a)
}

# ── 2. Group names: the published file uses short codes, the API long names.
# The mapping was read off the 696 MEPs present in both, where every short
# code resolves to exactly one long name.
EPG_LONG <- c(
  "EPP"                  = "Group of the European People's Party (Christian Democrats)",
  "Socialists_Democrats" = "Group of the Progressive Alliance of Socialists and Democrats in the European Parliament",
  "REG"                  = "Renew Europe Group",
  "Greens_EFA"           = "Group of the Greens/European Free Alliance",
  "ECR"                  = "European Conservatives and Reformists Group",
  "IDG"                  = "Identity and Democracy Group",
  "The Left"             = "The Left group in the European Parliament - GUE/NGL",
  "NI"                   = "Non-attached Members",
  "Non-attached Members" = "Non-attached Members"
)
epg_pub <- if ("EPG" %in% names(pub)) as.character(pub$EPG)[ip] else NA_character_
epg_pub <- unname(ifelse(epg_pub %in% names(EPG_LONG), EPG_LONG[epg_pub], epg_pub))
epg_new <- as.character(new$EPG)[iN]
info <- data.frame(
  WebisteEpID = ids,
  FullName    = pick("FullName"),
  Country     = pick("Country"),
  Party       = pick("Party"),
  EPG         = ifelse(!is.na(epg_pub), epg_pub, epg_new),
  birthdate   = pick("birthdate"),
  birthplace  = pick("birthplace"),
  Gender      = pick("Gender"),
  Photo       = pick("Photo"),
  title       = pick("title"),
  CV          = pick("CV"),
  stringsAsFactors = FALSE
)
# The 62 MEPs who only appear in the scraped half carry the API's ISO-3166
# alpha-3 codes ("DEU"), while the published half uses full English names.
# The app keys its flag and country-map lookups on the full names, so a mixed
# column would split Germany into two entries.
ISO3 <- c(AUT="Austria", BEL="Belgium", BGR="Bulgaria", HRV="Croatia", CYP="Cyprus",
          CZE="Czech Republic", DNK="Denmark", EST="Estonia", FIN="Finland",
          FRA="France", DEU="Germany", GRC="Greece", HUN="Hungary", IRL="Ireland",
          ITA="Italy", LVA="Latvia", LTU="Lithuania", LUX="Luxembourg", MLT="Malta",
          NLD="Netherlands", POL="Poland", PRT="Portugal", ROU="Romania",
          SVK="Slovakia", SVN="Slovenia", ESP="Spain", SWE="Sweden",
          GBR="United Kingdom")
unmapped <- setdiff(unique(info$Country[grepl("^[A-Z]{3}$", info$Country)]), names(ISO3))
if (length(unmapped)) cat("WARNING unmapped country codes:", paste(unmapped, collapse = ", "), "\n")
info$Country <- unname(ifelse(info$Country %in% names(ISO3), ISO3[info$Country], info$Country))

info$full <- info$FullName
bd <- suppressWarnings(as.Date(info$birthdate))
info$Age_At_Start <- round(as.numeric(difftime(TERM_START, bd, units = "days")) / 365.25, 1)
# Seniority. The published half already carries it; the 62 replacements do
# not, and defaulting them to zero would show a member who has served since
# 2009 as a newcomer. Derive theirs from the API's term memberships, the same
# way build_p10.R does.
info$Experience_at_Start <- suppressWarnings(as.numeric(pub$Experience_at_Start))[ip]
need_exp <- which(is.na(info$Experience_at_Start))
if (length(need_exp)) {
  source(file.path("scripts", "scrape", "ep_api.R"))
  for (i in need_exp) {
    detail <- ep_mep_detail(info$WebisteEpID[i])
    md <- if (is.null(detail)) NULL else detail$memberships
    if (is.null(md)) next
    terms <- md[!is.na(md$org) & grepl("^org/ep-", md$org), , drop = FALSE]
    # The API lists each term twice, tagged and untagged; deduplicate on the
    # term and its dates only, or the experience comes out doubled.
    terms <- unique(terms[, c("org", "start", "end")])
    if (!nrow(terms)) next
    st <- as.Date(terms$start, "%Y%m%d"); en <- as.Date(terms$end, "%Y%m%d")
    prev <- which(terms$org != "org/ep-9" & !is.na(st) & st < TERM_START)
    if (!length(prev)) { info$Experience_at_Start[i] <- 0; next }
    ee <- en[prev]                      # ifelse() would drop the Date class
    ee[is.na(ee)] <- TERM_START
    ee <- pmin(ee, TERM_START)
    info$Experience_at_Start[i] <- sum(pmax(as.numeric(ee - st[prev]), 0), na.rm = TRUE)
  }
  cat(sprintf("experience derived from the API for %d MEPs (%d had served before)\n",
              length(need_exp), sum(info$Experience_at_Start[need_exp] > 0, na.rm = TRUE)))
}
info$Experience_at_Start[is.na(info$Experience_at_Start)] <- 0
cat(sprintf("group names harmonised; EPG missing for %d MEPs\n", sum(is.na(info$EPG))))

# ── 3. Vote matrix ──────────────────────────────────────────────────────────
# The published half distinguishes three kinds of absence (4, 5, 6); the API
# only reports "not in any voting list". Codes 5 and 6 are folded into 4 so
# that one convention applies across the whole term, which is what the derived
# indices below are computed on.
px <- grep("^X[0-9]+$", names(pub), value = TRUE)
nx <- grep("^X[0-9]+$", names(new), value = TRUE)
n_old <- length(px)
cat(sprintf("votes: %d published + %d scraped -> %d\n", n_old, length(nx), n_old + length(nx)))

M <- matrix(0L, nrow = length(ids), ncol = n_old + length(nx),
            dimnames = list(ids, paste0("X", seq_len(n_old + length(nx)))))
old_block <- as.matrix(pub[, px]); storage.mode(old_block) <- "integer"
old_block[old_block %in% c(5L, 6L)] <- 4L
M[!is.na(ip), seq_len(n_old)] <- old_block[ip[!is.na(ip)], ]
new_block <- as.matrix(new[, nx]); storage.mode(new_block) <- "integer"
M[!is.na(iN), n_old + seq_along(nx)] <- new_block[iN[!is.na(iN)], ]

# ── 4. Combined vote metadata ───────────────────────────────────────────────
old_ids <- as.integer(sub("^X", "", px))
meta_old <- data.frame(
  Vote_ID   = seq_len(n_old),
  Date      = as.Date(pub_meta$date[match(old_ids, pub_meta$Vote_ID)], "%d.%m.%Y"),
  result    = pub_meta$result_code[match(old_ids, pub_meta$Vote_ID)],
  policy    = pub_meta$main_policy_name[match(old_ids, pub_meta$Vote_ID)],
  final     = pub_meta$final_vote[match(old_ids, pub_meta$Vote_ID)] == 1,
  stringsAsFactors = FALSE
)
# The API leaves the outcome empty on most votes, so it is taken from the
# tally; that rule reproduces the published result_code on 100% of EP9 votes.
FINAL_RX <- paste0("as a whole|single vote|legislative resolution|",
                   "^motion for a resolution$|^commission proposal$|^decision$|",
                   "consent|approbation|discharge|referral back|",
                   "^recommendation$|^draft decision$")
nt <- tolower(trimws(enc2utf8(as.character(new_meta$title)))); nt[is.na(nt)] <- ""
meta_new <- data.frame(
  Vote_ID = n_old + seq_len(nrow(new_meta)),
  Date    = new_meta$Date,
  result  = ifelse(new_meta$yes > new_meta$no, "+", "-"),
  policy  = new_meta$main_policy_name,
  final   = grepl(FINAL_RX, nt, useBytes = TRUE),
  stringsAsFactors = FALSE
)
meta <- rbind(meta_old, meta_new)
cat(sprintf("final votes: %d of %d (%.1f%%)\n", sum(meta$final, na.rm = TRUE), nrow(meta),
            100 * mean(meta$final, na.rm = TRUE)))

# ── 5. Derived indices, using the published definitions ─────────────────────
present <- M != 0L
info$Attendance_Score <- round(rowSums(present) / ncol(M), 4)

use  <- meta$result %in% c("+", "-", "-*")
Mu   <- M[, use, drop = FALSE]; plus <- meta$result[use] == "+"
agree    <- sweep(Mu == 1L, 2, plus, "&") | sweep(Mu == 2L, 2, !plus, "&")
disagree <- sweep(Mu == 1L, 2, !plus, "&") | sweep(Mu == 2L, 2, plus, "&")
tot_w <- rowSums(Mu != 0L)
info$Winning_Score <- round(ifelse(tot_w > 0, (rowSums(agree) - rowSums(disagree)) / tot_w, NA_real_), 4)

info$loyalty_score <- NA_real_
for (g in unique(info$EPG[!is.na(info$EPG)])) {
  rows <- which(info$EPG == g)
  sub  <- M[rows, , drop = FALSE]
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
cat("derived indices computed\n")

# ── 6. Activity counts over the whole term, straight from Parltrack ─────────
helper <- file.path("scripts", "scrape", "extract_activities.py")
dump   <- file.path("data", "api_cache", "ep_mep_activities.json.lz")
CATS <- c("CRE","WDECL","COMPARL","REPORT","REPORT_SHADOW","COMPARL_SHADOW","MOTION",
          "OQ","WEXP","WQ","MINT","IMOTION","PRUNACT")
if (file.exists(helper) && file.exists(dump)) {
  csv <- tempfile(fileext = ".csv")
  system2("python3", c(shQuote(helper), shQuote(dump), shQuote(csv),
                       as.character(TERM_START), as.character(TERM_END)), stdout = FALSE)
  if (file.exists(csv)) {
    acts <- utils::read.csv(csv, stringsAsFactors = FALSE); unlink(csv)
    j <- match(as.integer(ids), acts$mep_id)
    for (cl in CATS) info[[cl]] <- ifelse(is.na(j), 0L, acts[[cl]][j])
    cat(sprintf("activities: %d of %d MEPs have recorded activity\n",
                sum(rowSums(info[CATS]) > 0), nrow(info)))
  }
}
for (cl in CATS) if (!cl %in% names(info)) info[[cl]] <- 0L
A <- as.matrix(info[, CATS]); A[!is.finite(A)] <- 0
kc <- apply(A, 2, function(x) stats::sd(x) > 0)
info$Activity_Index <- if (any(kc))
  round(as.numeric(scale(rowMeans(scale(A[, kc, drop = FALSE])))), 4) else NA_real_

# ── 7. Topic scores over final votes, blocks taken from the original prep ───
ts_src <- file.path("scripts", "scrape", "scrape_topic_scores.R")
e <- new.env()
lines <- readLines(ts_src, warn = FALSE)
end <- grep("Validation: reproduce", lines)
eval(parse(text = paste(lines[seq_len(end[1] - 1)], collapse = "\n")), envir = e)
wide <- cbind(info, as.data.frame(M))
fin <- which(meta$final %in% TRUE & !is.na(meta$policy))
sc <- e$score_blocks(wide, meta$Vote_ID[fin], meta$policy[fin], fold_case = TRUE)
wide[names(sc)] <- sc
cat(sprintf("topic scores from %d classified final votes\n", length(fin)))

# ── 8. Coordinates, full matrix and final-vote subset ───────────────────────
coords <- function(mat, suffix) {
  rc <- pscl::rollcall(mat, yea = 1, nay = 2, missing = c(3, 4), notInLegis = 0,
                       legis.names = make.unique(as.character(info$FullName)))
  pol <- which(info$EPG == "European Conservatives and Reformists Group")[1]
  w <- wnominate::wnominate(rc, dims = 2, polarity = c(pol, pol),
                            minvotes = 20, lop = 0.025, verbose = FALSE)
  f  <- as.data.frame(lapply(as.data.frame(mat), factor))
  mc <- FactoMineR::MCA(f, ncp = 2, graph = FALSE)
  g  <- cluster::daisy(f, metric = "gower")
  cfg <- umap::umap.defaults; cfg$input <- "dist"; cfg$n_components <- 2
  set.seed(123)
  u <- umap::umap(as.matrix(g), config = cfg)
  out <- list(w$legislators$coord1D, w$legislators$coord2D,
              mc$ind$coord[, 1], mc$ind$coord[, 2], u$layout[, 1], u$layout[, 2])
  names(out) <- paste0(c("coord1D", "coord2D", "MCA1", "MCA2", "UMAP1", "UMAP2"), suffix)
  out
}
cat("computing coordinates over the full term (this takes a while)...\n")
full <- coords(M, "")
for (n in names(full)) wide[[n]] <- full[[n]]
red_cols <- paste0("X", meta$Vote_ID[meta$final %in% TRUE])
red_cols <- intersect(red_cols, colnames(M))
cat(sprintf("computing coordinates on %d final votes...\n", length(red_cols)))
red <- coords(M[, red_cols, drop = FALSE], "_red")
for (n in names(red)) wide[[n]] <- red[[n]]

saveRDS(wide, out_file, compress = "gzip")
cat(sprintf("\nwrote %s: %d MEPs x %d columns (%.1f MB)\n", out_file, nrow(wide), ncol(wide),
            file.info(out_file)$size / 1048576))
