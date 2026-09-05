# P9full joins the published 2019-2022 records to the 2022-2024 remainder
# scraped from the Parliament's API. The two halves use different conventions
# for group names, country names and absence codes, so the join is where
# things go wrong. These checks guard the seams.

skip_unless_built <- function() {
  if (!file.exists(proj_path("data/P9full_umap.rds"))) skip("P9full not built")
}

test_that("P9full carries every column the app selects", {
  skip_unless_built()
  if (!file.exists(proj_path("data/P9_umap.rds"))) skip("P9 missing")
  full <- suppressWarnings(readRDS(proj_path("data/P9full_umap.rds")))
  ref  <- suppressWarnings(readRDS(proj_path("data/P9_umap.rds")))
  src <- readLines(proj_path("server.R"), warn = FALSE)
  sel <- unlist(regmatches(src, gregexpr("(?<=select\\()[^)]*", src, perl = TRUE)))
  tok <- unique(unlist(strsplit(gsub("[\"']", "", paste(sel, collapse = ",")), "[,[:space:]]+")))
  need <- intersect(tok[grepl("^[A-Za-z][A-Za-z0-9_]*$", tok)], names(ref))
  expect_equal(setdiff(need, names(full)), character(0))
})

test_that("P9full is a superset of the published P9", {
  skip_unless_built()
  if (!file.exists(proj_path("data/P9_umap.rds"))) skip("P9 missing")
  full <- suppressWarnings(readRDS(proj_path("data/P9full_umap.rds")))
  ref  <- suppressWarnings(readRDS(proj_path("data/P9_umap.rds")))
  expect_true(all(as.character(ref$WebisteEpID) %in% as.character(full$WebisteEpID)))
  expect_gt(nrow(full), nrow(ref))
  n_votes <- function(d) sum(grepl("^X[0-9]+$", names(d)))
  expect_gt(n_votes(full), n_votes(ref))
})

test_that("P9full country names match the app's lookup, not ISO codes", {
  skip_unless_built()
  full <- suppressWarnings(readRDS(proj_path("data/P9full_umap.rds")))
  cn <- as.character(full$Country)
  expect_equal(cn[grepl("^[A-Z]{3}$", cn)], character(0))
  expect_true("Germany" %in% cn)
})

test_that("P9full uses one group-name convention across both halves", {
  skip_unless_built()
  full <- suppressWarnings(readRDS(proj_path("data/P9full_umap.rds")))
  epg <- unique(as.character(full$EPG))
  # The published half abbreviates ("EPP", "NI"); after harmonising, only the
  # Parliament's long names should survive.
  expect_equal(intersect(epg, c("EPP", "NI", "REG", "IDG", "ECR",
                                "Greens_EFA", "Socialists_Democrats")),
               character(0))
  expect_false(any(is.na(epg)))
})

test_that("P9full experience is neither doubled nor blanked", {
  skip_unless_built()
  full <- suppressWarnings(readRDS(proj_path("data/P9full_umap.rds")))
  yrs <- full$Experience_at_Start / 365.25
  # The EP began in 1979, so nobody can have served more than ~45 years.
  expect_lt(max(yrs, na.rm = TRUE), 46)
  # A whole cohort reading as zero would mean the seniority lookup silently
  # failed for the MEPs the published file never saw.
  expect_gt(sum(yrs > 0, na.rm = TRUE), nrow(full) / 3)
})

test_that("P9full folds the absence codes into one convention", {
  skip_unless_built()
  full <- suppressWarnings(readRDS(proj_path("data/P9full_umap.rds")))
  xc <- grep("^X[0-9]+$", names(full), value = TRUE)
  xc <- xc[round(seq(1, length(xc), length.out = 300))]
  expect_true(all(unlist(full[, xc]) %in% 0:4))
})

test_that("P9full has the topic scores the radar chart needs", {
  skip_unless_built()
  full <- suppressWarnings(readRDS(proj_path("data/P9full_umap.rds")))
  topics <- c("economic_votesScore", "social_votesScore", "foreign_policy_votesScore",
              "industry_votesScore", "education_votesScore", "budget_votesScore")
  expect_equal(setdiff(topics, names(full)), character(0))
  for (t in topics) expect_false(all(is.na(full[[t]])), label = t)
})

test_that("the app is wired up for P9full", {
  ui  <- paste(readLines(proj_path("ui.R"), warn = FALSE), collapse = "\n")
  srv <- paste(readLines(proj_path("server.R"), warn = FALSE), collapse = "\n")
  expect_true(grepl('"P9full"', ui, fixed = TRUE))
  # Party colours, seating order, axis labels, and the clustering defaults all
  # key on the legislature code; missing one shows up as grey dots or a
  # headline that stops mid-sentence.
  expect_gte(lengths(regmatches(srv, gregexpr('"P9full"', srv, fixed = TRUE)))[1], 4)
  expect_true(grepl("P9full = list(", srv, fixed = TRUE))
})
