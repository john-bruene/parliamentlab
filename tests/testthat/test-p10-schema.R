# EP10 is assembled by a different pipeline from EP6-EP9, so it has to be
# checked against what the app actually selects. Several bugs during
# development were exactly of this kind: ISO country codes where the app
# expects full names, duplicated term memberships doubling the experience.

test_that("P10 carries every column the app selects", {
  if (!file.exists(proj_path("data/P10_umap.rds")) || !file.exists(proj_path("data/P9_umap.rds")))
    skip("P10 not built")
  p10 <- suppressWarnings(readRDS(proj_path("data/P10_umap.rds")))
  ref <- suppressWarnings(readRDS(proj_path("data/P9_umap.rds")))
  src <- readLines(proj_path("server.R"), warn = FALSE)
  sel <- unlist(regmatches(src, gregexpr("(?<=select\\()[^)]*", src, perl = TRUE)))
  tok <- unique(unlist(strsplit(gsub("[\"']", "", paste(sel, collapse = ",")), "[,[:space:]]+")))
  need <- intersect(tok[grepl("^[A-Za-z][A-Za-z0-9_]*$", tok)], names(ref))
  expect_equal(setdiff(need, names(p10)), character(0))
})

test_that("P10 country names match the app's lookup, not ISO codes", {
  if (!file.exists(proj_path("data/P10_umap.rds"))) skip("P10 not built")
  p10 <- suppressWarnings(readRDS(proj_path("data/P10_umap.rds")))
  expect_true("Germany" %in% p10$Country)
  expect_false("DEU" %in% p10$Country)
})

test_that("P10 experience is not double counted", {
  if (!file.exists(proj_path("data/P10_umap.rds"))) skip("P10 not built")
  p10 <- suppressWarnings(readRDS(proj_path("data/P10_umap.rds")))
  # The EP began in 1979, so nobody can have more than ~45 years of service.
  expect_lt(max(p10$Experience_at_Start, na.rm = TRUE) / 365.25, 46)
})

test_that("P10 vote codes stay inside the documented set", {
  if (!file.exists(proj_path("data/P10_umap.rds"))) skip("P10 not built")
  p10 <- suppressWarnings(readRDS(proj_path("data/P10_umap.rds")))
  xc <- grep("^X[0-9]+$", names(p10), value = TRUE)[1:200]
  expect_true(all(unlist(p10[, xc]) %in% 0:4))
})
