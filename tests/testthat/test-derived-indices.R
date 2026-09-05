# The three derived indices use definitions that are easy to get wrong, and
# during development all three were at some point implemented with the
# "obvious" formula instead of the published one. These tests pin the actual
# definitions by recomputing them from the published files and comparing with
# the stored columns.
#
# Attendance counts any code other than 0, so recorded absences count as
# present. Loyalty compares an MEP's code with their group's most common code
# over all non-zero codes. Winning is a NET score in [-1, 1] against the
# official result_code, not a share.

skip_if_no_data <- function(f) {
  if (!file.exists(f)) skip(paste("data file not available:", f))
}

test_that("attendance matches the published EP9 column", {
  skip_if_no_data(proj_path("data/P9_umap.rds")); skip_if_no_data(proj_path("data/EP6_9_Voted.rds"))
  d <- suppressWarnings(readRDS(proj_path("data/P9_umap.rds")))
  v <- suppressWarnings(readRDS(proj_path("data/EP6_9_Voted.rds"))); v9 <- v[v$Legislature == 9, ]
  xc <- grep("^X[0-9]+$", names(d), value = TRUE)
  ids <- as.integer(sub("^X", "", xc))
  M <- as.matrix(d[, xc[ids %in% v9$Vote_ID]])
  att <- rowSums(M != 0, na.rm = TRUE) / rowSums(!is.na(M))
  expect_equal(unname(att), unname(d$Attendance_Score), tolerance = 1e-8)
})

test_that("winning score is a net score against result_code, not a share", {
  skip_if_no_data(proj_path("data/P9_umap.rds")); skip_if_no_data(proj_path("data/EP6_9_Voted.rds"))
  d <- suppressWarnings(readRDS(proj_path("data/P9_umap.rds")))
  v <- suppressWarnings(readRDS(proj_path("data/EP6_9_Voted.rds"))); v9 <- v[v$Legislature == 9, ]
  xc  <- grep("^X[0-9]+$", names(d), value = TRUE)
  ids <- as.integer(sub("^X", "", xc)); keep <- ids %in% v9$Vote_ID
  M <- as.matrix(d[, xc[keep]])
  rc <- v9$result_code[match(ids[keep], v9$Vote_ID)]
  use <- rc %in% c("+", "-", "-*")
  Mu <- M[, use, drop = FALSE]; plus <- rc[use] == "+"
  win <- apply(Mu, 1, function(row) {
    tot <- sum(row != 0 & !is.na(row))
    if (!tot) return(NA_real_)
    (sum(row == 1 & plus, na.rm = TRUE) + sum(row == 2 & !plus, na.rm = TRUE) -
     sum(row == 1 & !plus, na.rm = TRUE) - sum(row == 2 & plus, na.rm = TRUE)) / tot
  })
  expect_equal(unname(win), unname(d$Winning_Score), tolerance = 1e-8)
  expect_true(min(d$Winning_Score, na.rm = TRUE) < 0)   # it really is signed
})

test_that("vote codes mean what the pipeline assumes", {
  skip_if_no_data(proj_path("data/P9_umap.rds")); skip_if_no_data(proj_path("data/EP6_9_Voted.rds"))
  d <- suppressWarnings(readRDS(proj_path("data/P9_umap.rds")))
  v <- suppressWarnings(readRDS(proj_path("data/EP6_9_Voted.rds"))); v9 <- v[v$Legislature == 9, ]
  for (vid in c(1L, 500L, 5000L)) {
    col <- paste0("X", vid)
    if (!col %in% names(d)) next
    meta <- v9[v9$Vote_ID == vid, ]
    expect_equal(sum(d[[col]] == 1), as.integer(meta$yes))      # 1 = in favour
    expect_equal(sum(d[[col]] == 2), as.integer(meta$no))       # 2 = against
    expect_equal(sum(d[[col]] == 3), as.integer(meta$abstain))  # 3 = abstention
  }
})
