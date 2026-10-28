# load_parl() narrows the vote columns from double to integer before caching
# them, which takes a process holding every legislature from 333 MB to 213 MB.
# That is only safe while the codes stay whole numbers and while nothing
# downstream distinguishes the two types, so both are checked here rather than
# assumed.

# .compact_votes lives inside server.R next to the cache it feeds; lift just
# that function out rather than sourcing the whole app.
compact_votes <- local({
  src <- readLines(proj_path("server.R"), warn = FALSE)
  i <- grep("^\\.compact_votes <- function", src)[1]
  if (is.na(i)) stop(".compact_votes not found in server.R")
  j <- i + which(grepl("^\\}", src[seq(i + 1, length(src))]))[1]
  eval(parse(text = paste(src[i:j], collapse = "\n")))
  .compact_votes
})

PARLS <- c("P6", "P7", "P8", "P9", "P9full", "P10")

test_that("vote codes are whole numbers, so narrowing them cannot lose anything", {
  for (p in PARLS) {
    f <- proj_path(sprintf("data/%s_umap.rds", p))
    if (!file.exists(f)) next
    d <- suppressWarnings(readRDS(f))
    xc <- grep("^X[0-9]+$", names(d), value = TRUE)
    m <- as.matrix(d[, xc])
    expect_false(anyNA(m), label = paste(p, "NA in the vote matrix"))
    expect_true(all(m == trunc(m)), label = paste(p, "fractional vote code"))
    expect_true(all(m >= 0 & m <= 6), label = paste(p, "vote code outside 0-6"))
  }
})

test_that("narrowing preserves every value and shrinks the footprint", {
  for (p in PARLS) {
    f <- proj_path(sprintf("data/%s_umap.rds", p))
    if (!file.exists(f)) next
    d <- suppressWarnings(readRDS(f))
    xc <- grep("^X[0-9]+$", names(d), value = TRUE)
    before <- as.matrix(d[, xc])
    n <- compact_votes(d)
    expect_equal(dim(n), dim(d), label = paste(p, "shape changed"))
    expect_equal(names(n), names(d), label = paste(p, "columns changed"))
    expect_true(all(as.matrix(n[, xc]) == before), label = paste(p, "values changed"))
    expect_true(all(vapply(n[xc], is.integer, logical(1))), label = paste(p, "columns still double"))
    expect_lte(as.numeric(object.size(n)), as.numeric(object.size(d)))
  }
})

test_that("a column that is not a vote code is left alone", {
  # The guard matters for whatever gets added next: a truncated 0.5 would be
  # silent, and the file on disk would still look right.
  d <- data.frame(X1 = c(0, 1, 2), X2 = c(0.5, 1.5, 2.5), X3 = c(1, NA, 2),
                  EPG = c("a", "b", "c"), stringsAsFactors = FALSE)
  n <- compact_votes(d)
  expect_true(is.integer(n$X1))
  expect_true(is.double(n$X2))   # fractional
  expect_true(is.double(n$X3))   # carries an NA
  expect_equal(n$X2, d$X2)
  expect_equal(n$EPG, d$EPG)
})

test_that("nothing downstream can tell the two types apart", {
  # Both the MCA and the UMAP path run as.factor() over the vote columns
  # before FactoMineR or daisy() sees them, so the distinction is erased
  # before it can reach a coordinate.
  skip_if_not_installed("cluster")
  skip_if_not_installed("FactoMineR")
  f <- proj_path("data/P9_umap.rds")
  skip_if_not(file.exists(f))
  d <- suppressWarnings(readRDS(f))
  xc <- grep("^X[0-9]+$", names(d), value = TRUE)
  set.seed(42)
  cols <- sample(xc, 120)
  dbl <- d[, cols]
  int <- dbl
  for (j in names(int)) int[[j]] <- as.integer(int[[j]])

  fd <- as.data.frame(lapply(dbl, as.factor))
  fi <- as.data.frame(lapply(int, as.factor))
  expect_identical(fd, fi)

  gd <- cluster::daisy(fd, metric = "gower")
  gi <- cluster::daisy(fi, metric = "gower")
  expect_identical(as.vector(gd), as.vector(gi))

  md <- FactoMineR::MCA(fd, ncp = 2, graph = FALSE)
  mi <- FactoMineR::MCA(fi, ncp = 2, graph = FALSE)
  expect_identical(md$ind$coord, mi$ind$coord)
})
