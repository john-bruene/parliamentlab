# MCA with "not in office" (0) and "absent" (4-6) set aside. See R/mca_specific.R.

source(proj_path("R", "mca_specific.R"))

test_that("the junk index points at the 0 and 4-6 columns and nothing else", {
  skip_if_not_installed("FactoMineR")
  f <- data.frame(a = factor(c(0, 1, 2, 1)), b = factor(c(1, 2, 2, 1)),
                  c = factor(c(3, 0, 1, 4)))
  idx <- mca_junk_idx(f)
  cols <- colnames(FactoMineR::tab.disjonctif(f))
  expect_equal(length(idx), 3)
  expect_true(all(grepl("[0456]$", cols[idx])))
  expect_null(mca_junk_idx(f["b"]))
})

test_that("without any of those codes the result is plain MCA", {
  skip_if_not_installed("FactoMineR")
  set.seed(1)
  f <- as.data.frame(lapply(1:30, function(i) factor(sample(1:3, 40, TRUE))))
  expect_equal(mca_votes(f)$ind$coord,
               FactoMineR::MCA(f, ncp = 2, graph = FALSE)$ind$coord)
})

test_that("MEPs who were barely there no longer own an axis", {
  skip_if_not_installed("FactoMineR")
  # 60 MEPs, two independent cleavages (bloc on the first 100 votes, side on
  # the last 100) with some noise, so both axes have real structure to find.
  # Plus one MEP who left after ten votes and one who kept the seat but was
  # absent for all but ten.
  set.seed(7)
  bloc <- rep(1:2, each = 30)
  side <- rep(1:2, 30)
  M <- sapply(1:200, function(v) {
    g <- if (v <= 100) bloc else side
    x <- ifelse(g == 1, 1, 2)
    flip <- runif(60) < 0.15
    x[flip] <- 3 - x[flip]
    x
  })
  M <- rbind(M, c(rep(1, 10), rep(0, 190)), c(rep(2, 10), rep(4, 190)))
  f <- as.data.frame(lapply(as.data.frame(M), factor))
  plain    <- FactoMineR::MCA(f, ncp = 2, graph = FALSE)$ind$coord
  specific <- mca_votes(f)$ind$coord
  z <- function(x) abs(x - median(x)) / mad(x)
  # Plain MCA puts the two far out on axes of their own; specific MCA keeps
  # them inside the cloud and finds both cleavages.
  expect_gt(max(z(plain[, 1])[61:62], z(plain[, 2])[61:62]), 6)
  expect_true(all(z(specific[, 1])[61:62] < 6))
  expect_true(all(z(specific[, 2])[61:62] < 6))
  s <- specific[1:60, ]
  expect_gt(summary(lm(bloc ~ s))$r.squared, 0.8)
  expect_gt(summary(lm(side ~ s))$r.squared, 0.8)
})
