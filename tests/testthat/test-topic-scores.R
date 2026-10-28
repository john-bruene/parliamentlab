# The topic-score port is only trustworthy if it reproduces the published
# columns exactly. It matches policy labels case-sensitively on purpose: the
# original block list did, and folding case here would silently change the
# published numbers (VoteWatch wrote both "Budgetary Control" and
# "Budgetary control").

test_that("the topic-score port reproduces the published columns", {
  if (!file.exists(proj_path("data/P9_umap.rds")) || !file.exists(proj_path("data/EP6_9_Voted.rds")))
    skip("data files not available")
  src <- proj_path("scripts/scrape/scrape_topic_scores.R")
  if (!file.exists(src)) skip("scrape_topic_scores.R not present")

  # Load the definitions without running the script's own main section.
  e <- new.env()
  lines <- readLines(src, warn = FALSE)
  # Take only the definitions above the "Validation" banner. Evaluating the
  # whole file would run its main section and rewrite data/scraped as a side
  # effect of running the tests.
  end <- grep("Validation: reproduce", lines, fixed = FALSE)
  expect_true(length(end) == 1)
  eval(parse(text = paste(lines[seq_len(end[1] - 1)], collapse = "\n")), envir = e)

  d <- suppressWarnings(readRDS(proj_path("data/P9_umap.rds")))
  v <- suppressWarnings(readRDS(proj_path("data/EP6_9_Voted.rds")))
  fv <- v[v$Legislature == 9 & v$final_vote == 1, ]
  got <- e$score_blocks(d, fv$Vote_ID, fv$main_policy_name, fold_case = FALSE)
  for (cl in intersect(names(got), names(d))) {
    expect_equal(as.numeric(got[[cl]]), as.numeric(d[[cl]]), info = cl)
  }
})
