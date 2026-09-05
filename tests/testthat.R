# Entry point: Rscript tests/testthat.R
library(testthat)
test_dir("tests/testthat", reporter = "summary")
