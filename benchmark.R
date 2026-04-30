#!/usr/bin/env Rscript
# benchmark.R — Load testing & profiling for ParliamentLab
#
# Follows the workflow from Mastering Shiny, Chapter 23:
#   benchmark → profile → optimise → re-benchmark
#
# Prerequisites:
#   install.packages(c("shinyloadtest", "profvis"))
#   # shinycannon requires Java: https://rstudio.github.io/shinyloadtest/

# ===========================================================================
# STEP 1 — PROFILE a single session with profvis
# ===========================================================================
# Run this in RStudio to see a flame graph of where time is spent:
#
#   library(profvis)
#   profvis(runApp("."))   # interact with app, then close window
#
# Look for wide bars in the flame graph. Typical hot spots in this app:
#   - do_load_best()     → HDBSCAN (now precomputed, should be near zero)
#   - countryMapPlot     → sf rendering (now cached per session)
#   - filteredData       → debounced, fires at most once per 400ms
#   - clusterPlot_538    → k-means (now cached cross-user)

# ===========================================================================
# STEP 2 — RECORD a typical user session
# ===========================================================================
# Terminal A — start the app:
#   R -e "shiny::runApp('.', port = 7716)"
#
# Terminal B — record session (opens a browser):
#   R -e "shinyloadtest::record_session('http://127.0.0.1:7716')"
#
# In the browser, simulate a realistic session:
#   1. Wait for Step 5 to auto-load
#   2. Click to Step 3, change a slider, wait 2s
#   3. Click to Step 4, look at UMAP plot, wait 3s
#   4. Switch legislature (P8), wait 3s
#   5. Close
#
# Output: recording.log

# ===========================================================================
# STEP 3 — REPLAY with multiple simulated users (requires shinycannon + Java)
# ===========================================================================
# Install shinycannon: https://rstudio.github.io/shinyloadtest/#shinycannon
#
# Then from terminal, with the app still running on port 7716:
#
#   shinycannon recording.log http://127.0.0.1:7716 \
#     --workers 5 \
#     --loaded-duration-minutes 3 \
#     --output-dir runs/run_baseline
#
# After each optimisation, re-run with a different --output-dir:
#   --output-dir runs/run_after_debounce
#   --output-dir runs/run_after_bindcache

# ===========================================================================
# STEP 4 — ANALYSE results
# ===========================================================================

analyse_runs <- function(...) {
  dirs <- list(...)
  library(shinyloadtest)
  df <- load_runs(dirs)
  shinyloadtest_report(df, "benchmark_report.html", open_browser = TRUE)
  cat("Report saved to benchmark_report.html\n")
}

# Example:
#   analyse_runs("runs/run_baseline", "runs/run_after_debounce")

# ===========================================================================
# STEP 5 — Interpreting the session duration plot
# ===========================================================================
# The HTML report shows coloured bars per simulated user:
#
#   "Homepage" slow    → UI function doing too much (look at ui.R)
#   "Start session"slow→ server() init doing heavy work (check global loads)
#   "Calculate" slow   → reactive computations are the bottleneck
#                        → add more bindCache() or move work to precompute
#
# Target: all bars roughly the same width regardless of worker count.
# A 10x improvement (workers=10 same as workers=1) is realistic with caching.

cat("benchmark.R loaded. See comments above for usage instructions.\n")
cat("Quick profiling: profvis::profvis(shiny::runApp('.'))\n")
