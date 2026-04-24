# profile.R — ParliamentLab local profiling
#
# Usage (from the project root):
#   Rscript profile.R
#   # or inside RStudio: source("profile.R")
#
# 1. A browser tab opens automatically.
# 2. Walk through the app: select a legislature, run the data pipeline,
#    switch to Parliament View, open Exploratory tab, run clustering.
# 3. Close the browser tab (or press Ctrl-C in the terminal).
# 4. profvis_report.html opens in your browser — click any bar to see source.
#
# Requires:  profvis, htmlwidgets  (install once if missing)

if (!requireNamespace("profvis",     quietly = TRUE)) install.packages("profvis")
if (!requireNamespace("htmlwidgets", quietly = TRUE)) install.packages("htmlwidgets")

library(profvis)
library(shiny)

message("\n[profvis] Starting app — interact in the browser, then close the tab.\n")

p <- profvis(
  {
    runApp(
      appDir        = ".",
      launch.browser = TRUE,
      host          = "127.0.0.1",
      port          = 8888
    )
  },
  interval = 0.01,   # 10 ms sampling — fine enough to catch render bottlenecks
  torture  = FALSE
)

out <- file.path(getwd(), "profvis_report.html")
htmlwidgets::saveWidget(p, out, selfcontained = TRUE)
message("\n[profvis] Report saved → ", out)

# Also open it
if (interactive()) browseURL(out)
