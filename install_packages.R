# install_packages.R — run this once before launching the app
# Only installs what isn't already present.

pkgs <- c(
  "shiny", "shinythemes", "shinyBS", "shinyjs", "shinycssloaders",
  "DT", "reactable", "plotly", "ggiraph", "sparkline", "sortable",
  "ggplot2", "gridExtra", "scales", "bslib",
  "dplyr", "tidyr",
  "cluster", "clusterCrit", "dbscan", "umap", "FactoMineR",
  "packcircles", "sf",
  "qs"   # optional — faster data I/O; the app works without it
)

to_install <- pkgs[!pkgs %in% rownames(installed.packages())]

if (length(to_install) > 0) {
  message("Installing: ", paste(to_install, collapse = ", "))
  install.packages(to_install)
} else {
  message("All packages already installed.")
}
