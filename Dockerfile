# Basis-Image mit Shiny Server von Rocker
FROM rocker/shiny:latest

# Installiere benötigte System-Bibliotheken.
# - libudunits2/gdal/geos/proj:  sf
# - libssl/libcurl/libxml2:      httr, curl, xml2
# - cmake + libabsl-dev:         nloptr -> lme4 -> car -> FactoMineR/factoextra,
#                                und s2 -> sf
RUN apt-get update && apt-get install -y --no-install-recommends \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    cmake \
    libabsl-dev && \
    rm -rf /var/lib/apt/lists/*

# Installiere die R-Pakete, die in der App via library() geladen werden.
# Ncpus=4 beschleunigt die Source-Kompilation (sf, FactoMineR, umap ...).
# Die abschließende setdiff-Prüfung macht Build laut fehlschlagen, falls ein
# Paket nicht installiert werden konnte (install.packages warnt normalerweise
# nur, wodurch das Image heimlich mit fehlenden Deps durchgeht).
RUN R -e "options(Ncpus = 4); \
  pkgs <- c('shiny','shinythemes','DT','ggplot2','dplyr','tidyr','scales','gridExtra', \
            'sortable','plotly','shinyBS','shinyjs','reactable', \
            'clusterCrit','umap','dbscan','FactoMineR','factoextra', \
            'packcircles','viridis','ggiraph','sf','sparkline'); \
  install.packages(pkgs, repos = 'http://cran.rstudio.com/'); \
  missing <- setdiff(pkgs, rownames(installed.packages())); \
  if (length(missing) > 0) stop('install.packages failed for: ', paste(missing, collapse = ', '))"

# GitHub-only Pakete:
# - ggparliament: ist nicht (mehr) auf CRAN
# - waffle:       CRAN-Version ist veraltet und passt nicht zu dieser R-Version
RUN R -e "options(Ncpus = 4); \
  if (!requireNamespace('remotes', quietly = TRUE)) install.packages('remotes', repos = 'http://cran.rstudio.com/'); \
  remotes::install_github('zmeers/ggparliament', upgrade = 'never'); \
  remotes::install_github('hrbrmstr/waffle',    upgrade = 'never'); \
  gh_pkgs <- c('ggparliament','waffle'); \
  missing <- setdiff(gh_pkgs, rownames(installed.packages())); \
  if (length(missing) > 0) stop('github install failed for: ', paste(missing, collapse = ', '))"

# Kopiere alle Dateien (inklusive ui.R, server.R, Daten, www, etc.) in den Shiny-Server-Ordner
COPY . /srv/shiny-server/

# Exponiere den Standardport für Shiny (3838)
EXPOSE 3838

# Starte den Shiny-Server
CMD ["/usr/bin/shiny-server"]
