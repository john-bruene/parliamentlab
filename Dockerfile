# Basis-Image mit Shiny Server von Rocker
FROM rocker/shiny:latest

# Installiere benötigte System-Bibliotheken (wichtig vor allem für sf)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev && \
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
            'packcircles','viridis','ggiraph','sf','sparkline','waffle'); \
  install.packages(pkgs, repos = 'http://cran.rstudio.com/'); \
  missing <- setdiff(pkgs, rownames(installed.packages())); \
  if (length(missing) > 0) stop('install.packages failed for: ', paste(missing, collapse = ', '))"

# Installiere ggparliament von GitHub
RUN R -e 'if (!requireNamespace("remotes", quietly=TRUE)) install.packages("remotes", repos="http://cran.rstudio.com/"); remotes::install_github("zmeers/ggparliament")'

# Kopiere alle Dateien (inklusive ui.R, server.R, Daten, www, etc.) in den Shiny-Server-Ordner
COPY . /srv/shiny-server/

# Exponiere den Standardport für Shiny (3838)
EXPOSE 3838

# Starte den Shiny-Server
CMD ["/usr/bin/shiny-server"]
