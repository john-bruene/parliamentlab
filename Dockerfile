# Basis-Image mit Shiny Server von Rocker
FROM rocker/shiny:latest

# Installiere benötigte System-Bibliotheken.
# - libudunits2/gdal/geos/proj:  sf
# - libssl/libcurl/libxml2:      httr, curl, xml2
# - cmake + libabsl-dev:         nloptr -> lme4 -> car -> FactoMineR,
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
            'clusterCrit','umap','dbscan','FactoMineR', \
            'packcircles','ggiraph','sf','sparkline','bslib','shinycssloaders', \
            'qs','profvis','shinyloadtest'); \
  install.packages(pkgs, repos = 'http://cran.rstudio.com/'); \
  missing <- setdiff(pkgs, rownames(installed.packages())); \
  if (length(missing) > 0) stop('install.packages failed for: ', paste(missing, collapse = ', '))"

# ggparliament + waffle removed: parliament layout and waffle charts are now
# implemented locally in R/parliament_local.R — no GitHub dependency required.

# Kopiere alle Dateien (inklusive ui.R, server.R, Daten, www, etc.) in den Shiny-Server-Ordner
COPY . /srv/shiny-server/

# Precompute: convert .rds → .qs (faster I/O) and bake in HDBSCAN cluster
# assignments so that do_load_best() never has to run HDBSCAN at session start.
RUN cd /srv/shiny-server && Rscript precompute_all.R

# Exponiere den Standardport für Shiny (3838)
EXPOSE 3838

# Starte den Shiny-Server
CMD ["/usr/bin/shiny-server"]
