# Basis-Image mit Shiny Server von Rocker
FROM rocker/shiny:latest

# Installiere benötigte System-Bibliotheken (wichtig vor allem für Pakete wie sf, readxl, bslib etc.)
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev && \
    rm -rf /var/lib/apt/lists/*

# Installiere die R-Pakete, die in deiner App auf CRAN verfügbar sind
RUN R -e "install.packages(c('shiny','shinythemes','DT','ggplot2','car','nortest','tseries','RcmdrMisc','lmtest','sortable','plotly','shinyBS','shinyjs','fontawesome','reactable','bslib','shinyWidgets'), repos='http://cran.rstudio.com/')"

# Installiere ggparliament von GitHub
RUN R -e 'if (!requireNamespace("remotes", quietly=TRUE)) install.packages("remotes", repos="http://cran.rstudio.com/"); remotes::install_github("zmeers/ggparliament")'

# Kopiere alle Dateien (inklusive ui.R, server.R, Daten, www, etc.) in den Shiny-Server-Ordner
COPY . /srv/shiny-server/

# Exponiere den Standardport für Shiny (3838)
EXPOSE 3838

# Starte den Shiny-Server
CMD ["/usr/bin/shiny-server"]
