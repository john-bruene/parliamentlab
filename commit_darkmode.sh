#!/bin/bash
# Run from the parliamentlab project root:  bash commit_darkmode.sh
set -e
cd "$(dirname "$0")"

rm -f .git/index.lock

git add ui.R server.R R/parliament_local.R Dockerfile www/dark-mode.css \
        archive/dark-mode/dark-mode.css archive/dark-mode/IMPLEMENTATION.md

git commit \
  --author="john-bruene <47179444+john-bruene@users.noreply.github.com>" \
  -m "feat: title rename, drop factoextra/viridis/htmltools; archive dark mode

- Rename app title to 'ParliamentLab: A Cluster Analysis...'
- Remove library(factoextra): replace fviz_silhouette() with silhouette_gg()
  in R/parliament_local.R (pure ggplot2, same visual layout)
- Remove library(viridis): scale_fill_viridis() -> scale_fill_viridis_c()
  (built into ggplot2 >= 3.0, no extra package needed)
- Remove library(htmltools): loaded transitively by shiny
- Dockerfile: drop factoextra + viridis from CRAN install list
- Archive dark mode implementation in archive/dark-mode/ for future use
  (CSS + server.R snippets + re-activation instructions in IMPLEMENTATION.md)"

echo "✓ Committed. Run 'git push' when ready to deploy."
