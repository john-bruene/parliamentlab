#!/bin/bash
# Run from the parliamentlab project root:  bash commit_legislature_switcher.sh
set -e
cd "$(dirname "$0")"

rm -f .git/index.lock

git add ui.R server.R

git commit \
  --author="john-bruene <47179444+john-bruene@users.noreply.github.com>" \
  -m "feat: global legislature switcher + lazy loading + Step 5 direct access; fix crash

Legislature switcher (ui.R):
- selectInput('selectedP') integrated into navbarPage brand area (same row as tabs)
  Replaces the separate .legislature-bar div that overlapped with tab content
- Removed titlePanel (title now shown as brand text in navbar)
- Removed duplicate selectInput('selectedP') from Step 2 and Step 4
  Both now show a read-only label (textOutput 'selected_period_label')

Lazy loading (server.R):
- Remove global P6/P7/P8/P9 loads at startup (~150-300 MB/session saved)
- Per-session parl_cache reactiveValues: only the selected parliament
  is ever loaded; switching caches without reloading already-seen data
- EP6_9_Voted lazy-loaded via voted_cache() reactiveVal — only fetched
  when user triggers DW-NOMINATE / MCA / UMAP re-computation
- SHP_0 stays global (shared across sessions, 184 KB on disk)

Step 5 direct access (server.R):
- best_defaults list (from thesis Section 6): UMAP + HDBSCAN per parliament
  P6/P7: minPts=25 (Silhouette 0.785/0.844)
  P8/P9: minPts=20 (Silhouette 0.820/0.811)
- Auto-load on session start: observeEvent(input$selectedP, ignoreInit=FALSE)
  fires immediately with P9 default, showing exploration without any click
- Legislature switch also auto-reloads best defaults for the new parliament
- do_load_best() helper deduplicates logic between auto-init and manual button
- NA rows in UMAP1/UMAP2 dropped before hdbscan (fixes kNN crash on click)

Fix startup crash (server.R):
- Python EP6_9_Voted → load_voted() replacement had corrupted two things:
  1. File path 'data/EP6_9_Voted.rds' became 'data/load_voted().rds' (restored)
  2. Variable EP6_9_Voted_processed became load_voted()_processed (renamed EP6_9_processed)"

echo "✓ Committed. Run 'git push' when ready to deploy."
