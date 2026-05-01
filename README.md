<img src="parliamentlab_hex.png" align="right" height="110"/>

# ParliamentLab

Shiny app for exploring voting behavior in the European Parliament, written as part of a master's thesis on political group cohesion. It covers four legislative terms — EP6 through EP9 (2004–2024) — and walks through the full analysis pipeline: descriptive stats, feature engineering, dimensionality reduction (DW-NOMINATE, MCA, UMAP), and clustering (k-Means, HDBSCAN). Recommended clustering results load automatically on startup, so you can go straight to the exploration without clicking through everything first.

Live version: https://john-f-bruene.shinyapps.io/masterthesis/

---

## Running locally

Requires R ≥ 4.2 and RStudio. All data files are already in the repository, no separate download needed.

```r
# 1. install dependencies (first time only, takes a few minutes)
source("install_packages.R")

# 2. launch
shiny::runApp(".")
```

Or open `masterthesis_EUStat.Rproj` in RStudio and click **Run App**.

On first startup the app computes HDBSCAN clustering for the selected parliament, which takes a few seconds. If you want to skip that, run `Rscript precompute_all.R` once beforehand — it pre-bakes the results and converts the data to a faster format. Not required, just a nice speedup.

---

## Data

Roll-call votes from ParlTrack / VoteWatch EU, covering roughly 3,700 MEPs across four terms. DW-NOMINATE ideal point estimates were produced with W-NOMINATE. The `data/` folder contains preprocessed `.rds` files with vote scores, UMAP embeddings, and biographical data — no raw vote matrices. The full methodology is in the thesis.
