# ParliamentLab

<img src="www/parliamentlab_hex.png" align="right" width="135" alt="ParliamentLab hex sticker"/>

Shiny app for exploring voting behavior in the European Parliament. It covers four legislative terms, EP6 through EP9 (2004-2022), and walks through a full analysis pipeline: data preparation, feature engineering, exploration, dimensionality reduction (W-NOMINATE, MCA, UMAP) and clustering (k-Means, PAM, HDBSCAN).

You do not have to work through the steps in order. Recommended clustering results load automatically on startup, so the exploration and results views are populated the moment the app opens.

Live version: https://parliamentlab.eu (also reachable at https://parliamentlab.com)

---

## What the app does

The interface follows the shape of a research process, one tab per stage.

1. **Introduction** frames the problem with the FiveThirtyEight analysis of US House voting that inspired it.
2. **Data Preparation** merges the source datasets, handles missing values, and builds indices such as attendance, loyalty, activity and per-topic voting scores.
3. **Exploration** compares those measures across political groups and countries, and includes an interactive hemicycle where clicking a seat opens that MEP's profile.
4. **Clustering** applies dimensionality reduction and clustering, scores the result with silhouette, Davies-Bouldin and Calinski-Harabasz, and stress-tests it by re-running the clustering under different settings and on resampled data.
5. **Results** summarises the clusters and compares them against the official political groups.

---

## Running locally

Requires R version 4.2 or newer. All data files are already in the repository, so there is nothing to download separately.

```r
# 1. install dependencies (first time only, takes a few minutes)
source("scripts/install_packages.R")

# 2. launch
shiny::runApp(".")
```

Or open `parliamentlab.Rproj` in RStudio and click **Run App**.

Recommended clustering results ship with the repository, so the app is ready to use straight away. Running `Rscript scripts/precompute_all.R` once beforehand additionally converts the data to a faster format. It is optional, just a speedup.

To run it in a container instead, the `Dockerfile` builds a self-contained image.

---

## Repository layout

| Path | What it is |
| --- | --- |
| `ui.R`, `server.R` | The application |
| `R/parliament_local.R` | Local hemicycle layout, replaces the ggparliament dependency |
| `data/` | Raw sources and the compact `.rds` files the app reads |
| `www/` | Images, country flags and static assets |
| `docs/` | The landing page served at parliamentlab.eu via GitHub Pages |
| `scripts/install_packages.R` | Installs the R packages the app needs |
| `scripts/convert_data_to_rds.R` | Rebuilds the `.rds` files from the raw sources |
| `scripts/download_photos.R` | Prefetches MEP portraits into `www/mep_photos/` |
| `scripts/precompute_all.R` | Optional precomputation of clustering results |
| `scripts/benchmark.R`, `scripts/profile.R` | Development helpers for timing and profiling |

---

## Data

Roll-call votes from Parltrack and VoteWatch Europe, covering roughly 3,700 MEPs across four terms. DW-NOMINATE ideal point estimates were produced with W-NOMINATE.

The `data/` folder holds both the raw source tables (`*_umap_scores_red_NEW.csv` and `EP6_9_Voted_docs_new_datesfixed.xlsx`) and the compact `.rds` files the app actually reads. Each `P*_umap.rds` carries one row per MEP with the full roll-call vote matrix, about 13,500 vote columns for EP9, alongside the derived indices, UMAP embeddings and biographical fields. `scripts/convert_data_to_rds.R` regenerates the `.rds` files from the raw sources.

VoteWatch Europe shut down in 2022, so the raw files are kept in the repository rather than linked, to keep the analysis reproducible.

MEP portraits live in `www/mep_photos/`, fetched once from the European Parliament's public photo service by `scripts/download_photos.R` and downscaled to sidebar size. Shipping them means the running app never depends on an external request, which used to leave the odd portrait blank whenever a request was dropped. Re-run that script after adding a legislature; it skips photos already on disk.

---

## Citation

The app is described in the companion article:

> Brüne, J. F., Potts, S. and Bergherr, E. (2026). ParliamentLab: An interactive application for exploring voting behavior in the European Parliament. *European Political Science*. [doi:10.1017/S1682098326100629](https://doi.org/10.1017/S1682098326100629)

The article is open access under CC BY 4.0. The full methodology, including how the indices are defined, is described there.

---

## License

The code is released under the [MIT License](LICENSE). The bundled data comes from public sources with their own terms: Parltrack publishes under the Open Database License (ODbL), and the VoteWatch Europe data is used for research purposes.

MEP portraits in `www/mep_photos/` come from the European Parliament's public photo service (© European Union). They are included for educational and research use; `scripts/download_photos.R` can regenerate them from source.
