# ParliamentLab

<img src="www/parliamentlab_hex.png" alt="ParliamentLab hex sticker" align="right" width="135"/>

Shiny app for exploring voting behavior in the European Parliament. It covers five legislative terms, EP6 through EP10 (2004-2026), and walks through a full analysis pipeline: data preparation, feature engineering, exploration, dimensionality reduction (W-NOMINATE, MCA, UMAP) and clustering (k-Means, PAM, HDBSCAN).

You do not have to work through the steps in order. Recommended clustering results load automatically on startup, so the exploration and results views are populated the moment the app opens.

Live version: <https://parliamentlab.eu> (also reachable at <https://parliamentlab.com>)

------------------------------------------------------------------------

## What the app does

The interface follows the shape of a research process, one tab per stage.

1.  **Introduction** frames the problem with the FiveThirtyEight analysis of US House voting that inspired it.
2.  **Data Preparation** merges the source datasets, handles missing values, and builds indices such as attendance, loyalty, activity and per-topic voting scores.
3.  **Exploration** compares those measures across political groups and countries, and includes an interactive hemicycle where clicking a seat opens that MEP's profile.
4.  **Clustering** applies dimensionality reduction and clustering, scores the result with silhouette, Davies-Bouldin and Calinski-Harabasz, and stress-tests it by re-running the clustering under different settings and on resampled data.
5.  **Results** summarises the clusters and compares them against the official political groups.

------------------------------------------------------------------------

## Running locally

Requires R version 4.2 or newer. All data files are already in the repository, so there is nothing to download separately.

`renv.lock` pins the exact package versions the app was last verified with
(R 4.5.2, 162 packages). To reproduce that set:

``` r
renv::restore()
```

That also solves the case where an R upgrade leaves the packages behind: a new
minor version gets its own library, so `library(shiny)` suddenly fails even
though the app worked yesterday. Either restore into the new version, or point
the framework back at the old one:

``` bash
sudo ln -sfn 4.5-arm64 /Library/Frameworks/R.framework/Versions/Current
```

``` r
# 1. install dependencies (first time only, takes a few minutes)
source("scripts/install_packages.R")

# 2. launch
shiny::runApp(".")
```

Or open `parliamentlab.Rproj` in RStudio and click **Run App**.

Recommended clustering results ship with the repository, so the app is ready to use straight away. Running `Rscript scripts/precompute_all.R` once beforehand additionally converts the data to a faster format. It is optional, just a speedup.

To run it in a container instead, the `Dockerfile` builds a self-contained image.

### Tests

``` bash
Rscript tests/testthat.R
```

The suite pins the things that are easy to get wrong and were in fact got
wrong during development: the three derived-index definitions (attendance
counts any non-zero code, winning is a net score in [-1, 1], loyalty compares
against the group's most common code), the vote-code meanings, that the topic
score port reproduces the published columns exactly, and that the EP10 file
carries every column the app selects with country names rather than ISO codes
and no double-counted experience. Tests skip cleanly when a data file is
absent.

------------------------------------------------------------------------

## Repository layout

| Path | What it is |
|----|----|
| `ui.R`, `server.R` | The application |
| `R/parliament_local.R` | Local hemicycle layout, replaces the ggparliament dependency |
| `data/` | Raw sources and the compact `.rds` files the app reads |
| `www/` | Images, country flags and static assets |
| `docs/` | The landing page served at parliamentlab.eu via GitHub Pages |
| `scripts/install_packages.R` | Installs the R packages the app needs |
| `scripts/convert_data_to_rds.R` | Rebuilds the `.rds` files from the raw sources |
| `scripts/download_photos.R` | Prefetches MEP portraits into `www/mep_photos/` |
| `scripts/scrape/run_all.R` | Runs the whole data-extension pipeline in order |
| `scripts/scrape/build_p10.R` | Assembles `data/P10_umap.rds` in the app's schema |
| `scripts/scrape/fill_birthdates.R` | Fills missing MEP birth dates from Wikidata |
| `scripts/scrape/` | The individual stages (votes from the EP API; activity, policy area and topic scores via Parltrack) |
| `scripts/precompute_all.R` | Optional precomputation of clustering results |
| `scripts/benchmark.R`, `scripts/profile.R` | Development helpers for timing and profiling |
| `tests/` | Regression tests, run with `Rscript tests/testthat.R` |
| `renv.lock` | Pinned package versions (R 4.5.2) |

------------------------------------------------------------------------

## Data

Roll-call votes from Parltrack, VoteWatch Europe and the European Parliament's open data API, covering roughly 4,400 MEPs across five terms. DW-NOMINATE ideal point estimates were produced with W-NOMINATE.

The `data/` folder holds both the raw source tables (`*_umap_scores_red_NEW.csv` and `EP6_9_Voted_docs_new_datesfixed.xlsx`) and the compact `.rds` files the app actually reads. Each `P*_umap.rds` carries one row per MEP with the full roll-call vote matrix, about 13,500 vote columns for EP9, alongside the derived indices, UMAP embeddings and biographical fields. `scripts/convert_data_to_rds.R` regenerates the `.rds` files from the raw sources.

VoteWatch Europe shut down in 2022, so the raw files are kept in the repository rather than linked, to keep the analysis reproducible.

EP10 (2024-2026) is built from the European Parliament's own open data API by
`scripts/scrape/`, then assembled into `data/P10_umap.rds` by
`scripts/scrape/build_p10.R`. EP6-EP9 are untouched: those files back the
published article, and EP9 stores short group codes that would clash with the
long names the API returns.

Birth dates are missing for 123 of the 738 EP10 MEPs in the Parliament's API,
which would leave their age empty. `scripts/scrape/fill_birthdates.R` fills
them from Wikidata, joining on the EP person id (property P1186) rather than
by name, and never overwrites a date the API does publish. It recovered 122 of
the 123, leaving one. Checked against 40 MEPs whose date the API does publish,
Wikidata agreed on 39, the one difference being a month, so treat the filled
values as good but not authoritative. EP10's mean age lands at 50.1 years
against EP9's 50.2.

Two fields differ for EP10. There are no per-topic voting scores, because the
policy area behind them comes from Parltrack, whose dossier dump stops on
25 April 2024; the Results tab says so instead of drawing an empty radar
chart. And the "Use Only Final Votes" switch uses a final-vote flag
reconstructed from vote titles, since the API does not publish one: it selects
7.8% of EP10 roll-calls where the published EP9 flag covered 9.3%.

MEP portraits live in `www/mep_photos/`, fetched once from the European Parliament's public photo service by `scripts/download_photos.R` and downscaled to sidebar size. Shipping them means the running app never depends on an external request, which used to leave the odd portrait blank whenever a request was dropped. Re-run that script after adding a legislature; it skips photos already on disk.

------------------------------------------------------------------------

## Citation

The app is described in the companion article:

> Brüne, J. F., Potts, S. and Bergherr, E. (2026). ParliamentLab: An interactive application for exploring voting behavior in the European Parliament. *European Political Science*. [doi:10.1017/S1682098326100629](https://doi.org/10.1017/S1682098326100629)

The article is open access under CC BY 4.0. The full methodology, including how the indices are defined, is described there.

The software itself is archived on Zenodo: [doi:10.5281/zenodo.21863149](https://doi.org/10.5281/zenodo.21863149). That DOI always resolves to the latest release; `v1.0.0` specifically is [doi:10.5281/zenodo.21863150](https://doi.org/10.5281/zenodo.21863150).

------------------------------------------------------------------------

## Extending the dataset

The EP6-EP9 data came from VoteWatch Europe, which closed in 2022, so the
bundled files stop on 9 June 2022. `scripts/scrape/` rebuilds the same
structure from the European Parliament's own open data API, which publishes
roll-call votes with the individual MEP breakdown.

```bash
Rscript scripts/scrape/run_all.R                 # 2022-06-10 to today
Rscript scripts/scrape/run_all.R 2024-07-16 2025-12-31
```

`run_all.R` runs the four stages in order. That order matters: the first stage
rewrites the vote files from scratch, so running it on its own drops the
policy areas and topic scores the later stages add. The individual scripts
below can be run separately, but only after the first stage has completed.

The three derived indices (attendance, loyalty, winning) follow the exact
definitions used for the published EP6-EP9 data. Those definitions are not the
obvious ones: attendance counts any code other than 0, so recorded absences
count as present; loyalty compares the MEP's code with their group's most
common code over all non-zero codes; and winning is a net score in [-1, 1],
not a share. All three were verified by recomputing them from the original
files and matching the stored columns exactly.

Output lands in `data/scraped/`: one wide MEP-by-vote file per term plus the
matching vote metadata. Vote coding follows the existing files exactly
(1 for, 2 against, 3 abstention, 4 in office but did not vote, 0 not an MEP
at the time). Every API response is cached under `data/api_cache/`, so the
scrape is resumable and re-running it is nearly free.

Validated against the overlap with the VoteWatch data: for 6-9 June 2022 the
scraper finds the same roll-call votes, with 385 of 391 vote tallies matching
exactly, and 88% of MEPs joining by name to the existing EP9 file.

A full run over 2022-06-10 to 2026-09-04 (221 sitting days) yields:

| Term | MEPs | Roll-call votes | Period |
|---|---|---|---|
| EP9 (rest of term) | 758 | 5,806 | 2022-06-22 to 2024-04-25 |
| EP10 | 738 | 5,568 | 2024-07-17 to 2026-07-09 |

Political group and national party are complete for every MEP; the EP10 file
correctly picks up the groups formed after the 2024 election, such as Patriots
for Europe. Date of birth is missing for some MEPs (71 in EP9, 123 in EP10)
because the API does not publish it for them, so `Age_At_Start` is empty for
those rows.

### Activity counts

`scripts/scrape/scrape_activities.R` adds the second half: speeches, reports,
questions and the rest, which are the inputs behind `Activity_Index`.

```bash
Rscript scripts/scrape/scrape_activities.R
```

The Parliament's API does not expose per-MEP authorship (its document records
carry no author, and an author filter is accepted but silently ignored), so
this uses Parltrack, the same source as the original data. Parltrack ships the
file lzip-compressed, which R cannot read, so `extract_activities.py`
decompresses and counts in one streaming pass; it needs only python3, no extra
packages.

Two caveats worth knowing before using these numbers:

- Parltrack last refreshed the dump on **7 November 2024**, so EP10 activity
  stops there. Re-run once a newer dump appears.
- Coverage is uneven. In the EP9 window 434 of 758 MEPs have recorded
  activity; in the covered part of EP10 it is 709 of 738. The counts that are
  present are real and correctly dated, but the EP9 tail is under-complete.

`Activity_Index` is rebuilt from the counts and reproduces the original EP9
index at r = 0.98. The exact weights behind the published figures are not
recorded in the data, so this is a close reconstruction rather than an
identical one.

### Policy area

`scripts/scrape/scrape_policy.R` attaches `main_policy_name`, the field behind
the per-topic voting scores.

```bash
Rscript scripts/scrape/scrape_policy.R
```

The Parliament's vote records carry no committee or procedure reference, so
the chain runs through Parltrack: vote id, procedure reference, dossier,
responsible committee. The last hop reuses the committee-to-policy
correspondence already present in the original EP6-EP9 data (learned from
29,022 classified votes across 128 committees), so the label vocabulary stays
identical to the published dataset rather than introducing new categories.

The Parliament's vote ids and Parltrack's turned out to be the same
identifier, so the join is exact: 97.6% of scraped EP9 votes match a Parltrack
record by id, with no title matching involved.

Coverage:

| Term | Votes | With policy area |
|---|---|---|
| EP9 (rest of term) | 5,806 | 4,028 (69%) |
| EP10 | 5,568 | 0 |

Two reasons it is not complete. Only votes tied to a legislative procedure
reach a dossier, so resolutions, agenda votes and similar have no committee to
map from; VoteWatch classified those by hand, which is why the original file
is at 100%. And Parltrack's vote dump currently ends on 25 April 2024, so EP10
has no coverage at all yet.

### Topic scores

`scripts/scrape/scrape_topic_scores.R` rebuilds the 16 `*_votesScore` columns.
The block definitions are taken verbatim from the original preparation
(`archive/Data_Prep.Rmd`), so the columns line up with the published data.

```bash
Rscript scripts/scrape/scrape_topic_scores.R --validate   # check the port
Rscript scripts/scrape/scrape_topic_scores.R              # score scraped data
```

`--validate` re-runs the port on the original EP6-EP9 files and compares it
with the stored columns. It reproduces all 64 columns across the four terms
exactly, which is what establishes the port is faithful before it is applied
to anything new.

Two things to know before using the new columns.

**They are not comparable to the published ones.** The original restricted the
scores to final votes (`final_vote == 1`, about 9% of roll-calls). The
Parliament's API publishes no such flag and it cannot be reconstructed: vote
titles mark 53% of roll-calls as amendments where only 9% are final votes, and
the sitting-order field is empty for most votes. `--exclude-amendments` drops
amendment and paragraph votes, keeping about a third of classified votes,
which gets closer to a policy stance than to sheer voting volume. It is an
approximation, not the published method.

**The published columns undercount.** The original block list matched policy
labels case-sensitively, and VoteWatch was inconsistent: it wrote both
"Budgetary Control" and "Budgetary control", both "Gender equality" and
"Gender Equality", even "CIvil liberties". 937 of 6,301 classified final votes
(14.9%) fell through, 753 of them budgetary, so `budget_votesScore` is the
column most affected. The port reproduces that behaviour when validating, and
folds case when scoring new data.

The dimensionality-reduction coordinates (W-NOMINATE, MCA, UMAP) are computed
separately from the vote matrix, as before.

------------------------------------------------------------------------

## License

The code is released under the [MIT License](LICENSE). The bundled data comes from public sources with their own terms: Parltrack publishes under the Open Database License (ODbL), and the VoteWatch Europe data is used for research purposes.

MEP portraits in `www/mep_photos/` come from the European Parliament's public photo service (© European Union). They are included for educational and research use; `scripts/download_photos.R` can regenerate them from source.
