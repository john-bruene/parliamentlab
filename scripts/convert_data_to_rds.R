# scripts/convert_data_to_rds.R
#
# One-off conversion of the raw CSV / XLSX sources into compact, pre-transformed
# .rds files that the Shiny app loads at startup.
#
# Why:
#   - read.csv on the four UMAP CSV files (65 MB total) takes several seconds
#     and allocates a lot of intermediate memory on the 1 GB Fly.io machine.
#   - readRDS on the compressed .rds equivalents is an order of magnitude faster
#     and uses less peak memory.
#
# Transformations applied here (previously done inline in server.R):
#   - drop first three index columns (`Unnamed: 0`, `X.1`, `X`) that are
#     write/read artefacts from earlier pipeline stages
#   - flip sign of coord1D for P7, P8, P9 so that higher values = right
#   - flip sign of coord2D_red for P7, P8 (P9 unchanged)
#
# Run from the project root:
#     Rscript scripts/convert_data_to_rds.R
#
# Regenerate whenever the source CSV/XLSX files change.

suppressPackageStartupMessages({
  library(readxl)
})

message("Converting UMAP CSVs -> RDS ...")

convert_umap <- function(period, flip_1d, flip_2d_red) {
  src <- file.path("data", sprintf("%s_umap_scores_red_NEW.csv", period))
  dst <- file.path("data", sprintf("%s_umap.rds", period))
  df  <- read.csv(src)
  df  <- df[, -(1:3)]
  if (flip_1d)    df$coord1D     <- df$coord1D     * -1
  if (flip_2d_red) df$coord2D_red <- df$coord2D_red * -1
  saveRDS(df, dst, compress = "xz")
  message(sprintf("  %s: %d rows x %d cols  ->  %s (%.1f MB)",
                  period, nrow(df), ncol(df), dst,
                  file.info(dst)$size / 1024 / 1024))
}

convert_umap("P6", flip_1d = FALSE, flip_2d_red = FALSE)
convert_umap("P7", flip_1d = TRUE,  flip_2d_red = TRUE)
convert_umap("P8", flip_1d = TRUE,  flip_2d_red = TRUE)
convert_umap("P9", flip_1d = TRUE,  flip_2d_red = FALSE)

message("Converting EP6-9 voted docs XLSX -> RDS ...")
ep <- read_excel("data/EP6_9_Voted_docs_new_datesfixed.xlsx")
saveRDS(ep, "data/EP6_9_Voted.rds", compress = "xz")
message(sprintf("  %d rows x %d cols  ->  data/EP6_9_Voted.rds (%.1f MB)",
                nrow(ep), ncol(ep),
                file.info("data/EP6_9_Voted.rds")$size / 1024 / 1024))

message("Done.")
