#!/usr/bin/env Rscript
# download_photos.R — fetch every MEP portrait once and store it in
# www/mep_photos/ so the running app never has to reach europarl.eu.
#
# Run it from the project root:
#   Rscript scripts/download_photos.R
#
# The script is resumable: photos already on disk are skipped, so you can
# interrupt it and run it again. Re-run it after adding a new legislature.
#
# Photos are downscaled (default 240px wide) because the app shows them in a
# narrow sidebar. Full-size originals would be roughly twice the disk space
# for no visible gain. Downscaling uses `sips` on macOS or the magick package
# if installed; without either, the originals are kept as they are.

MAX_WIDTH <- 240   # pixels; the sidebar renders the photo at about half this
QUALITY   <- 80    # JPEG quality after downscaling
PAUSE     <- 0.05  # seconds between requests, to stay polite to europarl.eu

out_dir <- file.path("www", "mep_photos")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ── Collect every distinct photo URL across all legislatures ────────────────
periods <- c("P6", "P7", "P8", "P9", "P9full", "P10")
urls <- unlist(lapply(periods, function(p) {
  f <- sprintf("data/%s_umap.rds", p)
  if (!file.exists(f)) return(character(0))
  as.character(suppressWarnings(readRDS(f))$Photo)
}))
urls <- trimws(urls)
urls <- unique(urls[!is.na(urls) & nzchar(urls) & startsWith(urls, "http")])

cat(sprintf("Found %d distinct photo URLs.\n", length(urls)))

# ── Downscaling helper ──────────────────────────────────────────────────────
has_sips   <- nzchar(Sys.which("sips"))
has_magick <- requireNamespace("magick", quietly = TRUE)

shrink <- function(path) {
  if (has_magick) {
    img <- magick::image_read(path)
    img <- magick::image_resize(img, sprintf("%dx", MAX_WIDTH))
    magick::image_write(img, path, format = "jpeg", quality = QUALITY)
  } else if (has_sips) {
    system2("sips", c("-Z", MAX_WIDTH, "-s", "formatOptions", QUALITY, shQuote(path)),
            stdout = FALSE, stderr = FALSE)
  }
  invisible(NULL)
}

if (!has_sips && !has_magick) {
  message("Note: neither sips nor the magick package is available, ",
          "so photos are stored at full size.")
}

# ── Download loop ───────────────────────────────────────────────────────────
options(timeout = 20)
n_new <- 0L; n_skip <- 0L; failed <- character(0)

for (i in seq_along(urls)) {
  url   <- urls[i]
  fname <- gsub("[^A-Za-z0-9._-]", "", basename(url))
  dest  <- file.path(out_dir, fname)

  if (file.exists(dest) && file.info(dest)$size >= 100L) {
    n_skip <- n_skip + 1L
    next
  }

  tmp <- tempfile(fileext = ".jpg")
  ok <- FALSE
  for (attempt in 1:2) {   # one retry: the odd request gets dropped
    res <- tryCatch(download.file(url, tmp, quiet = TRUE, mode = "wb"),
                    error   = function(e) -1L,
                    warning = function(w) -1L)
    if (identical(as.integer(res), 0L) && file.exists(tmp) &&
        file.info(tmp)$size >= 100L) { ok <- TRUE; break }
    Sys.sleep(0.5)
  }

  if (ok) {
    file.rename(tmp, dest)
    shrink(dest)
    n_new <- n_new + 1L
  } else {
    failed <- c(failed, url)
    unlink(tmp)
  }

  if (i %% 100 == 0) {
    cat(sprintf("  %d/%d  (new %d, skipped %d, failed %d)\n",
                i, length(urls), n_new, n_skip, length(failed)))
  }
  Sys.sleep(PAUSE)
}

# ── Summary ─────────────────────────────────────────────────────────────────
files <- list.files(out_dir, full.names = TRUE)
total_mb <- sum(file.info(files)$size, na.rm = TRUE) / 1048576

cat("\n=== done ===\n")
cat(sprintf("downloaded : %d\n", n_new))
cat(sprintf("already had: %d\n", n_skip))
cat(sprintf("failed     : %d\n", length(failed)))
cat(sprintf("on disk    : %d files, %.1f MB in %s\n", length(files), total_mb, out_dir))

if (length(failed)) {
  cat("\nThese URLs could not be fetched (re-run the script to retry them):\n")
  cat(paste0("  ", utils::head(failed, 20), collapse = "\n"), "\n")
  if (length(failed) > 20) cat(sprintf("  ...and %d more\n", length(failed) - 20))
}
