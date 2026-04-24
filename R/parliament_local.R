# R/parliament_local.R
#
# Drop-in replacements for all GitHub-only package functions used in this app:
#
#   ggparliament:
#     parliament_data()        -> parliament_data_local()
#     geom_parliament_seats()  -> geom_parliament_seats_local()
#     theme_ggparliament()     -> theme_ggparliament_local()
#
#   waffle (hrbrmstr):
#     waffle()                 -> waffle_gg()
#
# Dependency: only ggplot2 (already loaded in server.R).
# Tested against ggparliament >= 0.3.0 output for EP P6-P9 seat counts.


# ── 1. Seat-layout engine ─────────────────────────────────────────────────────

#' Generate semicircle seat coordinates
#'
#' @param total_seats  Integer. Total number of seats to place.
#' @param n_rows       Integer. Number of concentric arcs.
#' @return data.frame with columns x, y, row  (nrow == total_seats),
#'         sorted by angle descending (π → 0, i.e. left → right visually)
#'         so that parties are assigned in contiguous left-to-right wedges.
.semicircle_coords <- function(total_seats, n_rows) {
  # Radii of the arcs: inner arc = 1.5, each subsequent arc +1
  r_min  <- 1.5
  radii  <- r_min + seq(0L, n_rows - 1L)   # 1.5, 2.5, 3.5, …

  # Distribute seats across rows proportional to arc length (∝ radius)
  raw           <- total_seats * radii / sum(radii)
  seats_per_row <- round(raw)

  # Correct rounding error so sum matches exactly
  delta <- total_seats - sum(seats_per_row)
  if (delta != 0L) {
    fracs <- raw - floor(raw)
    idx   <- order(fracs, decreasing = (delta > 0L))[seq_len(abs(delta))]
    seats_per_row[idx] <- seats_per_row[idx] + sign(delta)
  }

  # For every row: n+2 angles over [0, π], drop the two endpoints so seats
  # don't pile up exactly at the left/right poles.
  coords_list <- vector("list", n_rows)
  for (i in seq_len(n_rows)) {
    n   <- seats_per_row[i]
    r   <- radii[i]
    ang <- seq(0, pi, length.out = n + 2L)[-c(1L, n + 2L)]
    coords_list[[i]] <- data.frame(
      x     = r * cos(ang),
      y     = r * sin(ang),
      row   = i,
      angle = ang          # kept for sort, removed afterwards
    )
  }

  all_coords <- do.call(rbind, coords_list)

  # Sort by angle DESCENDING (π = left side first, 0 = right side last).
  # This fills the semicircle in vertical "columns" left→right so that
  # parties occupy contiguous wedges rather than contiguous arcs.
  all_coords <- all_coords[order(all_coords$angle, decreasing = TRUE), ]
  all_coords$angle <- NULL
  rownames(all_coords) <- NULL

  all_coords
}


# ── 2. parliament_data_local() ────────────────────────────────────────────────

#' Replaces ggparliament::parliament_data() — semicircle layout only
#'
#' @param election_data  data.frame, one row per party, already sorted in the
#'                       desired left-to-right order.
#' @param parl_rows      Integer. Number of seat rows (concentric arcs).
#' @param type           Character. Only "semicircle" is implemented locally;
#'                       kept as argument so call-sites don't need changes.
#' @param party_seats    Numeric vector, same length/order as election_data rows.
#'                       Number of seats per party.
#' @return election_data expanded to one row per seat, with x, y, row appended.
parliament_data_local <- function(election_data,
                                   parl_rows,
                                   type         = "semicircle",
                                   party_seats) {
  stopifnot(
    is.data.frame(election_data),
    length(party_seats) == nrow(election_data),
    parl_rows >= 1L
  )

  total_seats <- sum(party_seats)
  coords      <- .semicircle_coords(total_seats, parl_rows)

  # Expand election_data: one row per seat
  expanded          <- election_data[rep(seq_len(nrow(election_data)),
                                         times = as.integer(party_seats)), ]
  rownames(expanded) <- NULL

  expanded$x   <- coords$x
  expanded$y   <- coords$y
  expanded$row <- coords$row

  expanded
}


# ── 3. geom_parliament_seats_local() ─────────────────────────────────────────

#' Replaces ggparliament::geom_parliament_seats()
#'
#' Thin wrapper around geom_point(). All arguments forwarded to geom_point().
geom_parliament_seats_local <- function(...) {
  geom_point(size = 2.5, ...)
}


# ── 4. theme_ggparliament_local() ────────────────────────────────────────────

#' Replaces ggparliament::theme_ggparliament()
#'
#' @param legend Logical. If FALSE (default in the app), legend is suppressed.
theme_ggparliament_local <- function(legend = TRUE) {
  base <- theme_void() +
    theme(
      plot.title      = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  if (!legend) base <- base + theme(legend.position = "none")
  base
}


# ── 5. waffle_gg() ────────────────────────────────────────────────────────────

#' Replaces waffle::waffle() — ggplot2-based coloured-square grid
#'
#' @param parts      Named numeric vector: category -> count.
#' @param rows       Integer. Number of rows in the grid (default 10).
#' @param colors     Named character vector mapping category names to colours.
#'                   If NULL, ggplot2 default palette is used.
#' @param title      Character. Plot title (NULL = no title).
#' @param legend_pos Character. Passed to theme(legend.position = ...).
#' @return A ggplot object — compatible with +theme_minimal() etc., just like
#'         the waffle package return value.
waffle_gg <- function(parts, rows = 10, colors = NULL,
                      title = NULL, legend_pos = "bottom") {
  stopifnot(is.numeric(parts), length(parts) > 0L)
  if (is.null(names(parts))) names(parts) <- as.character(seq_along(parts))

  total <- sum(as.integer(parts))
  n_cols <- ceiling(total / rows)

  # Build seat grid: fill column-by-column, left-to-right, bottom-to-top
  idx  <- seq_len(total)
  grid <- data.frame(
    x        = ((idx - 1L) %% n_cols) + 1L,
    y        = rows - ((idx - 1L) %/% n_cols),
    category = rep(names(parts), times = as.integer(parts))
  )
  # Preserve legend order
  grid$category <- factor(grid$category, levels = names(parts))

  p <- ggplot(grid, aes(x = .data$x, y = .data$y, fill = .data$category)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    coord_equal(expand = FALSE) +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    labs(title = title, x = NULL, y = NULL, fill = NULL) +
    theme_void() +
    theme(
      legend.position  = legend_pos,
      legend.title     = element_blank(),
      legend.text      = element_text(size = 9),
      plot.title       = element_text(size = 14, face = "bold"),
      plot.background  = element_rect(fill = "white", colour = NA)
    )

  if (!is.null(colors)) {
    p <- p + scale_fill_manual(values = colors, drop = FALSE)
  }

  p
}


# ── 6. silhouette_gg() ────────────────────────────────────────────────────────

#' Replaces factoextra::fviz_silhouette()
#'
#' Draws a barplot of per-observation silhouette widths, coloured by cluster,
#' sorted descending within each cluster (same layout as fviz_silhouette).
#'
#' @param sil_obj   A `cluster::silhouette()` matrix (columns: cluster,
#'                  neighbor, sil_width). Coerced to data.frame internally.
#' @param avg_score Numeric. Pre-computed mean sil_width for the caption.
#' @return A ggplot object.
silhouette_gg <- function(sil_obj, avg_score) {
  df <- as.data.frame(unclass(sil_obj))   # matrix → data.frame
  df$obs <- seq_len(nrow(df))

  # Sort: by cluster ascending, then sil_width descending within cluster
  df <- df[order(df$cluster, -df$sil_width), ]
  df$pos <- seq_len(nrow(df))             # x-position after sort

  ggplot(df, aes(x = .data$pos, y = .data$sil_width,
                 fill = factor(.data$cluster))) +
    geom_bar(stat = "identity", width = 1, colour = NA) +
    geom_hline(yintercept = 0.5, linetype = "dashed",
               colour = "darkblue", linewidth = 0.75) +
    annotate("text", x = 1, y = 0.55,
             label = "Good Separation Threshold",
             colour = "darkblue", size = 4, hjust = 0, vjust = -0.5) +
    scale_x_continuous(breaks = NULL) +
    labs(
      title   = "Silhouette Plot",
      x       = NULL,
      y       = "Silhouette width",
      fill    = "Cluster",
      caption = paste("Average Silhouette Score:",
                      round(avg_score, 3))
    ) +
    theme_minimal() +
    theme(
      plot.title    = element_text(size = 16, face = "bold"),
      plot.caption  = element_text(colour = "red", face = "bold", size = 12),
      legend.position = "right"
    )
}
