# scripts/bridge_legislator_timeline.R
#
# Timeline visualisation: MEP ideological positions across P6–P9
# using Bridge Legislators (MEPs who served in ≥2 parliamentary terms).
#
# Output:
#   bridge_timeline.png        – standard figure
#   bridge_timeline_slide.png  – 16:9 slide version
#
# Run from the project root:
#   Rscript scripts/bridge_legislator_timeline.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggrepel)
})

# ── EPG normalisation ─────────────────────────────────────────────────────────
EPG_MAP <- c(
  "Group of the European People's Party (Christian Democrats) and European Democrats" = "EPP",
  "Group of the European People's Party (Christian Democrats)" = "EPP",
  "EPP"                                                        = "EPP",
  "Group of the Progressive Alliance of Socialists and Democrats in the European Parliament" = "S&D",
  "Socialist Group in the European Parliament"                 = "S&D",
  "Socialists_Democrats"                                       = "S&D",
  "Group of the Alliance of Liberals and Democrats for Europe" = "ALDE/RE",
  "REG"                                                        = "ALDE/RE",
  "European Conservatives and Reformists Group"                = "ECR",
  "ECR"                                                        = "ECR",
  "Group of the Greens/European Free Alliance"                 = "Greens/EFA",
  "Greens_EFA"                                                 = "Greens/EFA",
  "Confederal Group of the European United Left - Nordic Green Left" = "GUE/NGL",
  "The Left"                                                   = "GUE/NGL",
  "Non-attached Members"                                       = "NI",
  "NI"                                                         = "NI"
)

# EP conventional colours
EPG_COLORS <- c(
  "EPP"        = "#003399",
  "S&D"        = "#CC0000",
  "ALDE/RE"    = "#E4A000",
  "ECR"        = "#0EC3E4",
  "Greens/EFA" = "#009900",
  "GUE/NGL"    = "#8B0000",
  "NI"         = "#AAAAAA"
)

MAJOR_EPG <- names(EPG_COLORS)[names(EPG_COLORS) != "NI"]

# ── Load data ─────────────────────────────────────────────────────────────────
message("Loading data ...")

long <- bind_rows(
  lapply(c("P6","P7","P8","P9"), function(p) {
    df <- readRDS(file.path("data", sprintf("%s_umap.rds", p)))
    df |>
      select(WebisteEpID, FullName, EPG, Country, coord1D, coord2D) |>
      mutate(
        parliament       = as.integer(substr(p, 2, 2)),
        parliament_label = p,
        EPG_short        = coalesce(EPG_MAP[EPG], EPG)
      )
  })
)

# ── Bridge legislators ────────────────────────────────────────────────────────
bridge_ids <- long |>
  filter(!is.na(WebisteEpID) & !is.na(coord1D)) |>
  group_by(WebisteEpID) |>
  summarise(n_terms = n_distinct(parliament)) |>
  filter(n_terms >= 2) |>
  pull(WebisteEpID)

bridge <- long |>
  filter(WebisteEpID %in% bridge_ids, !is.na(coord1D))

message(sprintf("Bridge legislators: %d MEPs", length(bridge_ids)))

# ── Group means ───────────────────────────────────────────────────────────────
gm <- bridge |>
  filter(EPG_short %in% MAJOR_EPG) |>
  group_by(EPG_short, parliament) |>
  summarise(
    mean_pos = mean(coord1D, na.rm = TRUE),
    se_pos   = sd(coord1D, na.rm = TRUE) / sqrt(n()),
    n        = n(),
    .groups  = "drop"
  )

# ── Plot ──────────────────────────────────────────────────────────────────────
message("Building plot ...")

PARL_BREAKS  <- 6:9
PARL_LABELS  <- c("EP6\n2004–09", "EP7\n2009–14", "EP8\n2014–19", "EP9\n2019–24")

# End-of-series labels (last parliament each group appears in)
gm_labels <- gm |>
  group_by(EPG_short) |>
  slice_max(parliament, n = 1)

# Individual trajectory data (major groups only)
bridge_major <- bridge |> filter(EPG_short %in% MAJOR_EPG)

p <- ggplot() +

  # Background: individual MEP trajectories
  geom_line(
    data = bridge_major |>
      group_by(WebisteEpID) |> filter(n() >= 2) |> ungroup(),
    aes(x = parliament, y = coord1D, group = WebisteEpID,
        colour = EPG_short),
    alpha = 0.06, linewidth = 0.5
  ) +

  # SE ribbon
  geom_ribbon(
    data = gm,
    aes(x = parliament, ymin = mean_pos - se_pos,
        ymax = mean_pos + se_pos, fill = EPG_short),
    alpha = 0.15
  ) +

  # Group mean line
  geom_line(
    data = gm,
    aes(x = parliament, y = mean_pos, colour = EPG_short),
    linewidth = 2.8
  ) +

  # Group mean points
  geom_point(
    data = gm,
    aes(x = parliament, y = mean_pos, colour = EPG_short),
    shape = 21, fill = "white", size = 3.5, stroke = 2
  ) +

  # End labels
  geom_text(
    data = gm_labels,
    aes(x = parliament + 0.1, y = mean_pos, label = EPG_short,
        colour = EPG_short),
    hjust = 0, fontface = "bold", size = 3.5
  ) +

  # Reference line
  geom_hline(yintercept = 0, colour = "#cccccc", linetype = "dashed",
             linewidth = 0.7) +

  scale_x_continuous(breaks = PARL_BREAKS, labels = PARL_LABELS,
                     limits = c(5.8, 10.2)) +
  scale_colour_manual(values = EPG_COLORS, guide = "none") +
  scale_fill_manual(values = EPG_COLORS, guide = "none") +

  labs(
    title    = "Ideological Positions of Bridge Legislators Across Parliamentary Terms",
    subtitle = sprintf(
      "Mean W-NOMINATE score (±SE) for MEPs serving in ≥2 terms, by party group  ·  N = %d bridge legislators",
      length(bridge_ids)
    ),
    x = NULL,
    y = "W-NOMINATE 1st Dimension  (← Left   ·   Right →)"
  ) +

  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 15),
    plot.subtitle      = element_text(colour = "#666666", size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = "#eeeeee"),
    axis.text.x        = element_text(size = 11),
    plot.margin        = margin(12, 80, 12, 12)
  )

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave("bridge_timeline.png",       p, width = 13, height = 7.5, dpi = 200)
ggsave("bridge_timeline_slide.png", p, width = 16, height = 9,   dpi = 200)

message("Done.  Saved: bridge_timeline.png  |  bridge_timeline_slide.png")
