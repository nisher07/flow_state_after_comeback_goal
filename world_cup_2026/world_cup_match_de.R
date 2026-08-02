# =============================================================================
# World Cup 2026 case-study matches - German-language version
# Standalone presentation demo - not part of the model pipeline. Run with the
# working directory set to this folder (world_cup_2026/).
# Inputs:  world_cup_shots.csv  (manually entered from FotMob, both matches)
# Outputs: de/cumulative_xg_ger_civ.png
#          de/cumulative_xg_arg_egy.png
# =============================================================================
# Identical data and logic to world_cup_match.R; only the plot text and the
# printed summaries are translated to German.
# =============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(grid)   # rasterGrob PNG stamping, for goal markers
library(png)    # readPNG

# 0. Setup
source("../config.R")

# Font size in points is constant regardless of a plot's physical width; both
# charts here use the same width, but the helper keeps the convention
# consistent with the report scripts' subtitle sizing.
REPORT_SUBTITLE_PT <- 10
subtitle_size <- function(width_in) REPORT_SUBTITLE_PT * width_in / 10

DE_DIR <- "de"
dir.create(DE_DIR, showWarnings = FALSE, recursive = TRUE)

# Display names only - the underlying "team" column in world_cup_shots.csv
# stays in English so joins/filters below are unaffected.
team_name_de <- c(
  "Germany"      = "Deutschland",
  "Ivory Coast"  = "Elfenbeinküste",
  "Argentina"    = "Argentinien",
  "Egypt"        = "Ägypten"
)

ICON_BLUE <- "../assets/football_icons/blue.png"
ICON_RED  <- "../assets/football_icons/red.png"

for (f in c(ICON_BLUE, ICON_RED)) {
  if (!file.exists(f))
    stop("Fehlendes Icon: ", f, "\nErwartet wird assets/football_icons/.")
}

raster_blue <- grid::rasterGrob(png::readPNG(ICON_BLUE), interpolate = TRUE)
raster_red  <- grid::rasterGrob(png::readPNG(ICON_RED),  interpolate = TRUE)

# 1. Load shot data
shots_path <- "world_cup_shots.csv"

if (!file.exists(shots_path)) {
  stop("Schussdaten nicht gefunden unter ", shots_path)
}

shots_all <- read.csv(shots_path) |>
  mutate(minute = as.integer(minute), team = trimws(team))
cat(nrow(shots_all), "Schüsse geladen aus", shots_path, "\n\n")

required_cols <- c("minute", "team", "xg", "result")
missing <- setdiff(required_cols, names(shots_all))
if (length(missing) > 0) {
  stop("Schuss-CSV fehlen Spalten: ", paste(missing, collapse = ", "))
}

bad_xg <- shots_all |> filter(xg < 0 | xg > 1)
if (nrow(bad_xg) > 0) {
  print(bad_xg)
  stop("Unmögliche xG-Werte in world_cup_shots.csv (müssen in [0, 1] liegen) - siehe Zeilen oben.")
}

cat("Mannschaften in den Daten:", paste(sort(unique(shots_all$team)), collapse = ", "), "\n\n")

# 2. Cumulative-xG "Flow-Zustand" chart, one function for both matches

make_cum_plot_de <- function(team_eq, team_opp, eq_minute, out_file) {

  team_eq_de  <- team_name_de[[team_eq]]
  team_opp_de <- team_name_de[[team_opp]]

  shots <- shots_all |> filter(team %in% c(team_eq, team_opp))
  if (nrow(shots) == 0) stop("Keine Schüsse gefunden für ", team_eq, " vs ", team_opp)

  cumulative <- shots |>
    arrange(team, minute) |>
    group_by(team) |>
    mutate(cumulative_xg = cumsum(xg)) |>
    ungroup() |>
    bind_rows(tibble(team = c(team_eq, team_opp), minute = 0,
                     xg = 0, cumulative_xg = 0)) |>
    arrange(team, minute)

  cum_by_minute <- cumulative |>
    group_by(team, minute) |>
    summarise(cum_xg = max(cumulative_xg), .groups = "drop")

  goal_events <- shots |>
    filter(result == "Goal") |>
    distinct(team, minute) |>
    left_join(cum_by_minute, by = c("team", "minute")) |>
    arrange(minute) |>
    mutate(
      is_eq_team      = team == team_eq,
      eq_goals_after  = cumsum(team == team_eq),
      opp_goals_after = cumsum(team == team_opp),
      scoreline       = paste0(eq_goals_after, "-", opp_goals_after)
    )

  shade_end <- min(eq_minute + 20, 90)
  x_max_cum <- max(shots$minute, na.rm = TRUE) + 2
  y_max_cum <- max(cumulative$cumulative_xg, na.rm = TRUE) * 1.25

  p <- ggplot() +

    annotate("rect", xmin = eq_minute, xmax = shade_end,
             ymin = 0, ymax = y_max_cum, fill = COL_EQ, alpha = 0.07) +

    geom_hline(yintercept = 0, colour = COL_GRID, linewidth = 0.4) +

    geom_step(data = cumulative, direction = "hv",
              aes(x = minute, y = cumulative_xg, colour = team, group = team),
              linewidth = 1.6, lineend = "round") +

    {
      icon_h   <- y_max_cum * 0.08
      icon_gap <- y_max_cum * 0.05
      lapply(seq_len(nrow(goal_events)), function(i) {
        r      <- goal_events[i, ]
        raster <- if (r$is_eq_team) raster_blue else raster_red
        annotation_custom(raster,
                          xmin = r$minute - 1.5, xmax = r$minute + 1.5,
                          ymin = r$cum_xg + icon_gap,
                          ymax = r$cum_xg + icon_gap + icon_h)
      })
    } +

    geom_label(data = goal_events,
               aes(x = minute, y = cum_xg + y_max_cum * 0.19,
                   label = scoreline, colour = team),
               size = 3.0, fontface = "bold",
               label.padding = unit(0.15, "lines"),
               fill = COL_BG, alpha = 0.92) +

    geom_vline(xintercept = eq_minute,
               colour = COL_EQ, linewidth = 0.9, linetype = "dotted") +

    annotate("text", x = eq_minute + 1, y = y_max_cum * 0.32,
             label = paste0("Ausgleich\n(Minute ", eq_minute, ")"),
             colour = COL_EQ, size = 3.2, hjust = 0, fontface = "italic") +

    annotate("text", x = (eq_minute + shade_end) / 2, y = y_max_cum * 0.08,
             label = "+20-Minuten-Fenster",
             colour = COL_EQ, size = 2.9, hjust = 0.5,
             fontface = "italic", alpha = 0.75) +

    scale_colour_manual(
      values = setNames(c(COL_EQ, COL_OPP), c(team_eq, team_opp)),
      breaks = c(team_eq, team_opp),
      labels = c(team_eq_de, team_opp_de),
      guide  = guide_legend(override.aes = list(linewidth = 2))
    ) +
    scale_x_continuous(
      breaks = seq(0, 90, by = 15),
      labels = paste0(seq(0, 90, by = 15), "'"),
      limits = c(-5, x_max_cum + 15),
      expand = c(0, 1)
    ) +
    scale_y_continuous(
      limits = c(0, y_max_cum),
      labels = function(x) sprintf("%.1f xG", x)
    ) +

    labs(
      title    = paste0("Flow-Zustand in einem Spiel (", team_eq_de, " vs. ", team_opp_de, ")"),
      subtitle = paste0(
        "Laufende Gesamtzahl der erspielten Chancen (xG) beider Mannschaften im Spielverlauf\n",
        "· Eine steilere Linie = mehr und bessere Chancen in dieser Phase\n",
        "· Schattierter Bereich = 20 Minuten nach dem Ausgleichstor"
      ),
      x       = "Spielminute",
      y       = "Gesamtchancen (xG)",
      caption = "Daten: FotMob · Analyse: R (ggplot2, dplyr)"
    ) +
    theme_sport() +
    theme(
      panel.grid.major.y = element_line(colour = COL_GRID, linewidth = 0.25),
      panel.grid.major.x = element_line(colour = COL_GRID, linewidth = 0.15),
      plot.subtitle      = element_text(size = subtitle_size(13))
    )

  ggsave(out_file, p, width = 13, height = 6.5, dpi = 200, bg = COL_BG)
  cat("Gespeichert:", out_file, "\n")

  summary_stats <- shots |>
    mutate(period = if_else(minute < eq_minute, "Vor dem Ausgleich",
                            "Nach dem Ausgleich")) |>
    group_by(team, period) |>
    summarise(
      total_xg   = round(sum(xg), 3),
      n_shots    = n(),
      minutes    = if_else(first(period) == "Vor dem Ausgleich",
                           eq_minute, max(x_max_cum, 95) - eq_minute),
      xg_per_min = round(total_xg / minutes, 4),
      .groups    = "drop"
    )
  cat("\n== ", team_eq_de, " vs ", team_opp_de, " - Zusammenfassung ==\n", sep = "")
  print(summary_stats)
  cat("\n")

  invisible(p)
}

# 3. Build both match charts

make_cum_plot_de(team_eq = "Germany",   team_opp = "Ivory Coast",
                 eq_minute = 68, out_file = file.path(DE_DIR, "cumulative_xg_ger_civ.png"))

make_cum_plot_de(team_eq = "Argentina", team_opp = "Egypt",
                 eq_minute = 83, out_file = file.path(DE_DIR, "cumulative_xg_arg_egy.png"))

cat("Fertig: zwei kumulative xG-Diagramme in", DE_DIR, "gespeichert.\n")
