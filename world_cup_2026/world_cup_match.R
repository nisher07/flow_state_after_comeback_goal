# =============================================================================
# World Cup 2026 case-study matches
# Standalone presentation demo — not part of the model pipeline. Run with the
# working directory set to this folder (world_cup_2026/).
# Inputs:  world_cup_shots.csv  (manually entered from FotMob, both matches)
# Outputs: cumulative_xg_ger_civ.png
#          cumulative_xg_arg_egy.png
# =============================================================================
# WHAT THIS SCRIPT DOES
# ---------------------
# Takes shot-level data for two World Cup 2026 matches (manually entered) and
# produces one cumulative-xG "Flow State" chart per match: momentum-window
# shading after the equaliser, football-icon markers with scoreline labels at
# each goal, dotted equaliser line. The slope of each line in the pre/post
# regions tells the story.
#
# THE MATCHES
# -----------
# Germany 2 - 1 Ivory Coast (Round of 16, 20 June 2026)
#   Goals: Kessie 30', Undav 68' (equaliser), Undav 90+4' (winner)
#   Total xG (FotMob): Germany 1.89, Ivory Coast 1.22
#
# Argentina 3 - 2 Egypt
#   Goals: Egypt 15', Egypt 67', Argentina 79', Argentina 83' (equaliser),
#          Argentina 90+2' (winner)
# =============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(grid)   # rasterGrob PNG stamping, for goal markers
library(png)    # readPNG

# 0. Setup
# Colours (COL_EQ, COL_OPP, COL_NEUTRAL, COL_BG, COL_GRID, ...) and
# theme_sport() come from the project's shared visual identity in config.R -
# same theme as R/04_report_figures.R and R/05_article_figures.R, defined
# once instead of redefined per script.
source("../config.R")

# Football icon markers — the project's shared assets, via a relative path
# (script runs from this folder).
ICON_BLUE <- "../assets/football_icons/blue.png"
ICON_RED  <- "../assets/football_icons/red.png"

for (f in c(ICON_BLUE, ICON_RED)) {
  if (!file.exists(f))
    stop("Missing icon: ", f, "\nExpected assets/football_icons/ to exist.")
}

raster_blue <- grid::rasterGrob(png::readPNG(ICON_BLUE), interpolate = TRUE)
raster_red  <- grid::rasterGrob(png::readPNG(ICON_RED),  interpolate = TRUE)

# 1. Load shot data
# world_cup_shots.csv holds both matches, distinguished by team names.
# Columns: minute, team, xg, result, situation.

shots_path <- "world_cup_shots.csv"

if (!file.exists(shots_path)) {
  stop("Shot data not found at ", shots_path)
}

shots_all <- read.csv(shots_path) |>
  mutate(minute = as.integer(minute), team = trimws(team))
cat("Loaded", nrow(shots_all), "shots from", shots_path, "\n\n")

required_cols <- c("minute", "team", "xg", "result")
missing <- setdiff(required_cols, names(shots_all))
if (length(missing) > 0) {
  stop("Shot CSV missing columns: ", paste(missing, collapse = ", "))
}

# A single shot's xG must lie in [0, 1] - catches manual-entry typos.
bad_xg <- shots_all |> filter(xg < 0 | xg > 1)
if (nrow(bad_xg) > 0) {
  print(bad_xg)
  stop("Impossible xG values in world_cup_shots.csv (must be in [0, 1]) - fix the rows above.")
}

cat("Teams in data:", paste(sort(unique(shots_all$team)), collapse = ", "), "\n\n")

# 2. Cumulative-xG "Flow State" chart, one function for both matches

make_cum_plot <- function(team_eq, team_opp, eq_minute, out_file) {

  shots <- shots_all |> filter(team %in% c(team_eq, team_opp))
  if (nrow(shots) == 0) stop("No shots found for ", team_eq, " vs ", team_opp)

  cumulative <- shots |>
    arrange(team, minute) |>
    group_by(team) |>
    mutate(cumulative_xg = cumsum(xg)) |>
    ungroup() |>
    bind_rows(tibble(team = c(team_eq, team_opp), minute = 0,
                     xg = 0, cumulative_xg = 0)) |>
    arrange(team, minute)

  # Cumulative xG immediately after each goal (collapses same-minute ties) —
  # the height the goal marker sits at.
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

    # Momentum window shading
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
             label = paste0("Equaliser\n(min ", eq_minute, ")"),
             colour = COL_EQ, size = 3.2, hjust = 0, fontface = "italic") +

    annotate("text", x = (eq_minute + shade_end) / 2, y = y_max_cum * 0.08,
             label = "+20 min window",
             colour = COL_EQ, size = 2.9, hjust = 0.5,
             fontface = "italic", alpha = 0.75) +

    scale_colour_manual(
      values = setNames(c(COL_EQ, COL_OPP), c(team_eq, team_opp)),
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
      title    = paste0("Flow State in One Match (", team_eq, " vs ", team_opp, ")"),
      subtitle = paste0(
        "Running total of chances created (xG) by both teams as the match unfolds · A steeper line = more and better chances created in that phase \n · ",
        "Shaded area = 20 minutes after the comeback equaliser"
      ),
      x       = "Match minute",
      y       = "Total chances (xG)",
      caption = "Data: FotMob · Analysis: R (ggplot2, dplyr)"
    ) +
    theme_sport() +
    theme(
      panel.grid.major.y = element_line(colour = COL_GRID, linewidth = 0.25),
      panel.grid.major.x = element_line(colour = COL_GRID, linewidth = 0.15)
    )

  ggsave(out_file, p, width = 13, height = 6.5, dpi = 200, bg = COL_BG)
  cat("Saved:", out_file, "\n")

  # Per-match before/after summary
  summary_stats <- shots |>
    mutate(period = if_else(minute < eq_minute, "Before equaliser",
                            "After equaliser")) |>
    group_by(team, period) |>
    summarise(
      total_xg   = round(sum(xg), 3),
      n_shots    = n(),
      minutes    = if_else(first(period) == "Before equaliser",
                           eq_minute, max(x_max_cum, 95) - eq_minute),
      xg_per_min = round(total_xg / minutes, 4),
      .groups    = "drop"
    )
  cat("\n== ", team_eq, " vs ", team_opp, " — summary ==\n", sep = "")
  print(summary_stats)
  cat("\n")

  invisible(p)
}

# 3. Build both match charts

make_cum_plot(team_eq = "Germany",   team_opp = "Ivory Coast",
              eq_minute = 68, out_file = "cumulative_xg_ger_civ.png")

make_cum_plot(team_eq = "Argentina", team_opp = "Egypt",
              eq_minute = 83, out_file = "cumulative_xg_arg_egy.png")

cat("Done: two cumulative-xG charts written to this folder.\n")
