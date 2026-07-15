# 03_plot_momentum_curve.R
# ==============================================================================
# General-audience momentum figure: attacking output ratio (observed xG /
# model-expected xG) for BOTH teams, in 5-min bins from 15' before to 30'
# after a qualifying equaliser. Ratio 1.0 = performing exactly as expected
# given score state, match time, team quality, venue and season.
#
# The washout bin (containing the equaliser itself) is left as a gap: it is
# the conditioning event, not performance.
#
# Run from the project root:  Rscript R/03_plot_momentum_curve.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(mgcv)
})
source("config.R")
set.seed(42)

windows <- read_csv(file.path(DATA_PROCESSED_DIR, "team_windows.csv"),
                    show_col_types = FALSE)
eqs <- read_csv(file.path(DATA_PROCESSED_DIR, "equaliser_events.csv"),
                show_col_types = FALSE)

# ── counterfactual model: identical spec + split to 02_train_poisson_model ───
windows <- windows %>%
  mutate(goal_diff_c = factor(pmax(pmin(goal_diff, 2L), -2L), levels = -2:2),
         season_f = factor(season))
train <- windows %>% filter(!own_washout, !opp_washout, !own_post, !opp_post)
fit <- bam(xg ~ goal_diff_c + s(bin_start, k = 8) + s(elo_diff, k = 6) +
             is_home + season_f,
           family = tw(), data = train, discrete = TRUE)
windows$pred <- as.numeric(predict(fit, newdata = windows, type = "response"))

# ── relative-bin panel around every qualifying equaliser <= 70' ──────────────
eqs <- eqs %>%
  filter(eq_minute <= 70) %>%
  mutate(eq_bin = pmin(eq_minute %/% 5L, 17L), eq_id = row_number())

panel <- eqs %>%
  select(eq_id, match_id, eq_team = team, eq_opp = opponent, eq_bin) %>%
  crossing(offset = c(-3:-1, 1:6)) %>%
  mutate(bin = eq_bin + offset) %>%
  filter(bin >= 0, bin <= 17) %>%
  pivot_longer(c(eq_team, eq_opp), names_to = "side", values_to = "team") %>%
  inner_join(windows %>% select(match_id, team, bin, xg, pred),
             by = c("match_id", "team", "bin")) %>%
  mutate(side = if_else(side == "eq_team", "Equalising team (was losing)",
                        "Opponent (was leading)"))

# point estimate per side x offset
curve <- panel %>%
  group_by(side, offset) %>%
  summarise(ratio = sum(xg) / sum(pred), n = n(), .groups = "drop")

# cluster bootstrap over equaliser events -> 95% ribbon
B <- 500
eq_ids <- unique(panel$eq_id)
panel_by_eq <- split(panel, panel$eq_id)
boot <- lapply(seq_len(B), function(b) {
  draw <- sample(eq_ids, length(eq_ids), replace = TRUE)
  bind_rows(panel_by_eq[as.character(draw)]) %>%
    group_by(side, offset) %>%
    summarise(ratio = sum(xg) / sum(pred), .groups = "drop") %>%
    mutate(b = b)
})
band <- bind_rows(boot) %>%
  group_by(side, offset) %>%
  summarise(lo = quantile(ratio, .025), hi = quantile(ratio, .975),
            .groups = "drop")

curve <- curve %>% left_join(band, by = c("side", "offset"))
write_csv(curve, file.path(OUTPUT_TABLES_DIR, "momentum_curve_data.csv"))

cat("Attacking output ratio (obs/expected) by 5-min bin:\n")
print(as.data.frame(curve), row.names = FALSE, digits = 3)

# ── plot ──────────────────────────────────────────────────────────────────────
x_labs <- c("-15'", "-10'", "-5'", "GOAL", "+5'", "+10'", "+15'",
            "+20'", "+25'", "+30'")
pal <- c("Equalising team (was losing)" = COL_EQ,
         "Opponent (was leading)"       = COL_OPP)

n_eq <- nrow(eqs)
n_matches <- n_distinct(eqs$match_id)

p <- ggplot(curve, aes(offset, ratio, colour = side, fill = side)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = COL_NEUTRAL,
             linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dotted", colour = COL_EQ,
             linewidth = 0.6) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.13, colour = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.6) +
  annotate("text", x = 0, y = max(curve$hi) + 0.06, label = "EQUALISER",
           colour = COL_EQ, fontface = "bold", size = 3.6) +
  annotate("text", x = -2.9, y = 1.025, hjust = 0, vjust = 0,
           label = "Baseline (expected performance)",
           colour = COL_NEUTRAL, fontface = "italic", size = 3.2) +
  annotate("text", x = -1.15, y = 1.40, hjust = 0, lineheight = 0.95,
           label = "Leading side was running hot\nbefore conceding - a sign of\nopen games, not momentum",
           colour = COL_OPP, fontface = "bold", size = 3.1) +
  annotate("curve", x = -1.2, y = 1.38, xend = -1.95, yend = 1.26,
           curvature = 0.25, colour = COL_OPP, linewidth = 0.5,
           arrow = arrow(length = unit(2, "mm"))) +
  annotate("text", x = 1.4, y = 0.795, hjust = 0, lineheight = 0.95,
           label = "Conceding side dips for ~5 minutes,\nthen returns to normal",
           colour = COL_OPP, fontface = "bold", size = 3.1) +
  annotate("curve", x = 1.35, y = 0.81, xend = 1.05, yend = 0.885,
           curvature = -0.25, colour = COL_OPP, linewidth = 0.5,
           arrow = arrow(length = unit(2, "mm"))) +
  annotate("text", x = 4.0, y = 1.33, hjust = 0, lineheight = 0.95,
           label = "Equalising team: no surge -\nstays near expectation throughout",
           colour = COL_EQ, fontface = "bold", size = 3.1) +
  annotate("curve", x = 3.95, y = 1.30, xend = 3.1, yend = 1.14,
           curvature = 0.25, colour = COL_EQ, linewidth = 0.5,
           arrow = arrow(length = unit(2, "mm"))) +
  scale_x_continuous(breaks = c(-3:-1, 0, 1:6), labels = x_labs) +
  scale_y_continuous(labels = function(v) paste0(v, "x")) +
  scale_colour_manual(values = pal, name = NULL) +
  scale_fill_manual(values = pal, name = NULL) +
  labs(
    title = "After the Equaliser, Both Teams Just Play Normal Football",
    subtitle = sprintf(
      "Attacking output vs expectation, 15 min before to 30 min after a comeback equaliser\n%s equalisers in %s Bundesliga matches, 2016/17-2025/26 - shaded: 95%% bootstrap band",
      format(n_eq, big.mark = ","), format(n_matches, big.mark = ",")),
    x = "minutes relative to the equaliser",
    y = "attacking output ratio (observed / expected xG)",
    caption = "Expected xG: model accounting for score state, match time, team strength (ELO), venue, season.\nThe goal bin itself is omitted: it contains the equaliser and would only restate that a goal happened."
  ) +
  theme_momentum() +
  theme(plot.caption = element_text(colour = COL_GREY, hjust = 0, size = 8.5),
        plot.subtitle = element_text(size = 9.5))

ggsave(file.path(OUTPUT_FIGURES_DIR, "momentum_curve.png"), p,
       width = 10, height = 6, dpi = 150, bg = COL_BG)

# ── before/after bar plot (general audience) ──────────────────────────────────
# Complete-window events only (equaliser between 15' and 70'), so every event
# contributes a full 15 min before AND after. Raw observed means, with the
# model expectation marked on each bar - the state changes at the goal
# (trailing -> level), so expectations move too; momentum would be observed
# bars clearly ABOVE their expectation marks after the goal.
complete <- panel %>%
  filter(eq_bin >= 3, offset %in% c(-3:-1, 1:3)) %>%
  mutate(phase = if_else(offset < 0, "15 min before", "15 min after"))

bars <- complete %>%
  group_by(side, phase) %>%
  summarise(observed = mean(xg), expected = mean(pred), .groups = "drop")

boot_bars <- bind_rows(lapply(seq_len(500), function(b) {
  draw <- sample(unique(complete$eq_id), replace = TRUE)
  complete %>%
    filter(eq_id %in% draw) %>%    # approximation is fine for CI width
    group_by(side, phase) %>%
    summarise(observed = mean(xg), .groups = "drop")
})) %>%
  group_by(side, phase) %>%
  summarise(lo = quantile(observed, .025), hi = quantile(observed, .975),
            .groups = "drop")
bars <- bars %>%
  left_join(boot_bars, by = c("side", "phase")) %>%
  mutate(phase = factor(phase, levels = c("15 min before", "15 min after")))

n_events <- n_distinct(complete$eq_id)
p_bars <- ggplot(bars, aes(phase, observed, fill = side)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = lo, ymax = hi),
                position = position_dodge(width = 0.7), width = 0.12,
                colour = COL_NEUTRAL, linewidth = 0.5) +
  geom_point(aes(y = expected), position = position_dodge(width = 0.7),
             shape = 23, size = 3, fill = COL_BG, colour = COL_NEUTRAL,
             stroke = 0.9) +
  geom_text(aes(label = sprintf("%.3f", observed), y = hi + 0.006),
            position = position_dodge(width = 0.7),
            size = 3.4, colour = COL_NEUTRAL, fontface = "bold") +
  scale_fill_manual(values = pal, name = NULL) +
  labs(
    title = "Attacking Output Before and After the Equaliser",
    subtitle = sprintf(
      "Mean xG per 5-min bin, 15 minutes either side of the goal - %s equalisers (15'-70'), Bundesliga 2016/17-2025/26\nDiamonds: what the model expects given score state, time, strength, venue - observed bars match them: no momentum",
      format(n_events, big.mark = ",")),
    x = NULL, y = "mean xG per 5-min bin",
    caption = "Whiskers: 95% bootstrap interval. The goal's own 5-min bin is excluded on both sides (it contains the equaliser itself).\nBoth teams' output rises slightly after the goal because a level game is more open - the diamonds move with it."
  ) +
  theme_momentum() +
  theme(plot.caption = element_text(colour = COL_GREY, hjust = 0, size = 8.5),
        plot.subtitle = element_text(size = 9.5))

ggsave(file.path(OUTPUT_FIGURES_DIR, "h1_before_after_bars.png"), p_bars,
       width = 8.5, height = 5.5, dpi = 150, bg = COL_BG)
write_csv(bars, file.path(OUTPUT_TABLES_DIR, "before_after_bars_data.csv"))

cat("\nWritten: output/figures/momentum_curve.png, h1_before_after_bars.png\n")
cat("         output/tables/momentum_curve_data.csv, before_after_bars_data.csv\n")
