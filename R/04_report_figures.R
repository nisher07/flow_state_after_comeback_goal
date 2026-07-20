# 04_report_figures.R
# ==============================================================================
# ALL report figures live in this single file. Output: output/figures/report/
#
# Style: shared theme_sport() (bold title, grey subtitle, small data-source
# overrides are layered with theme(...). Legend wording: "Equalising team" / "Opponent".
#
# Run from the project root:  Rscript R/04_report_figures.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(mgcv)
  library(sandwich)
  library(lmtest)
  library(png)
  library(grid)
})
source("config.R")
set.seed(42)

REPORT_DIR <- file.path(OUTPUT_FIGURES_DIR, "report")
dir.create(REPORT_DIR, showWarnings = FALSE, recursive = TRUE)

ball_blue <- rasterGrob(readPNG("assets/football_icons/blue.png"),
                        interpolate = TRUE)

pal_side <- c("Equalising team" = COL_EQ, "Opponent" = COL_OPP)

# Shared theme: theme_sport() from config.R (the project's visual identity).
# Layout-specific needs are layered per plot with theme(...), never by
# redefining the theme. Shared mark conventions:
#   curves: linewidth 1.7, points 3.5, ribbons alpha 0.15
#   annotations: size 3.4 bold, arrows 0.7 curvature 0.3 closed 0.20cm
#   baseline note: size 3 italic grey40 · n-labels: 3.3 italic grey40

# H1 model (identical spec to 02_train_poisson_model.R)
windows_raw <- read_csv(file.path(DATA_PROCESSED_DIR, "team_windows.csv"),
                        show_col_types = FALSE) %>%
  mutate(goal_diff_c = factor(pmax(pmin(goal_diff, 2L), -2L), levels = -2:2),
         season_f = factor(season))

windows <- windows_raw %>% filter(!own_washout, !opp_washout)
train   <- windows %>% filter(!own_post, !opp_post)
treat   <- windows %>% filter(own_post, own_post_eq_minute <= 70)
mirror  <- windows %>% filter(opp_post, opp_post_eq_minute <= 70)

fit <- bam(xg ~ goal_diff_c + s(bin_start, k = 8) + s(elo_diff, k = 6) +
             is_home + season_f,
           family = tw(), data = train, discrete = TRUE)

train$pred  <- as.numeric(predict(fit, newdata = train,  type = "response"))
treat$pred  <- as.numeric(predict(fit, newdata = treat,  type = "response"))
mirror$pred <- as.numeric(predict(fit, newdata = mirror, type = "response"))
treat$eq_key  <- paste(treat$match_id,  treat$own_post_eq_minute)
mirror$eq_key <- paste(mirror$match_id, mirror$opp_post_eq_minute)

boot_ratio <- function(df, value, pred, key, B = 500) {
  ks <- split(seq_len(nrow(df)), df[[key]])
  r <- replicate(B, {
    idx <- unlist(ks[sample(length(ks), replace = TRUE)], use.names = FALSE)
    sum(df[[value]][idx]) / sum(df[[pred]][idx])
  })
  tibble(est = sum(df[[value]]) / sum(df[[pred]]),
         lo = unname(quantile(r, .025)), hi = unname(quantile(r, .975)))
}

# shared relative-bin panel around every equaliser <= 70'
eqs <- read_csv(file.path(DATA_PROCESSED_DIR, "equaliser_events.csv"),
                show_col_types = FALSE) %>%
  filter(eq_minute <= 70) %>%
  mutate(eq_bin = pmin(eq_minute %/% 5L, 17L), eq_id = row_number(),
         phase = factor(case_when(eq_minute < 30 ~ "Early equaliser (<30')",
                                  eq_minute <= 50 ~ "Mid equaliser (30'-50')",
                                  TRUE            ~ "Late equaliser (51'-70')"),
                        levels = c("Early equaliser (<30')",
                                   "Mid equaliser (30'-50')",
                                   "Late equaliser (51'-70')")))

windows_raw$pred <- as.numeric(predict(fit, newdata = windows_raw,
                                       type = "response"))
panel <- eqs %>%
  select(eq_id, match_id, eq_team = team, eq_opp = opponent, eq_bin, phase) %>%
  crossing(offset = c(-3:-1, 1:6)) %>%
  mutate(bin = eq_bin + offset) %>%
  filter(bin >= 0, bin <= 17) %>%
  pivot_longer(c(eq_team, eq_opp), names_to = "side", values_to = "team") %>%
  inner_join(windows_raw %>% select(match_id, team, bin, xg, pred),
             by = c("match_id", "team", "bin")) %>%
  mutate(side = if_else(side == "eq_team", "Equalising team", "Opponent"))

curve <- panel %>%
  group_by(side, offset) %>%
  summarise(ratio = sum(xg) / sum(pred), n = n(), .groups = "drop")

panel_by_eq <- split(panel, panel$eq_id)
boot <- bind_rows(lapply(seq_len(500), function(b) {
  draw <- sample(names(panel_by_eq), length(panel_by_eq), replace = TRUE)
  bind_rows(panel_by_eq[draw]) %>%
    group_by(side, offset) %>%
    summarise(ratio = sum(xg) / sum(pred), .groups = "drop")
}))
curve <- curve %>%
  left_join(boot %>% group_by(side, offset) %>%
              summarise(lo = quantile(ratio, .025), hi = quantile(ratio, .975),
                        .groups = "drop"),
            by = c("side", "offset"))

# 01: momentum decay curve (annotated)
f01 <- ggplot(curve, aes(offset, ratio, colour = side, fill = side)) +

  # momentum window highlight: GOAL -> +15'
  annotate("rect", xmin = 0, xmax = 3, ymin = -Inf, ymax = Inf,
           fill = COL_EQ, alpha = 0.07) +

  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50",
             linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dotted", colour = COL_EQ,
             linewidth = 0.6) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1.7, lineend = "round") +
  geom_point(size = 3.5) +

  annotation_custom(ball_blue, xmin = -0.22, xmax = 0.22,
                    ymin = 1.72, ymax = 1.80) +
  annotate("text", x = 0, y = 1.70, label = "EQUALISER",
           colour = COL_EQ, fontface = "bold", size = 4.5, vjust = 1) +

  annotate("text", x = -3.15, y = 1.03, hjust = 0, vjust = 0,
           label = "Baseline (expected performance)",
           colour = "grey40", fontface = "italic", size = 3) +

  annotate("text", x = 2.1, y = 0.78, hjust = 0, lineheight = 0.95,
           label = "Opponent dips for ~5 minutes,\nthen returns to normal",
           colour = COL_OPP, fontface = "bold", size = 3.4) +
  annotate("curve", x = 2.05, y = 0.795, xend = 1.12, yend = 0.845,
           curvature = -0.3, colour = COL_OPP, linewidth = 0.7,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +

  annotate("text", x = 4.4, y = 1.42, hjust = 0, lineheight = 0.95,
           label = "Equalising team never rises\nmeaningfully above baseline",
           colour = COL_EQ, fontface = "bold", size = 3.4) +
  annotate("curve", x = 4.35, y = 1.38, xend = 3.15, yend = 1.13,
           curvature = 0.3, colour = COL_EQ, linewidth = 0.7,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +

  scale_x_continuous(breaks = c(-3:-1, 0, 1:6),
                     labels = c("-15'", "-10'", "-5'", "GOAL", "+5'", "+10'",
                                "+15'", "+20'", "+25'", "+30'")) +
  scale_y_continuous(labels = function(v) sprintf("%.1fx", v)) +
  scale_colour_manual(values = pal_side, name = NULL) +
  scale_fill_manual(values = pal_side, name = NULL) +
  coord_cartesian(xlim = c(-3.2, 7.1), ylim = c(0.72, 1.8)) +
  labs(
    title    = "Opponent Holds the Momentum",
    subtitle = sprintf(
      "Attacking output of both teams, 15 minutes before to 30 minutes after a comeback equaliser · shaded strip = first 15 minutes after the goal\n%s equalisers (<= 70') in %s Bundesliga matches, 2016/17-2025/26 · shaded ribbons = 95%% uncertainty band",
      format(nrow(eqs), big.mark = ","),
      format(n_distinct(eqs$match_id), big.mark = ",")),
    x = "Minutes relative to comeback equaliser",
    y = "Attacking output ratio (observed / expected xG)",
    caption = "Data: Understat, ClubElo · Analysis: R (ggplot2, dplyr, mgcv)"
  ) +
  theme_sport()
ggsave(file.path(REPORT_DIR, "01_momentum_decay.png"), f01,
       width = 11, height = 6.2, dpi = 300, bg = COL_BG)

# 02: scoreboard - before vs after, both teams
board <- panel %>%
  filter(offset %in% c(-3:-1, 1:3)) %>%
  mutate(phase_ba = factor(if_else(offset < 0, "Before", "After"),
                           levels = c("Before", "After")),
         side = factor(side, levels = c("Opponent", "Equalising team"))) %>%
  group_by(side, phase_ba) %>%
  summarise(ratio = sum(xg) / sum(pred), .groups = "drop")

f02 <- ggplot(board, aes(phase_ba, ratio, fill = side)) +
  geom_col(width = 0.55) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40",
             linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2fx", ratio)), vjust = -0.6,
            colour = COL_NEUTRAL, fontface = "bold", size = 7) +
  annotate("text", x = 0.62, y = 1.035, hjust = 1, size = 2.8,
           colour = "grey45", fontface = "italic", label = "Baseline") +
  facet_wrap(~side) +
  scale_fill_manual(values = pal_side, guide = "none") +
  coord_cartesian(ylim = c(0, 1.55)) +
  scale_y_continuous(labels = function(v) sprintf("%.1fx", v)) +
  labs(
    title    = "Opponent Collapses, Scorer Holds",
    subtitle = sprintf("Mean attacking output 15 minutes before vs 15 minutes after the comeback equaliser\n%s equalisers (<= 70'), Bundesliga 2016/17-2025/26",
                       format(nrow(eqs), big.mark = ",")),
    x = NULL,
    y = "Attacking output ratio (observed / expected xG)",
    caption = "Data: Understat, ClubElo · Analysis: R (ggplot2, dplyr, mgcv)"
  ) +
  theme_sport() +
  theme(panel.grid.major.x = element_blank(),
        panel.spacing.x = unit(2, "lines"),
        axis.text.x = element_text(face = "bold", size = 14),
        strip.background = element_rect(fill = "#FFFFFF", colour = NA),
        strip.text = element_text(face = "bold", size = 15, colour = COL_NEUTRAL))
ggsave(file.path(REPORT_DIR, "02_scoreboard.png"), f02,
       width = 10, height = 5.8, dpi = 300, bg = COL_BG)

# 03: event-study curves split by equaliser timing
curve_phase <- panel %>%
  group_by(phase, side, offset) %>%
  summarise(ratio = sum(xg) / sum(pred), .groups = "drop")

n_phase <- eqs %>% count(phase) %>%
  mutate(label = sprintf("n = %s equalisers", format(n, big.mark = ",")))

f03 <- ggplot(curve_phase, aes(offset, ratio, colour = side)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50",
             linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dotted", colour = COL_NEUTRAL,
             linewidth = 0.5) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 2.6) +
  annotation_custom(ball_blue, xmin = -0.35, xmax = 0.35,
                    ymin = 2.24, ymax = 2.40) +
  annotate("text", x = 0, y = 2.19, label = "Equaliser",
           colour = COL_NEUTRAL, fontface = "bold", size = 3.6, hjust = 0.5) +
  geom_text(data = n_phase, aes(x = 6, y = 2.33, label = label),
            inherit.aes = FALSE, hjust = 1, size = 3.3, colour = "grey40",
            fontface = "italic") +
  facet_wrap(~phase, nrow = 1) +
  scale_x_continuous(breaks = c(-3, -1, 1, 3, 5),
                     labels = c("-15'", "-5'", "+5'", "+15'", "+25'")) +
  scale_y_continuous(labels = function(v) sprintf("%.1fx", v)) +
  scale_colour_manual(values = pal_side, name = NULL) +
  coord_cartesian(ylim = c(0.55, 2.42)) +
  labs(
    title    = "Early Shock vs. Late Stability",
    subtitle = "Attacking output of both teams split by equaliser timing · Opponent's pre-goal dominance and post-goal dip fade with match time\n· Equalising team's curve stays flat in all three panels · Bundesliga 2016/17-2025/26",
    x = "Minutes relative to comeback equaliser",
    y = "Attacking output ratio (observed / expected xG)",
    caption = "Data: Understat, ClubElo · Analysis: R (ggplot2, dplyr, mgcv)"
  ) +
  theme_sport() +
  theme(panel.spacing.x = unit(1.2, "lines"))
ggsave(file.path(REPORT_DIR, "03_timing_facet.png"), f03,
       width = 13, height = 6, dpi = 300, bg = COL_BG)

# 04: placebo null distribution
controls    <- train %>% filter(state == "level")
bin_counts  <- treat %>% count(bin)
ctrl_by_bin <- split(controls, controls$bin)

placebo <- replicate(1000, {
  s <- lapply(seq_len(nrow(bin_counts)), function(i) {
    pool <- ctrl_by_bin[[as.character(bin_counts$bin[i])]]
    pool[sample.int(nrow(pool), bin_counts$n[i], replace = TRUE), c("xg", "pred")]
  })
  s <- bind_rows(s)
  mean(s$xg - s$pred)
})

gap_treat  <- mean(treat$xg - treat$pred)
gap_mirror <- mean(mirror$xg - mirror$pred)
p_treat  <- (1 + sum(abs(placebo - mean(placebo)) >= abs(gap_treat  - mean(placebo)))) / 1001
p_mirror <- (1 + sum(abs(placebo - mean(placebo)) >= abs(gap_mirror - mean(placebo)))) / 1001

f04 <- ggplot(tibble(gap = placebo), aes(gap)) +
  geom_histogram(bins = 45, fill = COL_GRID, colour = COL_BG) +
  geom_vline(xintercept = gap_treat, colour = COL_EQ, linewidth = 1.1) +
  geom_vline(xintercept = gap_mirror, colour = COL_OPP, linewidth = 1.1) +
  annotate("text", x = gap_treat + 0.0004, y = Inf, vjust = 1.6, hjust = 0,
           colour = COL_EQ, size = 3.3, fontface = "bold",
           label = sprintf("Equalising team\n%+.4f (p = %.2f)", gap_treat, p_treat)) +
  annotate("text", x = gap_mirror - 0.0004, y = Inf, vjust = 1.6, hjust = 1,
           colour = COL_OPP, size = 3.3, fontface = "bold",
           label = sprintf("Opponent\n%+.4f (p = %.2f)", gap_mirror, p_mirror)) +
  labs(
    title    = "Post-Goal Changes Are Pure Chance",
    subtitle = "Grey: 1,000 placebo samples of ordinary level-score windows (no equaliser involved), matched on match time\n· a line inside the grey range = indistinguishable from normal fluctuation",
    x = "Average gap between observed and expected chances (xG per 5 minutes)",
    y = "Number of placebo samples",
    caption = "Data: Understat, ClubElo · Analysis: R (mgcv, dplyr)"
  ) +
  theme_sport()
ggsave(file.path(REPORT_DIR, "04_placebo.png"), f04,
       width = 10, height = 5.5, dpi = 300, bg = COL_BG)

# 05: H1 robustness forest
fit_shots <- bam(n_shots ~ goal_diff_c + s(bin_start, k = 8) + s(elo_diff, k = 6) +
                   is_home + season_f,
                 family = poisson(), data = train, discrete = TRUE)
treat$pred_shots <- as.numeric(predict(fit_shots, newdata = treat, type = "response"))

no_stop <- function(df) df %>% filter(!bin %in% c(9L, 17L))
fit_ns  <- bam(xg ~ goal_diff_c + s(bin_start, k = 8) + s(elo_diff, k = 6) +
                 is_home + season_f,
               family = tw(), data = no_stop(train), discrete = TRUE)
treat_ns <- no_stop(treat)
treat_ns$pred_ns <- as.numeric(predict(fit_ns, newdata = treat_ns, type = "response"))

ge <- read_csv(file.path(DATA_PROCESSED_DIR, "goal_events.csv"),
               show_col_types = FALSE)
nxt <- ge %>% filter(qualifying_eq) %>%
  transmute(match_id, team = scoring_team, own_post_eq_minute = minute, next_minute)
treat_tr <- treat %>%
  left_join(nxt, by = c("match_id", "team", "own_post_eq_minute")) %>%
  filter(is.na(next_minute) | bin_start < next_minute)

specs <- bind_rows(
  boot_ratio(treat,    "xg",      "pred",       "eq_key") %>% mutate(spec = "Main: xG (Tweedie)"),
  boot_ratio(treat,    "n_shots", "pred_shots", "eq_key") %>% mutate(spec = "Shot counts (Poisson)"),
  boot_ratio(treat_ns, "xg",      "pred_ns",    "eq_key") %>% mutate(spec = "Excl. stoppage bins"),
  boot_ratio(treat_tr, "xg",      "pred",       "eq_key") %>% mutate(spec = "Truncated at next goal"),
  boot_ratio(mirror,   "xg",      "pred",       "eq_key") %>% mutate(spec = "Opponent (mirror)")
) %>%
  mutate(side = if_else(spec == "Opponent (mirror)", "Opponent", "Equalising team"),
         spec = factor(spec, levels = rev(unique(spec))))

f05 <- ggplot(specs, aes(est, spec, colour = side)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50",
             linewidth = 0.5) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.18, linewidth = 0.8) +
  geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.3f", est)), vjust = -1, size = 3,
            colour = COL_NEUTRAL) +
  scale_colour_manual(values = pal_side, guide = "none") +
  labs(
    title    = "No-Surge Trend Holds Across Models",
    subtitle = "Post-equaliser output ratio under different analysis choices · whiskers = 95% uncertainty \n· every Equalising-team whisker crosses 1 (no effect)",
    x = "Attacking output ratio (observed / expected)",
    y = NULL,
    caption = "Data: Understat, ClubElo · Analysis: R (mgcv, dplyr)"
  ) +
  theme_sport()
ggsave(file.path(REPORT_DIR, "05_robustness.png"), f05,
       width = 10, height = 5.2, dpi = 300, bg = COL_BG)

# H2 logistic models (identical spec to 03_train_logistic_model.R)
eq_all <- read_csv(file.path(DATA_PROCESSED_DIR, "equaliser_events.csv"),
                   show_col_types = FALSE) %>%
  mutate(home_sign = if_else(is_home, 1, -1),
         elo_100 = elo_diff / 100,
         minute_c = (eq_minute - 60) / 15,
         score_f = factor(if_else(scoreline == "1-1", "1-1",
                          if_else(scoreline == "2-2", "2-2", "3-3+"))))
decided <- eq_all %>% filter(next_goal != "none") %>%
  mutate(y = as.integer(next_goal == "team"))

fit_p <- glm(y ~ elo_100 + home_sign, family = binomial(), data = decided)
fit_e <- glm(y ~ elo_100 + home_sign + minute_c + score_f,
             family = binomial(), data = decided)
cf <- coef(fit_p)
hs <- mean(decided$home_sign == 1)
decided$p_hat <- predict(fit_p, type = "response")

auc <- {
  r <- rank(decided$p_hat)
  n1 <- sum(decided$y == 1); n0 <- sum(decided$y == 0)
  (sum(r[decided$y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
acc <- mean((decided$p_hat > 0.5) == (decided$y == 1))

# 06: logistic fit over the raw outcomes (S-curve)
grid6 <- crossing(elo_100 = seq(-4.2, 4.2, 0.05), home_sign = c(1, -1)) %>%
  mutate(p = plogis(cf[1] + cf[2] * elo_100 + cf[3] * home_sign)) %>%
  group_by(elo_100) %>%
  summarise(p = hs * p[home_sign == 1] + (1 - hs) * p[home_sign == -1],
            .groups = "drop")

f06 <- ggplot() +
  geom_jitter(data = decided,
              aes(elo_100 * 100, y, colour = factor(y)),
              height = 0.035, width = 0, size = 1.8, alpha = 0.25) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey45",
             linewidth = 0.5) +
  geom_line(data = grid6, aes(elo_100 * 100, p), colour = COL_EQ,
            linewidth = 1.6) +
  scale_colour_manual(values = c(`1` = COL_EQ, `0` = COL_OPP),
                      labels = c(`1` = "Equalising team scored next (top row)",
                                 `0` = "Opponent scored next (bottom row)"),
                      name = NULL) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "50%", "1")) +
  labs(
    title    = "Higher ELO Boosts Next-Goal Odds",
    subtitle = "Each dot = one equaliser with a next goal (968) · the curve is the model's fitted chance the Equalising team scores next\n· dots vertically jittered for visibility",
    x = "Equalising team's ELO advantage (rating points)",
    y = "Outcome / fitted chance of scoring next",
    caption = "Data: Understat, ClubElo · Analysis: R (glm, binomial)"
  ) +
  theme_sport()
ggsave(file.path(REPORT_DIR, "06_logistic_fit.png"), f06,
       width = 10, height = 5.5, dpi = 300, bg = COL_BG)

# 07: logistic forest plot
or_rows <- function(f, d, model_lab) {
  ct <- coeftest(f, vcov = vcovCL(f, cluster = d$match_id))
  tibble(term = rownames(ct), or = exp(ct[, 1]),
         lo = exp(ct[, 1] - 1.96 * ct[, 2]), hi = exp(ct[, 1] + 1.96 * ct[, 2]),
         model = model_lab)
}
ors <- bind_rows(
  or_rows(fit_p, decided, "primary"),
  or_rows(fit_e, decided, "extended") %>%
    filter(!term %in% c("(Intercept)", "elo_100", "home_sign"))
) %>%
  mutate(label = recode(term,
    "(Intercept)" = "Momentum test (intercept)",
    "elo_100"     = "ELO advantage (per 100 pts)",
    "home_sign"   = "Home effect",
    "minute_c"    = "Equaliser minute (per 15')",
    "score_f2-2"  = "Scoreline 2-2 (vs 1-1)",
    "score_f3-3+" = "Scoreline 3-3+ (vs 1-1)")) %>%
  mutate(label = factor(label, levels = rev(unique(label))))

f07 <- ggplot(ors, aes(or, label, colour = model)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50",
             linewidth = 0.5) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.18, linewidth = 0.8) +
  geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.2f", or)), vjust = -1, size = 3,
            colour = COL_NEUTRAL) +
  scale_x_log10() +
  scale_colour_manual(values = c(primary = COL_EQ, extended = COL_GREY),
                      name = NULL) +
  labs(
    title    = "Strength and Home Effect Drive the Odds",
    subtitle = "How each factor multiplies the odds that the Equalising team scores next · whiskers = 95% uncertainty \n· dashed line at 1 = no effect · 968 equalisers with a next goal, match-clustered standard errors, log scale",
    x = "Odds ratio - next goal goes to the Equalising team",
    y = NULL,
    caption = "Data: Understat, ClubElo · Analysis: R (glm, binomial)"
  ) +
  theme_sport()
ggsave(file.path(REPORT_DIR, "07_logistic_forest.png"), f07,
       width = 10, height = 5.2, dpi = 300, bg = COL_BG)

# 08: logistic prediction curves by home effect
grid8 <- crossing(elo_100 = seq(-4, 4, 0.05), home_sign = c(1, -1)) %>%
  mutate(p = plogis(cf[1] + cf[2] * elo_100 + cf[3] * home_sign),
         venue = if_else(home_sign == 1, "Equaliser at home", "Equaliser away"))

pts8 <- decided %>%
  mutate(venue = if_else(home_sign == 1, "Equaliser at home", "Equaliser away"),
         bin = cut(elo_100, breaks = c(-Inf, -1.5, -0.75, -0.25, 0.25, 0.75, 1.5, Inf))) %>%
  group_by(venue, bin) %>%
  summarise(elo_100 = mean(elo_100), p = mean(y), n = n(), .groups = "drop")

cross <- tibble(venue = c("Equaliser at home", "Equaliser away"),
                x0 = c(-(cf[1] + cf[3]) / cf[2], -(cf[1] - cf[3]) / cf[2]) * 100)

pal8 <- c("Equaliser at home" = COL_EQ, "Equaliser away" = COL_NEUTRAL)
f08 <- ggplot(grid8, aes(elo_100 * 100, p, colour = venue)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey45",
             linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dotted", colour = COL_GRID) +
  geom_line(linewidth = 1.6) +
  geom_point(data = pts8, aes(size = n), alpha = 0.5) +
  geom_text(data = pts8, aes(label = n), size = 2.7, colour = "grey25",
            nudge_y = 0.045, show.legend = FALSE) +
  scale_size(range = c(1.5, 10), guide = "none") +
  geom_vline(data = cross, aes(xintercept = x0, colour = venue),
             linetype = "dotted", linewidth = 0.5, show.legend = FALSE) +
  geom_text(data = cross,
            aes(x = x0, y = 0.19, label = sprintf("50%% at %+.0f ELO", x0)),
            size = 2.9, hjust = -0.05, show.legend = FALSE) +
  scale_colour_manual(values = pal8, name = NULL) +
  scale_y_continuous(labels = function(v) sprintf("%.0f%%", v * 100)) +
  labs(
    title    = "Home Advantage Shifts Next-Goal Odds",
    subtitle = "The model's predicted chance the Equalising team scores next · dots = what actually happened in ELO groups,\nsized and labelled by number of equalisers in the group · 968 equalisers with a next goal",
    x = "Equalising team's ELO advantage (rating points)",
    y = "Chance the Equalising team scores next",
    caption = "Data: Understat, ClubElo · Analysis: R (glm, binomial)"
  ) +
  coord_cartesian(ylim = c(0.15, 0.85)) +
  theme_sport()
ggsave(file.path(REPORT_DIR, "08_logistic_prediction.png"), f08,
       width = 10, height = 5.8, dpi = 300, bg = COL_BG)

# 09: predicted probability by actual outcome (separation)
f09 <- ggplot(decided, aes(p_hat, fill = factor(y))) +
  geom_histogram(data = decided %>% filter(y == 0), bins = 35, alpha = 0.65,
                 colour = COL_BG) +
  geom_histogram(data = decided %>% filter(y == 1), bins = 35, alpha = 0.65,
                 colour = COL_BG) +
  geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey45",
             linewidth = 0.6) +
  annotate("text", x = 0.505, y = Inf, vjust = 1.4, hjust = 0, size = 3,
           colour = "grey40", label = "0.5 cutoff") +
  scale_fill_manual(values = c(`1` = COL_EQ, `0` = COL_OPP),
                    labels = c(`1` = "Equalising team scored next",
                               `0` = "Opponent scored next"),
                    name = "Actual outcome") +
  labs(
    title    = "Next Goal Is Mostly Unpredictable",
    subtitle = sprintf("The model's predicted chance for every equaliser, coloured by what actually happened · heavy overlap = the next goal is \nbarely predictable · accuracy %.2f, AUC %.2f (ELO + home effect model)", acc, auc),
    x = "Model-predicted chance the Equalising team scores next",
    y = "Number of equalisers",
    caption = "Data: Understat, ClubElo · Analysis: R (glm, binomial)"
  ) +
  theme_sport()
ggsave(file.path(REPORT_DIR, "09_predicted_probability.png"), f09,
       width = 10, height = 5.5, dpi = 300, bg = COL_BG)

# 10: feature importance (LR chi-square per term)
dd <- decided %>%
  mutate(s22 = as.integer(score_f == "2-2"),
         s33 = as.integer(score_f == "3-3+"))
full <- glm(y ~ elo_100 + home_sign + minute_c + s22 + s33,
            family = binomial(), data = dd)
dev_full <- deviance(full)
drop_dev <- function(f) deviance(glm(f, family = binomial(), data = dd)) - dev_full

importance <- tibble(
  term = c("Equalising itself\n(momentum, intercept)",
           "ELO advantage",
           "Home effect",
           "Equaliser minute",
           "Scoreline (2 df)"),
  chisq = c(drop_dev(y ~ 0 + elo_100 + home_sign + minute_c + s22 + s33),
            drop_dev(y ~ home_sign + minute_c + s22 + s33),
            drop_dev(y ~ elo_100 + minute_c + s22 + s33),
            drop_dev(y ~ elo_100 + home_sign + s22 + s33),
            drop_dev(y ~ elo_100 + home_sign + minute_c)),
  df = c(1, 1, 1, 1, 2)
) %>%
  mutate(p = pchisq(chisq, df, lower.tail = FALSE),
         major = term %in% c("ELO advantage", "Home effect"))

f10 <- ggplot(importance, aes(chisq, reorder(term, chisq), fill = major)) +
  geom_col(width = 0.55) +
  geom_vline(xintercept = qchisq(0.95, 1), linetype = "dashed",
             colour = "grey45", linewidth = 0.5) +
  annotate("text", x = qchisq(0.95, 1) + 0.8, y = 0.62, hjust = 0, size = 2.9,
           colour = "grey40", fontface = "italic",
           label = "5% significance (1 df)") +
  geom_text(aes(label = sprintf("%.1f  (p %s)", chisq,
                                if_else(p < 0.001, "< 0.001",
                                        sprintf("= %.2f", p)))),
            hjust = -0.08, size = 3.1, colour = COL_NEUTRAL) +
  scale_fill_manual(values = c(`TRUE` = COL_EQ, `FALSE` = "#A3B1D4"),
                    guide = "none") +
  coord_cartesian(xlim = c(0, max(importance$chisq) * 1.35)) +
  labs(
    title    = "ELO Dominates, Momentum Fails",
    subtitle = "How much the model worsens when each factor is removed · big bar = the factor matters \n· deep blue = the dominant factors · 'Equalising itself' ranked last: the momentum term contributes nothing",
    x = "Importance - likelihood-ratio chi-square (bigger = matters more)",
    y = NULL,
    caption = "Data: Understat, ClubElo · Analysis: R (glm, binomial)"
  ) +
  theme_sport()
ggsave(file.path(REPORT_DIR, "10_feature_importance.png"), f10,
       width = 10, height = 5.5, dpi = 300, bg = COL_BG)

cat("All 10 report figures written to", REPORT_DIR, "\n")
cat(sprintf("placebo p (Equalising team) = %.3f | placebo p (Opponent) = %.3f\n",
            p_treat, p_mirror))
