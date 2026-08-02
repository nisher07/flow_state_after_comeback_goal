# 04_report_figures_de.R
# ==============================================================================
# German-language version of 04_report_figures.R - identical data, models and
# layout, only the plot text is translated. Output: output/figures/report_de/
#
# Run from the project root:  Rscript R/04_report_figures_de.R

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

REPORT_DIR <- file.path(OUTPUT_FIGURES_DIR, "report_de")
dir.create(REPORT_DIR, showWarnings = FALSE, recursive = TRUE)

ball_blue <- rasterGrob(readPNG("assets/football_icons/blue.png"),
                        interpolate = TRUE)

pal_side <- c("Ausgleichsteam" = COL_EQ, "Gegner" = COL_OPP)

# Font size in points is constant regardless of a plot's physical width, so a
# wide figure's subtitle reads smaller than a narrow one once every PNG is
# displayed at a shared width. Scaling size to width keeps them all reading
# as the same size, at a baseline a little smaller than theme_sport()'s own.
REPORT_SUBTITLE_PT <- 10
subtitle_size <- function(width_in) REPORT_SUBTITLE_PT * width_in / 10

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
         phase = factor(case_when(eq_minute < 30 ~ "Früher Ausgleich (<30')",
                                  eq_minute <= 50 ~ "Mittlerer Ausgleich (30'-50')",
                                  TRUE            ~ "Später Ausgleich (51'-70')"),
                        levels = c("Früher Ausgleich (<30')",
                                   "Mittlerer Ausgleich (30'-50')",
                                   "Später Ausgleich (51'-70')")))

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
  mutate(side = if_else(side == "eq_team", "Ausgleichsteam", "Gegner"))

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
  annotate("text", x = 0, y = 1.70, label = "AUSGLEICH",
           colour = COL_EQ, fontface = "bold", size = 4.5, vjust = 1) +

  annotate("text", x = -3.15, y = 1.03, hjust = 0, vjust = 0,
           label = "Basislinie (erwartete Leistung)",
           colour = "grey40", fontface = "italic", size = 3) +

  annotate("text", x = 2.1, y = 0.78, hjust = 0, lineheight = 0.95,
           label = "Der Gegner fällt für ~5 Minuten ab,\ndann normalisiert er sich wieder",
           colour = COL_OPP, fontface = "bold", size = 3.4) +
  annotate("curve", x = 2.05, y = 0.795, xend = 1.12, yend = 0.845,
           curvature = -0.3, colour = COL_OPP, linewidth = 0.7,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +

  annotate("text", x = 4.4, y = 1.42, hjust = 0, lineheight = 0.95,
           label = "Das Ausgleichsteam steigt nie\nnennenswert über die Basislinie",
           colour = COL_EQ, fontface = "bold", size = 3.4) +
  annotate("curve", x = 4.35, y = 1.38, xend = 3.15, yend = 1.13,
           curvature = 0.3, colour = COL_EQ, linewidth = 0.7,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +

  scale_x_continuous(breaks = c(-3:-1, 0, 1:6),
                     labels = c("-15'", "-10'", "-5'", "TOR", "+5'", "+10'",
                                "+15'", "+20'", "+25'", "+30'")) +
  scale_y_continuous(labels = function(v) sprintf("%.1fx", v)) +
  scale_colour_manual(values = pal_side, name = NULL) +
  scale_fill_manual(values = pal_side, name = NULL) +
  coord_cartesian(xlim = c(-3.2, 7.1), ylim = c(0.72, 1.8)) +
  labs(
    title    = "Der Gegner hat das Momentum",
    subtitle = sprintf(
      "Angriffsleistung beider Mannschaften, 15 Minuten vor bis 30 Minuten nach einem Ausgleichstor · schattierter Bereich = erste 15 Minuten nach dem Tor\n%s Ausgleichstore (<= 70') in %s Bundesliga-Spielen, 2016/17-2025/26 · schattierte Bänder = 95%%-Unsicherheitsbereich",
      format(nrow(eqs), big.mark = ","),
      format(n_distinct(eqs$match_id), big.mark = ",")),
    x = "Minuten relativ zum Ausgleichstor",
    y = "Angriffsleistung (beobachtet / erwartetes xG)",
    caption = "Daten: Understat, ClubElo · Analyse: R (ggplot2, dplyr, mgcv)"
  ) +
  theme_sport() +
  theme(plot.subtitle = element_text(size = subtitle_size(11)))
ggsave(file.path(REPORT_DIR, "01_momentum_decay.png"), f01,
       width = 11, height = 6.2, dpi = 300, bg = COL_BG)

# 02: scoreboard - before vs after, both teams
board <- panel %>%
  filter(offset %in% c(-3:-1, 1:3)) %>%
  mutate(phase_ba = factor(if_else(offset < 0, "Vorher", "Nachher"),
                           levels = c("Vorher", "Nachher")),
         side = factor(side, levels = c("Gegner", "Ausgleichsteam"))) %>%
  group_by(side, phase_ba) %>%
  summarise(ratio = sum(xg) / sum(pred), .groups = "drop")

f02 <- ggplot(board, aes(phase_ba, ratio, fill = side)) +
  geom_col(width = 0.55) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40",
             linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2fx", ratio)), vjust = -0.6,
            colour = COL_NEUTRAL, fontface = "bold", size = 7) +
  annotate("text", x = 0.62, y = 1.035, hjust = 1, size = 2.8,
           colour = "grey45", fontface = "italic", label = "Basis") +
  facet_wrap(~side) +
  scale_fill_manual(values = pal_side, guide = "none") +
  coord_cartesian(ylim = c(0, 1.55)) +
  scale_y_continuous(labels = function(v) sprintf("%.1fx", v)) +
  labs(
    title    = "Der Gegner bricht ein, der Torschütze bleibt stabil",
    subtitle = sprintf("Mittlere Angriffsleistung 15 Minuten vor und 15 Minuten nach dem Ausgleichstor\n%s Ausgleichstore (<= 70'), Bundesliga 2016/17-2025/26",
                       format(nrow(eqs), big.mark = ",")),
    x = NULL,
    y = "Angriffsleistung (beobachtet / erwartetes xG)",
    caption = "Daten: Understat, ClubElo · Analyse: R (ggplot2, dplyr, mgcv)"
  ) +
  theme_sport() +
  theme(panel.grid.major.x = element_blank(),
        panel.spacing.x = unit(2, "lines"),
        axis.text.x = element_text(face = "bold", size = 14),
        strip.background = element_rect(fill = "#FFFFFF", colour = NA),
        strip.text = element_text(face = "bold", size = 15, colour = COL_NEUTRAL),
        plot.subtitle = element_text(size = subtitle_size(10)))
ggsave(file.path(REPORT_DIR, "02_scoreboard.png"), f02,
       width = 10, height = 5.8, dpi = 300, bg = COL_BG)

# 03: event-study curves split by equaliser timing
curve_phase <- panel %>%
  group_by(phase, side, offset) %>%
  summarise(ratio = sum(xg) / sum(pred), .groups = "drop")

n_phase <- eqs %>% count(phase) %>%
  mutate(label = sprintf("n = %s Ausgleichstore", format(n, big.mark = ",")))

f03 <- ggplot(curve_phase, aes(offset, ratio, colour = side)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50",
             linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dotted", colour = COL_NEUTRAL,
             linewidth = 0.5) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 2.6) +
  annotation_custom(ball_blue, xmin = -0.35, xmax = 0.35,
                    ymin = 2.24, ymax = 2.40) +
  annotate("text", x = 0, y = 2.19, label = "Ausgleich",
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
    title    = "Früher Schock, späte Stabilität",
    subtitle = "Angriffsleistung beider Mannschaften, aufgeschlüsselt nach dem Zeitpunkt des Ausgleichs\n· Die Dominanz des Gegners vor dem Tor und der Einbruch danach lassen mit der Spielzeit nach\n· Die Kurve des Ausgleichsteams bleibt in allen drei Feldern flach · Bundesliga 2016/17-2025/26",
    x = "Minuten relativ zum Ausgleichstor",
    y = "Angriffsleistung (beobachtet / erwartetes xG)",
    caption = "Daten: Understat, ClubElo · Analyse: R (ggplot2, dplyr, mgcv)"
  ) +
  theme_sport() +
  theme(panel.spacing.x = unit(1.2, "lines"),
        plot.subtitle = element_text(size = subtitle_size(13)))
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
           label = sprintf("Ausgleichsteam\n%+.4f (p = %.2f)", gap_treat, p_treat)) +
  annotate("text", x = gap_mirror - 0.0004, y = Inf, vjust = 1.6, hjust = 1,
           colour = COL_OPP, size = 3.3, fontface = "bold",
           label = sprintf("Gegner\n%+.4f (p = %.2f)", gap_mirror, p_mirror)) +
  labs(
    title    = "Veränderungen nach dem Tor sind reiner Zufall",
    subtitle = "Grau: 1.000 Placebo-Stichproben aus gewöhnlichen Phasen bei Gleichstand (ohne Ausgleichstor), abgeglichen nach Spielzeit\n· eine Linie innerhalb des grauen Bereichs = nicht von normaler Schwankung zu unterscheiden",
    x = "Durchschnittliche Differenz zwischen beobachteten und erwarteten Chancen (xG pro 5 Minuten)",
    y = "Anzahl der Placebo-Stichproben",
    caption = "Daten: Understat, ClubElo · Analyse: R (mgcv, dplyr)"
  ) +
  theme_sport() +
  theme(plot.subtitle = element_text(size = subtitle_size(10)))
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
  boot_ratio(treat,    "xg",      "pred",       "eq_key") %>% mutate(spec = "Haupt: xG (Tweedie)"),
  boot_ratio(treat,    "n_shots", "pred_shots", "eq_key") %>% mutate(spec = "Schussanzahl (Poisson)"),
  boot_ratio(treat_ns, "xg",      "pred_ns",    "eq_key") %>% mutate(spec = "Ohne Nachspielzeit-Bins"),
  boot_ratio(treat_tr, "xg",      "pred",       "eq_key") %>% mutate(spec = "Gekappt beim nächsten Tor"),
  boot_ratio(mirror,   "xg",      "pred",       "eq_key") %>% mutate(spec = "Gegner (Spiegel)")
) %>%
  mutate(side = if_else(spec == "Gegner (Spiegel)", "Gegner", "Ausgleichsteam"),
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
    title    = "Kein Leistungsschub – in jedem Modell bestätigt",
    subtitle = "Angriffsleistung nach dem Ausgleich unter verschiedenen Analyseansätzen · Fehlerbalken = 95%-Unsicherheitsbereich \n· jeder Fehlerbalken des Ausgleichsteams überschneidet sich mit 1 (kein Effekt)",
    x = "Angriffsleistung (beobachtet / erwartet)",
    y = NULL,
    caption = "Daten: Understat, ClubElo · Analyse: R (mgcv, dplyr)"
  ) +
  theme_sport() +
  theme(plot.subtitle = element_text(size = subtitle_size(10)))
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
  geom_point(data = decided,
             aes(elo_100 * 100, y, colour = factor(y)),
             size = 1.8, alpha = 0.12) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey45",
             linewidth = 0.5) +
  geom_line(data = grid6, aes(elo_100 * 100, p), colour = COL_EQ,
            linewidth = 1.6) +
  scale_colour_manual(values = c(`1` = COL_EQ, `0` = COL_OPP),
                      labels = c(`1` = "Ausgleichsteam traf als Nächstes (y = 1)",
                                 `0` = "Gegner traf als Nächstes (y = 0)"),
                      name = NULL) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "50%", "1")) +
  labs(
    title    = "Höhere ELO-Wertung erhöht die Chance auf das nächste Tor",
    subtitle = "Jeder Punkt = ein Ausgleichstor mit einem nächsten Tor (968), dargestellt beim tatsächlichen Ergebnis (0 oder 1)\n· die Kurve zeigt die vom Modell geschätzte Chance, dass das Ausgleichsteam als Nächstes trifft\n· dunklere Bereiche = viele überlappende Ausgleichstore bei ähnlichem ELO-Abstand",
    x = "ELO-Vorteil des Ausgleichsteams (Wertungspunkte)",
    y = "Ergebnis / geschätzte Trefferchance beim nächsten Tor",
    caption = "Daten: Understat, ClubElo · Analyse: R (glm, binomial)"
  ) +
  theme_sport() +
  theme(plot.subtitle = element_text(size = subtitle_size(10)))
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
  or_rows(fit_p, decided, "Primärmodell"),
  or_rows(fit_e, decided, "Erweitertes Modell") %>%
    filter(!term %in% c("(Intercept)", "elo_100", "home_sign"))
) %>%
  mutate(label = recode(term,
    "(Intercept)" = "Momentum-Test (Achsenabschnitt)",
    "elo_100"     = "ELO-Vorteil (pro 100 Punkte)",
    "home_sign"   = "Heimvorteil",
    "minute_c"    = "Ausgleichsminute (pro 15')",
    "score_f2-2"  = "Spielstand 2:2 (vs. 1:1)",
    "score_f3-3+" = "Spielstand 3:3+ (vs. 1:1)")) %>%
  mutate(label = factor(label, levels = rev(unique(label))))

f07 <- ggplot(ors, aes(or, label, colour = model)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50",
             linewidth = 0.5) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.18, linewidth = 0.8) +
  geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.2f", or)), vjust = -1, size = 3,
            colour = COL_NEUTRAL) +
  scale_x_log10() +
  scale_colour_manual(values = c("Primärmodell" = COL_EQ, "Erweitertes Modell" = COL_GREY),
                      name = NULL) +
  labs(
    title    = "Stärke und Heimvorteil bestimmen die Quote",
    subtitle = "Wie stark jeder Faktor die Quote erhöht, dass das Ausgleichsteam als Nächstes trifft\n· Fehlerbalken = 95%-Unsicherheitsbereich · gestrichelte Linie bei 1 = kein Effekt\n· 968 Ausgleichstore mit einem nächsten Tor, nach Spiel geclusterte Standardfehler,\nlogarithmische Skala",
    x = "Odds Ratio – nächstes Tor geht an das Ausgleichsteam",
    y = NULL,
    caption = "Daten: Understat, ClubElo · Analyse: R (glm, binomial)"
  ) +
  theme_sport() +
  theme(plot.subtitle = element_text(size = subtitle_size(10)))
ggsave(file.path(REPORT_DIR, "07_logistic_forest.png"), f07,
       width = 10, height = 5.2, dpi = 300, bg = COL_BG)

# 08: logistic prediction curves by home effect
grid8 <- crossing(elo_100 = seq(-4, 4, 0.05), home_sign = c(1, -1)) %>%
  mutate(p = plogis(cf[1] + cf[2] * elo_100 + cf[3] * home_sign),
         venue = if_else(home_sign == 1, "Ausgleich zu Hause", "Ausgleich auswärts"))

pts8 <- decided %>%
  mutate(venue = if_else(home_sign == 1, "Ausgleich zu Hause", "Ausgleich auswärts"),
         bin = cut(elo_100, breaks = c(-Inf, -1.5, -0.75, -0.25, 0.25, 0.75, 1.5, Inf))) %>%
  group_by(venue, bin) %>%
  summarise(elo_100 = mean(elo_100), p = mean(y), n = n(), .groups = "drop")

cross <- tibble(venue = c("Ausgleich zu Hause", "Ausgleich auswärts"),
                x0 = c(-(cf[1] + cf[3]) / cf[2], -(cf[1] - cf[3]) / cf[2]) * 100)

pal8 <- c("Ausgleich zu Hause" = COL_EQ, "Ausgleich auswärts" = COL_NEUTRAL)
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
            aes(x = x0, y = 0.19, label = sprintf("50%% bei %+.0f ELO", x0)),
            size = 2.9, hjust = -0.05, show.legend = FALSE) +
  scale_colour_manual(values = pal8, name = NULL) +
  scale_y_continuous(labels = function(v) sprintf("%.0f%%", v * 100)) +
  labs(
    title    = "Der Heimvorteil verschiebt die Chance auf das nächste Tor",
    subtitle = "Die vom Modell geschätzte Chance, dass das Ausgleichsteam als Nächstes trifft · Punkte = tatsächliche Ergebnisse in ELO-Gruppen,\nGröße und Beschriftung nach Anzahl der Ausgleichstore in der Gruppe · 968 Ausgleichstore mit einem nächsten Tor",
    x = "ELO-Vorteil des Ausgleichsteams (Wertungspunkte)",
    y = "Chance, dass das Ausgleichsteam als Nächstes trifft",
    caption = "Daten: Understat, ClubElo · Analyse: R (glm, binomial)"
  ) +
  coord_cartesian(ylim = c(0.15, 0.85)) +
  theme_sport() +
  theme(plot.subtitle = element_text(size = subtitle_size(10)))
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
           colour = "grey40", label = "Schwellenwert 0,5") +
  scale_fill_manual(values = c(`1` = COL_EQ, `0` = COL_OPP),
                    labels = c(`1` = "Ausgleichsteam traf als Nächstes",
                               `0` = "Gegner traf als Nächstes"),
                    name = "Tatsächliches Ergebnis") +
  labs(
    title    = "Das nächste Tor ist kaum vorhersagbar",
    subtitle = sprintf("Die vom Modell geschätzte Chance für jedes Ausgleichstor, eingefärbt nach dem tatsächlichen Ausgang\n· starke Überlappung = das nächste Tor ist kaum vorhersagbar\n· Trefferquote %.2f, AUC %.2f (Modell: ELO + Heimvorteil)", acc, auc),
    x = "Vom Modell geschätzte Chance, dass das Ausgleichsteam als Nächstes trifft",
    y = "Anzahl der Ausgleichstore",
    caption = "Daten: Understat, ClubElo · Analyse: R (glm, binomial)"
  ) +
  theme_sport() +
  theme(plot.subtitle = element_text(size = subtitle_size(10)))
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
  term = c("Der Ausgleich selbst\n(Momentum, Achsenabschnitt)",
           "ELO-Vorteil",
           "Heimvorteil",
           "Ausgleichsminute",
           "Spielstand (2 FG)"),
  chisq = c(drop_dev(y ~ 0 + elo_100 + home_sign + minute_c + s22 + s33),
            drop_dev(y ~ home_sign + minute_c + s22 + s33),
            drop_dev(y ~ elo_100 + minute_c + s22 + s33),
            drop_dev(y ~ elo_100 + home_sign + s22 + s33),
            drop_dev(y ~ elo_100 + home_sign + minute_c)),
  df = c(1, 1, 1, 1, 2)
) %>%
  mutate(p = pchisq(chisq, df, lower.tail = FALSE),
         major = term %in% c("ELO-Vorteil", "Heimvorteil"))

f10 <- ggplot(importance, aes(chisq, reorder(term, chisq), fill = major)) +
  geom_col(width = 0.55) +
  geom_vline(xintercept = qchisq(0.95, 1), linetype = "dashed",
             colour = "grey45", linewidth = 0.5) +
  annotate("text", x = qchisq(0.95, 1) + 0.8, y = 0.62, hjust = 0, size = 2.9,
           colour = "grey40", fontface = "italic",
           label = "5 %-Signifikanzniveau (1 FG)") +
  geom_text(aes(label = sprintf("%.1f  (p %s)", chisq,
                                if_else(p < 0.001, "< 0.001",
                                        sprintf("= %.2f", p)))),
            hjust = -0.08, size = 3.1, colour = COL_NEUTRAL) +
  scale_fill_manual(values = c(`TRUE` = COL_EQ, `FALSE` = "#A3B1D4"),
                    guide = "none") +
  coord_cartesian(xlim = c(0, max(importance$chisq) * 1.35)) +
  labs(
    title    = "ELO dominiert, Momentum versagt",
    subtitle = "Wie stark sich das Modell verschlechtert, wenn ein Faktor entfernt wird\n· großer Balken = der Faktor ist wichtig · dunkelblau = die dominanten Faktoren\n· 'Der Ausgleich selbst' liegt an letzter Stelle: der Momentum-Term trägt nichts bei",
    x = "Wichtigkeit – Likelihood-Ratio-Chi-Quadrat (größer = wichtiger)",
    y = NULL,
    caption = "Daten: Understat, ClubElo · Analyse: R (glm, binomial)"
  ) +
  theme_sport() +
  theme(plot.subtitle = element_text(size = subtitle_size(10)))
ggsave(file.path(REPORT_DIR, "10_feature_importance.png"), f10,
       width = 10, height = 5.5, dpi = 300, bg = COL_BG)

# 11: who scores next? football-pitch styled bars (raw counts, no adjustment)
n_total <- nrow(eq_all)
n_none  <- sum(eq_all$next_goal == "none")
n_team  <- sum(eq_all$next_goal == "team")
n_opp   <- sum(eq_all$next_goal == "opponent")
bt      <- binom.test(n_team, n_team + n_opp, p = 0.5)

PITCH_WHITE <- "white"

bars1 <- tibble(
  outcome = c("GEGNER\ntraf als Nächstes", "AUSGLEICHSTEAM\ntraf als Nächstes", "KEIN weiteres Tor"),
  n       = c(n_opp, n_team, n_none),
  colour  = c(COL_OPP, COL_EQ, COL_GREY),
  xmin    = c(0, 1.075, 2.15)
) %>%
  mutate(
    pct       = 100 * n / n_total,
    xmax      = xmin + 1,
    xmid      = (xmin + xmax) / 2,
    step_xmin = xmax - 0.47,
    h_main    = pct - 2.0,
    h_step    = pct,
    pct_label = sprintf("%.0f%%", pct)
  )

pitch_x0 <- 0.20
pitch_x1 <- 2.98
pitch_y0 <- 4.5
pitch_y1 <- sort(bars1$h_main, decreasing = TRUE)[2] + 3.5
cc_cx    <- bars1$xmid[2]
cc_cy    <- (pitch_y0 + pitch_y1) / 2

cc_rx <- 0.246
cc_ry <- 5.0

circle_full <- tibble(t = seq(0, 2 * pi, length.out = 120)) %>%
  mutate(x = cc_cx + cc_rx * cos(t), y = cc_cy + cc_ry * sin(t))
left_arc <- tibble(t = seq(-pi / 2, pi / 2, length.out = 60)) %>%
  mutate(x = pitch_x0 + cc_rx * cos(t), y = cc_cy + cc_ry * sin(t))
right_arc <- tibble(t = seq(pi / 2, 3 * pi / 2, length.out = 60)) %>%
  mutate(x = pitch_x1 + cc_rx * cos(t), y = cc_cy + cc_ry * sin(t))

f11 <- ggplot() +
  geom_rect(data = bars1, aes(xmin = xmin, xmax = xmax, ymin = 0,
                              ymax = h_main, fill = colour)) +
  geom_rect(data = bars1, aes(xmin = step_xmin, xmax = xmax, ymin = 0,
                              ymax = h_step, fill = colour)) +
  scale_fill_identity() +
  annotate("rect", xmin = pitch_x0, xmax = pitch_x1, ymin = pitch_y0,
           ymax = pitch_y1, fill = NA, colour = PITCH_WHITE, linewidth = 2.6) +
  geom_path(data = circle_full, aes(x, y), colour = PITCH_WHITE, linewidth = 2.6) +
  geom_path(data = left_arc,    aes(x, y), colour = PITCH_WHITE, linewidth = 2.6) +
  geom_path(data = right_arc,   aes(x, y), colour = PITCH_WHITE, linewidth = 2.6) +
  geom_text(data = bars1, aes(x = xmid, y = h_step + 1.8, label = pct_label,
                              colour = colour),
            vjust = 0, fontface = "bold", size = 12) +
  geom_text(data = bars1, aes(x = xmid, y = -1.6, label = outcome,
                              colour = colour),
            vjust = 1, fontface = "bold", size = 4.1, lineheight = 0.95) +
  scale_colour_identity() +
  scale_x_continuous(limits = c(-0.15, 3.45), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-7.5, 46), expand = c(0, 0)) +
  labs(
    title    = "Kein Vorteil beim nächsten Tor – laut Rohdaten",
    subtitle = sprintf(paste0(
      "Was nach allen %s Bundesliga-Ausgleichstoren passierte, 2016/17-2025/26\n",
      "jedes Ergebnis liegt nahe an einer gleichmäßigen Drei-Wege-Verteilung · der Unterschied zwischen\n",
      "Ausgleichsteam und Gegner ist statistisch bedeutungslos (p = %.3f)"),
      format(n_total, big.mark = ","), bt$p.value),
    caption  = "Daten: Understat · Analyse: R (ggplot2, dplyr)"
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = COL_BG, colour = NA),
    panel.background = element_rect(fill = COL_BG, colour = NA),
    plot.title       = element_text(face = "bold", size = 20, colour = COL_NEUTRAL,
                                    margin = margin(b = 5)),
    plot.subtitle    = element_text(size = subtitle_size(8.2), colour = "grey30",
                                    margin = margin(b = 8), lineheight = 1.2),
    plot.caption     = element_text(size = 8, colour = "grey50", hjust = 0,
                                    margin = margin(t = 10)),
    plot.margin      = margin(20, 30, 15, 30)
  )
ggsave(file.path(REPORT_DIR, "11_next_goal_pitch.png"), f11,
       width = 8.2, height = 7.4, dpi = 300, bg = COL_BG)

cat("Alle 11 Report-Abbildungen (deutsch) geschrieben nach", REPORT_DIR, "\n")
cat(sprintf("Placebo p (Ausgleichsteam) = %.3f | Placebo p (Gegner) = %.3f\n",
            p_treat, p_mirror))
cat(sprintf("Rohanteile: keins %.0f%% | Team %.0f%% | Gegner %.0f%% (Binomialtest p = %.3f)\n",
            100 * n_none / n_total, 100 * n_team / n_total,
            100 * n_opp / n_total, bt$p.value))
