# 02_train_poisson_model.R
# ==============================================================================
# Does attacking performance (xG per 5-min bin) improve after an
# equalising goal, beyond what game state, time, quality and home advantage predict?
#
# Process:
#   1. Split team_windows: training = all non-flagged bins; held out =
#      post-equaliser bins (treatment) and the opponent's mirror bins.
#   2. Fit Tweedie GAM (log link, power estimated) on training bins only.
#   3. Calibration checks — the model must predict normal windows well,
#      especially level-state mid/late-match bins, before it can serve as
#      the counterfactual.
#   4. Observed vs predicted on held-out treatment bins (equalisers <= 70',
#      full 15-min horizon), mirror bins evaluated separately.
#   5. Inference: placebo draws of matched control bins.
#   6. Robustness: Poisson on shot counts; drop stoppage-heavy bins 9 & 17;
#      drop treatment bins after the state changed again (truncation).
#
# All printed results are also saved to output/tables/h1_poisson_results.txt.
# Plots of the effect live in 04_report_figures.R.
#
# Run from the project root:  Rscript R/02_train_poisson_model.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(mgcv)
})
source("config.R")
set.seed(42)

# everything printed below is also written to this file
sink(file.path(OUTPUT_TABLES_DIR, "h1_poisson_results.txt"), split = TRUE)
cat("Tweedie model of xG per 5-min window\n")

windows <- read_csv(file.path(DATA_PROCESSED_DIR, "team_windows.csv"),
                    show_col_types = FALSE)

# 1. Split
windows <- windows %>%
  filter(!own_washout, !opp_washout) %>%          # washout bins: neither side
  mutate(
    goal_diff_c = factor(pmax(pmin(goal_diff, 2L), -2L), levels = -2:2),
    season_f    = factor(season)
  )

train  <- windows %>% filter(!own_post, !opp_post)
treat  <- windows %>% filter(own_post, own_post_eq_minute <= 70)
mirror <- windows %>% filter(opp_post, opp_post_eq_minute <= 70)

cat(sprintf("training bins: %s | treatment (eq <= 70'): %s | mirror: %s\n\n",
            format(nrow(train), big.mark = ","), nrow(treat), nrow(mirror)))

# 2. Fit "normal football" model (training bins only)
# bam + discrete for speed on ~100k rows; tw() estimates the Tweedie power.
form <- xg ~ goal_diff_c + s(bin_start, k = 8) + s(elo_diff, k = 6) +
        is_home + season_f
fit <- bam(form, family = tw(), data = train, discrete = TRUE)

p_hat <- as.numeric(gsub("[^0-9.]", "", sub(".*p=", "", fit$family$family)))
cat(sprintf("Tweedie power p = %.3f  (must lie strictly in (1,2))\n", p_hat))
if (p_hat <= 1.01 || p_hat >= 1.99) {
  warning("Tweedie power at boundary - family choice needs revisiting.")
}

print(summary(fit))   # coefficients: estimate, std.error, statistic, p-value
cat(sprintf("\nAIC            : %.1f\n", AIC(fit)))
cat(sprintf("Log-likelihood : %.1f (df = %.1f)\n",
            as.numeric(logLik(fit)), attr(logLik(fit), "df")))
cat("--------------------------------------------------------------\n")

# 3. Calibration on training data
train$pred <- as.numeric(predict(fit, newdata = train, type = "response"))

calib_slice <- train %>%
  mutate(period = case_when(bin_start < 30 ~ "min 0-29",
                            bin_start < 60 ~ "min 30-59",
                            TRUE           ~ "min 60-89")) %>%
  group_by(state, period) %>%
  summarise(predicted = mean(pred), observed = mean(xg), n = n(), .groups = "drop") %>%
  mutate(ratio = observed / predicted)

cat("\nCalibration by state x period (observed/predicted, training data):\n")
print(as.data.frame(calib_slice), row.names = FALSE, digits = 3)

# 4. Counterfactual comparison on held-out bins
gap_summary <- function(df, label) {
  pr <- as.numeric(predict(fit, newdata = df, type = "response"))
  tibble(set = label, n_bins = nrow(df),
         observed = mean(df$xg), predicted = mean(pr),
         gap = mean(df$xg - pr), ratio = mean(df$xg) / mean(pr))
}

effects <- bind_rows(
  gap_summary(treat,  "treatment (eq <= 70')"),
  gap_summary(mirror, "opponent mirror")
)

# 5. Placebo inference
B <- 1000
controls <- train %>% filter(state == "level")
bin_counts <- treat %>% count(bin)
ctrl_by_bin <- split(controls, controls$bin)

placebo <- replicate(B, {
  s <- lapply(seq_len(nrow(bin_counts)), function(i) {
    pool <- ctrl_by_bin[[as.character(bin_counts$bin[i])]]
    pool[sample.int(nrow(pool), bin_counts$n[i], replace = TRUE), c("xg", "pred")]
  })
  s <- bind_rows(s)
  mean(s$xg - s$pred)
})

gap_t1 <- effects$gap[1]
p_two  <- (1 + sum(abs(placebo - mean(placebo)) >= abs(gap_t1 - mean(placebo)))) / (B + 1)
cat(sprintf("\nPlacebo test (B=%d): treatment gap = %+.4f xG/bin | placebo mean = %+.4f, 95%% band [%.4f, %.4f] | two-sided p = %.3f\n",
            B, gap_t1, mean(placebo),
            quantile(placebo, .025), quantile(placebo, .975), p_two))

# 6. Robustness
cat("\nRobustness checks:\n")

# (a) Poisson on shot counts
fit_shots <- bam(update(form, n_shots ~ .), family = poisson(),
                 data = train, discrete = TRUE)
gap_shots <- function(df, label) {
  pr <- as.numeric(predict(fit_shots, newdata = df, type = "response"))
  sprintf("  %-38s observed %.3f vs predicted %.3f shots/bin (ratio %.3f)",
          label, mean(df$n_shots), mean(pr), mean(df$n_shots) / mean(pr))
}
cat(gap_shots(treat, "(a) shots, treatment:"), "\n")
cat(gap_shots(mirror,   "(a) shots, opponent mirror:"), "\n")

# (b) drop stoppage-overloaded bins (45-49', 85-89')
no_stop <- function(df) df %>% filter(!bin %in% c(9L, 17L))
fit_ns <- bam(form, family = tw(), data = no_stop(train), discrete = TRUE)
pr_ns  <- as.numeric(predict(fit_ns, newdata = no_stop(treat), type = "response"))
cat(sprintf("  (b) excl. stoppage bins:       gap %+.4f xG/bin (ratio %.3f)\n",
            mean(no_stop(treat)$xg - pr_ns),
            mean(no_stop(treat)$xg) / mean(pr_ns)))

# (c) truncation: drop treatment bins that start after the next goal
#     (state changed again -> no longer a clean post-equaliser bin)
ge <- read_csv(file.path(DATA_PROCESSED_DIR, "goal_events.csv"),
               show_col_types = FALSE)
next_after_eq <- ge %>%
  filter(qualifying_eq) %>%
  transmute(match_id, team = scoring_team,
            own_post_eq_minute = minute, next_minute)
treat_trunc <- treat %>%
  left_join(next_after_eq, by = c("match_id", "team", "own_post_eq_minute")) %>%
  filter(is.na(next_minute) | bin_start < next_minute)
pr_tr <- as.numeric(predict(fit, newdata = treat_trunc, type = "response"))
cat(sprintf("  (c) truncated at next goal:    gap %+.4f xG/bin (ratio %.3f, %d bins kept of %d)\n",
            mean(treat_trunc$xg - pr_tr), mean(treat_trunc$xg) / mean(pr_tr),
            nrow(treat_trunc), nrow(treat)))

# summary
effects <- effects %>%
  mutate(placebo_p_two_sided = c(p_two, NA),
         tweedie_power = p_hat)

cat("\n==========================================================\n")
cat("Poisson Model Summary (xG per 5-min bin)\n")
cat("==========================================================\n")
print(as.data.frame(effects), row.names = FALSE, digits = 3)
sink()
