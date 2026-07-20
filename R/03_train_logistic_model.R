# 03_train_logistic_model.R
# ==============================================================================
# After equalising, is the equalising team more likely to score the next
# goal than team quality predicts?
#
# Design: outcome = next goal scored by the equalising team.
# Covariates use SYMMETRIC coding - elo_diff and home_sign both flip sign if
# the two teams' labels are swapped. Under a no-momentum null the intercept
# is therefore exactly 0 (coin flip at equal quality, neutral venue), so the
# intercept IS the momentum test.
#
#   1. Primary: glm(next_by_eq ~ elo_diff + home_sign), match-clustered SEs;
#      momentum = intercept (OR 1 = coin flip at equal quality).
#   2. Extended: + equaliser minute + scoreline (do late/2-2 equalisers
#      behave differently?).
#   3. Censoring: 'no further goal' cases - balance check plus worst-case
#      bounds (all 'none' -> opponent / all 'none' -> team).
#
# All printed results are also saved to output/tables/h2_logistic_results.txt.
# All figures live in R/04_report_figures.R.
#
# Run from the project root:  Rscript R/03_train_logistic_model.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(sandwich)
  library(lmtest)
})
source("config.R")
set.seed(42)

# everything printed below is also written to this file
sink(file.path(OUTPUT_TABLES_DIR, "h2_logistic_results.txt"), split = TRUE)
cat("Logistic model of who scores next after an equaliser\n\n")

eqs <- read_csv(file.path(DATA_PROCESSED_DIR, "equaliser_events.csv"),
                show_col_types = FALSE) %>%
  mutate(
    home_sign  = if_else(is_home, 1, -1),
    elo_100    = elo_diff / 100,              # per-100-points scale
    minute_c   = (eq_minute - 60) / 15,       # centered, per-15-min scale
    score_f    = factor(if_else(scoreline == "1-1", "1-1",
                        if_else(scoreline == "2-2", "2-2", "3-3+")))
  )

# check how many equalisers are followed by a next goal vs. censored ('none')
decided <- eqs %>%
  filter(next_goal != "none") %>%
  mutate(y = as.integer(next_goal == "team"))

cat(sprintf("equalisers: %d | decided (next goal exists): %d | censored ('none'): %d\n\n",
            nrow(eqs), nrow(decided), sum(eqs$next_goal == "none")))

# censoring balance: is 'none' related to quality/venue/time?
cens <- glm(I(next_goal == "none") ~ elo_100 + home_sign + minute_c,
            family = binomial(), data = eqs)
cat("Censoring check ('none' vs covariates) - p-values:\n")
print(round(coef(summary(cens))[, 4], 3))
cat("(eq_minute inevitably predicts censoring - later equaliser, less time",
    "\n for another goal. Quality/venue should NOT predict it.)\n\n")

# 1. primary model
fit <- glm(y ~ elo_100 + home_sign, family = binomial(), data = decided)
ct  <- coeftest(fit, vcov = vcovCL(fit, cluster = decided$match_id))

print(summary(fit))   # coefficients: estimate, std.error, statistic, p-value
cat(sprintf("AIC            : %.1f\n", AIC(fit)))
cat(sprintf("Log-likelihood : %.1f (df = %d)\n",
            as.numeric(logLik(fit)), attr(logLik(fit), "df")))
cat("--------------------------------------------------------------\n\n")

cat("Primary logistic (match-clustered SEs):\n")
cat(sprintf("  %-12s OR %.3f  [%.3f, %.3f]  p = %.3f%s\n",
            rownames(ct), exp(ct[, 1]),
            exp(ct[, 1] - 1.96 * ct[, 2]), exp(ct[, 1] + 1.96 * ct[, 2]),
            ct[, 4],
            c("   <- momentum test (null: OR = 1)", "", "")))

# 2. extended model: does the picture change late / at higher scorelines?
fit_ext <- glm(y ~ elo_100 + home_sign + minute_c + score_f,
               family = binomial(), data = decided)
ct_ext <- coeftest(fit_ext, vcov = vcovCL(fit_ext, cluster = decided$match_id))
cat("Extended model (adds equaliser minute, scoreline):\n")
cat(sprintf("  %-12s OR %.3f  [%.3f, %.3f]  p = %.3f\n",
            rownames(ct_ext), exp(ct_ext[, 1]),
            exp(ct_ext[, 1] - 1.96 * ct_ext[, 2]),
            exp(ct_ext[, 1] + 1.96 * ct_ext[, 2]), ct_ext[, 4]))

# 3. censoring bounds
bound <- function(none_as) {
  d <- eqs %>% mutate(y = as.integer(
    if_else(next_goal == "none", none_as == "team", next_goal == "team")))
  f <- glm(y ~ elo_100 + home_sign, family = binomial(), data = d)
  exp(coef(f)[1])
}
cat(sprintf("\nWorst-case censoring bounds on the momentum OR: [%.3f, %.3f]\n",
            bound("opponent"), bound("team")))
cat("(true value lies inside; bounds are extreme assumptions about 'none' cases)\n")

# summary
cat("\n==========================================================\n")
cat("Logistic Model Summary (who scores next)\n")
cat("==========================================================\n")
cat(sprintf("momentum test (intercept): OR %.3f [%.3f, %.3f], p = %.3f  (null: OR = 1)\n",
            exp(ct[1, 1]), exp(ct[1, 1] - 1.96 * ct[1, 2]),
            exp(ct[1, 1] + 1.96 * ct[1, 2]), ct[1, 4]))

sink()
