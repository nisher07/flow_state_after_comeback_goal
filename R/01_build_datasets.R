# 01_build_datasets.R
# ==============================================================================
#   data/processed/goal_events.csv       one row per goal (backbone)
#   data/processed/team_windows.csv      one row per team x match x 5-min bin
#   data/processed/equaliser_events.csv  one row per qualifying equaliser (H2)
#
# Run from the project root:  Rscript R/01_build_datasets.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})
source("config.R")

N_BINS   <- 18L   # 5-min bins: 0-4, 5-9, ..., 85-89 (stoppage clipped into last bin)
POST_BINS <- 3L   # 15-min post-equaliser horizon = 3 bins after the washout bin

shots <- read_csv(file.path(DATA_RAW_DIR, "shots_raw.csv"), show_col_types = FALSE)
melo  <- read_csv(file.path(DATA_RAW_DIR, "match_elo.csv"),  show_col_types = FALSE)

# 1. goal_events
# Own goals sit under the conceding team; the goal credits the opponent
# (validated: 100% of matches reconcile under this convention).
goal_events <- shots %>%
  filter(result %in% c("Goal", "OwnGoal")) %>%
  mutate(scoring_team = if_else(result == "Goal",
                                team,
                                if_else(side == "h", away_team, home_team))) %>%
  arrange(match_id, minute, shot_id) %>%
  group_by(match_id) %>%
  mutate(
    goal_seq       = row_number(),
    score_h        = cumsum(scoring_team == home_team),
    score_a        = cumsum(scoring_team == away_team),
    score_h_before = score_h - (scoring_team == home_team),
    score_a_before = score_a - (scoring_team == away_team),
    is_own_goal    = result == "OwnGoal",
    is_equaliser   = if_else(scoring_team == home_team,
                             score_h_before - score_a_before == -1,
                             score_a_before - score_h_before == -1),
    # treatment definition: trailing by exactly one AND scored themselves
    qualifying_eq  = is_equaliser & !is_own_goal,
    next_scorer    = lead(scoring_team),
    next_minute    = lead(minute)
  ) %>%
  ungroup() %>%
  select(match_id, season, date, home_team, away_team, goal_seq, minute,
         shot_id, scoring_team, score_h_before, score_a_before, score_h,
         score_a, is_own_goal, is_equaliser, qualifying_eq,
         next_scorer, next_minute)

# 2. team_windows
bin_of <- function(minute) pmin(minute %/% 5L, N_BINS - 1L)

matches <- melo %>%
  select(match_id, season, date_only, home_team, away_team, home_elo, away_elo)

# skeleton: every match x 18 bins x both teams
windows <- matches %>%
  crossing(bin = 0:(N_BINS - 1L)) %>%
  crossing(is_home = c(TRUE, FALSE)) %>%
  mutate(
    team      = if_else(is_home, home_team, away_team),
    opponent  = if_else(is_home, away_team, home_team),
    elo_team  = if_else(is_home, home_elo, away_elo),
    elo_opp   = if_else(is_home, away_elo, home_elo),
    elo_diff  = elo_team - elo_opp,
    bin_start = bin * 5L
  )

# attacking output per team-bin (own-goal shots are not attacking output)
output <- shots %>%
  filter(result != "OwnGoal") %>%
  mutate(bin = bin_of(minute)) %>%
  group_by(match_id, team, bin) %>%
  summarise(xg = sum(xg), n_shots = n(), .groups = "drop")

# goals credited per team-bin (own goals credit the opponent)
goals_binned <- goal_events %>%
  mutate(bin = bin_of(minute)) %>%
  count(match_id, scoring_team, bin, name = "n_goals")

# score state at bin start: goals strictly before the bin's first minute
state <- goal_events %>%
  mutate(bin_first_affected = bin_of(minute)) %>%
  select(match_id, minute, scoring_team, bin_first_affected)

windows <- windows %>%
  left_join(output, by = c("match_id", "team", "bin")) %>%
  left_join(goals_binned,
            by = c("match_id", "team" = "scoring_team", "bin")) %>%
  mutate(across(c(xg, n_shots, n_goals), ~ replace_na(.x, 0)))

# cumulative goals before each bin start, per team
goals_for_state <- goal_events %>%
  select(match_id, minute, scoring_team)
state_grid <- windows %>%
  distinct(match_id, team, bin, bin_start) %>%
  left_join(goals_for_state, by = "match_id", relationship = "many-to-many") %>%
  group_by(match_id, team, bin, bin_start) %>%
  summarise(
    gf = sum(!is.na(minute) & scoring_team == team & minute < bin_start),
    ga = sum(!is.na(minute) & scoring_team != team & minute < bin_start),
    .groups = "drop"
  ) %>%
  mutate(goal_diff = gf - ga,
         state = case_when(goal_diff > 0 ~ "leading",
                           goal_diff < 0 ~ "trailing",
                           TRUE          ~ "level"))

windows <- windows %>%
  left_join(state_grid %>% select(match_id, team, bin, goal_diff, state),
            by = c("match_id", "team", "bin"))

# treatment flags from qualifying equalisers
eqs <- goal_events %>%
  filter(qualifying_eq) %>%
  transmute(match_id, eq_team = scoring_team, eq_minute = minute,
            eq_bin = bin_of(minute))

flag_windows <- function(w, e, prefix) {
  joined <- w %>%
    distinct(match_id, team, bin) %>%
    inner_join(e, by = "match_id", relationship = "many-to-many") %>%
    filter(if (prefix == "own") team == eq_team else team != eq_team)
  washout <- joined %>%
    filter(bin == eq_bin) %>%
    distinct(match_id, team, bin) %>%
    mutate("{prefix}_washout" := TRUE)
  post <- joined %>%
    filter(bin > eq_bin, bin <= eq_bin + POST_BINS) %>%
    group_by(match_id, team, bin) %>%
    summarise(eq_minute = max(eq_minute), .groups = "drop") %>%
    rename("{prefix}_post_eq_minute" := eq_minute) %>%
    mutate("{prefix}_post" := TRUE)
  w %>%
    left_join(washout, by = c("match_id", "team", "bin")) %>%
    left_join(post,    by = c("match_id", "team", "bin"))
}

windows <- windows %>%
  flag_windows(eqs, "own") %>%
  flag_windows(eqs, "opp") %>%
  mutate(across(c(own_washout, own_post, opp_washout, opp_post),
                ~ replace_na(.x, FALSE)),
         exposure_min = 5) %>%
  select(match_id, season, date = date_only, team, opponent, is_home,
         elo_diff, bin, bin_start, exposure_min, xg, n_shots, n_goals,
         goal_diff, state, own_washout, own_post, own_post_eq_minute,
         opp_washout, opp_post, opp_post_eq_minute)

# 3. equaliser_events
equaliser_events <- goal_events %>%
  filter(qualifying_eq) %>%
  left_join(matches %>% select(match_id, home_elo, away_elo), by = "match_id") %>%
  mutate(
    is_home     = scoring_team == home_team,
    opponent    = if_else(is_home, away_team, home_team),
    elo_diff    = if_else(is_home, home_elo - away_elo, away_elo - home_elo),
    scoreline   = paste0(score_h, "-", score_a),
    next_goal   = case_when(is.na(next_scorer)            ~ "none",
                            next_scorer == scoring_team   ~ "team",
                            TRUE                          ~ "opponent"),
    time_to_next = next_minute - minute
  ) %>%
  select(match_id, season, date, team = scoring_team, opponent, is_home,
         eq_minute = minute, scoreline, elo_diff, goal_seq,
         next_goal, next_goal_minute = next_minute, time_to_next)

# write
write_csv(goal_events,      file.path(DATA_PROCESSED_DIR, "goal_events.csv"))
write_csv(windows,          file.path(DATA_PROCESSED_DIR, "team_windows.csv"))
write_csv(equaliser_events, file.path(DATA_PROCESSED_DIR, "equaliser_events.csv"))

# validation summary
cat("==========================================================\n")
cat("DATASET BUILD SUMMARY\n")
cat("==========================================================\n")
cat(sprintf("goal_events       : %6d rows (%d matches)\n",
            nrow(goal_events), n_distinct(goal_events$match_id)))
cat(sprintf("  equalisers      : %6d  (qualifying: %d | own-goal, excluded as treatment: %d)\n",
            sum(goal_events$is_equaliser), sum(goal_events$qualifying_eq),
            sum(goal_events$is_equaliser & goal_events$is_own_goal)))
cat(sprintf("team_windows      : %6d rows (expected %d = matches x 18 bins x 2 teams)\n",
            nrow(windows), n_distinct(melo$match_id) * N_BINS * 2L))
cat(sprintf("  xG reconciles   : windows %.2f vs shots (non-OG) %.2f\n",
            sum(windows$xg), sum(shots$xg[shots$result != "OwnGoal"])))
cat(sprintf("  goals reconcile : windows %d vs goal_events %d\n",
            sum(windows$n_goals), nrow(goal_events)))
cat(sprintf("  bin-0 state all level: %s\n",
            all(windows$state[windows$bin == 0] == "level")))
cat(sprintf("  treatment windows (own_post): %d | washout: %d | opponent mirror: %d\n",
            sum(windows$own_post), sum(windows$own_washout), sum(windows$opp_post)))
cat(sprintf("  NA check (key cols): %s\n",
            ifelse(anyNA(windows %>% select(xg, n_shots, state, elo_diff, goal_diff)),
                   "FAIL - NAs present", "clean")))
cat(sprintf("equaliser_events  : %6d rows (no minute cutoff)\n", nrow(equaliser_events)))
cat(sprintf("  tier 1 (<= 70') : %6d\n", sum(equaliser_events$eq_minute <= 70)))
print(table(equaliser_events$next_goal))
cat(sprintf("  NA in elo_diff  : %d\n", sum(is.na(equaliser_events$elo_diff))))
cat("\nWritten to data/processed/: goal_events.csv, team_windows.csv, equaliser_events.csv\n")
