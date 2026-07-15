"""
03_validate.py
==============================================================================
Validates the raw data before any analysis:
  1. All seasons present, matches per season
  2. Shots per match plausible
  3. Goal rows reconcile with final scores — this also settles empirically
     which side OwnGoal rows are recorded under (matters for the equaliser
     filter downstream)
  4. Coordinates in [0, 1]
  5. Distinct values of the shot `result` field
  6. ELO coverage >= 80%
  7. Duplicate / missing match checks
"""

import os
import sys
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from config import DATA_RAW_DIR

EXPECTED_SEASONS = list(range(2016, 2026))
MATCHES_PER_SEASON = 306

# Known, documented gaps — matches that were never played to a finish and have
# no Understat shot data. Not an error; do not impute.
#   2024/25 Holstein Kiel vs Bochum (Dec 2024): abandoned (keeper struck by a
#   thrown lighter), later awarded 2-0 to Bochum by the DFB court.
KNOWN_MISSING = {2024: 1}

shots = pd.read_csv(os.path.join(DATA_RAW_DIR, "shots_raw.csv"))
meta  = pd.read_csv(os.path.join(DATA_RAW_DIR, "match_metadata.csv"))
melo  = pd.read_csv(os.path.join(DATA_RAW_DIR, "match_elo.csv"))

failures = []

def check(name, ok, detail=""):
    status = "PASS" if ok else "FAIL"
    print(f"[{status}] {name}" + (f" — {detail}" if detail else ""))
    if not ok:
        failures.append(name)


print("=" * 70)
print("1. SEASONS AND MATCH COUNTS")
print("=" * 70)
per_season = meta.groupby("season").size()
print(per_season.to_string())
check("All 10 seasons present (2016–2025)",
      sorted(shots["season"].unique()) == EXPECTED_SEASONS)
expected = {s: MATCHES_PER_SEASON - KNOWN_MISSING.get(s, 0) for s in EXPECTED_SEASONS}
short = per_season[per_season != pd.Series(expected)]
check(f"Every season has {MATCHES_PER_SEASON} matches (minus documented gaps: {KNOWN_MISSING})",
      short.empty,
      "" if short.empty else f"deviating: {short.to_dict()}")
if not short.empty:
    # Identify the missing fixtures: every team should have 34 apps per season
    for season in short.index:
        s = meta[meta["season"] == season]
        apps = pd.concat([s["home_team"], s["away_team"]]).value_counts()
        print(f"  Season {season}: teams with != 34 appearances: "
              f"{apps[apps != 34].to_dict()}")

print()
print("=" * 70)
print("2. SHOTS PER MATCH")
print("=" * 70)
spm = shots.groupby("match_id").size()
print(f"min {spm.min()} | median {spm.median():.0f} | mean {spm.mean():.1f} | max {spm.max()}")
check("Shots/match plausible (all between 5 and 70)",
      bool((spm >= 5).all() and (spm <= 70).all()),
      f"outliers: {spm[(spm < 5) | (spm > 70)].to_dict()}" if ((spm < 5) | (spm > 70)).any() else "")

print()
print("=" * 70)
print("3. GOAL RECONCILIATION AND OWN-GOAL CONVENTION")
print("=" * 70)
g = shots.assign(
    is_goal=shots["result"] == "Goal",
    is_og=shots["result"] == "OwnGoal",
)
agg = g.groupby(["match_id", "side"])[["is_goal", "is_og"]].sum().unstack(fill_value=0)
agg.columns = [f"{a}_{b}" for a, b in agg.columns]
agg = agg.reindex(columns=["is_goal_h", "is_goal_a", "is_og_h", "is_og_a"], fill_value=0)
rec = meta.set_index("match_id").join(agg).fillna(0)

# Convention A: OwnGoal row sits under the team that CONCEDES the shot
# (defending side) -> the goal counts for the OTHER team.
rec["home_A"] = rec["is_goal_h"] + rec["is_og_a"]
rec["away_A"] = rec["is_goal_a"] + rec["is_og_h"]
# Convention B: OwnGoal row sits under the team AWARDED the goal.
rec["home_B"] = rec["is_goal_h"] + rec["is_og_h"]
rec["away_B"] = rec["is_goal_a"] + rec["is_og_a"]

match_A = ((rec["home_A"] == rec["home_goals_final"]) &
           (rec["away_A"] == rec["away_goals_final"]))
match_B = ((rec["home_B"] == rec["home_goals_final"]) &
           (rec["away_B"] == rec["away_goals_final"]))
n = len(rec)
print(f"Convention A (OwnGoal under defending team, credited to opponent): "
      f"{match_A.sum()}/{n} matches reconcile ({match_A.mean()*100:.2f}%)")
print(f"Convention B (OwnGoal under team awarded the goal):                "
      f"{match_B.sum()}/{n} matches reconcile ({match_B.mean()*100:.2f}%)")
winner = "A" if match_A.sum() >= match_B.sum() else "B"
print(f"=> Empirical convention: {winner}")
best = match_A if winner == "A" else match_B
check("Goal rows reconcile with final scores in >= 99% of matches",
      best.mean() >= 0.99,
      f"{(~best).sum()} matches off")
if (~best).any():
    bad = rec[~best][["date", "home_team", "away_team",
                      "home_goals_final", "away_goals_final",
                      f"home_{winner}", f"away_{winner}"]]
    print(f"\nNon-reconciling matches ({len(bad)}):")
    print(bad.to_string())
check("Every match has at least one goal row or a 0-0 final score",
      bool(((rec[["is_goal_h", "is_goal_a", "is_og_h", "is_og_a"]].sum(axis=1) > 0) |
            ((rec["home_goals_final"] == 0) & (rec["away_goals_final"] == 0))).all()))

print()
print("=" * 70)
print("4. COORDINATES")
print("=" * 70)
in_range = shots["x"].between(0, 1) & shots["y"].between(0, 1)
check("All shot coordinates in [0, 1]",
      bool(in_range.all()),
      f"{(~in_range).sum()} rows outside" if (~in_range).any() else
      f"x: [{shots['x'].min():.3f}, {shots['x'].max():.3f}], "
      f"y: [{shots['y'].min():.3f}, {shots['y'].max():.3f}]")

print()
print("=" * 70)
print("5. DISTINCT `result` VALUES")
print("=" * 70)
print(shots["result"].value_counts().to_string())

print()
print("=" * 70)
print("6. ELO COVERAGE")
print("=" * 70)
cov = melo["elo_difference"].notna().mean()
print(f"Matches with both ELOs: {melo['elo_difference'].notna().sum()}/{len(melo)} "
      f"({cov*100:.1f}%)")
print("Coverage by season:")
print(melo.groupby("season")["elo_difference"]
          .apply(lambda s: f"{s.notna().mean()*100:.1f}%").to_string())
check("ELO coverage >= 80%", bool(cov >= 0.80))
unmapped = set(melo.loc[melo["home_elo_name"].isna(), "home_team"]) | \
           set(melo.loc[melo["away_elo_name"].isna(), "away_team"])
check("No team unmapped in ELO name map (no silent drops)",
      not unmapped, f"unmapped: {sorted(unmapped)}" if unmapped else "")

print()
print("=" * 70)
print("7. DUPLICATES / CONSISTENCY")
print("=" * 70)
check("shot_id unique", bool(shots["shot_id"].is_unique))
check("match_id sets identical across shots / metadata / ELO files",
      set(shots["match_id"]) == set(meta["match_id"]) == set(melo["match_id"]))
check("Minutes plausible (0–130)",
      bool(shots["minute"].between(0, 130).all()),
      f"range: [{shots['minute'].min()}, {shots['minute'].max()}]")
check("xG in [0, 1]", bool(shots["xg"].between(0, 1).all()))

print()
print("=" * 70)
if failures:
    print(f"RESULT: {len(failures)} CHECK(S) FAILED: {failures}")
    sys.exit(1)
print("RESULT: ALL CHECKS PASSED")
