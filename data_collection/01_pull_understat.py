"""
01_pull_understat.py
==============================================================================
Pulls all Bundesliga shot-level data from Understat for seasons 2016/17–2025/26.

CONFIRMED field names from real API output:
  Shot dict keys: id, minute, result, X, Y, xG, player, h_a, player_id,
                  situation, season, shotType, match_id, h_team, a_team,
                  h_goals, a_goals, date, player_assisted, lastAction

Every shot already contains match context (teams, date, final score).

Output files (saved to data/raw/):
  shots_raw.csv       one row per shot
  match_metadata.csv  one row per match (derived from shots)

"""

import asyncio
import aiohttp
import understat
import pandas as pd
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from config import DATA_RAW_DIR

LEAGUE              = "Bundesliga"
SEASONS             = list(range(2016, 2026))
OUTPUT_DIR          = DATA_RAW_DIR
SLEEP_BETWEEN       = 3          # seconds between seasons
TEST_ONE_SEASON     = False      # set True to test on 2022 only

SHOTS_PATH = os.path.join(OUTPUT_DIR, "shots_raw.csv")


async def fetch_season(session, year, done_match_ids):
    u = understat.Understat(session)

    print(f"  Fetching match list {year}/{str(year+1)[-2:]}...", end=" ", flush=True)
    matches = await u.get_league_results(LEAGUE, year)
    new = [m for m in matches if int(m["id"]) not in done_match_ids]
    print(f"{len(matches)} matches ({len(new)} new).")

    all_shots = []

    for i, m in enumerate(new):
        match_id = m["id"]
        try:
            shots = await u.get_match_shots(match_id)
            for side in ("h", "a"):
                for shot in shots.get(side, []):
                    all_shots.append({
                        "shot_id"          : shot.get("id", ""),
                        "match_id"         : shot.get("match_id", match_id),
                        "season"           : year,
                        "date"             : shot.get("date", ""),
                        "home_team"        : shot.get("h_team", ""),
                        "away_team"        : shot.get("a_team", ""),
                        "home_goals_final" : int(shot.get("h_goals", 0)),
                        "away_goals_final" : int(shot.get("a_goals", 0)),
                        "team"             : shot.get("h_team", "") if side == "h" else shot.get("a_team", ""),
                        "side"             : side,
                        "minute"           : int(shot.get("minute", 0)),
                        "xg"               : float(shot.get("xG", 0)),
                        "result"           : shot.get("result", ""),
                        "situation"        : shot.get("situation", ""),
                        "shot_type"        : shot.get("shotType", ""),
                        "last_action"      : shot.get("lastAction", ""),
                        "x"                : float(shot.get("X", 0)),
                        "y"                : float(shot.get("Y", 0)),
                        "player"           : shot.get("player", ""),
                        "player_id"        : shot.get("player_id", ""),
                        "player_assisted"  : shot.get("player_assisted", ""),
                        "own_goal"         : shot.get("result", "") == "OwnGoal",
                    })
        except Exception as e:
            print(f"\n  WARNING: match {match_id} failed: {e}")

        if (i + 1) % 50 == 0:
            print(f"    {i+1}/{len(new)} matches done...")

    df = pd.DataFrame(all_shots)
    print(f"  {len(df):,} shots collected.")
    return df


async def main():
    seasons_to_run = [2022] if TEST_ONE_SEASON else SEASONS

    if TEST_ONE_SEASON:
        print("TEST MODE — one season only (2022/23). Inspect output then set TEST_ONE_SEASON = False.\n")

    if os.path.exists(SHOTS_PATH):
        existing = pd.read_csv(SHOTS_PATH)
        done_match_ids = set(existing["match_id"].astype(int))
        print(f"Existing shots_raw.csv: {len(existing):,} shots, "
              f"{len(done_match_ids):,} matches — these will be skipped.")
        all_shots = [existing]
    else:
        done_match_ids = set()
        all_shots = []

    async with aiohttp.ClientSession() as session:
        for year in seasons_to_run:
            print(f"\nSeason {year}/{str(year+1)[-2:]}:")
            df = await fetch_season(session, year, done_match_ids)
            all_shots.append(df)
            if year != seasons_to_run[-1]:
                await asyncio.sleep(SLEEP_BETWEEN)

    shots = pd.concat(all_shots, ignore_index=True)

    shots_path = SHOTS_PATH
    shots.to_csv(shots_path, index=False)

    # Derive match_metadata — one row per match, needed for ELO join
    meta = (
        shots.groupby("match_id")
        .agg(
            season           = ("season",           "first"),
            date             = ("date",             "first"),
            home_team        = ("home_team",        "first"),
            away_team        = ("away_team",        "first"),
            home_goals_final = ("home_goals_final", "first"),
            away_goals_final = ("away_goals_final", "first"),
        )
        .reset_index()
    )
    meta["date_only"] = pd.to_datetime(meta["date"]).dt.strftime("%Y-%m-%d")
    meta.to_csv(os.path.join(OUTPUT_DIR, "match_metadata.csv"), index=False)

    print("\n" + "="*55)
    print("UNDERSTAT COLLECTION COMPLETE")
    print("="*55)
    print(f"Total shots    : {len(shots):,}")
    print(f"Total matches  : {len(meta):,}")
    print(f"Seasons        : {sorted(shots['season'].unique())}")
    print(f"Goals          : {(shots['result'] == 'Goal').sum():,}")
    print(f"Own goals      : {shots['own_goal'].sum():,}")
    print(f"\nAll result values   : {sorted(shots['result'].unique())}")
    print(f"All situation values: {sorted(shots['situation'].unique())}")
    print(f"\nShots per season:")
    print(shots.groupby("season").size().to_string())
    print(f"\nSaved: {shots_path}")


if __name__ == "__main__":
    asyncio.run(main())