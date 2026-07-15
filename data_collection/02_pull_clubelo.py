"""
02_pull_clubelo.py  
======================================================================
Fetches ELO ratings from ClubElo (api.clubelo.com) for every unique
match date in the Understat data.

CONFIRMED from real API output:
  Columns : Rank, Club, Country, Level, Elo, From, To
  Country codes: 'GER' for Germany  (NOT 'DEU' as previously assumed)
  Level values : integer 1 / 2 / 0

CONFIRMED Bundesliga team names in ClubElo (2022 snapshot):
  Bayern, RB Leipzig, Dortmund, Leverkusen, Frankfurt, Union Berlin,
  Freiburg, Gladbach, Wolfsburg, Mainz, Koeln, Hoffenheim, Stuttgart,
  Augsburg, Bochum, Hertha, Werder, Schalke

Output: data/raw/elo_raw.csv          — ELO per team per date (GER Buli only)
        data/raw/match_elo.csv        — one row per match, home_elo + away_elo added

Incremental: dates already present in elo_raw.csv are skipped, so re-runs only
fetch new match dates plus any dates that failed on a previous run.
"""

import asyncio
import aiohttp
import pandas as pd
import io
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from config import DATA_RAW_DIR

INPUT_META  = os.path.join(DATA_RAW_DIR, "match_metadata.csv")
OUTPUT_DIR  = DATA_RAW_DIR
ELO_RAW_PATH = os.path.join(OUTPUT_DIR, "elo_raw.csv")
SLEEP_SEC   = 0.4

# ── Name map: Understat spelling → ClubElo spelling ───────────────────────────
# Built from confirmed real data. Left side = exactly as Understat returns it
# in h_team / a_team fields. Right side = exactly as ClubElo returns it.
NAME_MAP = {
    'Bayern Munich'         : 'Bayern',
    'RasenBallsport Leipzig': 'RB Leipzig',
    'Borussia Dortmund'     : 'Dortmund',
    'Bayer Leverkusen'      : 'Leverkusen',
    'Eintracht Frankfurt'   : 'Frankfurt',
    'Union Berlin'          : 'Union Berlin',
    'Freiburg'              : 'Freiburg',
    'Borussia M.Gladbach'   : 'Gladbach',
    'Wolfsburg'             : 'Wolfsburg',
    'Mainz 05'              : 'Mainz',
    'FC Cologne'            : 'Koeln',
    'Hoffenheim'            : 'Hoffenheim',
    'VfB Stuttgart'         : 'Stuttgart',
    'Augsburg'              : 'Augsburg',
    'Bochum'                : 'Bochum',
    'Hertha Berlin'         : 'Hertha',
    'Werder Bremen'         : 'Werder',
    'Schalke 04'            : 'Schalke',
    'Hamburger SV'          : 'Hamburg',
    'Hannover 96'           : 'Hannover',
    'Nuernberg'             : 'Nuernberg',
    'Fortuna Duesseldorf'   : 'Duesseldorf',
    'Paderborn'             : 'Paderborn',
    'Greuther Fuerth'       : 'Fuerth',
    'Arminia Bielefeld'     : 'Bielefeld',
    'Darmstadt'             : 'Darmstadt',
    'Ingolstadt'            : 'Ingolstadt',
    'FC Heidenheim'         : 'Heidenheim',
    'Holstein Kiel'         : 'Holstein',
    'St. Pauli'             : 'St Pauli',
}


async def fetch_elo_for_date(session, date_str, retries=1):
    """
    Returns (df, failure_reason). df is None on any failure; failure_reason
    distinguishes an HTTP-status failure (e.g. "HTTP 500") from an exception
    (e.g. "TimeoutError: ..."), so a systematic ClubElo outage doesn't look
    identical to the handful of normal date gaps. Transient exceptions get
    one retry before being counted as a real failure.
    """
    url = f"http://api.clubelo.com/{date_str}"
    last_error = None
    for attempt in range(retries + 1):
        try:
            async with session.get(url, timeout=aiohttp.ClientTimeout(total=15)) as resp:
                if resp.status != 200:
                    return None, f"HTTP {resp.status}"
                text = await resp.text()
                df = pd.read_csv(io.StringIO(text))
                # CONFIRMED filter: Country == 'GER', Level == 1
                buli = df[(df["Country"] == "GER") & (df["Level"] == 1)].copy()
                buli["fetch_date"] = date_str
                return buli[["Club", "Elo", "fetch_date"]], None
        except Exception as e:
            last_error = f"{type(e).__name__}: {e}"
            if attempt < retries:
                await asyncio.sleep(1)

    print(f"  WARNING: {date_str} failed after {retries + 1} attempt(s): {last_error}")
    return None, last_error


async def main():
    if not os.path.exists(INPUT_META):
        print(f"ERROR: {INPUT_META} not found. Run 01_pull_understat.py first.")
        return

    meta = pd.read_csv(INPUT_META)
    all_dates = sorted(meta["date_only"].unique())

    if os.path.exists(ELO_RAW_PATH):
        existing_elo = pd.read_csv(ELO_RAW_PATH)
        done_dates = set(existing_elo["match_date"].unique())
        print(f"Existing elo_raw.csv: {len(existing_elo):,} rows, "
              f"{len(done_dates)} dates — these will be skipped.")
    else:
        existing_elo = None
        done_dates = set()

    unique_dates = [d for d in all_dates if d not in done_dates]
    print(f"Unique match dates to fetch ELO for: {len(unique_dates)} "
          f"(of {len(all_dates)} total)")
    if unique_dates:
        print(f"Range: {unique_dates[0]} -> {unique_dates[-1]}\n")

    all_elo = []
    failures = []
    async with aiohttp.ClientSession() as session:
        for i, date_str in enumerate(unique_dates):
            if i % 100 == 0:
                print(f"  {i}/{len(unique_dates)} dates fetched...")
            df, error = await fetch_elo_for_date(session, date_str)
            if df is not None:
                all_elo.append(df)
            else:
                failures.append((date_str, error))
            await asyncio.sleep(SLEEP_SEC)

    if failures:
        n_http = sum(1 for _, e in failures if e.startswith("HTTP"))
        n_exc = len(failures) - n_http
        print(f"\nWARNING: {len(failures)} / {len(unique_dates)} dates failed to fetch "
              f"({n_http} HTTP errors, {n_exc} exceptions).")
        if n_exc > len(unique_dates) * 0.05:
            print("  More than 5% of dates failed with exceptions (not clean HTTP "
                  "errors) — this looks like a systematic outage or network issue, "
                  "not normal ClubElo date gaps. Consider re-running before trusting "
                  "the coverage numbers below.")
    else:
        print("\nAll dates fetched successfully — no failures.")

    if all_elo:
        new_elo = pd.concat(all_elo, ignore_index=True)
        new_elo.columns = ["club_elo_name", "elo", "match_date"]
    else:
        new_elo = pd.DataFrame(columns=["club_elo_name", "elo", "match_date"])

    elo_raw = pd.concat([existing_elo, new_elo], ignore_index=True) \
              if existing_elo is not None else new_elo
    elo_raw.to_csv(ELO_RAW_PATH, index=False)
    print(f"\nELO rows total: {len(elo_raw):,} ({len(new_elo):,} newly fetched)")

    # ── Join ELO to match metadata ─────────────────────────────────────────
    # Strategy: map Understat team names to ClubElo names, then join on
    # (clubelo_name, match_date)
    meta["home_elo_name"] = meta["home_team"].map(NAME_MAP)
    meta["away_elo_name"] = meta["away_team"].map(NAME_MAP)

    # Report unmapped teams immediately — these will cause silent join failures
    unmapped = set(meta[meta["home_elo_name"].isna()]["home_team"].unique()) | \
               set(meta[meta["away_elo_name"].isna()]["away_team"].unique())
    if unmapped:
        print("\nWARNING — these Understat team names have no entry in NAME_MAP:")
        for name in sorted(unmapped):
            print(f"  '{name}'")
        print("Look up how ClubElo spells these teams and add them to NAME_MAP.")

    # Join home ELO
    elo_lookup = elo_raw.rename(columns={"club_elo_name": "home_elo_name",
                                          "elo": "home_elo",
                                          "match_date": "date_only"})
    meta = meta.merge(elo_lookup, on=["home_elo_name", "date_only"], how="left")

    # Join away ELO
    elo_lookup = elo_raw.rename(columns={"club_elo_name": "away_elo_name",
                                          "elo": "away_elo",
                                          "match_date": "date_only"})
    meta = meta.merge(elo_lookup, on=["away_elo_name", "date_only"], how="left")

    meta["elo_difference"] = meta["home_elo"] - meta["away_elo"]

    out_path = os.path.join(OUTPUT_DIR, "match_elo.csv")
    meta.to_csv(out_path, index=False)

    coverage = meta["elo_difference"].notna().mean() * 100
    print(f"\nELO join coverage: {coverage:.1f}%  ({meta['elo_difference'].notna().sum()} / {len(meta)} matches)")
    print(f"Saved: {out_path}")

    if coverage < 90:
        print("WARNING: Coverage below 90% — fix NAME_MAP for the unmapped teams above.")


if __name__ == "__main__":
    asyncio.run(main())