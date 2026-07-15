"""
config.py
==============================================================================
Central path configuration for the Python side of the project (data
collection phase). Import from this module instead of hardcoding paths, so
scripts keep working no matter where they're run from.
"""

import os

PROJECT_ROOT = os.path.dirname(os.path.abspath(__file__))

DATA_DIR            = os.path.join(PROJECT_ROOT, "data")
DATA_RAW_DIR         = os.path.join(DATA_DIR, "raw")
DATA_PROCESSED_DIR   = os.path.join(DATA_DIR, "processed")

OUTPUT_DIR           = os.path.join(PROJECT_ROOT, "output")
OUTPUT_FIGURES_DIR   = os.path.join(OUTPUT_DIR, "figures")
OUTPUT_TABLES_DIR    = os.path.join(OUTPUT_DIR, "tables")

NOTEBOOKS_DIR        = os.path.join(PROJECT_ROOT, "notebooks")

for _dir in (DATA_RAW_DIR, DATA_PROCESSED_DIR, OUTPUT_FIGURES_DIR, OUTPUT_TABLES_DIR):
    os.makedirs(_dir, exist_ok=True)
