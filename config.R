# config.R
# ==============================================================================
# Central path configuration for the R side of the project (analysis phase).
# Source this instead of hardcoding paths. Uses `here` so paths resolve
# correctly regardless of the working directory an R session starts in.

if (!requireNamespace("here", quietly = TRUE)) {
  stop("Package 'here' is required. Run: Rscript R/install_packages.R")
}
library(here)

# Pin the project root to this file's location. Fails loudly if the working
# directory is outside the project instead of silently resolving wrong paths.
here::i_am("config.R")

DATA_DIR           <- here("data")
DATA_RAW_DIR        <- here("data", "raw")
DATA_PROCESSED_DIR  <- here("data", "processed")

OUTPUT_DIR          <- here("output")
OUTPUT_FIGURES_DIR  <- here("output", "figures")
OUTPUT_TABLES_DIR   <- here("output", "tables")

NOTEBOOKS_DIR       <- here("notebooks")

for (d in c(DATA_RAW_DIR, DATA_PROCESSED_DIR, OUTPUT_FIGURES_DIR, OUTPUT_TABLES_DIR)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# ── Project plot theme ────────────────────────────────────────────────────────
# Used by every plot in the project. Grey marks carry direct value labels
# (low contrast on the cream surface is compensated by labelling, not color).
COL_EQ      <- "#1F3A93"   # equalising team — deep blue
COL_OPP     <- "#C0392B"   # opponent — warm red
COL_NEUTRAL <- "#2C3E50"   # structure / neutral text
COL_BG      <- "#FAF6F1"   # cream background
COL_GRID    <- "#D8D2C7"   # grid lines
COL_GREY    <- "#AAAAAA"   # no-goal / neutral category

theme_momentum <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = COL_BG, colour = NA),
      panel.background = ggplot2::element_rect(fill = COL_BG, colour = NA),
      panel.grid.major = ggplot2::element_line(colour = COL_GRID, linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      text             = ggplot2::element_text(colour = COL_NEUTRAL),
      axis.text        = ggplot2::element_text(colour = COL_NEUTRAL),
      plot.title       = ggplot2::element_text(face = "bold", hjust = 0),
      legend.position  = "top"
    )
}
