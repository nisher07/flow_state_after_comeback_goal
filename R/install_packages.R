# install_packages.R
# ==============================================================================
# R dependencies for this project (the R-side "requirements file").
# Run once:  Rscript R/install_packages.R
# Installs only what is missing.

required <- c(
  "here",       # project-root-relative paths (used by config.R)
  "tidyverse",  # data wrangling + ggplot2
  "lme4",       # mixed models (candidate for the momentum analysis)
  "broom",      # tidy model output
  "knitr",      # notebooks / reports
  "rmarkdown"   # notebooks / reports
)

missing <- setdiff(required, rownames(installed.packages()))
if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All required packages already installed.")
}

invisible(lapply(required, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) stop("Package failed to install: ", p)
}))
message("OK — all ", length(required), " required packages available.")
