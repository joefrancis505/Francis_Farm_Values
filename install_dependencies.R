#!/usr/bin/env Rscript
# =============================================================================
# install_dependencies.R — Install R packages required by the pipeline
# =============================================================================
# Run once before Master.R. Re-run if a fresh R version is missing packages.
# =============================================================================

pkgs <- c(
  # data wrangling and I/O
  "dplyr", "purrr", "readr", "tidyr", "data.table", "stringr", "digest", "tools",
  # IPUMS + HTTP
  "ipumsr", "httr",
  # spatial / raster
  "sf", "stars", "fasterize", "terra", "exactextractr", "elevatr",
  # econometrics + RDD
  "fixest", "rd2d"
)

missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) == 0) {
  cat("All required packages already installed.\n")
} else {
  cat("Installing:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, repos = "https://cloud.r-project.org")
}

# Sanity check
still_missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(still_missing) > 0)
  stop("Failed to install: ", paste(still_missing, collapse = ", "))
cat("\nAll dependencies satisfied.\n")
