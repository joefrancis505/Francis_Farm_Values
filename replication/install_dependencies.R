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
  "sf", "stars", "fasterize", "terra", "exactextractr",
  # econometrics
  "fixest"
)
# NB: rebuilding the shipped geographic covariates in Data/Geology_v2/ (not
# part of the standard pipeline) additionally requires the FedData package;
# see Scripts/00_Build_Geology_v2.R.

missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) == 0) {
  cat("All CRAN packages already installed.\n")
} else {
  cat("Installing:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, repos = "https://cloud.r-project.org")
}

# rd2d >= 1.0.0 is required for the native covs.eff covariate adjustment used in
# the spatial RDD (step 7). Version 1.0.0 may not yet be on CRAN; install from
# GitHub if the installed version is older or the package is missing.
need_rd2d <- !"rd2d" %in% rownames(installed.packages()) ||
  utils::packageVersion("rd2d") < "1.0.0"
if (need_rd2d) {
  if (!requireNamespace("remotes", quietly = TRUE))
    install.packages("remotes", repos = "https://cloud.r-project.org")
  cat("Installing rd2d (>= 1.0.0) from GitHub (rdpackages/rd2d)...\n")
  remotes::install_github("rdpackages/rd2d", subdir = "R/rd2d", upgrade = "never")
} else {
  cat("rd2d", as.character(utils::packageVersion("rd2d")), "already installed.\n")
}

# Sanity check
still_missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(still_missing) > 0)
  stop("Failed to install: ", paste(still_missing, collapse = ", "))
if (!"rd2d" %in% rownames(installed.packages()) ||
    utils::packageVersion("rd2d") < "1.0.0")
  stop("rd2d >= 1.0.0 is required but was not installed.")
cat("\nAll dependencies satisfied.\n")
