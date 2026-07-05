#!/usr/bin/env Rscript
# =============================================================================
# Master.R — Replication pipeline for
# "Did Slavery Impede the Growth of American Capitalism?
#  Two Failed Natural Experiments Using Farm Values per Acre"
# =============================================================================
# Runs the full pipeline end-to-end:
#
#   1. Download NHGIS census tables + shapefiles (1820-1900)
#   2. Build county-year database from NHGIS
#   3. Normalize to 1850 county boundaries (panel for the event study)
#   4. Build farm-value-per-acre geopackages for QGIS maps
#   5. Event study (Table 1, Figure 1)
#   6. Spatial RDD + balance tests (Tables 2 and 3)
#
# The geographic covariates for step 6 (county means of elevation and slope
# from the USGS NED, and clay/sand/silt from CONUS-SOIL) ship with the
# repository in Data/Geology_v2/ and are read directly. Regenerate them with
# Scripts/00_Build_Geology_v2.R if needed (multi-hour build; see its header).
#
# Required API key:
#   IPUMS_API_KEY     https://account.ipums.org/api_keys
#
# Either set it in your environment or put it in a .env file at the
# repository root.
#
# Outputs land under Output/ (Tables/, Figures/, Maps/).
# =============================================================================

# --- Locate ourselves --------------------------------------------------------
if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
REPL_ROOT <- normalizePath(getwd(), mustWork = TRUE)
if (!file.exists(file.path(REPL_ROOT, "Master.R")))
  stop("Run Master.R from the replication root (the directory containing this file).")
setwd(REPL_ROOT)
Sys.setenv(REPL_ROOT = REPL_ROOT)
DATA_DIR <- file.path(REPL_ROOT, "Data")
dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)

# --- Pipeline steps ----------------------------------------------------------
STEPS <- list(
  list(n = 1, name = "Download NHGIS census + shapefiles",
       file = "Scripts/00_Download_NHGIS.R"),
  list(n = 2, name = "Build county-year database",
       file = "Scripts/01_Database.R"),
  list(n = 3, name = "Normalize to 1850 county boundaries",
       file = "Scripts/02_Normalization.R"),
  list(n = 4, name = "Build farm-value-per-acre geopackages (for QGIS maps)",
       file = "Scripts/05_Make_Geopackages.R"),
  list(n = 5, name = "Event study (Table 1, Figure 1)",
       file = "Scripts/03_Analysis_Event_Study.R"),
  list(n = 6, name = "Spatial RDD + balance tests (Tables 2 and 3)",
       file = "Scripts/04_Analysis_Spatial_RDD.R")
)

run_step <- function(step) {
  cat(sprintf("\n%s\nStep %d: %s\n%s\n",
              strrep("=", 78), step$n, step$name, strrep("=", 78)))
  t0 <- Sys.time()
  source(file.path(REPL_ROOT, step$file), local = new.env())
  dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("\nStep %d done in %.1f s.\n", step$n, dt))
}

# --- Command-line interface --------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

show_list <- function() {
  cat("\nAvailable steps:\n")
  for (s in STEPS) cat(sprintf("  %d. %s\n", s$n, s$name))
}

run_only <- function(ns) {
  for (n in ns) {
    s <- Filter(function(x) x$n == n, STEPS)[[1]]
    if (is.null(s)) stop("Unknown step: ", n)
    run_step(s)
  }
}

run_from <- function(n) {
  for (s in STEPS) if (s$n >= n) run_step(s)
}

run_all <- function() { for (s in STEPS) run_step(s) }

if (length(args) == 0) {
  if (interactive()) {
    show_list()
    sel <- readline(prompt = "\nEnter step number to run (blank = all): ")
    if (!nzchar(sel)) run_all() else run_only(as.integer(strsplit(sel, "[, ]+")[[1]]))
  } else {
    run_all()
  }
} else if ("--list" %in% args) {
  show_list()
} else {
  for (a in args) {
    if (startsWith(a, "--only=")) {
      run_only(as.integer(strsplit(sub("^--only=", "", a), ",")[[1]]))
    } else if (startsWith(a, "--from=")) {
      run_from(as.integer(sub("^--from=", "", a)))
    } else {
      stop("Unknown argument: ", a,
           "\nUsage: Rscript Master.R [--list | --only=N[,M...] | --from=N]")
    }
  }
}

cat("\nMaster.R done. Outputs under Output/.\n")
