# =============================================================================
# _output_paths.R — shared output directories for the replication package.
# Every figure, table, map, and numbers CSV is written under Output/ at the
# replication root. Scripts source this file after resolving .repl_root.
# =============================================================================

if (!exists(".repl_root") || !nzchar(.repl_root)) {
  stop("_output_paths.R requires .repl_root to be set.\n",
       "  Run via Master.R, or set REPL_ROOT env var before sourcing stand-alone.")
}

OUTPUT_ROOT <- file.path(.repl_root, "Output")
FIG_DIR     <- file.path(OUTPUT_ROOT, "Figures")
TAB_DIR     <- file.path(OUTPUT_ROOT, "Tables")
MAP_DIR     <- file.path(OUTPUT_ROOT, "Maps")
NUM_DIR     <- file.path(OUTPUT_ROOT, "Numbers")

for (.d in c(FIG_DIR, TAB_DIR, MAP_DIR, NUM_DIR)) {
  dir.create(.d, recursive = TRUE, showWarnings = FALSE)
}
rm(.d)
