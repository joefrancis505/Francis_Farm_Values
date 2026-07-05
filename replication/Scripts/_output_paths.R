# =============================================================================
# _output_paths.R — shared output directories for the replication package.
# Every figure, table, map, and numbers CSV is written under Output/ at the
# replication root. Scripts source this file after resolving .repl_root.
# =============================================================================

# Resolve the replication root. When this file is sourced from a step script
# that Master.R runs in its own environment, `.repl_root` may not be visible in
# the scope source() evaluates in; fall back to the REPL_ROOT env var, which
# Master.R always exports.
if (!exists(".repl_root") || !nzchar(.repl_root)) {
  .repl_root <- Sys.getenv("REPL_ROOT")
}
if (!nzchar(.repl_root)) {
  stop("_output_paths.R requires .repl_root or the REPL_ROOT env var to be set.\n",
       "  Run via Master.R, or set REPL_ROOT before sourcing stand-alone.")
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
