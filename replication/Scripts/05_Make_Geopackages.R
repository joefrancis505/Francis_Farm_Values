# =============================================================================
# 05_Make_Geopackages.R — County GeoPackages for QGIS map rendering
# =============================================================================
# Writes one .gpkg per census year (1850-1900) with farm values per acre as a
# county-polygon attribute. The geopackages are intended to be opened in QGIS
# (or another GIS) to render Map 1 (1860) and Map 2 (1900) as deciles, on top
# of the 1820 free-slave state border shapefile.
#
# Reads  {DATA_DIR}/database.csv             (01_Database.R)
#        {DATA_DIR}/Shapefiles/{year}/        (00_Download_NHGIS.R)
#
# Writes {OUTPUT_DIR}/Maps/farmv_acre_{year}.gpkg  for year in 1850..1900
#
# Run from the replication root or via Master.R.
# =============================================================================

packages <- c("sf", "dplyr", "readr")
missing_pkgs <- packages[!packages %in% rownames(installed.packages())]
if (length(missing_pkgs) > 0)
  stop("Missing packages: ", paste(missing_pkgs, collapse = ", "),
       "\nRun install_dependencies.R from the replication root to install.")
invisible(lapply(packages, library, character.only = TRUE))

# ---- Output/ paths ----------------------------------------------------------
.repl_root <- Sys.getenv("REPL_ROOT")
if (!nzchar(.repl_root)) {
  .d <- normalizePath(getwd(), mustWork = FALSE)
  while (!file.exists(file.path(.d, "Master.R")) && .d != dirname(.d)) .d <- dirname(.d)
  .repl_root <- .d
}
source(file.path(.repl_root, "Scripts", "_output_paths.R"))
if (!exists("DATA_DIR")) DATA_DIR <- file.path(.repl_root, "Data")

# --- Helpers ----------------------------------------------------------------

read_counties <- function(yr) {
  shp <- list.files(file.path(DATA_DIR, "Shapefiles", yr),
                    pattern = "\\.shp$", full.names = TRUE)[1]
  if (is.na(shp) || !file.exists(shp)) {
    warning("No shapefile for ", yr); return(NULL)
  }
  st_read(shp, quiet = TRUE) %>% st_make_valid()
}

write_gpkg <- function(sf_obj, layer_name, yr) {
  out <- file.path(MAP_DIR, sprintf("%s_%d.gpkg", layer_name, yr))
  st_write(sf_obj, out, layer = layer_name, delete_dsn = TRUE, quiet = TRUE)
  val_col <- layer_name
  if (val_col %in% names(st_drop_geometry(sf_obj))) {
    n <- sum(!is.na(st_drop_geometry(sf_obj)[[val_col]]))
    cat(sprintf("  %s: %d features\n", basename(out), n))
  } else {
    cat(sprintf("  %s: %d features\n", basename(out), nrow(sf_obj)))
  }
}

# --- Build farm-values-per-acre layers --------------------------------------

cat("Loading database...\n")
db <- read_csv(file.path(DATA_DIR, "database.csv"), show_col_types = FALSE)

cat("\nFarm values per acre by census year:\n")
for (yr in seq(1850, 1900, 10)) {
  counties <- read_counties(yr); if (is.null(counties)) next
  d <- db %>% filter(year == yr)
  result <- counties %>%
    left_join(d, by = "GISJOIN") %>%
    mutate(farmv_acre = ifelse(improved + unimproved > 0,
                               farmv_total / (improved + unimproved),
                               NA_real_)) %>%
    select(GISJOIN, farmv_acre)
  write_gpkg(result, "farmv_acre", yr)
}

cat("\nGeoPackages written to", MAP_DIR, "\n")
cat("Open the 1860 and 1900 layers in QGIS, classify farmv_acre into deciles,\n")
cat("and overlay Data/Border/1820_border/1820_border.shp to reproduce Map 1\n")
cat("and Map 2 from the paper.\n")
