# =============================================================================
# 02_Normalization.R — Normalize panel to 1850 county boundaries
# =============================================================================
# Reads  {DATA_DIR}/database.csv + shapefiles
# Writes {DATA_DIR}/panel_data.csv  and  {DATA_DIR}/Normalized/{year}_normalized.csv
#
# Run from the replication root. DATA_DIR defaults to "Data".
# =============================================================================

packages <- c("sf", "stars", "fasterize", "dplyr", "readr",
              "exactextractr", "terra", "purrr")
missing_pkgs <- packages[!packages %in% rownames(installed.packages())]
if (length(missing_pkgs) > 0)
  stop("Missing packages: ", paste(missing_pkgs, collapse = ", "),
       "\nRun install_dependencies.R from the replication root to install.")
invisible(lapply(packages, library, character.only = TRUE))

if (!exists("DATA_DIR")) DATA_DIR <- "Data"
dir.create(file.path(DATA_DIR, "Normalized"), showWarnings = FALSE, recursive = TRUE)

# --- Cache helpers -----------------------------------------------------------
# Skip rebuilding an intermediate if (a) the cache exists, (b) it has every
# expected column, and (c) it is newer than every upstream input. Any failed
# check triggers a full recompute. Each check logs what it's doing so a stale
# cache is visible.

cache_is_valid <- function(cache_path, expected_cols, input_paths) {
  if (!file.exists(cache_path)) {
    cat("  cache miss:", basename(cache_path), "does not exist\n")
    return(FALSE)
  }
  hdr <- tryCatch(
    names(read_csv(cache_path, n_max = 0, show_col_types = FALSE)),
    error = function(e) character(0))
  missing <- setdiff(expected_cols, hdr)
  if (length(missing) > 0) {
    cat("  cache stale:", basename(cache_path),
        "missing columns:", paste(missing, collapse = ", "), "\n")
    return(FALSE)
  }
  cache_mtime <- file.mtime(cache_path)
  inputs <- input_paths[file.exists(input_paths)]
  if (length(inputs) > 0) {
    newest_input <- max(file.mtime(inputs))
    if (newest_input > cache_mtime) {
      cat("  cache stale:", basename(cache_path),
          "older than upstream inputs\n")
      return(FALSE)
    }
  }
  cat("  cache hit:", basename(cache_path), "\n")
  TRUE
}

fix_geometries <- function(sf_object) sf_object %>% st_make_valid() %>% st_buffer(0)

calculate_densities <- function(sf_object) {
  sf_object %>%
    mutate(across(c(farmv_total, improved, unimproved, enslaved, black, census_pop,
                    cotton, corn, livestock_val, cotton_acreage),
                  ~ . / SHAPE_AREA,
                  .names = "{.col}_density"))
}

rasterize_data <- function(sf_object, resolution = 200) {
  bbox       <- st_bbox(sf_object)
  crs_proj4  <- st_crs(sf_object)$proj4string
  template   <- rast(xmin=bbox["xmin"], xmax=bbox["xmax"],
                     ymin=bbox["ymin"], ymax=bbox["ymax"],
                     resolution=resolution)
  crs(template) <- crs_proj4

  variables <- c("farmv_total_density","improved_density","unimproved_density",
                 "enslaved_density","black_density","census_pop_density","cotton_density",
                 "corn_density","livestock_val_density","cotton_acreage_density")

  raster_list <- map(variables, function(v) {
    if (v %in% names(sf_object)) {
      sf_object[[v]] <- as.numeric(sf_object[[v]])
      terra::rasterize(vect(sf_object), template, field = v, fun = "sum")
    }
  })
  raster_list <- raster_list[!sapply(raster_list, is.null)]
  if (length(raster_list) == 0) stop("No variables rasterized.")
  stack <- do.call(c, raster_list)
  names(stack) <- variables[sapply(map(variables, function(v) v %in% names(sf_object)), isTRUE)]
  stack
}

zonal_stats <- function(raster_stack, zones_sf) {
  exact_extract(raster_stack, zones_sf, fun = "mean",
                append_cols = c("GISJOIN","STATENAM"))
}

convert_to_absolute <- function(zonal_stats_df, zones_sf) {
  zones_sf %>%
    left_join(zonal_stats_df, by = c("GISJOIN","STATENAM")) %>%
    mutate(
      farmv_total    = mean.farmv_total_density    * SHAPE_AREA,
      improved       = mean.improved_density       * SHAPE_AREA,
      unimproved     = mean.unimproved_density     * SHAPE_AREA,
      enslaved       = mean.enslaved_density       * SHAPE_AREA,
      black          = mean.black_density          * SHAPE_AREA,
      census_pop     = mean.census_pop_density     * SHAPE_AREA,
      cotton         = mean.cotton_density         * SHAPE_AREA,
      corn           = mean.corn_density           * SHAPE_AREA,
      livestock_val  = mean.livestock_val_density  * SHAPE_AREA,
      cotton_acreage = mean.cotton_acreage_density * SHAPE_AREA,
      state          = STATENAM
    ) %>%
    select(-starts_with("mean."), -STATENAM)
}

calculate_derived_variables <- function(df) {
  df %>% mutate(
    area       = SHAPE_AREA / 2589988.11,
    farmv      = ifelse(improved + unimproved > 0,
                        farmv_total / (improved + unimproved), NA),
    pc_enslaved = ifelse(census_pop > 0, (enslaved / census_pop) * 100, NA),
    pc_black    = ifelse(census_pop > 0, (black / census_pop) * 100, NA),
    cotton_pc  = ifelse(census_pop > 0, cotton / census_pop, NA),
    ccratio    = ifelse(corn > 0, cotton / corn, NA),
    cotton_share = ifelse(improved + unimproved > 0,
                          cotton_acreage / (improved + unimproved), NA)
  )
}

process_year <- function(year, census_data, zones_1850) {
  if (year != "1850") {
    shp_dir   <- file.path(DATA_DIR, "Shapefiles", year)
    shp_files <- list.files(shp_dir, pattern = "\\.shp$", full.names = TRUE)
    counties  <- st_read(shp_files[1], quiet = TRUE) %>%
      fix_geometries() %>%
      left_join(census_data, by = "GISJOIN") %>%
      filter(census_pop > 0) %>%
      mutate(livestock_val  = replace(livestock_val,  is.na(livestock_val),  0),
             cotton_acreage = replace(cotton_acreage, is.na(cotton_acreage), 0)) %>%
      calculate_densities()
    raster_data  <- rasterize_data(counties)
    zonal_result <- zonal_stats(raster_data, zones_1850)
    final        <- convert_to_absolute(zonal_result, zones_1850)
  } else {
    final <- zones_1850 %>%
      left_join(census_data, by = "GISJOIN") %>%
      mutate(state = STATENAM,
             livestock_val  = replace(livestock_val,  is.na(livestock_val),  0),
             cotton_acreage = replace(cotton_acreage, is.na(cotton_acreage), 0)) %>%
      select(-STATENAM)
  }
  final %>% calculate_derived_variables() %>% st_drop_geometry()
}

rasterize_yield_1880 <- function(zones_1850) {
  cat("Rasterizing 1880 cotton yield onto 1850 boundaries...\n")
  census_1880 <- read_csv(file.path(DATA_DIR, "database.csv"),
                          show_col_types = FALSE) %>% filter(year == 1880)
  shp_1880    <- list.files(file.path(DATA_DIR, "Shapefiles/1880"),
                            pattern = "\\.shp$", full.names = TRUE)[1]
  counties_1880 <- st_read(shp_1880, quiet = TRUE) %>%
    fix_geometries() %>%
    left_join(census_1880, by = "GISJOIN") %>%
    mutate(cotton_acreage = replace(cotton_acreage, is.na(cotton_acreage), 0),
           yield_1880 = ifelse(cotton_acreage > 0 & !is.na(cotton),
                               cotton / cotton_acreage, NA_real_))
  bbox     <- st_bbox(counties_1880)
  template <- rast(xmin=bbox["xmin"], xmax=bbox["xmax"],
                   ymin=bbox["ymin"], ymax=bbox["ymax"], resolution=200)
  crs(template)  <- st_crs(counties_1880)$proj4string
  yield_rast     <- terra::rasterize(vect(counties_1880), template,
                                     field = "yield_1880", fun = "mean")
  zones_crs      <- st_transform(zones_1850, st_crs(counties_1880))
  data.frame(GISJOIN    = zones_1850$GISJOIN,
             yield_1880 = exact_extract(yield_rast, zones_crs, fun = "mean"))
}

# --- Main -------------------------------------------------------------------

years     <- seq(1850, 1900, by = 10)
zones_1850 <- st_read(
  list.files(file.path(DATA_DIR, "Shapefiles/1850"),
             pattern = "\\.shp$", full.names = TRUE)[1],
  quiet = TRUE) %>% fix_geometries()

census_data <- read_csv(file.path(DATA_DIR, "database.csv"), show_col_types = FALSE)

required_year_cols <- c("GISJOIN", "state", "farmv_total", "improved",
                        "unimproved", "enslaved", "black", "census_pop",
                        "cotton", "corn", "livestock_val", "cotton_acreage",
                        "area", "farmv", "pc_enslaved", "pc_black",
                        "cotton_pc", "ccratio", "cotton_share")
database_path <- file.path(DATA_DIR, "database.csv")

for (year in years) {
  year_csv <- file.path(DATA_DIR, "Normalized",
                        sprintf("%d_normalized.csv", year))
  shp_year <- list.files(file.path(DATA_DIR, "Shapefiles",
                                   as.character(year)),
                         pattern = "\\.shp$", full.names = TRUE)
  cat("Processing", year, "...\n")
  if (cache_is_valid(year_csv, required_year_cols,
                     c(database_path, shp_year))) {
    next
  }
  result <- process_year(as.character(year),
                         filter(census_data, year == !!year), zones_1850)
  write_csv(result, year_csv)
}

panel_data <- map_dfr(years, function(year) {
  read_csv(file.path(DATA_DIR, "Normalized",
                     sprintf("%d_normalized.csv", year)),
           show_col_types = FALSE) %>% mutate(year = year)
})

# Impute pre-1880 cotton acreage. The rasterize_yield_1880() step builds a
# 200 m US-wide raster and is the single most memory-intensive step in the
# pipeline (~30-40 GB RSS at peak). Cache the GISJOIN -> yield_1880 map so
# re-runs skip it.
yield_cache <- file.path(DATA_DIR, "yield_1880_by_1850_gisjoin.csv")
shp_1880_path <- list.files(file.path(DATA_DIR, "Shapefiles/1880"),
                            pattern = "\\.shp$", full.names = TRUE)
cat("Checking 1880 yield cache...\n")
if (cache_is_valid(yield_cache, c("GISJOIN", "yield_1880"),
                   c(database_path, shp_1880_path))) {
  yield_1850 <- read_csv(yield_cache, show_col_types = FALSE)
} else {
  yield_1850 <- rasterize_yield_1880(zones_1850)
  write_csv(yield_1850, yield_cache)
  cat("Cached 1880 yield to", yield_cache, "\n")
}
panel_data <- panel_data %>%
  left_join(yield_1850, by = "GISJOIN") %>%
  mutate(cotton_acreage = ifelse(
    year < 1880 & (is.na(cotton_acreage) | cotton_acreage == 0) &
      !is.na(yield_1880) & yield_1880 > 0,
    cotton / yield_1880, cotton_acreage)) %>%
  select(-yield_1880) %>%
  mutate(cotton_share = ifelse(improved + unimproved > 0,
                               cotton_acreage / (improved + unimproved), NA))
cat("Imputed pre-1880 cotton acreage from spatially-transferred 1880 yields.\n")

# National average farm values
national_avg <- panel_data %>%
  filter(!(state %in% c("Alaska Territory","Hawaii Territory"))) %>%
  group_by(year) %>%
  summarize(total_farmv = sum(farmv_total, na.rm = TRUE),
            total_land  = sum(improved + unimproved, na.rm = TRUE),
            national_avg_farmv = total_farmv / total_land, .groups = "drop")

panel_data <- panel_data %>%
  left_join(national_avg %>% select(year, national_avg_farmv), by = "year") %>%
  mutate(farmv_na = farmv / national_avg_farmv * 100) %>%
  select(-national_avg_farmv)

# Add centroid coordinates
centroids_1850 <- st_centroid(zones_1850)
coords_1850    <- st_coordinates(centroids_1850)
coords_df      <- data.frame(
  GISJOIN   = zones_1850$GISJOIN,
  longitude = coords_1850[,1] / 1609.34,
  latitude  = coords_1850[,2] / 1609.34
)
panel_data <- panel_data %>% left_join(coords_df, by = "GISJOIN")
write_csv(panel_data, file.path(DATA_DIR, "panel_data.csv"))
cat(sprintf("Panel data saved to %s\n", file.path(DATA_DIR, "panel_data.csv")))
