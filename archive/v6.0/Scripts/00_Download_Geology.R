# =============================================================================
# 00_Download_Geology.R — Download elevation, slope, ruggedness & soil data
# =============================================================================
# Downloads SRTM15+ elevation (OpenTopography), computes slope and ruggedness
# (TRI), and downloads ISRIC SoilGrids texture (clay/sand/silt). Produces
# county-level zonal statistics for every census decade 1790–1900.
#
# Requires OPENTOPO_KEY (or OPENTOPOGRAPHY_API_KEY) in environment.
# Run from the replication root. DATA_DIR defaults to "Data".
# =============================================================================

options(timeout = 3600)

packages <- c("sf","terra","exactextractr","elevatr","httr","dplyr","readr")
missing_pkgs <- packages[!packages %in% rownames(installed.packages())]
if (length(missing_pkgs) > 0)
  stop("Missing packages: ", paste(missing_pkgs, collapse = ", "),
       "\nRun install_dependencies.R from the replication root to install.")
invisible(lapply(packages, library, character.only = TRUE))

if (!exists("DATA_DIR")) DATA_DIR <- "Data"

YEARS      <- seq(1790, 1900, by = 10)
CACHE      <- file.path(DATA_DIR, "Geology", "_rasters")
OUT_DIRS   <- list(
  slope      = file.path(DATA_DIR, "Geology", "USGS_slope"),
  elevation  = file.path(DATA_DIR, "Geology", "USGS_elevation"),
  ruggedness = file.path(DATA_DIR, "Geology", "USGS_ruggedness"),
  soilgrids  = file.path(DATA_DIR, "Geology", "Soilgrids")
)
CONUS      <- c(xmin=-125, ymin=24, xmax=-66, ymax=50)
ALBERS_CRS <- "EPSG:5070"
# NOTE: SoilGrids v2.0 WCS endpoint — live API, not Wayback-archived.
# Data version: accessed 2026-04. Future runs may retrieve an updated model.
SG_WCS     <- "https://maps.isric.org/mapserv"
SG_VARS    <- c("clay","sand","silt")
SG_DEPTHS  <- c("0-5cm","5-15cm","15-30cm")
TILE_DEG   <- 10

for (d in c(CACHE, unlist(OUT_DIRS)))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# --- Elevation ---------------------------------------------------------------

download_elevation <- function() {
  cat("\n=== SRTM15+ Elevation (OpenTopography) ===\n")
  p <- file.path(CACHE, "elevation_conus.tif")
  if (file.exists(p) && file.size(p) > 0) { cat("  Cached.\n"); return(terra::rast(p)) }
  key <- Sys.getenv("OPENTOPO_KEY")
  if (!nzchar(key)) key <- Sys.getenv("OPENTOPOGRAPHY_API_KEY")
  if (!nzchar(key)) key <- Sys.getenv("OpenTopography_API")
  if (!nzchar(key)) stop("Set OPENTOPO_KEY or OpenTopography_API env var.")
  conus_df  <- data.frame(x=c(CONUS["xmin"],CONUS["xmax"]),
                          y=c(CONUS["ymin"],CONUS["ymax"]))
  cat("  Downloading SRTM15+...\n")
  # SRTM15+ (OpenTopography live API, not Wayback-archived; accessed 2026-04).
  elev_r    <- get_elev_raster(locations=conus_df, prj="EPSG:4326",
                               src="srtm15plus", clip="bbox")
  elev_sr   <- terra::rast(elev_r)
  terra::writeRaster(elev_sr, p, overwrite=TRUE)
  cat("  Saved:", p, "\n")
  elev_sr
}

compute_slope <- function(elev) {
  cat("\n=== Slope from DEM ===\n")
  p <- file.path(CACHE, "slope_conus.tif")
  if (file.exists(p) && file.size(p) > 0) { cat("  Cached.\n"); return(terra::rast(p)) }
  r <- terra::terrain(terra::project(elev, ALBERS_CRS), v="slope", unit="degrees")
  terra::writeRaster(r, p, overwrite=TRUE); r
}

compute_ruggedness <- function(elev) {
  cat("\n=== Ruggedness (TRI) from DEM ===\n")
  p <- file.path(CACHE, "ruggedness_conus.tif")
  if (file.exists(p) && file.size(p) > 0) { cat("  Cached.\n"); return(terra::rast(p)) }
  r <- terra::terrain(terra::project(elev, ALBERS_CRS), v="TRI")
  terra::writeRaster(r, p, overwrite=TRUE); r
}

# --- SoilGrids ---------------------------------------------------------------

download_sg_tile <- function(variable, depth, xmin, ymin, xmax, ymax,
                             dest, retries = 4) {
  if (file.exists(dest) && file.size(dest) > 1000) return(dest)
  url <- sprintf(
    paste0("%s?map=/map/%s.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage",
           "&COVERAGEID=%s_%s_mean&FORMAT=image/tiff",
           "&SUBSET=long(%s,%s)&SUBSET=lat(%s,%s)",
           "&SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326",
           "&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326"),
    SG_WCS, variable, variable, depth, xmin, xmax, ymin, ymax)
  for (attempt in seq_len(retries)) {
    tryCatch({
      resp <- GET(url, timeout(180))
      ct   <- headers(resp)[["content-type"]]
      if (grepl("tiff|image", ct, ignore.case=TRUE)) {
        writeBin(content(resp, "raw"), dest); return(dest)
      }
    }, error = function(e) {
      cat(sprintf("      Tile attempt %d/%d: %s\n", attempt, retries, conditionMessage(e)))
    })
    if (attempt < retries) Sys.sleep(5 * attempt)
  }
  NULL
}

download_all_soilgrids <- function() {
  cat("\n=== SoilGrids WCS Downloads ===\n")
  paths <- list()
  for (var in SG_VARS) {
    for (depth in SG_DEPTHS) {
      key <- paste(var, depth, sep="_")
      cat(sprintf("  %s %s...\n", var, depth))
      ds  <- gsub("-","_", gsub("cm","", depth))
      mp  <- file.path(CACHE, sprintf("sg_%s_%s.tif", var, ds))
      if (file.exists(mp) && file.size(mp) > 0) { paths[[key]] <- mp; next }
      td  <- file.path(CACHE, "sg_tiles", paste0(var,"_",ds))
      dir.create(td, recursive=TRUE, showWarnings=FALSE)
      xs  <- seq(CONUS["xmin"], CONUS["xmax"]-1, by=TILE_DEG)
      ys  <- seq(CONUS["ymin"], CONUS["ymax"]-1, by=TILE_DEG)
      tiles <- character()
      for (x0 in xs) for (y0 in ys) {
        t <- download_sg_tile(var, depth, x0, y0,
                              min(x0+TILE_DEG, CONUS["xmax"]),
                              min(y0+TILE_DEG, CONUS["ymax"]),
                              file.path(td, sprintf("tile_%d_%d.tif", x0, y0)))
        if (!is.null(t)) tiles <- c(tiles, t)
      }
      if (length(tiles) > 0) {
        rasters <- lapply(tiles, terra::rast)
        merged  <- if (length(rasters)==1) rasters[[1]] else
          terra::merge(terra::sprc(rasters))
        terra::writeRaster(merged, mp, overwrite=TRUE)
        paths[[key]] <- mp
      }
    }
  }
  paths
}

# --- Zonal stats -------------------------------------------------------------

zonal_mean <- function(r, counties_sf, col_name) {
  if (is.character(r)) r <- terra::rast(r)
  if (st_crs(counties_sf)$wkt != terra::crs(r))
    counties_sf <- st_transform(counties_sf, terra::crs(r))
  data.frame(GISJOIN = counties_sf$GISJOIN,
             value   = exact_extract(r, counties_sf, fun="mean"),
             stringsAsFactors = FALSE) %>%
    rename(!!col_name := value)
}

save_csv <- function(df, path) {
  if (is.null(df) || nrow(df)==0) return(invisible(NULL))
  write_csv(df, path)
  cat(sprintf("    Saved: %s (%d counties)\n", basename(path), sum(!is.na(df[[2]]))))
}

# --- Per-year processing -----------------------------------------------------

process_year <- function(year, slope_r, elev_r, rug_r, sg_paths) {
  cat(sprintf("\nProcessing year %d\n", year))
  shp      <- list.files(file.path(DATA_DIR, "Shapefiles", year),
                         pattern="\\.shp$", full.names=TRUE)[1]
  counties <- st_read(shp, quiet=TRUE) %>% st_make_valid()

  if (!is.null(slope_r))
    save_csv(zonal_mean(slope_r, counties, "slope_mean"),
             file.path(OUT_DIRS$slope, sprintf("slope_%d.csv", year)))
  if (!is.null(elev_r))
    save_csv(zonal_mean(elev_r, counties, "elevation_mean"),
             file.path(OUT_DIRS$elevation, sprintf("elevation_%d.csv", year)))
  if (!is.null(rug_r))
    save_csv(zonal_mean(rug_r, counties, "ruggedness_mean"),
             file.path(OUT_DIRS$ruggedness, sprintf("ruggedness_%d.csv", year)))

  for (var in SG_VARS) {
    cat(sprintf("  %s...\n", tools::toTitleCase(var)))
    depth_dfs <- list()
    for (depth in SG_DEPTHS) {
      rp <- sg_paths[[paste(var, depth, sep="_")]]
      if (!is.null(rp) && file.exists(rp))
        depth_dfs[[depth]] <- zonal_mean(rp, counties, depth)
    }
    if (length(depth_dfs) > 0) {
      merged  <- Reduce(function(a,b) full_join(a,b,by="GISJOIN"), depth_dfs)
      dcols   <- setdiff(names(merged), "GISJOIN")
      weights <- c(5, 10, 15)
      col_name <- paste0(var, "_mean")
      merged[[col_name]] <- rowSums(
        mapply(function(c,w) merged[[c]]*w, dcols, weights), na.rm=TRUE) / sum(weights)
      save_csv(merged[c("GISJOIN", col_name)],
               file.path(OUT_DIRS$soilgrids, sprintf("%s_%d.csv", var, year)))
    }
  }
}

# --- Main --------------------------------------------------------------------

for (yr in YEARS) {
  d <- file.path(DATA_DIR, "Shapefiles", yr)
  if (!dir.exists(d) || length(list.files(d, pattern="\\.shp$"))==0)
    stop(sprintf("No shapefiles in %s. Run 00_Download_NHGIS.R first.", d))
}

elev_r    <- download_elevation()
slope_r   <- compute_slope(elev_r)
rug_r     <- compute_ruggedness(elev_r)
sg_paths  <- download_all_soilgrids()

for (yr in YEARS) process_year(yr, slope_r, elev_r, rug_r, sg_paths)

cat("\n00_Download_Geology.R complete.\n")
