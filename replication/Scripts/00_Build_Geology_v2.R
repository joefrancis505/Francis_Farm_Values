# =============================================================================
# 00_Build_Geology_v2.R
# Definitive, reproducible topography + soil covariates for the Farm-Values
# spatial RDD. Replaces the non-reproducible manual-QGIS slope build and the
# drifting SRTM15+/SoilGrids downloads.
#
# SOURCES (pinned, citable):
#   Topography (elevation, slope, ruggedness):
#       USGS 3DEP / National Elevation Dataset, 1 arc-second (~30 m).
#       Fetched via FedData::get_ned(). Slope/TRI computed AFTER projecting to
#       USA Contiguous Albers Equal Area (EPSG:5070); slope in degrees.
#   Soil (sand/silt/clay fractions):
#       Miller & White (1998), CONUS-SOIL, derived from USDA STATSGO.
#       dbwww.essc.psu.edu/dbtop/.link/1997-0009 (Geographic/NAD27 grid,
#       30 arc-second, 11 standard layers, integer percent).
#       Topsoil 0-30 cm = depth-weighted mean of layers 1-4 (thicknesses 2,2,4,4 in).
#       Same soil source as Bleakley & Rhode (2024).
#
# County sample matches 04: territories + Kansas/Nebraska excluded (44 states).
# State-by-state processing keeps the 30 m DEM out of continent-wide memory;
# resumable via per-state .rds checkpoints.
#
# USAGE:
#   Rscript 00_Build_Geology_v2.R                 # single process: all states, then assemble
#   Rscript 00_Build_Geology_v2.R <i> <N>         # worker i of N: process a slice of states
#   Rscript 00_Build_Geology_v2.R assemble        # read all partials -> write canonical CSVs
#
# Author: build script for Joseph Francis, King Cotton / Farm-Values. 2026-06-09.
# =============================================================================

suppressMessages({
  library(sf); library(terra); library(exactextractr)
  library(FedData); library(readr); library(dplyr); library(purrr)
})
sf_use_s2(FALSE)

ARGS   <- commandArgs(trailingOnly = TRUE)
MODE   <- if (length(ARGS) >= 1 && ARGS[1] == "assemble") "assemble" else "worker"
WORKER <- if (MODE == "worker" && length(ARGS) >= 2) as.integer(ARGS[1]) else 1L
NWORK  <- if (MODE == "worker" && length(ARGS) >= 2) as.integer(ARGS[2]) else 1L
SINGLE <- (MODE == "worker" && length(ARGS) == 0)   # bare run: process all + assemble

# ---- paths ------------------------------------------------------------------
# Repository root: walk up from the working directory to the dir holding
# Master.R (works when run from the root or from Scripts/).
PKG <- normalizePath(getwd(), mustWork = TRUE)
while (!file.exists(file.path(PKG, "Master.R")) && PKG != dirname(PKG))
  PKG <- dirname(PKG)
if (!file.exists(file.path(PKG, "Master.R")))
  stop("Run from inside the replication package (Master.R not found above getwd()).")
# NHGIS shapefiles as unpacked by 00_Download_NHGIS.R into Data/IPUMS_Raw/;
# the US_county_{year}.shp files are found by recursive search below, so the
# extract number in the folder name does not matter.
SHP_DIR   <- file.path(PKG, "Data/IPUMS_Raw")
SOIL_DIR  <- Sys.getenv("FV_SOIL_DIR", "/tmp/conus_soil")
OUT_DIR   <- file.path(PKG, "Data/Geology_v2")
CACHE_DIR <- file.path(Sys.getenv("FV_CACHE_ROOT", "/tmp"), sprintf("ned_cache_w%d", WORKER))
WORK_DIR  <- file.path(OUT_DIR, "_state_partials")
for (d in c(OUT_DIR, CACHE_DIR, WORK_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
terraOptions(memfrac = 0.4)
# Optional scratch redirect for the large NED reprojection spill (keeps the
# committed default on the system tempdir; set FV_TERRA_TMP to an external
# volume when the internal disk is tight).
FV_TERRA_TMP <- Sys.getenv("FV_TERRA_TMP", "")
if (nzchar(FV_TERRA_TMP)) {
  dir.create(FV_TERRA_TMP, recursive = TRUE, showWarnings = FALSE)
  terraOptions(tempdir = FV_TERRA_TMP)
}

ALBERS <- "EPSG:5070"
YEARS  <- c(1820, 1840, 1850, 1860, 1870, 1880, 1890, 1900)
VARS   <- c("elevation_mean","slope_mean","ruggedness_mean","sand_mean","silt_mean","clay_mean")
EXCLUDE <- c("Kansas Territory","Nebraska Territory","Indian Territory","Unorganized Territory",
             "Kansas","Nebraska","New Mexico Territory","Utah Territory","Washington Territory",
             "Colorado Territory","Dakota Territory","Nevada Territory","Arizona Territory",
             "Idaho Territory","Montana Territory","Wyoming Territory","Alaska","Hawaii",
             "Alaska Territory","Hawaii Territory")

# ---- soil: topsoil (0-30 cm) sand/silt/clay (built unless pure-assemble) -----
build_soil_topsoil <- function(var) {
  v <- readBin(file.path(SOIL_DIR, paste0(var, ".img")), "integer",
               n = 2984L*6936L*11L, size = 1, signed = FALSE)
  N <- 6936L*2984L
  r <- rast(nrows = 2984, ncols = 6936, nlyrs = 4,
            xmin = -124.75, xmax = -66.95, ymin = 24.533333, ymax = 49.40, crs = "EPSG:4267")
  for (b in 1:4) values(r[[b]]) <- as.vector(matrix(v[((b-1)*N+1):(b*N)], nrow = 6936, ncol = 2984))
  w <- c(2,2,4,4)
  top <- (r[[1]]*w[1] + r[[2]]*w[2] + r[[3]]*w[3] + r[[4]]*w[4]) / sum(w)
  top[top > 100] <- NA
  names(top) <- paste0(var, "_mean"); top
}

load_year <- function(yr) {
  f <- list.files(SHP_DIR, pattern = sprintf("US_county_%d\\.shp$", yr), recursive = TRUE, full.names = TRUE)[1]
  x <- st_read(f, quiet = TRUE) |> st_make_valid() |> st_transform(4326)
  x$GISJOIN <- as.character(x$GISJOIN); x$year <- yr; x
}

# =============================== ASSEMBLE ====================================
if (MODE == "assemble") {
  cat("Assembling canonical CSVs from state partials ...\n")
  parts <- list.files(WORK_DIR, pattern = "\\.rds$", full.names = TRUE)
  cat(sprintf("  %d state partials found\n", length(parts)))
  acc <- setNames(lapply(YEARS, function(y) setNames(vector("list", length(VARS)), VARS)), as.character(YEARS))
  for (p in parts) {
    so <- readRDS(p)
    for (y in names(so)) { df <- so[[y]]
      for (v in VARS) acc[[y]][[v]] <- c(acc[[y]][[v]], setNames(df[[v]], df$GISJOIN)) }
  }
  GEO  <- list(elevation_mean="USGS_elevation/elevation", slope_mean="USGS_slope/slope",
               ruggedness_mean="USGS_ruggedness/ruggedness")
  SOIL <- list(sand_mean="Soil/sand", silt_mean="Soil/silt", clay_mean="Soil/clay")
  OUT  <- c(GEO, SOIL)
  for (v in names(OUT)) dir.create(file.path(OUT_DIR, dirname(OUT[[v]])), recursive = TRUE, showWarnings = FALSE)
  for (y in as.character(YEARS)) for (v in names(OUT)) {
    vec <- acc[[y]][[v]]; if (is.null(vec) || !length(vec)) next
    vec <- vec[!duplicated(names(vec))]
    df <- setNames(data.frame(GISJOIN = names(vec), x = as.numeric(vec)), c("GISJOIN", v))
    write_csv(df, file.path(OUT_DIR, sprintf("%s_%s.csv", OUT[[v]], y)))
  }
  writeLines(c(
    "CONUS-SOIL + 3DEP topography covariates for Farm-Values spatial RDD",
    sprintf("Built: %s", format(Sys.time())), "",
    "TOPOGRAPHY: USGS 3DEP / NED 1 arc-second (~30 m) via FedData::get_ned().",
    "  Slope (deg) and ruggedness (TRI) computed after projection to EPSG:5070.",
    "  Elevation = mean of native NED (m). County means via exactextractr.",
    "", "SOIL: Miller & White (1998) CONUS-SOIL, USDA STATSGO; 30 arc-sec, NAD27,",
    "  dbwww.essc.psu.edu/dbtop/.link/1997-0009. Topsoil 0-30 cm = depth-weighted",
    "  mean of layers 1-4 (2,2,4,4 in). Same soil source as Bleakley & Rhode (2024).",
    "  Non-soil cells (sand=silt=clay=0, i.e. water/no-data) masked NA before",
    "  county averaging; county means are over valid soil cells only.",
    "", "Sample matches 04 (territories + KS/NE excluded). Derived CSVs are shipped",
    "as canonical analysis inputs (pinned against upstream API drift); regenerate",
    "with Scripts/00_Build_Geology_v2.R."), file.path(OUT_DIR, "PROVENANCE.txt"))
  cat("Assemble done.\n"); quit(save = "no")
}

# =============================== WORKER ======================================
cat("Ingesting CONUS-SOIL topsoil sand/silt/clay ...\n")
SOIL <- list(sand = build_soil_topsoil("sand"), silt = build_soil_topsoil("silt"), clay = build_soil_topsoil("clay"))
# Water/no-data mask: CONUS-SOIL codes non-soil cells (water bodies, etc.) as 0
# in all three fraction layers. A mineral soil cannot be 0% sand, 0% silt, and
# 0% clay simultaneously, so cells where the three topsoil fractions are all
# zero are masked NA jointly; county means are then taken over valid soil cells
# only (exact_extract "mean" ignores NA), and counties with no soil cells at
# all come out NA rather than spuriously zero.
.soil_tot <- SOIL$sand + SOIL$silt + SOIL$clay
for (.v in names(SOIL)) SOIL[[.v]] <- mask(SOIL[[.v]], .soil_tot <= 0, maskvalues = TRUE)
rm(.soil_tot)

counties <- lapply(YEARS, load_year); names(counties) <- as.character(YEARS)
all_states <- counties |> map(~ st_drop_geometry(.x) |> distinct(STATENAM)) |> bind_rows() |>
  distinct(STATENAM) |> pull(STATENAM)
all_states <- all_states[!is.na(all_states) & all_states != "" &
                         !(all_states %in% EXCLUDE) & !grepl("Territory", all_states)]
all_states <- sort(all_states)
if (NWORK > 1) all_states <- all_states[(seq_along(all_states) - 1L) %% NWORK == (WORKER - 1L)]
cat(sprintf("[worker %d/%d] %d states: %s\n", WORKER, NWORK, length(all_states), paste(all_states, collapse=", ")))

process_state <- function(stt) {
  partial <- file.path(WORK_DIR, paste0(gsub("[^A-Za-z0-9]","_",stt), ".rds"))
  if (file.exists(partial)) { cat(sprintf("  [skip] %s\n", stt)); return(invisible()) }
  geoms <- lapply(YEARS, function(y) { cy <- counties[[as.character(y)]]; cy[cy$STATENAM == stt, ] })
  geoms <- geoms[vapply(geoms, nrow, 1L) > 0]; if (!length(geoms)) return(invisible())
  tmpl <- do.call(rbind, lapply(geoms, function(g) g[,"GISJOIN"]))
  bb <- st_as_sfc(st_bbox(st_buffer(st_union(tmpl), 0.25)))
  ned <- tryCatch(get_ned(template = vect(st_as_sf(bb)), label = gsub("[^A-Za-z0-9]","",stt), extraction.dir = CACHE_DIR),
                  error = function(e){ cat("    get_ned ERROR:", conditionMessage(e), "\n"); NULL })
  if (is.null(ned)) return(invisible())
  alb <- project(ned, ALBERS); slp <- terrain(alb, v="slope", unit="degrees"); tri <- terrain(alb, v="TRI")
  out <- list()
  for (y in YEARS) {
    cy <- counties[[as.character(y)]]; cy <- cy[cy$STATENAM == stt, ]; if (!nrow(cy)) next
    cy_a <- st_transform(cy, ALBERS)
    out[[as.character(y)]] <- data.frame(
      GISJOIN = cy$GISJOIN,
      elevation_mean  = exact_extract(alb, cy_a, "mean", progress = FALSE),
      slope_mean      = exact_extract(slp, cy_a, "mean", progress = FALSE),
      ruggedness_mean = exact_extract(tri, cy_a, "mean", progress = FALSE),
      sand_mean = exact_extract(SOIL$sand, st_transform(cy, crs(SOIL$sand)), "mean", progress = FALSE),
      silt_mean = exact_extract(SOIL$silt, st_transform(cy, crs(SOIL$silt)), "mean", progress = FALSE),
      clay_mean = exact_extract(SOIL$clay, st_transform(cy, crs(SOIL$clay)), "mean", progress = FALSE),
      stringsAsFactors = FALSE)
  }
  saveRDS(out, partial)
  rm(ned, alb, slp, tri); gc()
  unlink(list.files(CACHE_DIR, full.names = TRUE, recursive = TRUE), recursive = TRUE)
  cat(sprintf("  [done] %s\n", stt))
}

t0 <- Sys.time()
for (i in seq_along(all_states)) {
  cat(sprintf("[w%d %d/%d] %s  (%.1f min)\n", WORKER, i, length(all_states), all_states[i],
              as.numeric(difftime(Sys.time(), t0, units="mins"))))
  tryCatch(process_state(all_states[i]), error = function(e) cat("  STATE ERROR:", conditionMessage(e), "\n"))
}
cat(sprintf("[worker %d] finished %d states in %.1f min\n", WORKER, length(all_states),
            as.numeric(difftime(Sys.time(), t0, units="mins"))))
