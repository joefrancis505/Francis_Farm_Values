# =============================================================================
# 04_Analysis_Spatial_RDD.R — Replicate Tables 2 and 3
# =============================================================================
# Spatial regression discontinuity design (RDD) at the free-slave state border
# using the rd2d package of Cattaneo, Titiunik, and Yu (2025). The estimator
# is extended with the partialling-out covariate adjustment of Calonico et al.
# (2019), implemented in rd2d_covariate_adjusted.R.
#
# Specifications:
#   (1)  Unadjusted outcomes
#   (2)  Geography-adjusted (slope, elevation, ruggedness, clay, sand, silt)
#   (3)  Geography + enslaved-population-share adjusted (1850 and 1860 only)
#
# The paper presents columns (1) and (2) of Table 2. Column (3) is computed
# here for completeness so the contribution of the enslaved-population-share
# control can be inspected; the script writes all three to the CSV outputs.
#
# Reads  {DATA_DIR}/Border/1820_border/1820_border.shp  (hand-built)
#        {DATA_DIR}/database.csv                       (01_Database.R)
#        {DATA_DIR}/Shapefiles/{year}/                  (00_Download_NHGIS.R)
#        {DATA_DIR}/Geology/USGS_*/                     (00_Download_Geology.R)
#        {DATA_DIR}/Geology/Soilgrids/                  (00_Download_Geology.R)
#
# Writes {OUTPUT_DIR}/Tables/Table_2_RDD_Outcomes.csv
#        {OUTPUT_DIR}/Tables/Table_3_Balance_Tests.csv
#        {OUTPUT_DIR}/Tables/RDD_Combined_Output.txt
# =============================================================================

packages <- c("sf", "rd2d", "dplyr", "readr", "tidyr", "data.table")
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
source(file.path(.repl_root, "Scripts", "rd2d_covariate_adjusted.R"))
if (!exists("DATA_DIR")) DATA_DIR <- file.path(.repl_root, "Data")

# =============================================================
# 1. Helper Functions
# =============================================================

load_and_fix_shapefile <- function(path) {
  st_read(path, quiet = TRUE) %>%
    st_make_valid() %>%
    st_cast("MULTILINESTRING")
}

generate_border_points <- function(border, n = 100) {
  border_line <- if (st_geometry_type(border, by_geometry = FALSE) == "MULTILINESTRING") {
    st_cast(border, "LINESTRING")
  } else border
  pts <- st_line_sample(border_line, n = n, type = "regular")
  pts <- st_cast(pts, "POINT")
  pts_sf <- st_sf(point_id = 1:n, geometry = pts)
  coords <- st_coordinates(pts_sf)
  list(points_sf = pts_sf,
       coords_matrix = data.frame(b1 = coords[,"X"], b2 = coords[,"Y"]))
}

extract_aate <- function(res, n_pts) {
  w <- rep(1/n_pts, n_pts)
  est <- res$results$Est.q
  cov_mat <- res$cov.q
  aate <- sum(w * est)
  se <- sqrt(as.numeric(w %*% cov_mat %*% w))
  z <- aate / se
  p <- 2 * pnorm(abs(z), lower.tail = FALSE)
  ci <- aate + c(-1, 1) * qnorm(0.975) * se
  list(coef = aate, se = se, z = z, p = p,
       ci_lower = ci[1], ci_upper = ci[2], n_pts = n_pts)
}

add_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else ""
}

fmt_row <- function(label, coef, se, p, n_pts = NA, width = 56) {
  stars <- add_stars(p)
  lbl <- sprintf("%-*s", width, label)
  est <- sprintf("%9.3f%-3s", coef, stars)
  se_str <- sprintf("%9.3f", se)
  p_str <- sprintf("%9.4f", p)
  if (!is.na(n_pts)) {
    sprintf("%s %s %s %s %6d", lbl, est, se_str, p_str, n_pts)
  } else {
    sprintf("%s %s %s %s", lbl, est, se_str, p_str)
  }
}

run_rd2d_base <- function(sf_data, outcome_col, border_coords, n_pts, label,
                          kernel = "tri", kernel_type = "prod", bwselect = "mserd",
                          p = 1, q = 2, vce = "hc1") {
  d <- filter(sf_data, !is.na(.data[[outcome_col]]) & is.finite(.data[[outcome_col]]))
  Y <- d[[outcome_col]]
  X <- st_coordinates(d)
  t <- d$treatment
  cat(sprintf("  %s (%d obs)...\n", label, length(Y)))
  res <- rd2d(Y = Y, X = X, t = t, b = as.matrix(border_coords),
              kernel = kernel, kernel_type = kernel_type, bwselect = bwselect,
              vce = vce, p = p, q = q)
  aate <- extract_aate(res, n_pts)
  cat(sprintf("    AATE = %.3f (SE = %.3f, p = %.4f)\n",
              aate$coef, aate$se, aate$p))
  list(res = res, aate = aate, label = label, n_obs = length(Y))
}

# =============================================================
# 2. Load Common Data
# =============================================================

cat("Loading border shapefile...\n")
border_file <- file.path(DATA_DIR, "Border/1820_border/1820_border.shp")
border <- load_and_fix_shapefile(border_file) %>% st_cast("LINESTRING")

N_PTS <- 100
border_100 <- generate_border_points(border, n = N_PTS)
border_len <- as.numeric(st_length(border))
border_dists <- seq(0, border_len, length.out = N_PTS) / 1609.344

slave_states <- c(
  "Alabama", "Arkansas", "Delaware", "District of Columbia", "Florida",
  "Georgia", "Kentucky", "Louisiana", "Maryland", "Mississippi", "Missouri",
  "North Carolina", "South Carolina", "Tennessee", "Texas", "Virginia",
  "West Virginia"
)
excluded_territories <- c("Kansas Territory", "Nebraska Territory",
                          "Indian Territory", "Unorganized Territory",
                          "Kansas", "Nebraska", "New Mexico Territory", "Utah Territory",
                          "Washington Territory", "Colorado Territory", "Dakota Territory",
                          "Nevada Territory", "Arizona Territory", "Idaho Territory",
                          "Montana Territory", "Wyoming Territory", "Indian Territory")

# =============================================================
# 3. Prepare County Data for Each Year
# =============================================================

cat("Loading database...\n")
rdd_db <- read_csv(file.path(DATA_DIR, "database.csv"), show_col_types = FALSE)

read_geology <- function(type, yr) {
  p <- file.path(DATA_DIR, paste0("Geology/USGS_", type, "/", type, "_", yr, ".csv"))
  if (!file.exists(p)) return(NULL)
  read_csv(p, show_col_types = FALSE) %>%
    select(GISJOIN, contains(paste0(type, "_mean"))) %>%
    mutate(GISJOIN = as.character(GISJOIN)) %>%
    distinct(GISJOIN, .keep_all = TRUE)
}

read_soilvar <- function(var, yr) {
  p <- file.path(DATA_DIR, paste0("Geology/Soilgrids/", var, "_", yr, ".csv"))
  if (!file.exists(p)) return(NULL)
  read_csv(p, show_col_types = FALSE) %>%
    mutate(GISJOIN = as.character(GISJOIN)) %>%
    distinct(GISJOIN, .keep_all = TRUE)
}

prepare_county_sf <- function(yr) {
  cat(sprintf("\nPreparing spatial data for %d...\n", yr))

  d <- rdd_db %>%
    filter(year == yr, !(state %in% excluded_territories)) %>%
    mutate(treatment = as.integer(state %in% slave_states))

  shp <- list.files(file.path(DATA_DIR, "Shapefiles", yr),
                    pattern = "\\.shp$", full.names = TRUE)[1]
  counties <- st_read(shp, quiet = TRUE) %>% st_transform(st_crs(border))

  if ("STATENAM" %in% names(counties)) counties <- rename(counties, state = STATENAM)

  centroids <- counties %>%
    st_centroid() %>%
    mutate(GISJOIN = as.character(GISJOIN)) %>%
    filter(!(state %in% excluded_territories)) %>%
    mutate(treatment = as.integer(state %in% slave_states)) %>%
    left_join(d %>% select(-state, -treatment), by = "GISJOIN")

  centroids <- centroids %>%
    mutate(
      farmv = ifelse(improved + unimproved > 0,
                     farmv_total / (improved + unimproved), NA),
      pc_enslaved = ifelse(census_pop > 0,
                           (enslaved / census_pop) * 100, NA),
      # IHS transformation retains zero-value counties; for non-zero values it
      # is numerically very close to log.
      log_farmv = ifelse(!is.na(farmv), asinh(farmv), NA)
    )

  for (type in c("slope", "elevation", "ruggedness")) {
    geo <- read_geology(type, yr)
    if (!is.null(geo)) centroids <- left_join(centroids, geo, by = "GISJOIN")
  }
  for (v in c("clay", "sand", "silt")) {
    soil <- read_soilvar(v, yr)
    if (!is.null(soil)) centroids <- left_join(centroids, soil, by = "GISJOIN")
  }

  centroids
}

# =============================================================
# 4. Run Analysis for 1850 to 1900
# =============================================================

balance_vars <- list(
  list(col = "slope_mean",      label = "Slope"),
  list(col = "elevation_mean",  label = "Elevation"),
  list(col = "ruggedness_mean", label = "Ruggedness (TRI)"),
  list(col = "clay_mean",       label = "Clay content"),
  list(col = "sand_mean",       label = "Sand fraction"),
  list(col = "silt_mean",       label = "Silt fraction")
)

cov_cols_geog  <- c("slope_mean", "elevation_mean", "ruggedness_mean",
                    "clay_mean", "sand_mean", "silt_mean")
cov_cols_slave <- c(cov_cols_geog, "pc_enslaved")

outcome_specs <- list(
  list(kernel = "tri", kernel_type = "prod", bwselect = "mserd",  p = 1, q = 2, label = "tri_mserd"),
  list(kernel = "tri", kernel_type = "prod", bwselect = "imserd", p = 1, q = 2, label = "tri_imserd"),
  list(kernel = "epa", kernel_type = "prod", bwselect = "mserd",  p = 1, q = 2, label = "epa_mserd"),
  list(kernel = "tri", kernel_type = "prod", bwselect = "mserd",  p = 2, q = 3, label = "tri_mserd_p2")
)

all_balance_results <- list()
all_outcome_results <- list()

for (yr in seq(1850, 1900, by = 10)) {
  sf_yr <- prepare_county_sf(yr)

  cat(sprintf("\n--- Outcome and Balance analyses (%d) ---\n", yr))
  for (sp in outcome_specs) {
    # (1) Unadjusted
    rd_unadj <- run_rd2d_base(sf_yr, "log_farmv", border_100$coords_matrix, N_PTS,
                              label = paste("Farmv", yr, "(Unadj)", sp$label),
                              kernel = sp$kernel, kernel_type = sp$kernel_type,
                              bwselect = sp$bwselect, p = sp$p, q = sp$q)

    all_outcome_results[[length(all_outcome_results) + 1]] <- data.frame(
      year = yr, variable = "Log Farm Value", sample = paste0("unadjusted_", sp$label),
      aate = rd_unadj$aate$coef, se = rd_unadj$aate$se, p = rd_unadj$aate$p, n_pts = N_PTS
    )

    # (2) Geography-adjusted (Column 2 of Table 2)
    rd_geog <- run_rd2d_cov(sf_yr, "log_farmv", cov_cols_geog, border_100$coords_matrix, N_PTS,
                            label = paste("Farmv", yr, "(Geog)", sp$label),
                            kernel = sp$kernel, kernel_type = sp$kernel_type,
                            bwselect = sp$bwselect, p = sp$p, q = sp$q)
    aate_geog <- extract_aate_cov(rd_geog$res, N_PTS)
    all_outcome_results[[length(all_outcome_results) + 1]] <- data.frame(
      year = yr, variable = "Log Farm Value", sample = paste0("geog_adjusted_", sp$label),
      aate = aate_geog$coef, se = aate_geog$se, p = aate_geog$p, n_pts = N_PTS
    )

    # Balance tests on geographic covariates (primary spec only).
    # Use the geography-adjusted outcome regression bandwidth at each evaluation
    # point, as stated in the Table 3 footnote (same specification as Column 2
    # of Table 2). rd2d's results data frame stores per-eval-point bandwidths
    # in h01/h02/h11/h12; pass these as the `h` matrix to the balance call.
    if (sp$label == "tri_mserd") {
      geog_bw <- as.matrix(rd_geog$res$results[, c("h01", "h02", "h11", "h12")])
      for (bv in balance_vars) {
        cat(sprintf("  Balance test: %s (%d) using outcome BW\n", bv$label, yr))
        d_bal <- filter(sf_yr, !is.na(.data[[bv$col]]) & is.finite(.data[[bv$col]]))
        res_bal <- rd2d(Y = d_bal[[bv$col]], X = st_coordinates(d_bal), t = d_bal$treatment,
                        b = as.matrix(border_100$coords_matrix),
                        h = geog_bw, kernel = sp$kernel, kernel_type = sp$kernel_type,
                        vce = "hc1", p = sp$p, q = sp$q)
        aate_bal <- extract_aate(res_bal, N_PTS)

        all_balance_results[[length(all_balance_results) + 1]] <- data.frame(
          year = yr, variable = bv$label,
          aate = aate_bal$coef, se = aate_bal$se, p = aate_bal$p,
          ci_lower = aate_bal$ci_lower, ci_upper = aate_bal$ci_upper,
          n_pts = N_PTS
        )
      }
    }

    # (3) Geography + enslaved-population-share (1850 and 1860 only).
    if (yr <= 1860) {
      rd_slave <- run_rd2d_cov(sf_yr, "log_farmv", cov_cols_slave, border_100$coords_matrix, N_PTS,
                               label = paste("Farmv", yr, "(Geog+Slave)", sp$label),
                               kernel = sp$kernel, kernel_type = sp$kernel_type,
                               bwselect = sp$bwselect, p = sp$p, q = sp$q)
      aate_slave <- extract_aate_cov(rd_slave$res, N_PTS)
      all_outcome_results[[length(all_outcome_results) + 1]] <- data.frame(
        year = yr, variable = "Log Farm Value", sample = paste0("geog_and_slave_adjusted_", sp$label),
        aate = aate_slave$coef, se = aate_slave$se, p = aate_slave$p, n_pts = N_PTS
      )
    }
  }
}

# =============================================================
# 5. Export CSVs and Reports
# =============================================================

balance_df <- bind_rows(all_balance_results)
outcome_df <- bind_rows(all_outcome_results)

write_csv(outcome_df, file.path(TAB_DIR, "Table_2_RDD_Outcomes.csv"))
write_csv(balance_df, file.path(TAB_DIR, "Table_3_Balance_Tests.csv"))

sink(file.path(TAB_DIR, "RDD_Combined_Output.txt"))
cat("Spatial RDD Results (rd2d)\n")
cat(strrep("=", 80), "\n\n")

cat("--- Balance Tests (Table 3) ---\n\n")
cat(sprintf("%-56s %12s %9s %9s\n", "Analysis", "AATE", "SE", "p-value"))
cat(strrep("-", 88), "\n")
for (i in seq_len(nrow(balance_df))) {
  r <- balance_df[i, ]
  cat(fmt_row(sprintf("%s (%d)", r$variable, r$year), r$aate, r$se, r$p), "\n")
}
cat("\n")

cat("--- Outcomes (Table 2) ---\n\n")
hdr <- sprintf("%-56s %12s %9s %9s %6s", "Analysis", "AATE", "SE", "p-value", "N pts")
cat(hdr, "\n")
cat(strrep("-", nchar(hdr)), "\n")
for (i in seq_len(nrow(outcome_df))) {
  r <- outcome_df[i, ]
  cat(fmt_row(sprintf("%s (%d) [%s]", r$variable, r$year, r$sample),
              r$aate, r$se, r$p, r$n_pts), "\n")
}
sink()

cat("\nDone. Table 2 and Table 3 written to", TAB_DIR, "\n")
