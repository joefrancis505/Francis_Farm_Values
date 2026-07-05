# =============================================================================
# 00_Download_NHGIS.R — Download NHGIS County Data & Shapefiles (1820–1900)
# =============================================================================
# Every year and dataset below corresponds to an IPUMS NHGIS data table used
# in the analysis. Amend DATASET_SPECS and re-run if additional NT tables are
# needed; the spec hash and extract history together make the rerun cheap.
#
# Outputs:
#   {DATA_DIR}/census.csv          — county-level panel, 1820 & 1840-1900
#   {DATA_DIR}/Shapefiles/{year}/  — county shapefiles for each year above
#
# Run from the replication root. DATA_DIR defaults to "Data".
# =============================================================================

options(timeout = 7200) # 2 hours for large files

packages <- c("dplyr", "purrr", "readr", "ipumsr", "stringr", "httr", "tools", "digest")
missing_pkgs <- packages[!packages %in% rownames(installed.packages())]
if (length(missing_pkgs) > 0)
  stop("Missing packages: ", paste(missing_pkgs, collapse = ", "),
       "\nRun install_dependencies.R from the replication root to install.")
invisible(lapply(packages, library, character.only = TRUE))

source(file.path("Scripts", "download_utils.R"))

if (!exists("DATA_DIR")) DATA_DIR <- "Data"
dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(DATA_DIR, "IPUMS_Raw"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(DATA_DIR, "Shapefiles"), recursive = TRUE, showWarnings = FALSE)

is_file_valid <- function(file_path) {
  if (!file.exists(file_path)) return(FALSE)
  if (grepl("\\.zip$", file_path)) {
    res <- tryCatch({
      system2("unzip", args = c("-t", shQuote(file_path)), stdout = FALSE, stderr = FALSE)
    }, error = function(e) 1)
    return(res == 0)
  }
  return(TRUE)
}

# --- Year and dataset specifications -----------------------------------------
# The event study and spatial RDD use the 1850-1900 county-level agricultural
# and population censuses. 1820 and 1840 are included to support extensions and
# to keep the extract self-contained; downstream scripts filter to 1850-1900.
# 1820 has no agricultural census (the first ag census was 1840).

years        <- c("1820","1840","1850","1860","1870","1880","1890","1900")
download_dir <- normalizePath(file.path(DATA_DIR, "IPUMS_Raw"))

DATASET_SPECS <- list(
  "1820" = list(list(ds = "1820_cPop",  tables = c("NT1","NT2","NT3"))),
  "1840" = list(list(ds = "1840_cPopX", tables = c("NT1","NT2","NT3","NT5")),
                list(ds = "1840_cAg",   tables = c("NT2"))),
  "1850" = list(list(ds = "1850_cPAX",  tables = c("NT1","NT3","NT6")),
                list(ds = "1850_cAg",   tables = c("NT2","NT3","NT6"))),
  "1860" = list(list(ds = "1860_cPAX",  tables = c("NT1","NT3","NT6")),
                list(ds = "1860_cAg",   tables = c("NT1","NT2","NT3","NT4","NT5"))),
  "1870" = list(list(ds = "1870_cPAX",  tables = c("NT1","NT3","NT4")),
                list(ds = "1870_cAg",   tables = c("NT1","NT2","NT3","NT7","NT9"))),
  "1880" = list(list(ds = "1880_cPAX",  tables = c("NT1","NT3","NT4")),
                list(ds = "1880_cAg",   tables = c("NT1","NT9","NT11","NT15A","NT16"))),
  "1890" = list(list(ds = "1890_cPHAM", tables = c("NT1","NT3","NT6")),
                list(ds = "1890_cAg",   tables = c("NT1","NT8","NT9A","NT21","NT22"))),
  "1900" = list(list(ds = "1900_cPHAM", tables = c("NT1","NT3","NT7")),
                list(ds = "1900_cAg",   tables = c("NT1","NT7","NT10","NT11","NT12","NT13","NT38","NT39")))
)

get_data_specs <- function(year) {
  lapply(DATASET_SPECS[[year]], function(s)
    ds_spec(s$ds, data_tables = s$tables, geog_levels = "county"))
}

current_spec <- paste(
  unlist(lapply(years, function(y)
    sapply(DATASET_SPECS[[y]], function(s)
      paste(s$ds, paste(sort(s$tables), collapse = ","), sep = ":")))),
  collapse = "|")

# Embed a short spec hash in the extract description so find_existing_nhgis()
# only reuses an extract whose datasets and tables match the current spec.
# Any edit to DATASET_SPECS changes the hash, which forces a new extract.
spec_hash <- substr(digest::digest(current_spec, algo = "sha1"), 1, 8)
EXTRACT_DESCRIPTION <- sprintf(
  "Farm Values — Bulk Extract 1820-1900 (Counties) [spec %s]", spec_hash
)

spec_file  <- file.path(DATA_DIR, "census_spec.hash")
saved_spec <- if (file.exists(spec_file)) readLines(spec_file, warn = FALSE)[1] else ""

# --- Idempotent guard --------------------------------------------------------

files_exist <-
  file.exists(file.path(DATA_DIR, "census.csv")) &&
  all(sapply(years, function(y)
    length(list.files(file.path(DATA_DIR, "Shapefiles", y),
                      pattern = "\\.shp$")) > 0))

if (files_exist && current_spec == saved_spec) {
  cat("All NHGIS outputs exist and spec is unchanged. Skipping download.\n")
} else {

  if (files_exist && current_spec != saved_spec)
    cat("Spec has changed — re-downloading NHGIS data.\n")

  api_key <- get_ipums_api_key()
  set_ipums_default_collection("nhgis")

  # --- Discovery: Check history for existing or in-progress extract -----------

  find_existing_nhgis <- function(api_key) {
    cat("Checking IPUMS history for existing NHGIS extract...\n")
    history <- with_retry(get_extract_history("nhgis", api_key = api_key))

    for (ext in history) {
      if (ext$description == EXTRACT_DESCRIPTION && ext$status == "completed") {
        cat(sprintf("  Found completed extract #%d.\n", ext$number))
        return(ext)
      }
    }

    for (ext in history) {
      if (ext$description == EXTRACT_DESCRIPTION && ext$status %in% c("queued", "started")) {
        cat(sprintf("  Found in-progress extract #%d (status: %s). Waiting...\n",
                    ext$number, ext$status))
        return(wait_for_extract(ext, api_key = api_key))
      }
    }

    return(NULL)
  }

  ready_extract <- find_existing_nhgis(api_key)

  if (is.null(ready_extract)) {
    cat("No suitable extract found. Defining new bulk extract...\n")
    all_data_specs <- purrr::flatten(purrr::map(years, get_data_specs))
    all_shapefiles <- paste0("us_county_", years, "_tl2000")

    bulk_extract <- define_extract_agg(
      collection  = "nhgis",
      description = EXTRACT_DESCRIPTION,
      datasets    = all_data_specs,
      shapefiles  = all_shapefiles
    )

    cat("Submitting bulk extract for 1820-1900...\n")
    submitted_extract <- submit_extract(bulk_extract, api_key = api_key)
    cat(sprintf("Submitted extract #%d. Waiting for completion...\n", submitted_extract$number))
    ready_extract     <- wait_for_extract(submitted_extract, api_key = api_key)
  }

  # --- Resumable per-file download -----------------------------------------
  # ipumsr::download_extract() aborts when a partial zip is on disk and has
  # no per-file resume. We pull each URL with robust_ipums_download() (curl
  # -C -, internal retries, keepalive) and skip any zip that already passes
  # `unzip -t`. Partial/corrupt zips are re-fetched and resumed from the
  # byte offset curl detects.

  links <- ready_extract$download_links
  urls  <- c(links$table_data$url, links$gis_data$url)
  if (length(urls) == 0) urls <- unlist(links$data_file)  # legacy field name
  urls <- urls[!is.na(urls) & nzchar(urls)]
  if (length(urls) == 0) stop("Could not find NHGIS download URLs.")

  downloaded_files <- character()
  for (u in urls) {
    dest <- file.path(download_dir, basename(u))
    if (!is_file_valid(dest)) {
      ok <- robust_ipums_download(u, dest, api_key)
      if (!ok || !is_file_valid(dest))
        stop(sprintf("Download failed or zip invalid: %s", basename(dest)))
    } else {
      cat(sprintf("  %s already downloaded and valid. Skipping.\n", basename(dest)))
    }
    downloaded_files <- c(downloaded_files, dest)
  }

  # --- Unzip ----------------------------------------------------------------

  cat("Unzipping files...\n")
  for (zip_file in downloaded_files) {
    if (grepl("\\.zip$", zip_file)) {
      extract_dir <- file.path(download_dir,
                               gsub("\\.zip$", "", basename(zip_file)))

      if (!dir.exists(extract_dir)) {
        dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
        utils::unzip(zip_file, exdir = extract_dir)
      }

      inner_zips <- list.files(extract_dir, pattern = "\\.zip$",
                               full.names = TRUE, recursive = TRUE)
      for (iz in inner_zips) {
        inner_dir <- file.path(dirname(iz), gsub("\\.zip$", "", basename(iz)))
        if (!dir.exists(inner_dir)) {
          dir.create(inner_dir, recursive = TRUE, showWarnings = FALSE)
          utils::unzip(iz, exdir = inner_dir)
        }
      }
    }
  }

  # --- Organise by year -----------------------------------------------------

  id_cols <- c("GISJOIN","YEAR","REGIONA","DIVISIONA","STATEA","COUNTYA",
               "AREANAME","NAME_E","STATE","COUNTY")

  organize_year <- function(year) {
    cat("Processing data for", year, "...\n")

    shp_dest <- file.path(DATA_DIR, "Shapefiles", year)
    dir.create(shp_dest, recursive = TRUE, showWarnings = FALSE)

    shp_pattern <- paste0(".*county_", year, ".*\\.shp$")
    shp_files   <- list.files(download_dir, pattern = shp_pattern,
                              full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
    for (shp in shp_files) {
      base  <- tools::file_path_sans_ext(shp)
      parts <- list.files(dirname(shp),
                          pattern = paste0("^", basename(base), "\\.[a-zA-Z0-9]+$"),
                          full.names = TRUE)
      for (f in parts) file.copy(f, file.path(shp_dest, basename(f)), overwrite = TRUE)
    }

    csv_files <- list.files(download_dir, pattern = paste0(year, ".*\\.csv$"),
                            full.names = TRUE, recursive = TRUE)
    csv_files <- csv_files[!grepl("codebook", csv_files)]
    if (length(csv_files) == 0) return(NULL)

    tables <- lapply(csv_files, function(p) {
      readr::read_csv(p, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
        mutate(across(any_of(id_cols), as.character)) %>%
        mutate(across(-any_of(id_cols), as.numeric))
    })

    combined <- tables[[1]]
    if (length(tables) > 1) {
      for (i in 2:length(tables)) {
        combined <- full_join(combined,
                              tables[[i]] %>% select(-any_of(setdiff(id_cols, "GISJOIN"))),
                              by = "GISJOIN")
      }
    }
    combined$year <- as.integer(year)
    combined
  }

  all_data <- purrr::map(years, organize_year)
  all_data <- all_data[!sapply(all_data, is.null)]
  merged   <- dplyr::bind_rows(all_data)
  readr::write_csv(merged, file.path(DATA_DIR, "census.csv"))
  writeLines(current_spec, spec_file)
  cat(sprintf("Complete. Saved to %s\n", file.path(DATA_DIR, "census.csv")))

}
