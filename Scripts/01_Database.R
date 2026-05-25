# =============================================================================
# 01_Database.R — Build clean panel from unified NHGIS data
# =============================================================================
# Reads  {DATA_DIR}/census.csv
# Writes {DATA_DIR}/database.csv
#
# Run from the replication root. DATA_DIR defaults to "Data".
# =============================================================================

library(dplyr)
library(purrr)
library(readr)

if (!exists("DATA_DIR")) DATA_DIR <- "Data"

# Try census.csv first; fall back to census_1790_1900.csv for backward compat
census_file <- file.path(DATA_DIR, "census.csv")
if (!file.exists(census_file)) {
  census_file <- file.path(DATA_DIR, "census_1790_1900.csv")
}
cat("Loading census data from", census_file, "...\n")
census_data <- read_csv(
  census_file,
  col_types = cols(GISJOIN = "c", STATEA = "c", COUNTYA = "c"),
  show_col_types = FALSE
)

process_year <- function(target_year, data) {
  cat("Processing year:", target_year, "...\n")
  yr      <- as.integer(target_year)
  yr_data <- data %>% filter(year == yr)
  if (nrow(yr_data) == 0) { warning("No data for ", target_year); return(NULL) }

  col <- function(name) {
    # Handle .x/.y suffixes from joins in 00_Download_NHGIS.R
    candidates <- c(name, paste0(name, ".x"), paste0(name, ".y"))
    found <- candidates[candidates %in% names(yr_data)]
    if (length(found) > 0) yr_data[[found[1]]] else rep(NA_real_, nrow(yr_data))
  }

  base <- yr_data %>% transmute(
    GISJOIN,
    year  = yr,
    state = if ("STATE" %in% names(yr_data)) STATE else NA_character_,
    STATEA,
    COUNTYA
  )

  if (target_year == "1840") {
    # ACZ001-012: Free Non-white, ACZ013-024: Slave
    # AB4017: Ginned Cotton (lbs)
    base %>% mutate(
      census_pop=col("ACD001"),
      enslaved=col("ACZ013")+col("ACZ014")+col("ACZ015")+col("ACZ016")+col("ACZ017")+
               col("ACZ018")+col("ACZ019")+col("ACZ020")+col("ACZ021")+col("ACZ022")+
               col("ACZ023")+col("ACZ024"),
      black=col("ACZ001")+col("ACZ002")+col("ACZ003")+col("ACZ004")+col("ACZ005")+
            col("ACZ006")+col("ACZ007")+col("ACZ008")+col("ACZ009")+col("ACZ010")+
            col("ACZ011")+col("ACZ012")+enslaved,
      farmv_total=NA_real_, improved=NA_real_, unimproved=NA_real_,
      cotton=col("AB4017"), corn=NA_real_, livestock_val=NA_real_, cotton_acreage=NA_real_)
  } else if (target_year == "1850") {
    # AE6002: Nonwhite Free, AE6003: Nonwhite Slave
    base %>% mutate(census_pop=col("ADQ001"), enslaved=col("AE6003"),
      black=col("AE6002")+col("AE6003"),
      farmv_total=col("ADJ001"), improved=col("ADI001"), unimproved=col("ADI002"),
      cotton=col("ADM007")*400, corn=col("ADM003"),
      livestock_val=NA_real_, cotton_acreage=NA_real_)
  } else if (target_year == "1860") {
    # AH3002: Free colored, AH3003: Slave
    base %>% mutate(census_pop=col("AG3001"), enslaved=col("AH3003"),
      black=col("AH3002")+col("AH3003"),
      farmv_total=col("AGV001"), improved=col("AGP001"), unimproved=col("AGP002"),
      cotton=col("AGY007")*400, corn=col("AGY003"),
      livestock_val=col("AGX001"), cotton_acreage=NA_real_)
  } else if (target_year == "1870") {
    # AJ3001: Total Pop (1870_cPAX NT1), AK3002: Colored (1870_cPAX NT4)
    base %>% mutate(census_pop=col("AJ3001"), enslaved=0,
      black=col("AK3002"),
      farmv_total=col("AJV001"), improved=col("AJU001"),
      unimproved=col("AJU002")+col("AJU003"),
      cotton=col("AJ1010")*400, corn=col("AJ1004"),
      livestock_val=col("AJZ001"), cotton_acreage=NA_real_)
  } else if (target_year == "1880") {
    # AOB001: Total Pop (1880_cPAX NT1), APP002: Colored (1880_cPAX NT4)
    # AOH011: Cotton (Bales)
    # 1880 bale weights: 500 lbs for TX, AR, MO; 475 lbs elsewhere.
    base %>% mutate(census_pop=col("AOB001"), enslaved=0,
      black=col("APP002"),
      farmv_total=col("AOD001")+col("AOD002")+col("AOD003"),
      improved=col("AOS001"), unimproved=col("AOS002"),
      cotton=ifelse(state %in% c("Texas", "Arkansas", "Missouri"),
                    col("AOH011") * 500, col("AOH011") * 475),
      corn=col("AOH007"),
      livestock_val=col("AOD003"), cotton_acreage=col("AOJ007"))
  } else if (target_year == "1890") {
    # AUM001: Total Pop (1890_cPHAM NT1), AV0007+0008: Colored M/F (1890_cPHAM NT6)
    base %>% mutate(census_pop=col("AUM001"), enslaved=0,
      black=col("AV0007")+col("AV0008"),
      farmv_total=col("AUK001")+col("AUK002")+col("AUK003"),
      improved=col("AUJ001"), unimproved=col("AUJ002"),
      cotton=col("ATB007")*477, corn=col("ATB003"),
      livestock_val=col("AUK003"), cotton_acreage=col("ATC007"))
  } else if (target_year == "1900") {
    # AYM001: Total Pop (1900_cPHAM NT1), AZ3003+3004: Negro M/F (1900_cPHAM NT7)
    base %>% mutate(census_pop=col("AYM001"), enslaved=0,
      black=col("AZ3003")+col("AZ3004"),
      farmv_total=col("AWW001")+col("AWW002")+col("AWW003")+col("AWW004"),
      improved=col("AWU001"), unimproved=col("AWT001")-col("AWU001"),
      cotton=(col("AXO024")+col("AXO026"))*500, corn=col("AXO003"),
      livestock_val=col("AWW004"), cotton_acreage=col("AXN021")+col("AXN022"))
  } else {
    warning("No specification for year ", target_year); return(NULL)
  }
}

years_to_process <- as.character(seq(1840, 1900, by = 10))
final_database   <- map_dfr(years_to_process, ~process_year(.x, data = census_data))
write_csv(final_database, file.path(DATA_DIR, "database.csv"))
cat(sprintf("\nDatabase saved to %s\n", file.path(DATA_DIR, "database.csv")))
cat(sprintf("  %d rows, %d columns\n", nrow(final_database), ncol(final_database)))
