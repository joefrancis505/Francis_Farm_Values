# =============================================================================
# 03_Analysis_Event_Study.R — Replicate Table 1 and Figure 1
# =============================================================================
# Two-way fixed effects (TWFE) event study for log farm values per acre,
# 1850-1900, on a 1850-county-boundary panel. Treatment is the share of the
# 1860 population that was enslaved, interacted with year dummies (1860 is the
# reference year). Column 2 adds 1860 cotton production per improved acre,
# also interacted with year dummies, as a pre-treatment cotton-intensity
# control.
#
# Reads  {DATA_DIR}/panel_data.csv  (built by 02_Normalization.R)
# Writes {OUTPUT_DIR}/Tables/Table_1_Event_Study.txt
#        {OUTPUT_DIR}/Figures/Figure_1_Event_Study.pdf
#
# Run from the replication root (Rscript Scripts/03_Analysis_Event_Study.R)
# or via Master.R.
# =============================================================================

packages <- c("dplyr", "fixest")
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

# --- 1. Load and Prepare Data ------------------------------------------------

data <- read.csv(file.path(DATA_DIR, "panel_data.csv")) %>%
  filter(year >= 1850)

states_to_include <- c(
  "Alabama", "Arkansas", "Connecticut", "Delaware", "District of Columbia",
  "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kentucky", "Louisiana",
  "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
  "Missouri", "New Hampshire", "New Jersey", "New York", "North Carolina", "Ohio",
  "Pennsylvania", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont",
  "Virginia", "West Virginia", "Wisconsin"
)

slavery_states <- c(
  "Alabama", "Arkansas", "Delaware", "Florida", "Georgia", "Kentucky", "Louisiana",
  "Maryland", "Mississippi", "Missouri", "North Carolina", "South Carolina",
  "Tennessee", "Texas", "Virginia", "West Virginia", "District of Columbia"
)

# 1a. Fix weights at 1850 levels (exogenous weighting).
weights_1850 <- data %>%
  filter(year == 1850) %>%
  mutate(weight_1850 = improved + unimproved) %>%
  select(GISJOIN, weight_1850)

# 1b. Prepare panel.
data <- data %>%
  filter(state %in% states_to_include) %>%
  left_join(weights_1850, by = "GISJOIN") %>%
  mutate(
    farmv = log(farmv_total / (improved + unimproved)),
    slavery = ifelse(state %in% slavery_states, 1, 0),
    weight = weight_1850
  ) %>%
  filter(!is.na(farmv) & is.finite(farmv))

# 1c. Pre-treatment cotton-intensity control: 1860 cotton production per
# improved acre, assigned as a fixed county attribute to every panel year and
# interacted with year dummies in the regression.
cotton_1860 <- data %>%
  filter(year == 1860) %>%
  mutate(cotton_per_imp_1860 = ifelse(is.na(improved) | improved == 0,
                                      NA_real_, cotton / improved)) %>%
  select(GISJOIN, cotton_per_imp_1860)

data <- data %>% left_join(cotton_1860, by = "GISJOIN")

# Fix pc_enslaved at 1860 levels for all panel years (fixed-exposure design).
data <- data %>%
  group_by(GISJOIN) %>%
  mutate(
    val_1860 = if(any(year == 1860)) pc_enslaved[year == 1860][1] else NA_real_,
    pc_enslaved = val_1860
  ) %>%
  select(-val_1860) %>%
  ungroup()

# Set up event study panel.
years_to_include <- seq(1850, 1900, by = 10)
event_study_data <- data %>%
  filter(year %in% years_to_include) %>%
  mutate(event_time = year - 1860)

# --- 2. Models (Table 1) -----------------------------------------------------

cat("\n--- Running Event Study (Table 1) ---\n")

models <- list(
  col1 = feols(farmv ~ i(year, pc_enslaved, ref = 1860) | GISJOIN + year,
               data = event_study_data, weights = ~ weight),

  col2 = feols(farmv ~ i(year, pc_enslaved, ref = 1860) +
                 i(year, cotton_per_imp_1860, ref = 1860) | GISJOIN + year,
               data = event_study_data, weights = ~ weight)
)

# --- 3. Export Results -------------------------------------------------------

sink(file.path(TAB_DIR, "Table_1_Event_Study.txt"))
cat("TABLE 1: EVENT STUDY OF ABOLITION, 1850-1900\n")
cat(strrep("=", 60), "\n\n")

cat("Column (1) — No cotton control\n")
print(summary(models$col1, cluster = ~ GISJOIN), digits = 3)
cat("\n")

cat("Column (2) — Cotton-per-improved-acre interacted with year dummies\n")
print(summary(models$col2, cluster = ~ GISJOIN), digits = 3)
cat("\n")
sink()

# --- 4. Figure 1: Event-study coefficient plot -------------------------------

cat("\n--- Generating Figure 1 ---\n")

setup_plot <- function(width, height, top_margin = 0.2, bottom_margin = 0.6,
                       left_margin = 1.2, right_margin = 1.2) {
  par(pin = c(width - left_margin - right_margin, height - top_margin - bottom_margin))
  par(mai = c(bottom_margin, left_margin, top_margin, right_margin))
  par(family = "sans", cex = 1.2, cex.axis = 1.2, cex.lab = 1.2,
      tck = 0.01, lwd = 0.5, las = 1, mgp = c(3.5, 0.8, 0))
}

format_labels <- function(x) format(x, scientific = FALSE, trim = TRUE)

create_event_plot <- function(model, coef_pattern, ylim_range, filename,
                              y_by = NULL, y_digits = NULL) {
  event_study_coef <- coef(model)
  event_study_se   <- sqrt(diag(vcov(model, cluster = "GISJOIN")))

  coef_indices <- grep(coef_pattern, names(event_study_coef))
  if (length(coef_indices) == 0) return()

  selected_coef <- event_study_coef[coef_indices]
  selected_se   <- event_study_se[coef_indices]
  event_times   <- as.numeric(gsub(".*::([0-9]+):.*", "\\1", names(selected_coef)))

  valid_idx     <- !is.na(event_times) & !is.na(selected_coef) & is.finite(selected_coef)
  event_times   <- event_times[valid_idx]
  selected_coef <- selected_coef[valid_idx]
  selected_se   <- selected_se[valid_idx]
  if (length(event_times) == 0) return()

  pdf(file.path(FIG_DIR, filename), width = 9.2, height = 5.5)
  setup_plot(9.2, 5.5)
  plot(event_times, selected_coef, type = "n", xlab = " ", ylab = "Coefficient",
       xlim = c(min(event_times) - 2, max(event_times) + 2),
       ylim = ylim_range, axes = FALSE)

  axis(1, at = seq(1850, 1900, by = 10),
       labels = format_labels(seq(1850, 1900, by = 10)),
       lwd = 0, lwd.ticks = 0.8)

  y_at <- if (!is.null(y_by)) seq(ylim_range[1], ylim_range[2], by = y_by)
          else pretty(ylim_range, n = 6)
  y_labs <- if (!is.null(y_digits)) sprintf(paste0("%.", y_digits, "f"), y_at)
            else format_labels(y_at)
  axis(2, at = y_at, labels = y_labs, lwd = 0, lwd.ticks = 0.8)

  abline(h = 0, lty = 2, col = "gray")
  abline(v = 1865, lty = 2, col = "gray")
  segments(event_times, selected_coef - 1.96 * selected_se,
           event_times, selected_coef + 1.96 * selected_se,
           col = "grey60", lwd = 1.0)
  points(event_times, selected_coef, pch = 16, cex = 0.9, col = "black")
  points(1860, 0, pch = 16, cex = 0.9, col = "black")
  box(lwd = 0.5)
  dev.off()
}

create_event_plot(models$col2, "year::[0-9]+:pc_enslaved",
                  ylim_range = c(-0.03, 0.01),
                  filename = "Figure_1_Event_Study.pdf",
                  y_by = 0.01, y_digits = 2)

cat("Table 1 and Figure 1 complete.\n")
