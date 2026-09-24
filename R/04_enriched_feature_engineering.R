# =============================================================================
# PHASE 2 — ENRICHED FEATURE ENGINEERING
# Project: Explainable Urban Water Demand Forecasting and Decision-Support System
#
# Purpose:
#   1. Create locality-aware historical demand features
#   2. Create leakage-safe rolling statistics
#   3. Create cyclical time features
#   4. Create useful contextual/infrastructure features
#   5. Produce a clean enriched modelling dataset
#
# IMPORTANT:
#   - Original SOWEKI data is NOT modified.
#   - total_water_demand_reference is NEVER used as a predictor.
#   - synthetic_demand_share is NEVER used as a predictor.
#   - Current/target water_demand is NEVER used to calculate predictor features.
#   - All historical demand features use ONLY previous observations.
# =============================================================================

options(stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------
# 1. Paths
# -----------------------------------------------------------------------------

PROJECT_DIR <- normalizePath(
  file.path(getwd()),
  winslash = "/",
  mustWork = FALSE
)

DATA_DIR <- file.path(PROJECT_DIR, "data")
OUTPUT_DIR <- file.path(PROJECT_DIR, "outputs")

INPUT_FILE <- file.path(
  DATA_DIR,
  "urban_water_demand_enriched.csv"
)

OUTPUT_FILE <- file.path(
  DATA_DIR,
  "enriched_features.csv"
)

FEATURE_DESCRIPTION_FILE <- file.path(
  OUTPUT_DIR,
  "enriched_feature_description.csv"
)

REPORT_FILE <- file.path(
  OUTPUT_DIR,
  "enriched_feature_engineering_report.txt"
)

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 2. Check input
# -----------------------------------------------------------------------------

if (!file.exists(INPUT_FILE)) {
  stop(
    "Input file not found:\n",
    INPUT_FILE,
    "\nMake sure Phase 1 has been completed."
  )
}

cat("============================================================\n")
cat("PHASE 2 — ENRICHED FEATURE ENGINEERING\n")
cat("============================================================\n\n")

cat("Project directory:\n", PROJECT_DIR, "\n\n")
cat("Input file:\n", INPUT_FILE, "\n\n")

# -----------------------------------------------------------------------------
# 3. Load enriched dataset
# -----------------------------------------------------------------------------

df <- read.csv(
  INPUT_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

cat("Rows loaded:", nrow(df), "\n")
cat("Columns loaded:", ncol(df), "\n\n")

# -----------------------------------------------------------------------------
# 4. Validate required columns
# -----------------------------------------------------------------------------

required_columns <- c(
  "timestamp",
  "locality",
  "zone_type",
  "area_km2",
  "population",
  "population_density",
  "residential_pct",
  "commercial_pct",
  "industrial_pct",
  "storage_capacity_mld",
  "pipeline_age_years",
  "distribution_efficiency",
  "temperature_c",
  "humidity_pct",
  "rainfall_mm",
  "wind_speed_kmh",
  "is_rain_event",
  "is_holiday",
  "is_weekend",
  "festival_period",
  "hour",
  "minute",
  "day_of_week",
  "month",
  "year",
  "synthetic_demand_share",
  "total_water_demand_reference",
  "water_demand",
  "water_supply_hours"
)

missing_columns <- setdiff(required_columns, names(df))

if (length(missing_columns) > 0) {
  stop(
    "The following required columns are missing:\n",
    paste(missing_columns, collapse = ", ")
  )
}

cat("Required columns: OK\n\n")

# -----------------------------------------------------------------------------
# 5. Convert timestamp
# -----------------------------------------------------------------------------

df$timestamp <- as.POSIXct(
  df$timestamp,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)

if (any(is.na(df$timestamp))) {
  stop("Some timestamp values could not be parsed.")
}

# -----------------------------------------------------------------------------
# 6. Sort chronologically within locality
# -----------------------------------------------------------------------------

df <- df[order(df$locality, df$timestamp), ]

rownames(df) <- NULL

# -----------------------------------------------------------------------------
# 7. Helper functions
# -----------------------------------------------------------------------------

# Historical lag.
#
# lag_k(x, 1)
# means previous observation.
#
# IMPORTANT:
# This function NEVER looks into the future.

create_lag <- function(x, k) {

  n <- length(x)

  if (n <= k) {
    return(rep(NA_real_, n))
  }

  c(
    rep(NA_real_, k),
    x[1:(n - k)]
  )
}


# Rolling mean using ONLY previous observations.
#
# For example:
# rolling_mean(x, 4)
#
# At time t:
#   mean(x[t-4], x[t-3], x[t-2], x[t-1])
#
# x[t] itself is NOT included.

rolling_mean_previous <- function(x, window) {

  n <- length(x)

  result <- rep(NA_real_, n)

  if (n <= window) {
    return(result)
  }

  for (i in (window + 1):n) {

    previous_values <- x[(i - window):(i - 1)]

    if (all(is.finite(previous_values))) {
      result[i] <- mean(previous_values)
    }
  }

  result
}


# Rolling standard deviation using ONLY previous observations.

rolling_sd_previous <- function(x, window) {

  n <- length(x)

  result <- rep(NA_real_, n)

  if (n <= window) {
    return(result)
  }

  for (i in (window + 1):n) {

    previous_values <- x[(i - window):(i - 1)]

    if (all(is.finite(previous_values))) {
      result[i] <- sd(previous_values)
    }
  }

  result
}


# -----------------------------------------------------------------------------
# 8. Create locality-specific demand features
# -----------------------------------------------------------------------------

cat("Creating locality-specific historical demand features...\n")

locality_groups <- split(
  seq_len(nrow(df)),
  df$locality
)

df$lag_1 <- NA_real_
df$lag_4 <- NA_real_
df$lag_96 <- NA_real_
df$lag_672 <- NA_real_

df$rolling_mean_4 <- NA_real_
df$rolling_mean_96 <- NA_real_
df$rolling_mean_672 <- NA_real_

df$rolling_sd_96 <- NA_real_
df$rolling_sd_672 <- NA_real_


for (loc in names(locality_groups)) {

  idx <- locality_groups[[loc]]

  demand <- df$water_demand[idx]

  # ---------------------------------------------------------------------------
  # Historical lags
  # ---------------------------------------------------------------------------

  df$lag_1[idx] <- create_lag(demand, 1)

  df$lag_4[idx] <- create_lag(demand, 4)

  df$lag_96[idx] <- create_lag(demand, 96)

  df$lag_672[idx] <- create_lag(demand, 672)


  # ---------------------------------------------------------------------------
  # Rolling features
  # ---------------------------------------------------------------------------

  df$rolling_mean_4[idx] <-
    rolling_mean_previous(demand, 4)

  df$rolling_mean_96[idx] <-
    rolling_mean_previous(demand, 96)

  df$rolling_mean_672[idx] <-
    rolling_mean_previous(demand, 672)

  df$rolling_sd_96[idx] <-
    rolling_sd_previous(demand, 96)

  df$rolling_sd_672[idx] <-
    rolling_sd_previous(demand, 672)
}

cat("Historical demand features created.\n\n")

# -----------------------------------------------------------------------------
# 9. Cyclical time features
# -----------------------------------------------------------------------------

cat("Creating cyclical calendar features...\n")

# Hour of day
#
# 23:00 and 00:00 are close to each other.
# Sine/cosine representation captures this circular relationship.

df$hour_sin <- sin(
  2 * pi * df$hour / 24
)

df$hour_cos <- cos(
  2 * pi * df$hour / 24
)


# Day of week
#
# day_of_week:
# Monday = 1
# ...
# Sunday = 7

df$dow_sin <- sin(
  2 * pi * (df$day_of_week - 1) / 7
)

df$dow_cos <- cos(
  2 * pi * (df$day_of_week - 1) / 7
)


# Month

df$month_sin <- sin(
  2 * pi * (df$month - 1) / 12
)

df$month_cos <- cos(
  2 * pi * (df$month - 1) / 12
)

cat("Cyclical time features created.\n\n")

# -----------------------------------------------------------------------------
# 10. Weather-derived features
# -----------------------------------------------------------------------------

cat("Creating weather/context features...\n")

# Temperature-humidity interaction
df$temperature_humidity_index <-
  df$temperature_c * df$humidity_pct / 100


# Rainfall intensity indicator
df$heavy_rain_event <-
  as.integer(df$rainfall_mm >= 5)


# Wind category
df$high_wind_event <-
  as.integer(df$wind_speed_kmh >= 20)


# Temperature categories
df$high_temperature_event <-
  as.integer(df$temperature_c >= 35)

# -----------------------------------------------------------------------------
# 11. Infrastructure-derived features
# -----------------------------------------------------------------------------

# Effective storage relative to population.
#
# This is a contextual feature, not a target-derived feature.

df$storage_per_1000_people <-
  df$storage_capacity_mld /
  (df$population / 1000)


# Supply efficiency interaction

df$supply_efficiency_index <-
  df$water_supply_hours *
  df$distribution_efficiency


# Infrastructure stress indicator
#
# Higher pipeline age + lower efficiency can indicate greater
# infrastructure-related stress.

df$infrastructure_stress <-
  df$pipeline_age_years *
  (1 - df$distribution_efficiency)


# -----------------------------------------------------------------------------
# 12. Land-use derived features
# -----------------------------------------------------------------------------

# Check that land-use percentages are valid.

land_use_sum <-
  df$residential_pct +
  df$commercial_pct +
  df$industrial_pct

df$land_use_total_pct <- land_use_sum


# Dominant land-use category

df$dominant_land_use <- apply(
  df[, c(
    "residential_pct",
    "commercial_pct",
    "industrial_pct"
  )],
  1,
  function(x) {

    categories <- c(
      "Residential",
      "Commercial",
      "Industrial"
    )

    categories[which.max(x)]
  }
)

# -----------------------------------------------------------------------------
# 13. Demand intensity feature
# -----------------------------------------------------------------------------

# Demand per 1,000 residents.
#
# This uses the CURRENT target and therefore MUST NOT be used
# as an ML predictor.
#
# It is retained only for descriptive/error-analysis purposes.

df$demand_per_1000_people <-
  df$water_demand /
  (df$population / 1000)

# -----------------------------------------------------------------------------
# 14. Explicit leakage protection
# -----------------------------------------------------------------------------

# These variables describe the target or synthetic allocation and must
# NEVER be used as model predictors.

leakage_columns <- c(
  "water_demand",
  "total_water_demand_reference",
  "synthetic_demand_share",
  "demand_per_1000_people"
)

# -----------------------------------------------------------------------------
# 15. Define modelling feature set
# -----------------------------------------------------------------------------

model_features <- c(

  # Locality/context
  "locality",
  "zone_type",

  # Demographics
  "population",
  "area_km2",
  "population_density",

  # Land use
  "residential_pct",
  "commercial_pct",
  "industrial_pct",

  # Infrastructure
  "storage_capacity_mld",
  "pipeline_age_years",
  "distribution_efficiency",
  "water_supply_hours",
  "storage_per_1000_people",
  "supply_efficiency_index",
  "infrastructure_stress",

  # Weather
  "temperature_c",
  "humidity_pct",
  "rainfall_mm",
  "wind_speed_kmh",
  "is_rain_event",
  "heavy_rain_event",
  "high_wind_event",
  "high_temperature_event",
  "temperature_humidity_index",

  # Calendar
  "is_holiday",
  "is_weekend",
  "festival_period",
  "hour",
  "minute",
  "day_of_week",
  "month",
  "year",

  # Cyclical time
  "hour_sin",
  "hour_cos",
  "dow_sin",
  "dow_cos",
  "month_sin",
  "month_cos",

  # Historical demand
  "lag_1",
  "lag_4",
  "lag_96",
  "lag_672",
  "rolling_mean_4",
  "rolling_mean_96",
  "rolling_mean_672",
  "rolling_sd_96",
  "rolling_sd_672"
)

# -----------------------------------------------------------------------------
# 16. Verify model features exist
# -----------------------------------------------------------------------------

missing_model_features <-
  setdiff(model_features, names(df))

if (length(missing_model_features) > 0) {

  stop(
    "Missing model features:\n",
    paste(
      missing_model_features,
      collapse = ", "
    )
  )
}

# -----------------------------------------------------------------------------
# 17. Leakage audit
# -----------------------------------------------------------------------------

cat("Running leakage audit...\n")

leakage_in_model_features <-
  intersect(
    model_features,
    leakage_columns
  )

if (length(leakage_in_model_features) > 0) {

  stop(
    "LEAKAGE DETECTED. These target-related columns are in the model feature set:\n",
    paste(
      leakage_in_model_features,
      collapse = ", "
    )
  )
}

cat("Leakage audit: PASSED\n\n")

# -----------------------------------------------------------------------------
# 18. Check lag logic
# -----------------------------------------------------------------------------

cat("Checking historical lag logic...\n")

# For every locality, verify that lag_1 equals the previous
# water-demand observation.

lag_check <- TRUE

for (loc in names(locality_groups)) {

  idx <- locality_groups[[loc]]

  demand <- df$water_demand[idx]

  expected_lag_1 <- create_lag(demand, 1)

  actual_lag_1 <- df$lag_1[idx]

  valid <- which(
    !is.na(expected_lag_1) &
    !is.na(actual_lag_1)
  )

  if (length(valid) > 0) {

    if (!all(
      abs(
        expected_lag_1[valid] -
        actual_lag_1[valid]
      ) < 1e-10
    )) {

      lag_check <- FALSE

      break
    }
  }
}

if (!lag_check) {

  stop(
    "Historical lag validation failed."
  )
}

cat("Lag validation: PASSED\n\n")

# -----------------------------------------------------------------------------
# 19. Validate land-use percentages
# -----------------------------------------------------------------------------

land_use_invalid <- sum(
  abs(df$land_use_total_pct - 100) > 0.01,
  na.rm = TRUE
)

cat(
  "Rows with land-use total different from 100%:",
  land_use_invalid,
  "\n\n"
)

# -----------------------------------------------------------------------------
# 20. Save enriched modelling dataset
# -----------------------------------------------------------------------------

# Keep the target and descriptive columns as well, but clearly separate
# them from the predictor list.

output_columns <- c(
  "timestamp",
  "locality",
  "zone_type",

  # Target
  "water_demand",

  # Original/context variables
  "area_km2",
  "population",
  "population_density",
  "residential_pct",
  "commercial_pct",
  "industrial_pct",
  "storage_capacity_mld",
  "pipeline_age_years",
  "distribution_efficiency",
  "temperature_c",
  "humidity_pct",
  "rainfall_mm",
  "wind_speed_kmh",
  "is_rain_event",
  "is_holiday",
  "is_weekend",
  "festival_period",
  "hour",
  "minute",
  "day_of_week",
  "month",
  "year",
  "water_supply_hours",

  # Historical features
  "lag_1",
  "lag_4",
  "lag_96",
  "lag_672",

  # Rolling features
  "rolling_mean_4",
  "rolling_mean_96",
  "rolling_mean_672",
  "rolling_sd_96",
  "rolling_sd_672",

  # Cyclical features
  "hour_sin",
  "hour_cos",
  "dow_sin",
  "dow_cos",
  "month_sin",
  "month_cos",

  # Weather-derived
  "temperature_humidity_index",
  "heavy_rain_event",
  "high_wind_event",
  "high_temperature_event",

  # Infrastructure-derived
  "storage_per_1000_people",
  "supply_efficiency_index",
  "infrastructure_stress",

  # Land-use
  "land_use_total_pct",
  "dominant_land_use",

  # Descriptive only
  "demand_per_1000_people"
)

# Only retain columns that exist.
output_columns <- output_columns[
  output_columns %in% names(df)
]

enriched_output <- df[, output_columns]

write.csv(
  enriched_output,
  OUTPUT_FILE,
  row.names = FALSE
)

cat("Saved enriched feature dataset:\n")
cat(OUTPUT_FILE, "\n\n")

# -----------------------------------------------------------------------------
# 21. Feature description table
# -----------------------------------------------------------------------------

feature_description <- data.frame(

  feature = c(

    # Historical
    "lag_1",
    "lag_4",
    "lag_96",
    "lag_672",
    "rolling_mean_4",
    "rolling_mean_96",
    "rolling_mean_672",
    "rolling_sd_96",
    "rolling_sd_672",

    # Time
    "hour_sin",
    "hour_cos",
    "dow_sin",
    "dow_cos",
    "month_sin",
    "month_cos",

    # Weather
    "temperature_humidity_index",
    "heavy_rain_event",
    "high_wind_event",
    "high_temperature_event",

    # Infrastructure
    "storage_per_1000_people",
    "supply_efficiency_index",
    "infrastructure_stress",

    # Land use
    "land_use_total_pct",
    "dominant_land_use",

    # Descriptive
    "demand_per_1000_people"
  ),

  category = c(

    rep("Historical demand", 9),
    rep("Cyclical time", 6),
    rep("Weather derived", 4),
    rep("Infrastructure derived", 3),
    rep("Land use", 2),
    "Descriptive only"
  ),

  description = c(

    "Previous locality demand observation.",
    "Demand four observations earlier.",
    "Demand 96 observations earlier (approximately 24 hours for 15-minute data).",
    "Demand 672 observations earlier (approximately 7 days for 15-minute data).",

    "Mean of the previous 4 locality demand observations.",
    "Mean of the previous 96 locality demand observations.",
    "Mean of the previous 672 locality demand observations.",
    "Standard deviation of the previous 96 locality demand observations.",
    "Standard deviation of the previous 672 locality demand observations.",

    "Sine encoding of hour of day.",
    "Cosine encoding of hour of day.",
    "Sine encoding of day of week.",
    "Cosine encoding of day of week.",
    "Sine encoding of month.",
    "Cosine encoding of month.",

    "Temperature-humidity interaction.",
    "Indicator for rainfall of at least 5 mm.",
    "Indicator for wind speed of at least 20 km/h.",
    "Indicator for temperature of at least 35 degrees Celsius.",

    "Storage capacity relative to population.",
    "Water supply hours multiplied by distribution efficiency.",
    "Pipeline age multiplied by infrastructure inefficiency.",

    "Check of residential + commercial + industrial percentages.",
    "Largest land-use category.",

    "Current demand per 1,000 people. NOT a model predictor because it uses the target."
  ),

  leakage_safe = c(

    rep(TRUE, 9),
    rep(TRUE, 6),
    rep(TRUE, 4),
    rep(TRUE, 3),
    rep(TRUE, 2),
    FALSE
  ),

  stringsAsFactors = FALSE
)

write.csv(
  feature_description,
  FEATURE_DESCRIPTION_FILE,
  row.names = FALSE
)

# -----------------------------------------------------------------------------
# 22. Generate report
# -----------------------------------------------------------------------------

total_rows <- nrow(df)

total_columns <- ncol(df)

complete_cases_model_features <-
  sum(
    complete.cases(
      df[, model_features, drop = FALSE]
    )
  )

usable_percentage <-
  100 * complete_cases_model_features / total_rows

report_lines <- c(

  "============================================================",
  "PHASE 2 — ENRICHED FEATURE ENGINEERING REPORT",
  "============================================================",
  "",
  paste("Input rows:", total_rows),
  paste("Input columns:", total_columns),
  paste("Number of localities:", length(unique(df$locality))),
  paste("Localities:", paste(unique(df$locality), collapse = ", ")),
  "",
  "Historical demand features:",
  "  lag_1",
  "  lag_4",
  "  lag_96",
  "  lag_672",
  "  rolling_mean_4",
  "  rolling_mean_96",
  "  rolling_mean_672",
  "  rolling_sd_96",
  "  rolling_sd_672",
  "",
  "Cyclical time features:",
  "  hour_sin / hour_cos",
  "  dow_sin / dow_cos",
  "  month_sin / month_cos",
  "",
  "Weather-derived features:",
  "  temperature_humidity_index",
  "  heavy_rain_event",
  "  high_wind_event",
  "  high_temperature_event",
  "",
  "Infrastructure-derived features:",
  "  storage_per_1000_people",
  "  supply_efficiency_index",
  "  infrastructure_stress",
  "",
  "Leakage protection:",
  "  water_demand is the prediction target.",
  "  total_water_demand_reference is excluded from predictors.",
  "  synthetic_demand_share is excluded from predictors.",
  "  demand_per_1000_people is descriptive only and excluded from predictors.",
  "  Historical features use previous observations only.",
  "",
  paste(
    "Complete rows across model features:",
    complete_cases_model_features
  ),

  paste(
    "Usable percentage:",
    sprintf("%.2f%%", usable_percentage)
  ),

  "",
  paste(
    "Rows with invalid land-use totals:",
    land_use_invalid
  ),

  "",
  "Validation:",
  "  Required columns: PASSED",
  "  Leakage audit: PASSED",
  "  Lag validation: PASSED",
  "",
  "Synthetic data disclosure:",
  "  Locality, demographic, infrastructure and weather/context variables",
  "  originate from the Phase 1 synthetic experimental context layer.",
  "  They are not presented as measured real-world locality/weather records.",
  "",
  "Original SOWEKI source data was not modified.",
  "",
  "============================================================"
)

writeLines(
  report_lines,
  REPORT_FILE
)

# -----------------------------------------------------------------------------
# 23. Final console summary
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("PHASE 2 COMPLETED\n")
cat("============================================================\n")

cat("Rows:", nrow(enriched_output), "\n")
cat("Columns:", ncol(enriched_output), "\n")

cat(
  "Model features:",
  length(model_features),
  "\n"
)

cat(
  "Complete model-feature rows:",
  complete_cases_model_features,
  "\n"
)

cat(
  "Usable rows:",
  sprintf("%.2f%%", usable_percentage),
  "\n"
)

cat("\nCreated files:\n")
cat("1.", OUTPUT_FILE, "\n")
cat("2.", FEATURE_DESCRIPTION_FILE, "\n")
cat("3.", REPORT_FILE, "\n")

cat("\nLeakage audit: PASSED\n")
cat("Lag validation: PASSED\n")

cat("\nOriginal SOWEKI dataset remains untouched.\n")

cat("\n============================================================\n")