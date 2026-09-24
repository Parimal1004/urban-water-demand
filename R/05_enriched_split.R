# ============================================================
# PHASE 3 — ENRICHED TRAIN / TEST SPLIT + LEAKAGE CHECKS
# ============================================================

cat("============================================================\n")
cat("PHASE 3 — ENRICHED TRAIN / TEST SPLIT + LEAKAGE CHECKS\n")
cat("============================================================\n\n")


# ------------------------------------------------------------
# 1. PROJECT PATHS
# ------------------------------------------------------------

PROJECT_DIR <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

INPUT_FILE <- file.path(
  PROJECT_DIR,
  "data",
  "enriched_features.csv"
)

TRAIN_FILE <- file.path(
  PROJECT_DIR,
  "data",
  "enriched_train.csv"
)

TEST_FILE <- file.path(
  PROJECT_DIR,
  "data",
  "enriched_test.csv"
)

REPORT_FILE <- file.path(
  PROJECT_DIR,
  "outputs",
  "enriched_split_report.txt"
)

cat("Project directory:\n")
cat(PROJECT_DIR, "\n\n")

cat("Input file:\n")
cat(INPUT_FILE, "\n\n")


# ------------------------------------------------------------
# 2. CHECK INPUT FILE
# ------------------------------------------------------------

if (!file.exists(INPUT_FILE)) {
  stop(
    paste0(
      "Input file not found:\n",
      INPUT_FILE,
      "\n\nRun Phase 2 first."
    )
  )
}


# ------------------------------------------------------------
# 3. LOAD DATA
# ------------------------------------------------------------

data <- read.csv(
  INPUT_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

cat("Rows loaded:", nrow(data), "\n")
cat("Columns loaded:", ncol(data), "\n\n")


# ------------------------------------------------------------
# 4. REQUIRED COLUMN CHECK
# ------------------------------------------------------------

required_columns <- c(
  "timestamp",
  "locality",
  "water_demand"
)

missing_columns <- setdiff(
  required_columns,
  names(data)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing required columns:",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}

cat("Required columns: OK\n\n")


# ------------------------------------------------------------
# 5. ROBUST TIMESTAMP CONVERSION
# ------------------------------------------------------------

cat("Converting timestamps...\n")

timestamp_raw <- trimws(
  as.character(data$timestamp)
)

# Try the formats used by the project.
timestamp_formats <- c(
  "%Y-%m-%d %H:%M:%S",
  "%Y-%m-%d %H:%M",
  "%Y/%m/%d %H:%M:%S",
  "%Y/%m/%d %H:%M",
  "%d-%m-%Y %H:%M:%S",
  "%d-%m-%Y %H:%M"
)

timestamp_converted <- as.POSIXct(
  rep(NA_character_, length(timestamp_raw)),
  tz = "UTC"
)

for (fmt in timestamp_formats) {

  missing_idx <- which(
    is.na(timestamp_converted)
  )

  if (length(missing_idx) == 0) {
    break
  }

  parsed <- as.POSIXct(
    timestamp_raw[missing_idx],
    format = fmt,
    tz = "UTC"
  )

  valid_idx <- !is.na(parsed)

  if (any(valid_idx)) {
    timestamp_converted[
      missing_idx[valid_idx]
    ] <- parsed[valid_idx]
  }
}

# Final fallback using as.POSIXct
# This handles standard ISO timestamps that R can parse directly.
missing_idx <- which(
  is.na(timestamp_converted)
)

if (length(missing_idx) > 0) {

  parsed <- suppressWarnings(
    as.POSIXct(
      timestamp_raw[missing_idx],
      tz = "UTC"
    )
  )

  valid_idx <- !is.na(parsed)

  if (any(valid_idx)) {
    timestamp_converted[
      missing_idx[valid_idx]
    ] <- parsed[valid_idx]
  }
}

data$timestamp <- timestamp_converted

failed_timestamps <- sum(
  is.na(data$timestamp)
)

cat(
  "Timestamp conversion failures:",
  failed_timestamps,
  "\n"
)

if (failed_timestamps > 0) {

  cat(
    "\nExamples of timestamps that could not be parsed:\n"
  )

  print(
    head(
      timestamp_raw[
        is.na(data$timestamp)
      ],
      10
    )
  )

  stop(
    "Timestamp conversion failed."
  )
}

cat("Timestamp conversion: PASSED\n\n")


# ------------------------------------------------------------
# 6. SORT DATA
# ------------------------------------------------------------

data <- data[
  order(
    data$locality,
    data$timestamp
  ),
]

row.names(data) <- NULL

cat(
  "Data sorted by locality and timestamp.\n\n"
)


# ------------------------------------------------------------
# 7. DATA QUALITY CHECKS
# ------------------------------------------------------------

cat("Running data quality checks...\n")

missing_target <- sum(
  is.na(data$water_demand)
)

duplicate_rows <- sum(
  duplicated(
    data[
      c(
        "locality",
        "timestamp"
      )
    ]
  )
)

negative_target <- sum(
  data$water_demand < 0,
  na.rm = TRUE
)

cat(
  "Missing target values:",
  missing_target,
  "\n"
)

cat(
  "Duplicate locality-timestamp rows:",
  duplicate_rows,
  "\n"
)

cat(
  "Negative demand values:",
  negative_target,
  "\n\n"
)

if (missing_target > 0) {
  stop(
    "Missing values found in water_demand."
  )
}

if (duplicate_rows > 0) {
  stop(
    "Duplicate locality-timestamp combinations found."
  )
}

if (negative_target > 0) {
  stop(
    "Negative water_demand values found."
  )
}

cat("Data quality checks: PASSED\n\n")


# ------------------------------------------------------------
# 8. MODEL FEATURES
# ------------------------------------------------------------

model_features <- c(

  # Locality / zone
  "locality",
  "zone_type",

  # Geography / population
  "area_km2",
  "population",
  "population_density",

  # Land use
  "residential_pct",
  "commercial_pct",
  "industrial_pct",
  "dominant_land_use",

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


# ------------------------------------------------------------
# 9. MODEL FEATURE CHECK
# ------------------------------------------------------------

missing_features <- setdiff(
  model_features,
  names(data)
)

if (length(missing_features) > 0) {

  stop(
    paste(
      "Missing model features:",
      paste(
        missing_features,
        collapse = ", "
      )
    )
  )
}

cat("Model feature check: PASSED\n")
cat(
  "Number of model features:",
  length(model_features),
  "\n\n"
)


# ------------------------------------------------------------
# 10. LEAKAGE AUDIT
# ------------------------------------------------------------

cat("Running leakage audit...\n")

forbidden_features <- c(
  "water_demand",
  "total_water_demand_reference",
  "synthetic_demand_share",
  "demand_per_1000_people"
)

leakage_features_present <- intersect(
  model_features,
  forbidden_features
)

if (length(leakage_features_present) > 0) {

  stop(
    paste(
      "LEAKAGE DETECTED:",
      paste(
        leakage_features_present,
        collapse = ", "
      )
    )
  )
}

cat("Leakage audit: PASSED\n\n")


# ------------------------------------------------------------
# 11. IDENTIFY LOCALITIES
# ------------------------------------------------------------

localities <- unique(
  data$locality
)

cat(
  "Number of localities:",
  length(localities),
  "\n"
)

cat(
  "Localities:",
  paste(
    localities,
    collapse = ", "
  ),
  "\n\n"
)


# ------------------------------------------------------------
# 12. CHRONOLOGICAL 80/20 SPLIT
# ------------------------------------------------------------

cat(
  "Creating chronological 80/20 train-test split...\n\n"
)

cat(
  "No random shuffling is used.\n"
)

cat(
  "First 80% of each locality -> training\n"
)

cat(
  "Final 20% of each locality -> testing\n\n"
)

train_list <- list()
test_list <- list()

for (loc in localities) {

  loc_data <- data[
    data$locality == loc,
  ]

  loc_data <- loc_data[
    order(loc_data$timestamp),
  ]

  row.names(loc_data) <- NULL

  n <- nrow(loc_data)

  if (n < 2) {
    stop(
      paste(
        "Not enough observations for:",
        loc
      )
    )
  }

  train_n <- floor(
    0.80 * n
  )

  train_list[[loc]] <- loc_data[
    1:train_n,
  ]

  test_list[[loc]] <- loc_data[
    (train_n + 1):n,
  ]
}

train_data <- do.call(
  rbind,
  train_list
)

test_data <- do.call(
  rbind,
  test_list
)

row.names(train_data) <- NULL
row.names(test_data) <- NULL

cat(
  "Training rows:",
  nrow(train_data),
  "\n"
)

cat(
  "Testing rows:",
  nrow(test_data),
  "\n\n"
)


# ------------------------------------------------------------
# 13. CHRONOLOGICAL VALIDATION
# ------------------------------------------------------------

cat(
  "Checking chronological separation...\n\n"
)

chronology_valid <- TRUE

for (loc in localities) {

  train_loc <- train_data[
    train_data$locality == loc,
  ]

  test_loc <- test_data[
    test_data$locality == loc,
  ]

  train_last <- max(
    train_loc$timestamp
  )

  test_first <- min(
    test_loc$timestamp
  )

  valid <- train_last < test_first

  cat(
    loc,
    ":",
    ifelse(
      valid,
      "PASSED",
      "FAILED"
    ),
    "\n"
  )

  if (!valid) {
    chronology_valid <- FALSE
  }
}

if (!chronology_valid) {
  stop(
    "Chronological split validation FAILED."
  )
}

cat(
  "\nChronological split validation: PASSED\n\n"
)


# ------------------------------------------------------------
# 14. TRAIN / TEST OVERLAP CHECK
# ------------------------------------------------------------

cat(
  "Checking train/test overlap...\n"
)

train_keys <- paste(
  train_data$locality,
  train_data$timestamp,
  sep = "___"
)

test_keys <- paste(
  test_data$locality,
  test_data$timestamp,
  sep = "___"
)

overlap <- intersect(
  train_keys,
  test_keys
)

if (length(overlap) > 0) {

  stop(
    paste(
      "Train/test overlap detected:",
      length(overlap)
    )
  )
}

cat(
  "Train/test overlap: NONE\n\n"
)


# ------------------------------------------------------------
# 15. FEATURE COMPLETENESS
# ------------------------------------------------------------

cat(
  "Checking model-feature completeness...\n"
)

train_complete <- complete.cases(
  train_data[
    ,
    model_features,
    drop = FALSE
  ]
)

test_complete <- complete.cases(
  test_data[
    ,
    model_features,
    drop = FALSE
  ]
)

train_usable <- sum(
  train_complete
)

test_usable <- sum(
  test_complete
)

cat(
  "Complete training rows:",
  train_usable,
  "/",
  nrow(train_data),
  "\n"
)

cat(
  "Complete testing rows:",
  test_usable,
  "/",
  nrow(test_data),
  "\n\n"
)


# ------------------------------------------------------------
# 16. CREATE FINAL MODEL DATASETS
# ------------------------------------------------------------

train_model <- train_data[
  train_complete,
]

test_model <- test_data[
  test_complete,
]

row.names(train_model) <- NULL
row.names(test_model) <- NULL

cat(
  "Usable training rows:",
  nrow(train_model),
  "\n"
)

cat(
  "Usable testing rows:",
  nrow(test_model),
  "\n\n"
)


# ------------------------------------------------------------
# 17. TARGET CHECK
# ------------------------------------------------------------

if (any(is.na(train_model$water_demand))) {

  stop(
    "Missing training target values."
  )
}

if (any(is.na(test_model$water_demand))) {

  stop(
    "Missing testing target values."
  )
}

cat(
  "Target completeness: PASSED\n\n"
)


# ------------------------------------------------------------
# 18. FINAL LEAKAGE CHECK
# ------------------------------------------------------------

cat(
  "Final leakage check...\n"
)

actual_predictor_leakage <- intersect(
  forbidden_features,
  model_features
)

if (length(actual_predictor_leakage) > 0) {

  stop(
    paste(
      "Leakage detected:",
      paste(
        actual_predictor_leakage,
        collapse = ", "
      )
    )
  )
}

cat(
  "Final leakage check: PASSED\n\n"
)


# ------------------------------------------------------------
# 19. SAVE TRAIN DATA
# ------------------------------------------------------------

write.csv(
  train_model,
  TRAIN_FILE,
  row.names = FALSE
)

cat(
  "Saved training dataset:\n",
  TRAIN_FILE,
  "\n\n"
)


# ------------------------------------------------------------
# 20. SAVE TEST DATA
# ------------------------------------------------------------

write.csv(
  test_model,
  TEST_FILE,
  row.names = FALSE
)

cat(
  "Saved testing dataset:\n",
  TEST_FILE,
  "\n\n"
)


# ------------------------------------------------------------
# 21. GENERATE REPORT
# ------------------------------------------------------------

total_usable <- (
  nrow(train_model) +
  nrow(test_model)
)

train_pct <- (
  nrow(train_model) /
    total_usable
) * 100

test_pct <- (
  nrow(test_model) /
    total_usable
) * 100

report_lines <- c(

  "============================================================",
  "PHASE 3 — ENRICHED TRAIN / TEST SPLIT REPORT",
  "============================================================",
  "",

  paste(
    "Source rows:",
    nrow(data)
  ),

  paste(
    "Source columns:",
    ncol(data)
  ),

  "",

  paste(
    "Number of localities:",
    length(localities)
  ),

  paste(
    "Localities:",
    paste(
      localities,
      collapse = ", "
    )
  ),

  "",

  "Split strategy:",
  "Chronological 80/20 split performed separately for each locality.",
  "No random shuffling was used.",

  "",

  paste(
    "Training rows:",
    nrow(train_model)
  ),

  paste(
    "Testing rows:",
    nrow(test_model)
  ),

  paste(
    "Training percentage:",
    round(
      train_pct,
      2
    ),
    "%"
  ),

  paste(
    "Testing percentage:",
    round(
      test_pct,
      2
    ),
    "%"
  ),

  "",

  paste(
    "Model features:",
    length(model_features)
  ),

  "",

  "Data quality: PASSED",
  "Leakage audit: PASSED",
  "Chronological split: PASSED",
  "Train/test overlap: NONE",
  "Target completeness: PASSED",
  "Final leakage check: PASSED",

  "",

  "Excluded leakage columns:",
  paste(
    forbidden_features,
    collapse = ", "
  ),

  "",

  "Original SOWEKI dataset: UNCHANGED",

  "",

  "============================================================"
)

writeLines(
  report_lines,
  REPORT_FILE
)


# ------------------------------------------------------------
# 22. FINAL SUMMARY
# ------------------------------------------------------------

cat(
  "============================================================\n"
)

cat(
  "PHASE 3 COMPLETED\n"
)

cat(
  "============================================================\n"
)

cat(
  "Source rows:",
  nrow(data),
  "\n"
)

cat(
  "Source columns:",
  ncol(data),
  "\n"
)

cat(
  "Model features:",
  length(model_features),
  "\n\n"
)

cat(
  "Training rows:",
  nrow(train_model),
  "\n"
)

cat(
  "Testing rows:",
  nrow(test_model),
  "\n\n"
)

cat(
  "Leakage audit: PASSED\n"
)

cat(
  "Chronological split: PASSED\n"
)

cat(
  "Train/test overlap: NONE\n"
)

cat(
  "Target completeness: PASSED\n"
)

cat(
  "Final leakage check: PASSED\n\n"
)

cat(
  "Created files:\n"
)

cat(
  "1.",
  TRAIN_FILE,
  "\n"
)

cat(
  "2.",
  TEST_FILE,
  "\n"
)

cat(
  "3.",
  REPORT_FILE,
  "\n\n"
)

cat(
  "Original SOWEKI dataset remains untouched.\n"
)

cat(
  "============================================================\n"
)