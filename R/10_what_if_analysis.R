# ============================================================
# PHASE 8 - WHAT-IF ANALYSIS
# Urban Water Demand Prediction
# ============================================================

options(stringsAsFactors = FALSE)

cat("\n============================================\n")
cat(" PHASE 8 - WHAT-IF ANALYSIS\n")
cat("============================================\n\n")

# ------------------------------------------------------------
# 1. Load required package
# ------------------------------------------------------------

if (!requireNamespace("randomForest", quietly = TRUE)) {
  stop(
    "Package 'randomForest' is not installed. Run: install.packages('randomForest')"
  )
}

library(randomForest)

# ------------------------------------------------------------
# 2. Project setup
# ------------------------------------------------------------

PROJECT_DIR <- getwd()

DATA_DIR <- file.path(PROJECT_DIR, "data")
MODEL_DIR <- file.path(PROJECT_DIR, "models")
OUTPUT_DIR <- file.path(PROJECT_DIR, "outputs")

dir.create(
  OUTPUT_DIR,
  showWarnings = FALSE,
  recursive = TRUE
)

MODEL_FILE <- file.path(
  MODEL_DIR,
  "enriched_random_forest_model.rds"
)

DATA_FILE <- file.path(
  DATA_DIR,
  "enriched_features.csv"
)

# ------------------------------------------------------------
# 3. Validate files
# ------------------------------------------------------------

if (!file.exists(MODEL_FILE)) {
  stop(
    paste(
      "Random Forest model not found:",
      MODEL_FILE
    )
  )
}

if (!file.exists(DATA_FILE)) {
  stop(
    paste(
      "Enriched feature data not found:",
      DATA_FILE
    )
  )
}

cat("Required files found.\n\n")

# ------------------------------------------------------------
# 4. Load model and data
# ------------------------------------------------------------

cat("Loading Random Forest model...\n")

rf_model <- readRDS(
  MODEL_FILE
)

cat("Random Forest model loaded.\n\n")

cat("Loading enriched feature data...\n")

data <- read.csv(
  DATA_FILE,
  stringsAsFactors = FALSE
)

cat(
  "Rows:",
  nrow(data),
  "\n\n"
)

# ------------------------------------------------------------
# 5. Convert categorical variables
# ------------------------------------------------------------

categorical_features <- c(
  "locality",
  "zone_type",
  "dominant_land_use"
)

for (col in categorical_features) {

  if (col %in% names(data)) {

    data[[col]] <- as.factor(
      data[[col]]
    )
  }
}

# ------------------------------------------------------------
# 6. Exact model features
# ------------------------------------------------------------

model_features <- c(
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
  "water_supply_hours",
  "lag_1",
  "lag_4",
  "lag_96",
  "lag_672",
  "rolling_mean_4",
  "rolling_mean_96",
  "rolling_mean_672",
  "rolling_sd_96",
  "rolling_sd_672",
  "hour_sin",
  "hour_cos",
  "dow_sin",
  "dow_cos",
  "month_sin",
  "month_cos",
  "temperature_humidity_index",
  "heavy_rain_event",
  "high_wind_event",
  "high_temperature_event",
  "storage_per_1000_people",
  "supply_efficiency_index",
  "infrastructure_stress"
)

# ------------------------------------------------------------
# 7. Helper function
# ------------------------------------------------------------

safe_sd <- function(x) {

  value <- sd(
    x,
    na.rm = TRUE
  )

  if (is.na(value)) {
    return(0)
  }

  value
}

# ------------------------------------------------------------
# 8. Prediction function
# ------------------------------------------------------------

create_prediction <- function(
  scenario,
  reference_row
) {

  new_data <- reference_row

  # ----------------------------------------------------------
  # Direct scenario variables
  # ----------------------------------------------------------

  new_data$temperature_c <-
    scenario$temperature_c

  new_data$humidity_pct <-
    scenario$humidity_pct

  new_data$rainfall_mm <-
    scenario$rainfall_mm

  new_data$wind_speed_kmh <-
    scenario$wind_speed_kmh

  new_data$water_supply_hours <-
    scenario$water_supply_hours

  new_data$storage_capacity_mld <-
    scenario$storage_capacity_mld

  new_data$pipeline_age_years <-
    scenario$pipeline_age_years

  new_data$distribution_efficiency <-
    scenario$distribution_efficiency

  new_data$is_holiday <-
    scenario$is_holiday

  new_data$is_weekend <-
    scenario$is_weekend

  new_data$festival_period <-
    scenario$festival_period

  # ----------------------------------------------------------
  # Historical demand features
  # ----------------------------------------------------------

  new_data$lag_1 <-
    scenario$lag_1

  new_data$lag_4 <-
    scenario$lag_4

  new_data$lag_96 <-
    scenario$lag_96

  new_data$lag_672 <-
    scenario$lag_672

  # ----------------------------------------------------------
  # Weather-derived variables
  # ----------------------------------------------------------

  new_data$is_rain_event <-
    as.integer(
      scenario$rainfall_mm >= 5
    )

  new_data$heavy_rain_event <-
    as.integer(
      scenario$rainfall_mm >= 10
    )

  new_data$high_wind_event <-
    as.integer(
      scenario$wind_speed_kmh >= 25
    )

  new_data$high_temperature_event <-
    as.integer(
      scenario$temperature_c >= 35
    )

  # ----------------------------------------------------------
  # Weather interaction
  # ----------------------------------------------------------

  new_data$temperature_humidity_index <-
    scenario$temperature_c *
    (
      1 +
        scenario$humidity_pct / 100
    )

  # ----------------------------------------------------------
  # Infrastructure-derived variables
  # ----------------------------------------------------------

  population_value <-
    as.numeric(
      new_data$population
    )

  new_data$storage_per_1000_people <-
    scenario$storage_capacity_mld /
    (
      population_value / 1000
    )

  new_data$supply_efficiency_index <-
    scenario$water_supply_hours *
    (
      scenario$distribution_efficiency / 100
    )

  max_pipeline_age <-
    max(
      data$pipeline_age_years,
      na.rm = TRUE
    )

  new_data$infrastructure_stress <-
    (
      scenario$pipeline_age_years /
        max_pipeline_age
    ) +
    (
      1 -
        scenario$distribution_efficiency / 100
    )

  # ----------------------------------------------------------
  # Rolling features
  # ----------------------------------------------------------

  lag_values <- c(
    scenario$lag_1,
    scenario$lag_4,
    scenario$lag_96,
    scenario$lag_672
  )

  new_data$rolling_mean_4 <-
    mean(
      c(
        scenario$lag_1,
        scenario$lag_4
      ),
      na.rm = TRUE
    )

  new_data$rolling_mean_96 <-
    mean(
      c(
        scenario$lag_1,
        scenario$lag_4,
        scenario$lag_96
      ),
      na.rm = TRUE
    )

  new_data$rolling_mean_672 <-
    mean(
      lag_values,
      na.rm = TRUE
    )

  new_data$rolling_sd_96 <-
    safe_sd(
      c(
        scenario$lag_1,
        scenario$lag_4,
        scenario$lag_96
      )
    )

  new_data$rolling_sd_672 <-
    safe_sd(
      lag_values
    )

  # ----------------------------------------------------------
  # Match factor levels used by model
  # ----------------------------------------------------------

  new_data$locality <- factor(
    new_data$locality,
    levels = levels(data$locality)
  )

  new_data$zone_type <- factor(
    new_data$zone_type,
    levels = levels(data$zone_type)
  )

  if ("dominant_land_use" %in% names(data)) {

    new_data$dominant_land_use <- factor(
      new_data$dominant_land_use,
      levels = levels(data$dominant_land_use)
    )
  }

  # ----------------------------------------------------------
  # Select ONLY model features
  # ----------------------------------------------------------

  prediction_data <- new_data[
    ,
    model_features,
    drop = FALSE
  ]

  # ----------------------------------------------------------
  # Predict
  # ----------------------------------------------------------

  prediction <- predict(
    rf_model,
    newdata = prediction_data
  )

  as.numeric(prediction)
}

# ------------------------------------------------------------
# 9. Find complete reference observations
# ------------------------------------------------------------

cat("Preparing reference observations...\n")

reference_rows <- data[
  complete.cases(
    data[
      ,
      model_features,
      drop = FALSE
    ]
  ),
  ,
  drop = FALSE
]

if (nrow(reference_rows) == 0) {

  stop(
    "No complete reference observations available."
  )
}

# One observation per locality

reference_rows <- reference_rows[
  !duplicated(
    reference_rows$locality
  ),
  ,
  drop = FALSE
]

cat(
  "Reference localities:",
  nrow(reference_rows),
  "\n\n"
)

# ------------------------------------------------------------
# 10. Create baseline predictions
# ------------------------------------------------------------

cat("Creating baseline scenarios...\n")

baseline_results <- data.frame()

for (
  i in seq_len(
    nrow(reference_rows)
  )
) {

  ref <- reference_rows[i, ]

  scenario <- list(

    temperature_c =
      as.numeric(ref$temperature_c),

    humidity_pct =
      as.numeric(ref$humidity_pct),

    rainfall_mm =
      as.numeric(ref$rainfall_mm),

    wind_speed_kmh =
      as.numeric(ref$wind_speed_kmh),

    water_supply_hours =
      as.numeric(ref$water_supply_hours),

    storage_capacity_mld =
      as.numeric(ref$storage_capacity_mld),

    pipeline_age_years =
      as.numeric(ref$pipeline_age_years),

    distribution_efficiency =
      as.numeric(ref$distribution_efficiency),

    is_holiday =
      as.numeric(ref$is_holiday),

    is_weekend =
      as.numeric(ref$is_weekend),

    festival_period =
      as.numeric(ref$festival_period),

    lag_1 =
      as.numeric(ref$lag_1),

    lag_4 =
      as.numeric(ref$lag_4),

    lag_96 =
      as.numeric(ref$lag_96),

    lag_672 =
      as.numeric(ref$lag_672)
  )

  prediction <- create_prediction(
    scenario,
    ref
  )

  baseline_results <- rbind(
    baseline_results,
    data.frame(

      locality =
        as.character(ref$locality),

      zone_type =
        as.character(ref$zone_type),

      baseline_prediction =
        prediction
    )
  )
}

cat(
  "Baseline predictions created.\n\n"
)

# ------------------------------------------------------------
# 11. Define what-if scenarios
# ------------------------------------------------------------

cat("Creating what-if scenarios...\n")

scenario_definitions <- list(

  Higher_Temperature = list(
    temperature_change = 5,
    humidity_change = 0,
    rainfall_change = 0,
    supply_change = 0,
    efficiency_change = 0
  ),

  Heavy_Rainfall = list(
    temperature_change = 0,
    humidity_change = 5,
    rainfall_change = 20,
    supply_change = 0,
    efficiency_change = 0
  ),

  Reduced_Supply = list(
    temperature_change = 0,
    humidity_change = 0,
    rainfall_change = 0,
    supply_change = -3,
    efficiency_change = 0
  ),

  Improved_Efficiency = list(
    temperature_change = 0,
    humidity_change = 0,
    rainfall_change = 0,
    supply_change = 0,
    efficiency_change = 5
  ),

  Older_Pipeline = list(
    temperature_change = 0,
    humidity_change = 0,
    rainfall_change = 0,
    supply_change = 0,
    efficiency_change = -5
  )
)

# ------------------------------------------------------------
# 12. Generate scenario predictions
# ------------------------------------------------------------

scenario_results <- data.frame()

for (
  scenario_name in names(
    scenario_definitions
  )
) {

  changes <- scenario_definitions[[scenario_name]]

  for (
    i in seq_len(
      nrow(reference_rows)
    )
  ) {

    ref <- reference_rows[i, ]

    baseline_prediction <-
      baseline_results[
        baseline_results$locality ==
          as.character(ref$locality),
        "baseline_prediction"
      ][1]

    scenario <- list(

      temperature_c =
        as.numeric(ref$temperature_c) +
        changes$temperature_change,

      humidity_pct =
        min(
          100,
          max(
            0,
            as.numeric(ref$humidity_pct) +
            changes$humidity_change
          )
        ),

      rainfall_mm =
        max(
          0,
          as.numeric(ref$rainfall_mm) +
          changes$rainfall_change
        ),

      wind_speed_kmh =
        as.numeric(ref$wind_speed_kmh),

      water_supply_hours =
        max(
          0,
          as.numeric(ref$water_supply_hours) +
          changes$supply_change
        ),

      storage_capacity_mld =
        as.numeric(ref$storage_capacity_mld),

      pipeline_age_years =
        as.numeric(ref$pipeline_age_years),

      distribution_efficiency =
        min(
          100,
          max(
            0,
            as.numeric(ref$distribution_efficiency) +
            changes$efficiency_change
          )
        ),

      is_holiday =
        as.numeric(ref$is_holiday),

      is_weekend =
        as.numeric(ref$is_weekend),

      festival_period =
        as.numeric(ref$festival_period),

      lag_1 =
        as.numeric(ref$lag_1),

      lag_4 =
        as.numeric(ref$lag_4),

      lag_96 =
        as.numeric(ref$lag_96),

      lag_672 =
        as.numeric(ref$lag_672)
    )

    prediction <- create_prediction(
      scenario,
      ref
    )

    absolute_change <-
      prediction -
      baseline_prediction

    percent_change <-
      ifelse(
        baseline_prediction != 0,
        (
          absolute_change /
          baseline_prediction
        ) * 100,
        NA
      )

    scenario_results <- rbind(
      scenario_results,
      data.frame(

        locality =
          as.character(ref$locality),

        zone_type =
          as.character(ref$zone_type),

        scenario =
          scenario_name,

        baseline_prediction =
          baseline_prediction,

        scenario_prediction =
          prediction,

        absolute_change =
          absolute_change,

        percent_change =
          percent_change
      )
    )
  }
}

# ------------------------------------------------------------
# 13. Display results
# ------------------------------------------------------------

cat("\n============================================\n")
cat(" WHAT-IF ANALYSIS RESULTS\n")
cat("============================================\n\n")

print(
  scenario_results,
  row.names = FALSE
)

# ------------------------------------------------------------
# 14. Save baseline predictions
# ------------------------------------------------------------

write.csv(
  baseline_results,
  file.path(
    OUTPUT_DIR,
    "what_if_baseline_predictions.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# 15. Save scenario results
# ------------------------------------------------------------

write.csv(
  scenario_results,
  file.path(
    OUTPUT_DIR,
    "what_if_scenario_results.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# 16. Scenario summary
# ------------------------------------------------------------

scenario_summary <- aggregate(
  cbind(
    absolute_change,
    percent_change
  ) ~ scenario,
  data = scenario_results,
  FUN = mean,
  na.rm = TRUE
)

names(scenario_summary)[
  names(scenario_summary) ==
    "absolute_change"
] <- "mean_absolute_change"

names(scenario_summary)[
  names(scenario_summary) ==
    "percent_change"
] <- "mean_percent_change"

cat("\n============================================\n")
cat(" SCENARIO SUMMARY\n")
cat("============================================\n\n")

print(
  scenario_summary,
  row.names = FALSE
)

write.csv(
  scenario_summary,
  file.path(
    OUTPUT_DIR,
    "what_if_scenario_summary.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# 17. Report
# ------------------------------------------------------------

report <- c(

  "============================================",
  "PHASE 8 - WHAT-IF ANALYSIS REPORT",
  "Urban Water Demand Prediction",
  "============================================",
  "",

  "Model used:",
  "Enriched Random Forest",
  "",

  "SCENARIOS TESTED",
  "--------------------------------------------",
  "Higher Temperature: +5 C",
  "Heavy Rainfall: +20 mm rainfall and +5% humidity",
  "Reduced Supply: -3 hours",
  "Improved Efficiency: +5 percentage points",
  "Older Pipeline: -5 percentage points efficiency",
  "",

  "INTERPRETATION NOTE",
  "--------------------------------------------",
  "These are model-based scenario predictions,",
  "not causal estimates.",
  "",
  "The contextual variables in the enriched",
  "dataset are synthetic experimental features.",
  "",

  "OUTPUT FILES",
  "--------------------------------------------",
  "what_if_baseline_predictions.csv",
  "what_if_scenario_results.csv",
  "what_if_scenario_summary.csv",
  "what_if_analysis_report.txt",
  "",

  "============================================"
)

writeLines(
  report,
  file.path(
    OUTPUT_DIR,
    "what_if_analysis_report.txt"
  )
)

# ------------------------------------------------------------
# 18. Finish
# ------------------------------------------------------------

cat("\n============================================\n")
cat(" PHASE 8 COMPLETED SUCCESSFULLY\n")
cat("============================================\n\n")

cat("Created files:\n")
cat("- outputs/what_if_baseline_predictions.csv\n")
cat("- outputs/what_if_scenario_results.csv\n")
cat("- outputs/what_if_scenario_summary.csv\n")
cat("- outputs/what_if_analysis_report.txt\n")

cat("\n")