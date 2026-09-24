# ============================================================
# PHASE 7 - LOCALITY COMPARISON
# Urban Water Demand Prediction
# ============================================================

cat("\n============================================\n")
cat(" PHASE 7 - LOCALITY COMPARISON\n")
cat("============================================\n\n")

options(stringsAsFactors = FALSE)

# ------------------------------------------------------------
# 1. Project setup
# ------------------------------------------------------------

PROJECT_DIR <- getwd()

DATA_DIR <- file.path(
  PROJECT_DIR,
  "data"
)

OUTPUT_DIR <- file.path(
  PROJECT_DIR,
  "outputs"
)

dir.create(
  OUTPUT_DIR,
  showWarnings = FALSE,
  recursive = TRUE
)

# ------------------------------------------------------------
# 2. Input files
# ------------------------------------------------------------

ENRICHED_DATA_FILE <- file.path(
  DATA_DIR,
  "urban_water_demand_enriched.csv"
)

PREDICTION_FILE <- file.path(
  OUTPUT_DIR,
  "enriched_predictions_with_errors.csv"
)

LOCALITY_ERROR_FILE <- file.path(
  OUTPUT_DIR,
  "locality_error_metrics.csv"
)

# ------------------------------------------------------------
# 3. Validate files
# ------------------------------------------------------------

required_files <- c(
  ENRICHED_DATA_FILE,
  PREDICTION_FILE,
  LOCALITY_ERROR_FILE
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {

  stop(
    paste(
      "Required file(s) missing:",
      paste(
        missing_files,
        collapse = ", "
      )
    )
  )
}

cat("All required files found.\n\n")

# ------------------------------------------------------------
# 4. Load enriched locality data
# ------------------------------------------------------------

cat("Loading enriched locality data...\n")

data <- read.csv(
  ENRICHED_DATA_FILE,
  stringsAsFactors = FALSE
)

cat(
  "Rows:",
  nrow(data),
  "\n"
)

cat(
  "Columns:",
  ncol(data),
  "\n\n"
)

# ------------------------------------------------------------
# 5. Load predictions
# ------------------------------------------------------------

cat("Loading prediction data...\n")

predictions <- read.csv(
  PREDICTION_FILE,
  stringsAsFactors = FALSE
)

cat(
  "Prediction rows:",
  nrow(predictions),
  "\n\n"
)

# ------------------------------------------------------------
# 6. Load locality error metrics
# ------------------------------------------------------------

locality_errors <- read.csv(
  LOCALITY_ERROR_FILE,
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 7. Required locality columns
# ------------------------------------------------------------

context_columns <- c(
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
  "water_supply_hours"
)

missing_context <- setdiff(
  context_columns,
  names(data)
)

if (length(missing_context) > 0) {

  stop(
    paste(
      "Missing locality context columns:",
      paste(
        missing_context,
        collapse = ", "
      )
    )
  )
}

# ------------------------------------------------------------
# 8. Create locality context summary
# ------------------------------------------------------------

cat("Creating locality context summary...\n")

locality_context <- unique(
  data[
    ,
    context_columns,
    drop = FALSE
  ]
)

locality_context <- locality_context[
  !duplicated(
    locality_context$locality
  ),
  ,
  drop = FALSE
]

rownames(locality_context) <- NULL

# ------------------------------------------------------------
# 9. Calculate demand statistics
# ------------------------------------------------------------

cat("Calculating demand statistics...\n")

if (!"water_demand" %in% names(data)) {

  stop(
    "water_demand column not found in enriched dataset."
  )
}

# Calculate each statistic separately.
# This avoids the aggregate()/do.call() issue.

mean_demand <- aggregate(
  water_demand ~ locality,
  data = data,
  FUN = mean,
  na.rm = TRUE
)

median_demand <- aggregate(
  water_demand ~ locality,
  data = data,
  FUN = median,
  na.rm = TRUE
)

minimum_demand <- aggregate(
  water_demand ~ locality,
  data = data,
  FUN = min,
  na.rm = TRUE
)

maximum_demand <- aggregate(
  water_demand ~ locality,
  data = data,
  FUN = max,
  na.rm = TRUE
)

demand_sd <- aggregate(
  water_demand ~ locality,
  data = data,
  FUN = sd,
  na.rm = TRUE
)

names(mean_demand)[2] <-
  "mean_demand"

names(median_demand)[2] <-
  "median_demand"

names(minimum_demand)[2] <-
  "minimum_demand"

names(maximum_demand)[2] <-
  "maximum_demand"

names(demand_sd)[2] <-
  "demand_sd"

demand_summary <- merge(
  mean_demand,
  median_demand,
  by = "locality"
)

demand_summary <- merge(
  demand_summary,
  minimum_demand,
  by = "locality"
)

demand_summary <- merge(
  demand_summary,
  maximum_demand,
  by = "locality"
)

demand_summary <- merge(
  demand_summary,
  demand_sd,
  by = "locality"
)

# ------------------------------------------------------------
# 10. Calculate average weather conditions
# ------------------------------------------------------------

cat("Calculating weather summary...\n")

weather_summary <- aggregate(
  cbind(
    temperature_c,
    humidity_pct,
    rainfall_mm,
    wind_speed_kmh
  ) ~ locality,
  data = data,
  FUN = mean,
  na.rm = TRUE
)

names(weather_summary)[
  names(weather_summary) == "temperature_c"
] <- "mean_temperature_c"

names(weather_summary)[
  names(weather_summary) == "humidity_pct"
] <- "mean_humidity_pct"

names(weather_summary)[
  names(weather_summary) == "rainfall_mm"
] <- "mean_rainfall_mm"

names(weather_summary)[
  names(weather_summary) == "wind_speed_kmh"
] <- "mean_wind_speed_kmh"

# ------------------------------------------------------------
# 11. Calculate weekday/weekend demand
# ------------------------------------------------------------

cat("Calculating weekday/weekend demand...\n")

weekday_data <- data[
  data$is_weekend == 0,
  ,
  drop = FALSE
]

weekend_data <- data[
  data$is_weekend == 1,
  ,
  drop = FALSE
]

weekday_summary <- aggregate(
  water_demand ~ locality,
  data = weekday_data,
  FUN = mean,
  na.rm = TRUE
)

names(weekday_summary)[2] <-
  "mean_weekday_demand"

weekend_summary <- aggregate(
  water_demand ~ locality,
  data = weekend_data,
  FUN = mean,
  na.rm = TRUE
)

names(weekend_summary)[2] <-
  "mean_weekend_demand"

# ------------------------------------------------------------
# 12. Holiday and festival demand
# ------------------------------------------------------------

cat("Calculating holiday/festival demand...\n")

holiday_data <- data[
  data$is_holiday == 1,
  ,
  drop = FALSE
]

festival_data <- data[
  data$festival_period == 1,
  ,
  drop = FALSE
]

if (nrow(holiday_data) > 0) {

  holiday_summary <- aggregate(
    water_demand ~ locality,
    data = holiday_data,
    FUN = mean,
    na.rm = TRUE
  )

  names(holiday_summary)[2] <-
    "mean_holiday_demand"

} else {

  holiday_summary <- data.frame(
    locality = character(0),
    mean_holiday_demand = numeric(0)
  )
}

if (nrow(festival_data) > 0) {

  festival_summary <- aggregate(
    water_demand ~ locality,
    data = festival_data,
    FUN = mean,
    na.rm = TRUE
  )

  names(festival_summary)[2] <-
    "mean_festival_demand"

} else {

  festival_summary <- data.frame(
    locality = character(0),
    mean_festival_demand = numeric(0)
  )
}

# ------------------------------------------------------------
# 13. Merge locality summaries
# ------------------------------------------------------------

cat("Combining locality information...\n")

locality_comparison <- merge(
  locality_context,
  demand_summary,
  by = "locality",
  all.x = TRUE
)

locality_comparison <- merge(
  locality_comparison,
  weather_summary,
  by = "locality",
  all.x = TRUE
)

locality_comparison <- merge(
  locality_comparison,
  weekday_summary,
  by = "locality",
  all.x = TRUE
)

locality_comparison <- merge(
  locality_comparison,
  weekend_summary,
  by = "locality",
  all.x = TRUE
)

locality_comparison <- merge(
  locality_comparison,
  holiday_summary,
  by = "locality",
  all.x = TRUE
)

locality_comparison <- merge(
  locality_comparison,
  festival_summary,
  by = "locality",
  all.x = TRUE
)

# ------------------------------------------------------------
# 14. Merge model error metrics
# ------------------------------------------------------------

locality_comparison <- merge(
  locality_comparison,
  locality_errors,
  by.x = "locality",
  by.y = "Locality",
  all.x = TRUE
)

# ------------------------------------------------------------
# 15. Additional locality indicators
# ------------------------------------------------------------

locality_comparison$storage_per_1000_people <-
  locality_comparison$storage_capacity_mld /
  (
    locality_comparison$population /
      1000
  )

locality_comparison$supply_efficiency_percent <-
  locality_comparison$distribution_efficiency

locality_comparison$infrastructure_stress_indicator <-
  (
    locality_comparison$pipeline_age_years /
      max(
        locality_comparison$pipeline_age_years,
        na.rm = TRUE
      )
  ) +
  (
    1 -
      locality_comparison$distribution_efficiency /
        100
  )

# ------------------------------------------------------------
# 16. Reorder columns
# ------------------------------------------------------------

preferred_order <- c(

  "locality",
  "zone_type",

  "population",
  "area_km2",
  "population_density",

  "residential_pct",
  "commercial_pct",
  "industrial_pct",

  "storage_capacity_mld",
  "pipeline_age_years",
  "distribution_efficiency",
  "water_supply_hours",

  "mean_temperature_c",
  "mean_humidity_pct",
  "mean_rainfall_mm",
  "mean_wind_speed_kmh",

  "mean_demand",
  "median_demand",
  "minimum_demand",
  "maximum_demand",
  "demand_sd",

  "mean_weekday_demand",
  "mean_weekend_demand",
  "mean_holiday_demand",
  "mean_festival_demand",

  "Linear_MAE",
  "Linear_RMSE",
  "Linear_R2",

  "Random_Forest_MAE",
  "Random_Forest_RMSE",
  "Random_Forest_R2",

  "storage_per_1000_people",
  "supply_efficiency_percent",
  "infrastructure_stress_indicator"
)

available_order <- preferred_order[
  preferred_order %in%
    names(locality_comparison)
]

locality_comparison <- locality_comparison[
  ,
  available_order,
  drop = FALSE
]

# ------------------------------------------------------------
# 17. Sort locality
# ------------------------------------------------------------

locality_comparison <- locality_comparison[
  order(
    locality_comparison$locality
  ),
  ,
  drop = FALSE
]

rownames(locality_comparison) <- NULL

# ------------------------------------------------------------
# 18. Print comparison
# ------------------------------------------------------------

cat("\n============================================\n")
cat(" LOCALITY COMPARISON\n")
cat("============================================\n\n")

print(
  locality_comparison,
  row.names = FALSE
)

# ------------------------------------------------------------
# 19. Model performance by locality
# ------------------------------------------------------------

model_performance <- data.frame(

  locality =
    locality_comparison$locality,

  linear_mae =
    locality_comparison$Linear_MAE,

  random_forest_mae =
    locality_comparison$Random_Forest_MAE,

  linear_rmse =
    locality_comparison$Linear_RMSE,

  random_forest_rmse =
    locality_comparison$Random_Forest_RMSE,

  linear_r2 =
    locality_comparison$Linear_R2,

  random_forest_r2 =
    locality_comparison$Random_Forest_R2
)

model_performance$mae_difference <-
  model_performance$linear_mae -
  model_performance$random_forest_mae

model_performance$rmse_difference <-
  model_performance$linear_rmse -
  model_performance$random_forest_rmse

model_performance$r2_difference <-
  model_performance$random_forest_r2 -
  model_performance$linear_r2

# ------------------------------------------------------------
# 20. Save locality comparison
# ------------------------------------------------------------

write.csv(
  locality_comparison,
  file.path(
    OUTPUT_DIR,
    "locality_comparison.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# 21. Save model performance
# ------------------------------------------------------------

write.csv(
  model_performance,
  file.path(
    OUTPUT_DIR,
    "locality_model_performance.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# 22. Generate report
# ------------------------------------------------------------

report <- c(

  "============================================",
  "PHASE 7 - LOCALITY COMPARISON REPORT",
  "Urban Water Demand Prediction",
  "============================================",
  "",

  paste(
    "Number of localities:",
    nrow(locality_comparison)
  ),

  "",
  "LOCALITY SUMMARY",
  "--------------------------------------------"
)

for (
  i in seq_len(
    nrow(locality_comparison)
  )
) {

  row <- locality_comparison[i, ]

  report <- c(
    report,

    paste(
      "Locality:",
      row$locality
    ),

    paste(
      "Zone type:",
      row$zone_type
    ),

    paste(
      "Population:",
      row$population
    ),

    paste(
      "Population density:",
      round(
        row$population_density,
        2
      )
    ),

    paste(
      "Mean demand:",
      round(
        row$mean_demand,
        4
      )
    ),

    paste(
      "Maximum demand:",
      round(
        row$maximum_demand,
        4
      )
    ),

    paste(
      "Random Forest MAE:",
      round(
        row$Random_Forest_MAE,
        6
      )
    ),

    paste(
      "Random Forest RMSE:",
      round(
        row$Random_Forest_RMSE,
        6
      )
    ),

    paste(
      "Random Forest R2:",
      round(
        row$Random_Forest_R2,
        6
      )
    ),

    ""
  )
}

report <- c(
  report,

  "IMPORTANT NOTE",
  "--------------------------------------------",

  "Locality attributes and weather/context values",
  "are synthetic experimental variables generated",
  "for the enriched modelling framework.",

  "They should be explicitly disclosed as synthetic",
  "in the final project report.",

  "This analysis is descriptive and comparative;",
  "it does not establish causal relationships.",

  "",
  "============================================"
)

writeLines(
  report,
  file.path(
    OUTPUT_DIR,
    "locality_comparison_report.txt"
  )
)

# ------------------------------------------------------------
# 23. Final output
# ------------------------------------------------------------

cat("\n============================================\n")
cat(" PHASE 7 COMPLETED SUCCESSFULLY\n")
cat("============================================\n\n")

cat("Created files:\n")

cat(
  "- outputs/locality_comparison.csv\n"
)

cat(
  "- outputs/locality_model_performance.csv\n"
)

cat(
  "- outputs/locality_comparison_report.txt\n"
)

cat("\n============================================\n")