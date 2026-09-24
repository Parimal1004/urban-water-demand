# ============================================================
# PHASE 4 - TRAIN ENRICHED MODELS
# Urban Water Demand Prediction
# ============================================================

cat("\n============================================\n")
cat(" PHASE 4 - ENRICHED MODEL TRAINING\n")
cat("============================================\n\n")

# ------------------------------------------------------------
# 1. Setup
# ------------------------------------------------------------

options(stringsAsFactors = FALSE)

PROJECT_DIR <- getwd()

TRAIN_FILE <- file.path(PROJECT_DIR, "data", "enriched_train.csv")
TEST_FILE  <- file.path(PROJECT_DIR, "data", "enriched_test.csv")

MODEL_DIR  <- file.path(PROJECT_DIR, "models")
OUTPUT_DIR <- file.path(PROJECT_DIR, "outputs")

dir.create(MODEL_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

cat("Project directory:\n", PROJECT_DIR, "\n\n")

# ------------------------------------------------------------
# 2. Load packages
# ------------------------------------------------------------

required_packages <- c("randomForest")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(randomForest)

# ------------------------------------------------------------
# 3. Load data
# ------------------------------------------------------------

cat("Loading training data...\n")

train <- read.csv(
  TRAIN_FILE,
  stringsAsFactors = FALSE
)

cat("Training rows:", nrow(train), "\n")
cat("Training columns:", ncol(train), "\n\n")

cat("Loading test data...\n")

test <- read.csv(
  TEST_FILE,
  stringsAsFactors = FALSE
)

cat("Testing rows:", nrow(test), "\n")
cat("Testing columns:", ncol(test), "\n\n")

# ------------------------------------------------------------
# 4. Convert categorical variables
# ------------------------------------------------------------

categorical_features <- c(
  "locality",
  "zone_type",
  "dominant_land_use"
)

for (col in categorical_features) {

  if (col %in% names(train)) {

    train[[col]] <- as.factor(train[[col]])

    # Match test factor levels to training levels
    if (col %in% names(test)) {
      test[[col]] <- factor(
        test[[col]],
        levels = levels(train[[col]])
      )
    }
  }
}

# ------------------------------------------------------------
# 5. Define model features
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

target <- "water_demand"

# ------------------------------------------------------------
# 6. Validate columns
# ------------------------------------------------------------

missing_train <- setdiff(
  c(model_features, target),
  names(train)
)

missing_test <- setdiff(
  c(model_features, target),
  names(test)
)

if (length(missing_train) > 0) {
  stop(
    paste(
      "Missing columns in training data:",
      paste(missing_train, collapse = ", ")
    )
  )
}

if (length(missing_test) > 0) {
  stop(
    paste(
      "Missing columns in test data:",
      paste(missing_test, collapse = ", ")
    )
  )
}

# ------------------------------------------------------------
# 7. Remove incomplete rows
# ------------------------------------------------------------

cat("Preparing model data...\n")

train_complete <- complete.cases(
  train[, c(model_features, target)]
)

test_complete <- complete.cases(
  test[, c(model_features, target)]
)

train_model <- train[train_complete, ]
test_model  <- test[test_complete, ]

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
# 8. Prepare matrices/data frames
# ------------------------------------------------------------

x_train <- train_model[, model_features]
y_train <- train_model[[target]]

x_test <- test_model[, model_features]
y_test <- test_model[[target]]

# ------------------------------------------------------------
# 9. Linear Regression
# ------------------------------------------------------------

cat("============================================\n")
cat(" TRAINING LINEAR REGRESSION\n")
cat("============================================\n\n")

cat("Training Linear Regression...\n")

lm_formula <- as.formula(
  paste(
    target,
    "~",
    paste(model_features, collapse = " + ")
  )
)

lm_model <- lm(
  lm_formula,
  data = train_model
)

cat("Linear Regression completed.\n\n")

# Predictions
lm_predictions <- predict(
  lm_model,
  newdata = test_model
)

# Metrics
lm_mae <- mean(
  abs(lm_predictions - y_test),
  na.rm = TRUE
)

lm_rmse <- sqrt(
  mean(
    (lm_predictions - y_test)^2,
    na.rm = TRUE
  )
)

lm_r2 <- 1 -
  sum((y_test - lm_predictions)^2) /
  sum((y_test - mean(y_test))^2)

cat("Linear Regression Results:\n")
cat("MAE :", round(lm_mae, 6), "\n")
cat("RMSE:", round(lm_rmse, 6), "\n")
cat("R2  :", round(lm_r2, 6), "\n\n")

saveRDS(
  lm_model,
  file.path(
    MODEL_DIR,
    "enriched_linear_model.rds"
  )
)

# ------------------------------------------------------------
# 10. Random Forest sampling
# ------------------------------------------------------------

cat("============================================\n")
cat(" RANDOM FOREST PREPARATION\n")
cat("============================================\n\n")

RF_SAMPLE_PER_LOCALITY <- 10000

set.seed(42)

localities <- unique(train_model$locality)

cat(
  "Number of localities:",
  length(localities),
  "\n"
)

cat(
  "Maximum RF samples per locality:",
  RF_SAMPLE_PER_LOCALITY,
  "\n\n"
)

sample_indices <- c()

for (loc in localities) {

  loc_indices <- which(
    train_model$locality == loc
  )

  sample_size <- min(
    RF_SAMPLE_PER_LOCALITY,
    length(loc_indices)
  )

  selected <- sample(
    loc_indices,
    size = sample_size,
    replace = FALSE
  )

  sample_indices <- c(
    sample_indices,
    selected
  )

  cat(
    "Locality:",
    as.character(loc),
    "| available:",
    length(loc_indices),
    "| selected:",
    sample_size,
    "\n"
  )
}

rf_train <- train_model[sample_indices, ]

cat(
  "\nTotal RF training rows:",
  nrow(rf_train),
  "\n\n"
)

x_rf <- rf_train[, model_features]
y_rf <- rf_train[[target]]

# ------------------------------------------------------------
# 11. Random Forest with visible progress
# ------------------------------------------------------------

cat("============================================\n")
cat(" TRAINING RANDOM FOREST\n")
cat("============================================\n\n")

TOTAL_TREES <- 50
TREES_PER_BATCH <- 5

cat(
  "Total trees:",
  TOTAL_TREES,
  "\n"
)

cat(
  "Trees per batch:",
  TREES_PER_BATCH,
  "\n\n"
)

cat("Starting Random Forest training...\n\n")

set.seed(42)

rf_model <- NULL

start_time <- Sys.time()

for (start_tree in seq(
  1,
  TOTAL_TREES,
  by = TREES_PER_BATCH
)) {

  end_tree <- min(
    start_tree + TREES_PER_BATCH - 1,
    TOTAL_TREES
  )

  trees_in_batch <- end_tree - start_tree + 1

  cat(
    sprintf(
      "[%s] Training trees %d-%d of %d...\n",
      format(Sys.time(), "%H:%M:%S"),
      start_tree,
      end_tree,
      TOTAL_TREES
    )
  )

  flush.console()

  batch_model <- randomForest(
    x = x_rf,
    y = y_rf,
    ntree = trees_in_batch,
    importance = TRUE
  )

  if (is.null(rf_model)) {

    rf_model <- batch_model

  } else {

    rf_model <- combine(
      rf_model,
      batch_model
    )
  }

  elapsed <- as.numeric(
    difftime(
      Sys.time(),
      start_time,
      units = "secs"
    )
  )

  progress_pct <- (
    end_tree / TOTAL_TREES
  ) * 100

  cat(
    sprintf(
      "    Progress: %d/%d trees (%.0f%%) | elapsed: %.1f sec\n\n",
      end_tree,
      TOTAL_TREES,
      progress_pct,
      elapsed
    )
  )

  flush.console()
}

training_time <- as.numeric(
  difftime(
    Sys.time(),
    start_time,
    units = "secs"
  )
)

cat("============================================\n")
cat(" RANDOM FOREST TRAINING COMPLETED\n")
cat("============================================\n")

cat(
  "Training time:",
  round(training_time, 2),
  "seconds\n\n"
)

# ------------------------------------------------------------
# 12. Random Forest prediction
# ------------------------------------------------------------

cat("Generating Random Forest predictions...\n")

rf_predictions <- predict(
  rf_model,
  newdata = x_test
)

cat("Predictions completed.\n\n")

# ------------------------------------------------------------
# 13. Random Forest metrics
# ------------------------------------------------------------

rf_mae <- mean(
  abs(rf_predictions - y_test),
  na.rm = TRUE
)

rf_rmse <- sqrt(
  mean(
    (rf_predictions - y_test)^2,
    na.rm = TRUE
  )
)

rf_r2 <- 1 -
  sum((y_test - rf_predictions)^2) /
  sum((y_test - mean(y_test))^2)

cat("Random Forest Results:\n")
cat("MAE :", round(rf_mae, 6), "\n")
cat("RMSE:", round(rf_rmse, 6), "\n")
cat("R2  :", round(rf_r2, 6), "\n\n")

# ------------------------------------------------------------
# 14. Save Random Forest model
# ------------------------------------------------------------

saveRDS(
  rf_model,
  file.path(
    MODEL_DIR,
    "enriched_random_forest_model.rds"
  )
)

cat("Random Forest model saved.\n\n")

# ------------------------------------------------------------
# 15. Model comparison
# ------------------------------------------------------------

comparison <- data.frame(
  Model = c(
    "Enriched Linear Regression",
    "Enriched Random Forest"
  ),
  MAE = c(
    lm_mae,
    rf_mae
  ),
  RMSE = c(
    lm_rmse,
    rf_rmse
  ),
  R2 = c(
    lm_r2,
    rf_r2
  )
)

write.csv(
  comparison,
  file.path(
    OUTPUT_DIR,
    "enriched_model_comparison.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# 16. Test predictions
# ------------------------------------------------------------

prediction_output <- data.frame(
  actual_water_demand = y_test,
  linear_regression_prediction = lm_predictions,
  random_forest_prediction = rf_predictions
)

if ("locality" %in% names(test_model)) {
  prediction_output$locality <-
    test_model$locality
}

if ("timestamp" %in% names(test_model)) {
  prediction_output$timestamp <-
    test_model$timestamp
}

write.csv(
  prediction_output,
  file.path(
    OUTPUT_DIR,
    "enriched_test_predictions.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# 17. Feature importance
# ------------------------------------------------------------

cat("Extracting Random Forest feature importance...\n")

importance_matrix <- importance(
  rf_model
)

importance_df <- data.frame(
  Feature = rownames(importance_matrix),
  importance_matrix,
  row.names = NULL
)

# Sort by IncNodePurity when available
if ("IncNodePurity" %in% names(importance_df)) {

  importance_df <- importance_df[
    order(
      importance_df$IncNodePurity,
      decreasing = TRUE
    ),
  ]
}

write.csv(
  importance_df,
  file.path(
    OUTPUT_DIR,
    "enriched_feature_importance.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# 18. Training report
# ------------------------------------------------------------

report_lines <- c(
  "============================================",
  "ENRICHED MODEL TRAINING REPORT",
  "Urban Water Demand Prediction",
  "============================================",
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
    "RF training rows:",
    nrow(rf_train)
  ),
  paste(
    "Number of predictors:",
    length(model_features)
  ),
  paste(
    "Random Forest trees:",
    TOTAL_TREES
  ),
  paste(
    "RF trees per batch:",
    TREES_PER_BATCH
  ),
  paste(
    "RF training time (seconds):",
    round(training_time, 2)
  ),
  "",
  "--------------------------------------------",
  "LINEAR REGRESSION",
  "--------------------------------------------",
  paste(
    "MAE:",
    round(lm_mae, 6)
  ),
  paste(
    "RMSE:",
    round(lm_rmse, 6)
  ),
  paste(
    "R2:",
    round(lm_r2, 6)
  ),
  "",
  "--------------------------------------------",
  "RANDOM FOREST",
  "--------------------------------------------",
  paste(
    "MAE:",
    round(rf_mae, 6)
  ),
  paste(
    "RMSE:",
    round(rf_rmse, 6)
  ),
  paste(
    "R2:",
    round(rf_r2, 6)
  ),
  "",
  "--------------------------------------------",
  "TRAINING DESIGN",
  "--------------------------------------------",
  "Random Forest used locality-stratified sampling.",
  paste(
    "Maximum samples per locality:",
    RF_SAMPLE_PER_LOCALITY
  ),
  "The full test set was retained for evaluation.",
  "Original SOWEKI dataset was not modified.",
  "No target-derived reference columns were used",
  "as model predictors.",
  "",
  "============================================"
)

writeLines(
  report_lines,
  file.path(
    OUTPUT_DIR,
    "enriched_model_training_report.txt"
  )
)

# ------------------------------------------------------------
# 19. Final summary
# ------------------------------------------------------------

cat("\n============================================\n")
cat(" PHASE 4 COMPLETED SUCCESSFULLY\n")
cat("============================================\n\n")

cat("Model comparison:\n")
print(comparison)

cat("\nFiles created:\n")
cat(
  "- models/enriched_linear_model.rds\n",
  "- models/enriched_random_forest_model.rds\n",
  "- outputs/enriched_model_comparison.csv\n",
  "- outputs/enriched_test_predictions.csv\n",
  "- outputs/enriched_feature_importance.csv\n",
  "- outputs/enriched_model_training_report.txt\n"
)

cat("\n============================================\n")