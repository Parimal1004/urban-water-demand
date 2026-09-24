# ============================================================
# PHASE 6 - EXPLAINABILITY + ERROR ANALYSIS
# Urban Water Demand Prediction
# ============================================================

cat("\n============================================\n")
cat(" PHASE 6 - EXPLAINABILITY + ERROR ANALYSIS\n")
cat("============================================\n\n")

options(stringsAsFactors = FALSE)

# ------------------------------------------------------------
# 1. Project setup
# ------------------------------------------------------------

PROJECT_DIR <- getwd()

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

PREDICTION_FILE <- file.path(
  OUTPUT_DIR,
  "enriched_test_predictions.csv"
)

IMPORTANCE_FILE <- file.path(
  OUTPUT_DIR,
  "enriched_feature_importance.csv"
)

COMPARISON_FILE <- file.path(
  OUTPUT_DIR,
  "enriched_model_comparison.csv"
)

# ------------------------------------------------------------
# 3. Validate files
# ------------------------------------------------------------

required_files <- c(
  PREDICTION_FILE,
  IMPORTANCE_FILE,
  COMPARISON_FILE
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

cat("All Phase 4 output files found.\n\n")

# ------------------------------------------------------------
# 4. Load prediction data
# ------------------------------------------------------------

cat("Loading predictions...\n")

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
# 5. Validate prediction columns
# ------------------------------------------------------------

required_prediction_columns <- c(
  "actual_water_demand",
  "linear_regression_prediction",
  "random_forest_prediction"
)

missing_prediction_columns <- setdiff(
  required_prediction_columns,
  names(predictions)
)

if (length(missing_prediction_columns) > 0) {
  
  stop(
    paste(
      "Missing prediction columns:",
      paste(
        missing_prediction_columns,
        collapse = ", "
      )
    )
  )
}

# ------------------------------------------------------------
# 6. Calculate residuals and absolute errors
# ------------------------------------------------------------

predictions$linear_residual <-
  predictions$actual_water_demand -
  predictions$linear_regression_prediction

predictions$rf_residual <-
  predictions$actual_water_demand -
  predictions$random_forest_prediction

predictions$linear_absolute_error <-
  abs(
    predictions$linear_residual
  )

predictions$rf_absolute_error <-
  abs(
    predictions$rf_residual
  )

predictions$linear_squared_error <-
  predictions$linear_residual^2

predictions$rf_squared_error <-
  predictions$rf_residual^2

# ------------------------------------------------------------
# 7. Overall error analysis
# ------------------------------------------------------------

actual <- predictions$actual_water_demand

linear_pred <-
  predictions$linear_regression_prediction

rf_pred <-
  predictions$random_forest_prediction

# Linear Regression

linear_mae <- mean(
  abs(
    actual - linear_pred
  ),
  na.rm = TRUE
)

linear_rmse <- sqrt(
  mean(
    (
      actual - linear_pred
    )^2,
    na.rm = TRUE
  )
)

linear_r2 <- 1 -
  sum(
    (
      actual - linear_pred
    )^2,
    na.rm = TRUE
  ) /
  sum(
    (
      actual - mean(
        actual,
        na.rm = TRUE
      )
    )^2,
    na.rm = TRUE
  )

# Random Forest

rf_mae <- mean(
  abs(
    actual - rf_pred
  ),
  na.rm = TRUE
)

rf_rmse <- sqrt(
  mean(
    (
      actual - rf_pred
    )^2,
    na.rm = TRUE
  )
)

rf_r2 <- 1 -
  sum(
    (
      actual - rf_pred
    )^2,
    na.rm = TRUE
  ) /
  sum(
    (
      actual - mean(
        actual,
        na.rm = TRUE
      )
    )^2,
    na.rm = TRUE
  )

overall_metrics <- data.frame(
  
  Model = c(
    "Enriched Linear Regression",
    "Enriched Random Forest"
  ),
  
  MAE = c(
    linear_mae,
    rf_mae
  ),
  
  RMSE = c(
    linear_rmse,
    rf_rmse
  ),
  
  R2 = c(
    linear_r2,
    rf_r2
  )
)

# ------------------------------------------------------------
# 8. Print overall metrics
# ------------------------------------------------------------

cat("============================================\n")
cat(" OVERALL ERROR ANALYSIS\n")
cat("============================================\n\n")

print(overall_metrics)

# ------------------------------------------------------------
# 9. Error analysis by locality
# ------------------------------------------------------------

if (!"locality" %in% names(predictions)) {
  
  cat(
    "\nWARNING: locality column not found.\n",
    "Locality-level analysis will be skipped.\n"
  )
  
  locality_metrics <- NULL
  
} else {
  
  cat(
    "\nCalculating error metrics by locality...\n"
  )
  
  locality_values <- unique(
    predictions$locality
  )
  
  locality_results <- list()
  
  for (loc in locality_values) {
    
    subset_data <- predictions[
      predictions$locality == loc,
      ,
      drop = FALSE
    ]
    
    loc_actual <-
      subset_data$actual_water_demand
    
    loc_linear <-
      subset_data$linear_regression_prediction
    
    loc_rf <-
      subset_data$random_forest_prediction
    
    # Linear metrics
    
    loc_linear_mae <- mean(
      abs(
        loc_actual - loc_linear
      ),
      na.rm = TRUE
    )
    
    loc_linear_rmse <- sqrt(
      mean(
        (
          loc_actual - loc_linear
        )^2,
        na.rm = TRUE
      )
    )
    
    loc_linear_r2 <- 1 -
      sum(
        (
          loc_actual - loc_linear
        )^2,
        na.rm = TRUE
      ) /
      sum(
        (
          loc_actual -
            mean(
              loc_actual,
              na.rm = TRUE
            )
        )^2,
        na.rm = TRUE
      )
    
    # RF metrics
    
    loc_rf_mae <- mean(
      abs(
        loc_actual - loc_rf
      ),
      na.rm = TRUE
    )
    
    loc_rf_rmse <- sqrt(
      mean(
        (
          loc_actual - loc_rf
        )^2,
        na.rm = TRUE
      )
    )
    
    loc_rf_r2 <- 1 -
      sum(
        (
          loc_actual - loc_rf
        )^2,
        na.rm = TRUE
      ) /
      sum(
        (
          loc_actual -
            mean(
              loc_actual,
              na.rm = TRUE
            )
        )^2,
        na.rm = TRUE
      )
    
    locality_results[[length(
      locality_results
    ) + 1]] <- data.frame(
      
      Locality = as.character(loc),
      
      Rows = nrow(
        subset_data
      ),
      
      Linear_MAE =
        loc_linear_mae,
      
      Linear_RMSE =
        loc_linear_rmse,
      
      Linear_R2 =
        loc_linear_r2,
      
      Random_Forest_MAE =
        loc_rf_mae,
      
      Random_Forest_RMSE =
        loc_rf_rmse,
      
      Random_Forest_R2 =
        loc_rf_r2
    )
  }
  
  locality_metrics <- do.call(
    rbind,
    locality_results
  )
}

# ------------------------------------------------------------
# 10. Print locality metrics
# ------------------------------------------------------------

if (!is.null(locality_metrics)) {
  
  cat("\n============================================\n")
  cat(" ERROR BY LOCALITY\n")
  cat("============================================\n\n")
  
  print(
    locality_metrics,
    row.names = FALSE
  )
}

# ------------------------------------------------------------
# 11. Feature importance
# ------------------------------------------------------------

cat("\n============================================\n")
cat(" FEATURE IMPORTANCE\n")
cat("============================================\n\n")

importance <- read.csv(
  IMPORTANCE_FILE,
  stringsAsFactors = FALSE
)

# Sort using IncNodePurity when available

if (
  "IncNodePurity"
  %in%
  names(importance)
) {
  
  importance <- importance[
    order(
      importance$IncNodePurity,
      decreasing = TRUE
    ),
    ,
    drop = FALSE
  ]
  
  cat(
    "Top 15 features by IncNodePurity:\n\n"
  )
  
  print(
    head(
      importance,
      15
    ),
    row.names = FALSE
  )
}

# ------------------------------------------------------------
# 12. Top 15 feature importance
# ------------------------------------------------------------

top_features <- head(
  importance,
  15
)

write.csv(
  top_features,
  file.path(
    OUTPUT_DIR,
    "top_15_feature_importance.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# 13. Largest prediction errors
# ------------------------------------------------------------

cat("\n============================================\n")
cat(" LARGEST PREDICTION ERRORS\n")
cat("============================================\n\n")

# Random Forest errors

largest_rf_errors <- predictions[
  order(
    predictions$rf_absolute_error,
    decreasing = TRUE
  ),
  ,
  drop = FALSE
]

largest_rf_errors <- head(
  largest_rf_errors,
  20
)

cat(
  "Top 20 Random Forest errors:\n\n"
)

columns_to_show <- c(
  "actual_water_demand",
  "random_forest_prediction",
  "rf_residual",
  "rf_absolute_error"
)

if ("locality" %in% names(
  largest_rf_errors
)) {
  
  columns_to_show <- c(
    "locality",
    columns_to_show
  )
}

if ("timestamp" %in% names(
  largest_rf_errors
)) {
  
  columns_to_show <- c(
    "timestamp",
    columns_to_show
  )
}

print(
  largest_rf_errors[
    ,
    columns_to_show,
    drop = FALSE
  ],
  row.names = FALSE
)

write.csv(
  largest_rf_errors,
  file.path(
    OUTPUT_DIR,
    "largest_random_forest_errors.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# 14. Residual summary
# ------------------------------------------------------------

residual_summary <- data.frame(
  
  Model = c(
    "Enriched Linear Regression",
    "Enriched Random Forest"
  ),
  
  Mean_Residual = c(
    mean(
      predictions$linear_residual,
      na.rm = TRUE
    ),
    mean(
      predictions$rf_residual,
      na.rm = TRUE
    )
  ),
  
  Median_Absolute_Error = c(
    median(
      predictions$linear_absolute_error,
      na.rm = TRUE
    ),
    median(
      predictions$rf_absolute_error,
      na.rm = TRUE
    )
  ),
  
  Maximum_Absolute_Error = c(
    max(
      predictions$linear_absolute_error,
      na.rm = TRUE
    ),
    max(
      predictions$rf_absolute_error,
      na.rm = TRUE
    )
  ),
  
  Error_SD = c(
    sd(
      predictions$linear_residual,
      na.rm = TRUE
    ),
    sd(
      predictions$rf_residual,
      na.rm = TRUE
    )
  )
)

cat("\n============================================\n")
cat(" RESIDUAL SUMMARY\n")
cat("============================================\n\n")

print(
  residual_summary,
  row.names = FALSE
)

# ------------------------------------------------------------
# 15. Save residual summary
# ------------------------------------------------------------

write.csv(
  residual_summary,
  file.path(
    OUTPUT_DIR,
    "residual_summary.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# 16. Save overall metrics
# ------------------------------------------------------------

write.csv(
  overall_metrics,
  file.path(
    OUTPUT_DIR,
    "enriched_error_metrics.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# 17. Save predictions with errors
# ------------------------------------------------------------

write.csv(
  predictions,
  file.path(
    OUTPUT_DIR,
    "enriched_predictions_with_errors.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# 18. Generate text report
# ------------------------------------------------------------

report <- c(

  "============================================",
  "PHASE 6 - EXPLAINABILITY + ERROR ANALYSIS",
  "Urban Water Demand Prediction",
  "============================================",
  "",

  "OVERALL MODEL PERFORMANCE",
  "--------------------------------------------",

  paste(
    "Linear Regression MAE:",
    sprintf(
      "%.6f",
      linear_mae
    )
  ),

  paste(
    "Linear Regression RMSE:",
    sprintf(
      "%.6f",
      linear_rmse
    )
  ),

  paste(
    "Linear Regression R2:",
    sprintf(
      "%.6f",
      linear_r2
    )
  ),

  "",

  paste(
    "Random Forest MAE:",
    sprintf(
      "%.6f",
      rf_mae
    )
  ),

  paste(
    "Random Forest RMSE:",
    sprintf(
      "%.6f",
      rf_rmse
    )
  ),

  paste(
    "Random Forest R2:",
    sprintf(
      "%.6f",
      rf_r2
    )
  ),

  "",

  "FEATURE IMPORTANCE",
  "--------------------------------------------",

  paste(
    "Feature importance file:",
    "top_15_feature_importance.csv"
  ),

  "",

  "ERROR ANALYSIS",
  "--------------------------------------------",

  paste(
    "Prediction rows analyzed:",
    nrow(predictions)
  ),

  paste(
    "Mean Linear Regression residual:",
    sprintf(
      "%.6f",
      mean(
        predictions$linear_residual,
        na.rm = TRUE
      )
    )
  ),

  paste(
    "Mean Random Forest residual:",
    sprintf(
      "%.6f",
      mean(
        predictions$rf_residual,
        na.rm = TRUE
      )
    )
  ),

  "",

  "OUTPUT FILES",
  "--------------------------------------------",

  "enriched_error_metrics.csv",
  "residual_summary.csv",
  "top_15_feature_importance.csv",
  "largest_random_forest_errors.csv",
  "enriched_predictions_with_errors.csv",

  if (!is.null(locality_metrics))
    "locality_error_metrics.csv",

  "",

  "============================================"
)

# ------------------------------------------------------------
# 19. Save locality metrics
# ------------------------------------------------------------

if (!is.null(locality_metrics)) {
  
  write.csv(
    locality_metrics,
    file.path(
      OUTPUT_DIR,
      "locality_error_metrics.csv"
    ),
    row.names = FALSE
  )
}

# ------------------------------------------------------------
# 20. Save report
# ------------------------------------------------------------

writeLines(
  report,
  file.path(
    OUTPUT_DIR,
    "explainability_error_analysis_report.txt"
  )
)

# ------------------------------------------------------------
# 21. Final output
# ------------------------------------------------------------

cat("\n============================================\n")
cat(" PHASE 6 COMPLETED SUCCESSFULLY\n")
cat("============================================\n\n")

cat("Created files:\n")

cat(
  "- outputs/enriched_error_metrics.csv\n"
)

cat(
  "- outputs/residual_summary.csv\n"
)

cat(
  "- outputs/top_15_feature_importance.csv\n"
)

cat(
  "- outputs/largest_random_forest_errors.csv\n"
)

cat(
  "- outputs/enriched_predictions_with_errors.csv\n"
)

if (!is.null(locality_metrics)) {
  
  cat(
    "- outputs/locality_error_metrics.csv\n"
  )
}

cat(
  "- outputs/explainability_error_analysis_report.txt\n"
)

cat("\n============================================\n")
