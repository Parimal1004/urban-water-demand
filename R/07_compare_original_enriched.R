# ============================================================
# PHASE 5 - ORIGINAL vs ENRICHED MODEL COMPARISON
# Urban Water Demand Prediction
# ============================================================

cat("\n============================================\n")
cat(" PHASE 5 - ORIGINAL vs ENRICHED COMPARISON\n")
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
# 2. Original model results
# ------------------------------------------------------------

# These are the verified results from the original project.

original_results <- data.frame(
  
  Model = c(
    "Original Linear Regression",
    "Original Random Forest"
  ),
  
  MAE = c(
    0.503588,
    0.497095
  ),
  
  RMSE = c(
    0.881922,
    0.861863
  ),
  
  R2 = c(
    0.871987,
    0.877744
  ),
  
  Dataset = c(
    "Original SOWEKI features",
    "Original SOWEKI features"
  ),
  
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 3. Load enriched model results
# ------------------------------------------------------------

enriched_file <- file.path(
  OUTPUT_DIR,
  "enriched_model_comparison.csv"
)

if (!file.exists(enriched_file)) {
  
  stop(
    paste(
      "Enriched model comparison file not found:",
      enriched_file
    )
  )
}

enriched_results <- read.csv(
  enriched_file,
  stringsAsFactors = FALSE
)

# Rename model names for consistency

enriched_results$Model <- c(
  "Enriched Linear Regression",
  "Enriched Random Forest"
)

enriched_results$Dataset <- c(
  "Enriched contextual features",
  "Enriched contextual features"
)

# Keep required columns

enriched_results <- enriched_results[
  ,
  c(
    "Model",
    "MAE",
    "RMSE",
    "R2",
    "Dataset"
  )
]

# ------------------------------------------------------------
# 4. Combine results
# ------------------------------------------------------------

comparison <- rbind(
  original_results,
  enriched_results
)

rownames(comparison) <- NULL

cat("============================================\n")
cat(" COMPLETE MODEL COMPARISON\n")
cat("============================================\n\n")

print(comparison)

# ------------------------------------------------------------
# 5. Calculate improvement percentages
# ------------------------------------------------------------

original_linear <- original_results[
  original_results$Model ==
    "Original Linear Regression",
  ,
  drop = FALSE
]

enriched_linear <- enriched_results[
  enriched_results$Model ==
    "Enriched Linear Regression",
  ,
  drop = FALSE
]

original_rf <- original_results[
  original_results$Model ==
    "Original Random Forest",
  ,
  drop = FALSE
]

enriched_rf <- enriched_results[
  enriched_results$Model ==
    "Enriched Random Forest",
  ,
  drop = FALSE
]

# ------------------------------------------------------------
# Linear Regression improvement
# ------------------------------------------------------------

linear_mae_improvement <- (
  (
    original_linear$MAE -
      enriched_linear$MAE
  ) /
    original_linear$MAE
) * 100

linear_rmse_improvement <- (
  (
    original_linear$RMSE -
      enriched_linear$RMSE
  ) /
    original_linear$RMSE
) * 100

linear_r2_improvement <- (
  (
    enriched_linear$R2 -
      original_linear$R2
  ) /
    original_linear$R2
) * 100

# ------------------------------------------------------------
# Random Forest improvement
# ------------------------------------------------------------

rf_mae_improvement <- (
  (
    original_rf$MAE -
      enriched_rf$MAE
  ) /
    original_rf$MAE
) * 100

rf_rmse_improvement <- (
  (
    original_rf$RMSE -
      enriched_rf$RMSE
  ) /
    original_rf$RMSE
) * 100

rf_r2_improvement <- (
  (
    enriched_rf$R2 -
      original_rf$R2
  ) /
    original_rf$R2
) * 100

# ------------------------------------------------------------
# 6. Improvement table
# ------------------------------------------------------------

improvement_table <- data.frame(
  
  Model = c(
    "Linear Regression",
    "Random Forest"
  ),
  
  Original_MAE = c(
    original_linear$MAE,
    original_rf$MAE
  ),
  
  Enriched_MAE = c(
    enriched_linear$MAE,
    enriched_rf$MAE
  ),
  
  MAE_Improvement_Percent = c(
    linear_mae_improvement,
    rf_mae_improvement
  ),
  
  Original_RMSE = c(
    original_linear$RMSE,
    original_rf$RMSE
  ),
  
  Enriched_RMSE = c(
    enriched_linear$RMSE,
    enriched_rf$RMSE
  ),
  
  RMSE_Improvement_Percent = c(
    linear_rmse_improvement,
    rf_rmse_improvement
  ),
  
  Original_R2 = c(
    original_linear$R2,
    original_rf$R2
  ),
  
  Enriched_R2 = c(
    enriched_linear$R2,
    enriched_rf$R2
  ),
  
  R2_Improvement_Percent = c(
    linear_r2_improvement,
    rf_r2_improvement
  )
)

# ------------------------------------------------------------
# 7. Print improvement results
# ------------------------------------------------------------

cat("\n============================================\n")
cat(" IMPROVEMENT FROM ORIGINAL TO ENRICHED\n")
cat("============================================\n\n")

print(improvement_table)

# ------------------------------------------------------------
# 8. Save comparison table
# ------------------------------------------------------------

comparison_file <- file.path(
  OUTPUT_DIR,
  "original_vs_enriched_model_comparison.csv"
)

write.csv(
  comparison,
  comparison_file,
  row.names = FALSE
)

# ------------------------------------------------------------
# 9. Save improvement table
# ------------------------------------------------------------

improvement_file <- file.path(
  OUTPUT_DIR,
  "original_vs_enriched_improvement.csv"
)

write.csv(
  improvement_table,
  improvement_file,
  row.names = FALSE
)

# ------------------------------------------------------------
# 10. Generate text report
# ------------------------------------------------------------

report <- c(
  
  "============================================",
  "ORIGINAL vs ENRICHED MODEL COMPARISON",
  "Urban Water Demand Prediction",
  "============================================",
  "",
  
  "ORIGINAL MODEL RESULTS",
  "--------------------------------------------",
  
  paste(
    "Original Linear Regression MAE:",
    sprintf(
      "%.6f",
      original_linear$MAE
    )
  ),
  
  paste(
    "Original Linear Regression RMSE:",
    sprintf(
      "%.6f",
      original_linear$RMSE
    )
  ),
  
  paste(
    "Original Linear Regression R2:",
    sprintf(
      "%.6f",
      original_linear$R2
    )
  ),
  
  "",
  
  paste(
    "Original Random Forest MAE:",
    sprintf(
      "%.6f",
      original_rf$MAE
    )
  ),
  
  paste(
    "Original Random Forest RMSE:",
    sprintf(
      "%.6f",
      original_rf$RMSE
    )
  ),
  
  paste(
    "Original Random Forest R2:",
    sprintf(
      "%.6f",
      original_rf$R2
    )
  ),
  
  "",
  "ENRICHED MODEL RESULTS",
  "--------------------------------------------",
  
  paste(
    "Enriched Linear Regression MAE:",
    sprintf(
      "%.6f",
      enriched_linear$MAE
    )
  ),
  
  paste(
    "Enriched Linear Regression RMSE:",
    sprintf(
      "%.6f",
      enriched_linear$RMSE
    )
  ),
  
  paste(
    "Enriched Linear Regression R2:",
    sprintf(
      "%.6f",
      enriched_linear$R2
    )
  ),
  
  "",
  
  paste(
    "Enriched Random Forest MAE:",
    sprintf(
      "%.6f",
      enriched_rf$MAE
    )
  ),
  
  paste(
    "Enriched Random Forest RMSE:",
    sprintf(
      "%.6f",
      enriched_rf$RMSE
    )
  ),
  
  paste(
    "Enriched Random Forest R2:",
    sprintf(
      "%.6f",
      enriched_rf$R2
    )
  ),
  
  "",
  "IMPROVEMENT",
  "--------------------------------------------",
  
  paste(
    "Linear Regression MAE improvement (%):",
    sprintf(
      "%.2f",
      linear_mae_improvement
    )
  ),
  
  paste(
    "Linear Regression RMSE improvement (%):",
    sprintf(
      "%.2f",
      linear_rmse_improvement
    )
  ),
  
  paste(
    "Linear Regression R2 change (%):",
    sprintf(
      "%.2f",
      linear_r2_improvement
    )
  ),
  
  "",
  
  paste(
    "Random Forest MAE improvement (%):",
    sprintf(
      "%.2f",
      rf_mae_improvement
    )
  ),
  
  paste(
    "Random Forest RMSE improvement (%):",
    sprintf(
      "%.2f",
      rf_rmse_improvement
    )
  ),
  
  paste(
    "Random Forest R2 change (%):",
    sprintf(
      "%.2f",
      rf_r2_improvement
    )
  ),
  
  "",
  "IMPORTANT NOTES",
  "--------------------------------------------",
  
  "Original results come from the original SOWEKI-based model.",
  
  "Enriched results use the enriched contextual feature dataset.",
  
  "The enriched Random Forest uses locality-stratified",
  "training sampling and 50 trees.",
  
  "The full enriched test set was retained for evaluation.",
  
  "Synthetic contextual variables are experimental features",
  "and should be disclosed in the final report.",
  
  "Original SOWEKI data was not modified.",
  
  "",
  "============================================"
)

report_file <- file.path(
  OUTPUT_DIR,
  "original_vs_enriched_comparison_report.txt"
)

writeLines(
  report,
  report_file
)

# ------------------------------------------------------------
# 11. Final output
# ------------------------------------------------------------

cat("\n============================================\n")
cat(" PHASE 5 COMPLETED SUCCESSFULLY\n")
cat("============================================\n\n")

cat("Created files:\n")

cat(
  "- outputs/original_vs_enriched_model_comparison.csv\n"
)

cat(
  "- outputs/original_vs_enriched_improvement.csv\n"
)

cat(
  "- outputs/original_vs_enriched_comparison_report.txt\n"
)

cat("\n============================================\n")