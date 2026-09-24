# ============================================================
# PHASE 9 - AUTOMATED REPORT GENERATION
# Urban Water Demand Prediction
# ============================================================

options(stringsAsFactors = FALSE)

cat("\n============================================\n")
cat(" PHASE 9 - AUTOMATED REPORT GENERATION\n")
cat("============================================\n\n")

# ------------------------------------------------------------
# 1. Project setup
# ------------------------------------------------------------

PROJECT_DIR <- getwd()

DATA_DIR <- file.path(PROJECT_DIR, "data")
OUTPUT_DIR <- file.path(PROJECT_DIR, "outputs")

dir.create(
  OUTPUT_DIR,
  showWarnings = FALSE,
  recursive = TRUE
)

# ------------------------------------------------------------
# 2. Required files
# ------------------------------------------------------------

required_files <- c(
  "enriched_model_comparison.csv",
  "original_vs_enriched_model_comparison.csv",
  "original_vs_enriched_improvement.csv",
  "enriched_error_metrics.csv",
  "locality_comparison.csv",
  "locality_model_performance.csv",
  "top_15_feature_importance.csv",
  "largest_random_forest_errors.csv",
  "what_if_scenario_results.csv",
  "what_if_scenario_summary.csv"
)

cat("Checking required output files...\n\n")

missing_files <- required_files[
  !file.exists(
    file.path(
      OUTPUT_DIR,
      required_files
    )
  )
]

if (length(missing_files) > 0) {

  cat("Missing files:\n")

  for (file in missing_files) {
    cat("-", file, "\n")
  }

  stop(
    "\nRun the relevant previous phases before generating the report."
  )
}

cat("All required files found.\n\n")

# ------------------------------------------------------------
# 3. Load data
# ------------------------------------------------------------

cat("Loading analysis results...\n")

model_comparison <- read.csv(
  file.path(
    OUTPUT_DIR,
    "enriched_model_comparison.csv"
  )
)

original_enriched <- read.csv(
  file.path(
    OUTPUT_DIR,
    "original_vs_enriched_model_comparison.csv"
  )
)

improvement <- read.csv(
  file.path(
    OUTPUT_DIR,
    "original_vs_enriched_improvement.csv"
  )
)

error_metrics <- read.csv(
  file.path(
    OUTPUT_DIR,
    "enriched_error_metrics.csv"
  )
)

locality_comparison <- read.csv(
  file.path(
    OUTPUT_DIR,
    "locality_comparison.csv"
  )
)

locality_performance <- read.csv(
  file.path(
    OUTPUT_DIR,
    "locality_model_performance.csv"
  )
)

feature_importance <- read.csv(
  file.path(
    OUTPUT_DIR,
    "top_15_feature_importance.csv"
  )
)

largest_errors <- read.csv(
  file.path(
    OUTPUT_DIR,
    "largest_random_forest_errors.csv"
  )
)

what_if_results <- read.csv(
  file.path(
    OUTPUT_DIR,
    "what_if_scenario_results.csv"
  )
)

what_if_summary <- read.csv(
  file.path(
    OUTPUT_DIR,
    "what_if_scenario_summary.csv"
  )
)

cat("Analysis files loaded successfully.\n\n")

# ------------------------------------------------------------
# 4. Helper functions
# ------------------------------------------------------------

fmt <- function(
  x,
  digits = 4
) {

  if (length(x) == 0 || is.na(x)) {
    return("NA")
  }

  format(
    round(
      as.numeric(x),
      digits
    ),
    nsmall = digits,
    trim = TRUE
  )
}

pct <- function(
  x,
  digits = 2
) {

  if (length(x) == 0 || is.na(x)) {
    return("NA")
  }

  paste0(
    format(
      round(
        as.numeric(x),
        digits
      ),
      nsmall = digits,
      trim = TRUE
    ),
    "%"
  )
}

# ------------------------------------------------------------
# 5. Extract model metrics
# ------------------------------------------------------------

rf_row <- model_comparison[
  grepl(
    "Random",
    model_comparison$Model,
    ignore.case = TRUE
  ),
  ,
  drop = FALSE
]

lr_row <- model_comparison[
  grepl(
    "Linear",
    model_comparison$Model,
    ignore.case = TRUE
  ),
  ,
  drop = FALSE
]

rf_mae <- if (nrow(rf_row) > 0) rf_row$MAE[1] else NA
rf_rmse <- if (nrow(rf_row) > 0) rf_row$RMSE[1] else NA
rf_r2 <- if (nrow(rf_row) > 0) rf_row$R2[1] else NA

lr_mae <- if (nrow(lr_row) > 0) lr_row$MAE[1] else NA
lr_rmse <- if (nrow(lr_row) > 0) lr_row$RMSE[1] else NA
lr_r2 <- if (nrow(lr_row) > 0) lr_row$R2[1] else NA

# ------------------------------------------------------------
# 6. Dataset information
# ------------------------------------------------------------

enriched_file <- file.path(
  DATA_DIR,
  "enriched_features.csv"
)

if (file.exists(enriched_file)) {

  enriched_data <- read.csv(
    enriched_file,
    stringsAsFactors = FALSE,
    nrows = 5
  )

  enriched_rows <- NA

  # Get total rows without loading the entire file again.
  connection <- file(
    enriched_file,
    open = "r"
  )

  row_count <- 0

  while (length(
    readLines(
      connection,
      n = 10000,
      warn = FALSE
    )
  ) > 0) {

    row_count <- row_count + 10000
  }

  close(connection)

  enriched_rows <- NA
} else {

  enriched_rows <- NA
}

# Use locality comparison for locality count
number_localities <- nrow(
  locality_comparison
)

# ------------------------------------------------------------
# 7. Locality summary
# ------------------------------------------------------------

locality_lines <- c()

for (
  i in seq_len(
    nrow(locality_comparison)
  )
) {

  row <- locality_comparison[i, ]

  locality_lines <- c(
    locality_lines,

    paste0(
      "- ",
      row$locality,
      " (",
      row$zone_type,
      "): population ",
      format(
        as.numeric(row$population),
        big.mark = ","
      ),
      ", mean demand ",
      fmt(
        row$mean_demand,
        3
      ),
      ", Random Forest MAE ",
      fmt(
        row$Random_Forest_MAE,
        4
      ),
      "."
    )
  )
}

# ------------------------------------------------------------
# 8. Feature importance summary
# ------------------------------------------------------------

feature_lines <- c()

top_n <- min(
  10,
  nrow(feature_importance)
)

if (top_n > 0) {

  for (
    i in seq_len(top_n)
  ) {

    row <- feature_importance[i, ]

    feature_lines <- c(
      feature_lines,

      paste0(
        i,
        ". ",
        row$Feature,
        " — importance ",
        fmt(
          row$Importance,
          2
        )
      )
    )
  }
}

# ------------------------------------------------------------
# 9. What-if summary
# ------------------------------------------------------------

what_if_lines <- c()

if (nrow(what_if_summary) > 0) {

  for (
    i in seq_len(
      nrow(what_if_summary)
    )
  ) {

    row <- what_if_summary[i, ]

    what_if_lines <- c(
      what_if_lines,

      paste0(
        "- ",
        row$scenario,
        ": mean predicted change ",
        fmt(
          row$mean_absolute_change,
          4
        ),
        " (",
        pct(
          row$mean_percent_change,
          2
        ),
        ")."
      )
    )
  }
}

# ------------------------------------------------------------
# 10. Build report
# ------------------------------------------------------------

report <- c(

  "============================================================",
  "EXPLAINABLE URBAN WATER DEMAND FORECASTING",
  "AND DECISION-SUPPORT SYSTEM",
  "AUTOMATED ANALYSIS REPORT",
  "============================================================",
  "",

  "Generated automatically from the project outputs.",
  "",

  "1. PROJECT OVERVIEW",
  "------------------------------------------------------------",
  "The system extends the original water-demand forecasting",
  "workflow with locality, weather, infrastructure, land-use,",
  "calendar and historical demand features.",
  "",
  "The enriched model configuration contains locality-level",
  "context and engineered temporal, weather and infrastructure",
  "features.",
  "",
  "Synthetic contextual variables are used for experimental",
  "analysis and are not presented as measured real-world data.",
  "",

  "2. MODEL PERFORMANCE",
  "------------------------------------------------------------",
  paste0(
    "Enriched Linear Regression:",
    " MAE = ",
    fmt(lr_mae),
    ", RMSE = ",
    fmt(lr_rmse),
    ", R2 = ",
    fmt(lr_r2)
  ),

  paste0(
    "Enriched Random Forest:",
    " MAE = ",
    fmt(rf_mae),
    ", RMSE = ",
    fmt(rf_rmse),
    ", R2 = ",
    fmt(rf_r2)
  ),

  "",

  "3. ORIGINAL VS ENRICHED CONFIGURATION",
  "------------------------------------------------------------",

  "The comparison evaluates the original and enriched",
  "feature configurations using their respective test",
  "evaluation results.",

  "",

  "Detailed results are available in:",
  "original_vs_enriched_model_comparison.csv",
  "original_vs_enriched_improvement.csv",

  "",

  "4. LOCALITY ANALYSIS",
  "------------------------------------------------------------",

  paste0(
    "Number of analyzed localities: ",
    number_localities
  ),

  "",

  locality_lines,

  "",

  "5. FEATURE IMPORTANCE",
  "------------------------------------------------------------",

  "Top Random Forest features:",

  "",

  feature_lines,

  "",

  "6. ERROR ANALYSIS",
  "------------------------------------------------------------",

  "Error analysis was performed using the enriched test",
  "predictions. Locality-level metrics and largest prediction",
  "errors are available in the generated CSV files.",

  "",

  "Largest-error analysis file:",
  "largest_random_forest_errors.csv",

  "",

  "7. WHAT-IF ANALYSIS",
  "------------------------------------------------------------",

  "The What-If module evaluates model predictions under",
  "alternative contextual scenarios without retraining",
  "the Random Forest model.",

  "",

  what_if_lines,

  "",

  "These scenario outputs are model-based predictions and",
  "should not be interpreted as causal estimates.",

  "",

  "8. DATA AND LEAKAGE CONTROLS",
  "------------------------------------------------------------",

  "The target variable and explicitly identified target-derived",
  "variables were excluded from the model predictor set.",

  "The original SOWEKI dataset remains unchanged.",

  "Chronological train/test splitting was used for the enriched",
  "modeling workflow.",

  "",

  "9. OUTPUT FILES",
  "------------------------------------------------------------",

  "Model comparison:",
  "- enriched_model_comparison.csv",
  "- original_vs_enriched_model_comparison.csv",
  "- original_vs_enriched_improvement.csv",

  "",

  "Explainability and error analysis:",
  "- enriched_error_metrics.csv",
  "- locality_error_metrics.csv",
  "- top_15_feature_importance.csv",
  "- largest_random_forest_errors.csv",

  "",

  "Locality analysis:",
  "- locality_comparison.csv",
  "- locality_model_performance.csv",

  "",

  "What-If analysis:",
  "- what_if_baseline_predictions.csv",
  "- what_if_scenario_results.csv",
  "- what_if_scenario_summary.csv",

  "",

  "10. IMPORTANT LIMITATIONS",
  "------------------------------------------------------------",

  "1. The locality/context layer contains synthetic experimental",
  "   variables.",
  "",
  "2. Weather variables are synthetic experimental inputs rather",
  "   than live weather observations.",
  "",
  "3. What-If results describe model responses to changed inputs",
  "   and do not establish causal relationships.",
  "",
  "4. The enriched model should be validated with real locality",
  "   and weather data before operational deployment.",

  "",

  "============================================================",
  "END OF REPORT",
  "============================================================"
)

# ------------------------------------------------------------
# 11. Save text report
# ------------------------------------------------------------

report_file <- file.path(
  OUTPUT_DIR,
  "automated_water_demand_report.txt"
)

writeLines(
  report,
  report_file
)

cat(
  "Text report created:\n",
  report_file,
  "\n\n"
)

# ------------------------------------------------------------
# 12. Create CSV executive summary
# ------------------------------------------------------------

executive_summary <- data.frame(

  Metric = c(
    "Linear Regression MAE",
    "Linear Regression RMSE",
    "Linear Regression R2",
    "Random Forest MAE",
    "Random Forest RMSE",
    "Random Forest R2",
    "Number of Localities"
  ),

  Value = c(
    lr_mae,
    lr_rmse,
    lr_r2,
    rf_mae,
    rf_rmse,
    rf_r2,
    number_localities
  )
)

write.csv(
  executive_summary,
  file.path(
    OUTPUT_DIR,
    "automated_report_executive_summary.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# 13. Final message
# ------------------------------------------------------------

cat("============================================\n")
cat(" PHASE 9 COMPLETED SUCCESSFULLY\n")
cat("============================================\n\n")

cat("Created:\n")
cat("- outputs/automated_water_demand_report.txt\n")
cat("- outputs/automated_report_executive_summary.csv\n")

cat("\n")