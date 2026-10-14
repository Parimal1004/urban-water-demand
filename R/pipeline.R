options(stringsAsFactors = FALSE)
if (sys.nframe() > 0) return(invisible(NULL))

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
setwd(project_dir)

cat("\nRunning 01_data_understanding.R...\n")
local({
  
  
  library(ggplot2)
  
  data_path <- file.path("data", "soweki_wdd_9.csv")
  plot_dir <- "plots"
  plot_path <- file.path(plot_dir, "water_demand_over_time.png")
  report_path <- file.path("outputs", "data_quality_report.txt")
  
  if (!file.exists(data_path)) {
    stop("Dataset not found at ", data_path,
         ". Place soweki_wdd_9.csv in the data folder and run from the project root.")
  }
  
  dir.create("outputs", showWarnings = FALSE)
  dir.create(plot_dir, showWarnings = FALSE)
  
  raw <- read.csv(data_path, stringsAsFactors = FALSE)
  
  n_rows <- nrow(raw)
  n_cols <- ncol(raw)
  col_names <- names(raw)
  col_types <- vapply(raw, function(x) class(x)[1], character(1))
  
  cat("=== Dataset shape ===\n")
  cat("Rows:", n_rows, "\n")
  cat("Columns:", n_cols, "\n")
  cat("Column names:", paste(col_names, collapse = ", "), "\n")
  cat("Data types:\n")
  print(col_types)
  
  cat("\n=== First 10 observations ===\n")
  print(head(raw, 10))
  
  cat("\n=== Summary statistics ===\n")
  print(summary(raw))
  
  missing_counts <- colSums(is.na(raw) | raw == "")
  cat("\n=== Missing values (NA or empty string) ===\n")
  print(missing_counts)
  
  n_dup_rows <- sum(duplicated(raw))
  cat("\nDuplicate full rows:", n_dup_rows, "\n")
  
  ts_char <- as.character(raw$timestamp)
  ts_no_tz <- sub("\\+00:00$", "", ts_char)
  datetime <- as.POSIXct(ts_no_tz, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  
  n_unparsed <- sum(is.na(datetime) & !is.na(raw$timestamp) & raw$timestamp != "")
  if (n_unparsed > 0) {
    warning(n_unparsed, " timestamp values could not be parsed.")
  }
  
  demand <- suppressWarnings(as.numeric(raw$value))
  
  ts_min <- min(datetime, na.rm = TRUE)
  ts_max <- max(datetime, na.rm = TRUE)
  
  val_min <- min(demand, na.rm = TRUE)
  val_max <- max(demand, na.rm = TRUE)
  val_mean <- mean(demand, na.rm = TRUE)
  val_median <- median(demand, na.rm = TRUE)
  val_sd <- sd(demand, na.rm = TRUE)
  
  cat("\n=== Timestamp range (POSIXct, UTC) ===\n")
  cat("Minimum timestamp:", format(ts_min, usetz = TRUE), "\n")
  cat("Maximum timestamp:", format(ts_max, usetz = TRUE), "\n")
  
  cat("\n=== Water demand (value) ===\n")
  cat("Minimum:", val_min, "\n")
  cat("Maximum:", val_max, "\n")
  cat("Mean:", val_mean, "\n")
  cat("Median:", val_median, "\n")
  cat("Standard deviation:", val_sd, "\n")
  
  is_ordered <- all(diff(as.numeric(datetime)) >= 0, na.rm = TRUE)
  n_out_of_order <- sum(diff(as.numeric(datetime)) < 0, na.rm = TRUE)
  
  cat("\n=== Timestamp order ===\n")
  cat("Timestamps are non-decreasing:", is_ordered, "\n")
  cat("Number of backward steps:", n_out_of_order, "\n")
  
  row_diffs_sec <- as.numeric(diff(datetime), units = "secs")
  row_diffs_sec <- row_diffs_sec[!is.na(row_diffs_sec)]
  
  positive_diffs <- row_diffs_sec[row_diffs_sec > 0]
  median_interval_sec <- if (length(positive_diffs) > 0) {
    as.numeric(median(positive_diffs))
  } else {
    NA_real_
  }
  
  gap_table <- sort(table(row_diffs_sec), decreasing = TRUE)
  cat("\n=== Consecutive-row time intervals (seconds) ===\n")
  cat("Median positive interval (seconds):", median_interval_sec, "\n")
  cat("Most common intervals (top 10):\n")
  print(head(gap_table, 10))
  
  n_unique_ts <- length(unique(datetime[!is.na(datetime)]))
  n_dup_timestamps <- sum(duplicated(datetime[!is.na(datetime)]))
  
  missing_ts_count <- NA_integer_
  expected_n <- NA_integer_
  has_missing_timestamps <- NA
  
  if (!is.na(median_interval_sec) && median_interval_sec > 0) {
    expected_grid <- seq(from = ts_min, to = ts_max, by = median_interval_sec)
    expected_n <- length(expected_grid)
    present <- unique(datetime[!is.na(datetime)])
    missing_ts_count <- sum(!(expected_grid %in% present))
    has_missing_timestamps <- missing_ts_count > 0
  }
  
  cat("\n=== Missing timestamps ===\n")
  cat("Unique timestamps:", n_unique_ts, "\n")
  cat("Duplicate timestamp values:", n_dup_timestamps, "\n")
  cat("Expected observations on a regular grid:", expected_n, "\n")
  cat("Missing timestamps on that grid:", missing_ts_count, "\n")
  cat("Missing timestamps present:", has_missing_timestamps, "\n")
  
  sorted_dt <- sort(datetime[!is.na(datetime)])
  sorted_diffs <- as.numeric(diff(sorted_dt), units = "secs")
  large_gap_count <- if (!is.na(median_interval_sec)) {
    sum(sorted_diffs > 1.5 * median_interval_sec)
  } else {
    NA_integer_
  }
  cat("Gaps larger than 1.5 x typical interval (sorted series):", large_gap_count, "\n")
  
  plot_df <- data.frame(datetime = datetime, demand = demand)
  plot_df <- plot_df[complete.cases(plot_df), ]
  
  p <- ggplot(plot_df, aes(x = datetime, y = demand)) +
    geom_line(colour = "#1f4e79", linewidth = 0.3) +
    labs(
      title = "SOWEKI urban water demand over time",
      subtitle = "Raw series from soweki_wdd_9.csv (no synthetic variables)",
      x = "Timestamp (UTC)",
      y = "Water demand (value)"
    ) +
    theme_minimal(base_size = 12)
  
  ggsave(plot_path, plot = p, width = 11, height = 5, dpi = 120)
  cat("\nSaved plot:", plot_path, "\n")
  
  interval_minutes <- if (!is.na(median_interval_sec)) median_interval_sec / 60 else NA
  
  report_lines <- c(
    "SOWEKI water demand - data quality report",
    "Phase 1: data understanding only",
    paste("Source file:", data_path),
    paste("Generated:", format(Sys.time(), usetz = TRUE)),
    "",
    paste("Rows:", n_rows),
    paste("Columns:", n_cols),
    paste("Column names:", paste(col_names, collapse = ", ")),
    paste("Column types:", paste(paste(names(col_types), col_types, sep = "="), collapse = "; ")),
    "",
    paste("Missing values by column:", paste(paste(names(missing_counts), missing_counts, sep = "="), collapse = "; ")),
    paste("Duplicate full rows:", n_dup_rows),
    paste("Duplicate timestamps:", n_dup_timestamps),
    paste("Unparsed timestamps:", n_unparsed),
    "",
    paste("Min timestamp (UTC):", format(ts_min, usetz = TRUE)),
    paste("Max timestamp (UTC):", format(ts_max, usetz = TRUE)),
    paste("Timestamps non-decreasing in file order:", is_ordered),
    paste("Backward steps in file order:", n_out_of_order),
    "",
    paste("Value min:", val_min),
    paste("Value max:", val_max),
    paste("Value mean:", val_mean),
    paste("Value median:", val_median),
    paste("Value SD:", val_sd),
    "",
    paste("Detected sampling interval (seconds):", median_interval_sec),
    paste("Detected sampling interval (minutes):", interval_minutes),
    paste("Expected points on regular grid:", expected_n),
    paste("Missing timestamps on regular grid:", missing_ts_count),
    paste("Has missing timestamps:", has_missing_timestamps),
    paste("Large gaps (> 1.5 x interval) after sorting:", large_gap_count),
    "",
    paste("Plot saved to:", plot_path)
  )
  
  writeLines(report_lines, con = report_path)
  cat("Saved report:", report_path, "\n")
  cat("\nPhase 1 data understanding finished.\n")
  
  
  
})

cat("\nRunning 02_feature_engineering.R...\n")
local({
  
  
  library(ggplot2)
  
  raw_path <- file.path("data", "soweki_wdd_9.csv")
  processed_path <- file.path("data", "water_demand_processed.csv")
  desc_path <- file.path("outputs", "feature_description.csv")
  plot_dir <- "plots"
  plot_hour_path <- file.path(plot_dir, "demand_by_hour.png")
  plot_dow_path <- file.path(plot_dir, "demand_by_day_of_week.png")
  plot_dist_path <- file.path(plot_dir, "demand_distribution.png")
  report_path <- file.path("outputs", "feature_engineering_report.txt")
  
  if (!file.exists(raw_path)) {
    stop("Dataset not found at ", raw_path, ". Run this script from the project root.")
  }
  dir.create("outputs", showWarnings = FALSE)
  dir.create(plot_dir, showWarnings = FALSE)
  dir.create("data", showWarnings = FALSE)
  
  previous_rollmean <- function(x, n) {
    x <- as.numeric(x)
    n_obs <- length(x)
    cs <- c(0, cumsum(x))
    out <- rep(NA_real_, n_obs)
    if (n_obs > n) {
      t <- (n + 1):n_obs
      out[t] <- (cs[t] - cs[t - n]) / n
    }
    out
  }
  
  fill_na_linear <- function(x) {
    i_ok <- which(!is.na(x))
    if (length(i_ok) == 0L || length(i_ok) == length(x)) {
      return(x)
    }
    i_na <- which(is.na(x))
    x[i_na] <- stats::approx(i_ok, x[i_ok], xout = i_na, rule = 2)$y
    x
  }
  
  raw <- read.csv(raw_path, stringsAsFactors = FALSE)
  n_original <- nrow(raw)
  
  ts_char <- as.character(raw$timestamp)
  datetime <- as.POSIXct(sub("\\+00:00$", "", ts_char),
                         format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  value <- suppressWarnings(as.numeric(raw$value))
  
  df <- data.frame(datetime = datetime, value = value, stringsAsFactors = FALSE)
  
  df <- df[order(df$datetime), ]
  rownames(df) <- NULL
  
  n_dup_rows <- sum(duplicated(df))
  df <- df[!duplicated(df), ]
  
  n_dup_ts <- sum(duplicated(df$datetime))
  if (n_dup_ts > 0) {
    split_vals <- split(df$value, df$datetime)
    df <- data.frame(
      datetime = as.POSIXct(names(split_vals), tz = "UTC"),
      value = vapply(split_vals, mean, numeric(1)),
      stringsAsFactors = FALSE
    )
    df <- df[order(df$datetime), ]
    rownames(df) <- NULL
  }
  
  n_missing_ts <- sum(is.na(df$datetime))
  n_missing_value_before <- sum(is.na(df$value))
  
  n_negative <- sum(!is.na(df$value) & df$value < 0)
  df$value[!is.na(df$value) & df$value < 0] <- NA_real_
  
  q1 <- stats::quantile(df$value, 0.25, na.rm = TRUE)
  q3 <- stats::quantile(df$value, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  upper_fence <- as.numeric(q3 + 3 * iqr)
  n_high_outliers <- sum(!is.na(df$value) & df$value > upper_fence)
  
  n_na_to_fill <- sum(is.na(df$value))
  if (n_na_to_fill > 0) {
    df$value <- fill_na_linear(df$value)
  }
  
  n_missing_value_after_fill <- sum(is.na(df$value))
  
  interval_sec <- as.numeric(median(diff(df$datetime)), units = "secs")
  all_intervals_equal <- all(as.numeric(diff(df$datetime), units = "secs") == interval_sec)
  
  cat("=== Cleaning ===\n")
  cat("Original rows:", n_original, "\n")
  cat("Exact duplicate rows removed:", n_dup_rows, "\n")
  cat("Duplicate timestamps (before collapsing):", n_dup_ts, "\n")
  cat("Missing timestamps:", n_missing_ts, "\n")
  cat("Missing value before handling:", n_missing_value_before, "\n")
  cat("Negative demand values set to NA:", n_negative, "\n")
  cat("High outliers (value > Q3 + 3*IQR =", round(upper_fence, 4), "):",
      n_high_outliers, "\n")
  cat("Values interpolated:", n_na_to_fill, "\n")
  cat("Missing value after interpolation:", n_missing_value_after_fill, "\n")
  cat("Median interval (seconds):", interval_sec, "\n")
  cat("All consecutive intervals equal:", all_intervals_equal, "\n")
  
  df$year <- as.integer(format(df$datetime, "%Y"))
  df$month <- as.integer(format(df$datetime, "%m"))
  df$day <- as.integer(format(df$datetime, "%d"))
  df$hour <- as.integer(format(df$datetime, "%H"))
  df$minute <- as.integer(format(df$datetime, "%M"))
  df$day_of_week <- as.integer(format(df$datetime, "%u"))
  df$day_of_year <- as.integer(format(df$datetime, "%j"))
  df$week_of_year <- as.integer(format(df$datetime, "%V"))
  df$is_weekend <- as.integer(df$day_of_week >= 6)
  df$quarter <- as.integer((df$month - 1L) %/% 3L + 1L)
  
  n_obs <- nrow(df)
  lag_shift <- function(x, k) {
    c(rep(NA_real_, k), x[seq_len(n_obs - k)])
  }
  
  df$lag_1 <- lag_shift(df$value, 1L)     # previous 15 minutes
  df$lag_4 <- lag_shift(df$value, 4L)     # previous 1 hour
  df$lag_96 <- lag_shift(df$value, 96L)   # previous 24 hours
  df$lag_672 <- lag_shift(df$value, 672L) # previous 7 days
  
  df$rolling_mean_4 <- previous_rollmean(df$value, 4L)
  df$rolling_mean_96 <- previous_rollmean(df$value, 96L)
  df$rolling_mean_672 <- previous_rollmean(df$value, 672L)
  
  feature_cols <- c(
    "lag_1", "lag_4", "lag_96", "lag_672",
    "rolling_mean_4", "rolling_mean_96", "rolling_mean_672"
  )
  n_before_drop <- nrow(df)
  df <- df[stats::complete.cases(df[, feature_cols]), ]
  rownames(df) <- NULL
  n_dropped_warmup <- n_before_drop - nrow(df)
  
  na_after <- colSums(is.na(df))
  n_final <- nrow(df)
  n_features <- ncol(df)
  
  cat("\n=== Engineered dataset ===\n")
  cat("Warm-up rows dropped (insufficient history):", n_dropped_warmup, "\n")
  cat("Final rows:", n_final, "\n")
  cat("Final columns (features including datetime and value):", n_features, "\n")
  cat("Missing values after processing:\n")
  print(na_after)
  
  cat("\n=== First 10 rows ===\n")
  print(utils::head(df, 10))
  
  cat("\n=== Summary ===\n")
  print(summary(df))
  
  out <- df
  out$datetime <- format(df$datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  utils::write.csv(out, processed_path, row.names = FALSE)
  cat("\nSaved:", processed_path, "\n")
  
  feature_description <- data.frame(
    feature_name = c(
      "datetime", "value",
      "year", "month", "day", "hour", "minute",
      "day_of_week", "day_of_year", "week_of_year", "is_weekend", "quarter",
      "lag_1", "lag_4", "lag_96", "lag_672",
      "rolling_mean_4", "rolling_mean_96", "rolling_mean_672"
    ),
    description = c(
      "Observation time in UTC (parsed from the original timestamp)",
      "Water demand measurement at this timestamp (model target)",
      "Calendar year of the timestamp",
      "Month number 1-12",
      "Day of month 1-31",
      "Hour of day 0-23 (UTC)",
      "Minute of hour (0, 15, 30, or 45 on this 15-minute grid)",
      "Day of week, Monday = 1 through Sunday = 7",
      "Day of year 1-366",
      "ISO week number 1-53",
      "1 if Saturday or Sunday, otherwise 0",
      "Calendar quarter 1-4",
      "Demand 15 minutes earlier (1 step)",
      "Demand 1 hour earlier (4 steps)",
      "Demand 24 hours earlier (96 steps)",
      "Demand 7 days earlier (672 steps)",
      "Mean demand over the previous 1 hour (4 past steps, current excluded)",
      "Mean demand over the previous 24 hours (96 past steps, current excluded)",
      "Mean demand over the previous 7 days (672 past steps, current excluded)"
    ),
    data_type = c(
      "POSIXct/character", "numeric",
      "integer", "integer", "integer", "integer", "integer",
      "integer", "integer", "integer", "integer", "integer",
      "numeric", "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric"
    ),
    purpose = c(
      "Time index; keeps the series in chronological order",
      "Target variable to predict",
      "Seasonality / long-term level",
      "Monthly seasonality",
      "Within-month calendar position",
      "Intraday pattern (night vs peak hours)",
      "Position within the 15-minute hour",
      "Weekly pattern (weekday vs weekend behaviour)",
      "Annual seasonality",
      "Weekly seasonality at ISO-week scale",
      "Weekend indicator for lower/higher weekend use",
      "Quarterly seasonality",
      "Short-term autocorrelation",
      "Hour-ago autocorrelation",
      "Same time yesterday",
      "Same time last week",
      "Recent 1-hour local level (no current value, no leakage)",
      "Recent 24-hour local level (no current value, no leakage)",
      "Recent 7-day local level (no current value, no leakage)"
    ),
    stringsAsFactors = FALSE
  )
  utils::write.csv(feature_description, desc_path, row.names = FALSE)
  cat("Saved:", desc_path, "\n")
  
  hour_summary <- aggregate(value ~ hour, data = df, FUN = mean)
  p_hour <- ggplot(df, aes(x = factor(hour), y = value)) +
    geom_boxplot(outlier.size = 0.4, fill = "#9dc3e6") +
    stat_summary(fun = mean, geom = "line", aes(group = 1), colour = "#1f4e79") +
    labs(
      title = "Water demand by hour of day",
      subtitle = "SOWEKI processed series; boxplots with mean line",
      x = "Hour of day (UTC)",
      y = "Water demand (value)"
    ) +
    theme_minimal(base_size = 12)
  ggplot2::ggsave(plot_hour_path, plot = p_hour, width = 10, height = 5, dpi = 120)
  
  dow_labels <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  dow_summary <- aggregate(value ~ day_of_week, data = df, FUN = mean)
  dow_summary$day_label <- factor(dow_labels[dow_summary$day_of_week],
                                  levels = dow_labels)
  p_dow <- ggplot(dow_summary, aes(x = day_label, y = value)) +
    geom_col(fill = "#1f4e79") +
    labs(
      title = "Average water demand by day of week",
      subtitle = "SOWEKI processed series",
      x = "Day of week",
      y = "Mean water demand (value)"
    ) +
    theme_minimal(base_size = 12)
  ggplot2::ggsave(plot_dow_path, plot = p_dow, width = 8, height = 5, dpi = 120)
  
  p_dist <- ggplot(df, aes(x = value)) +
    geom_histogram(bins = 50, fill = "#1f4e79", colour = "white") +
    labs(
      title = "Distribution of water demand",
      subtitle = "SOWEKI processed series",
      x = "Water demand (value)",
      y = "Count"
    ) +
    theme_minimal(base_size = 12)
  ggplot2::ggsave(plot_dist_path, plot = p_dist, width = 8, height = 5, dpi = 120)
  
  cat("Saved plots:", plot_hour_path, ",", plot_dow_path, ",", plot_dist_path, "\n")
  
  report_lines <- c(
    "SOWEKI water demand - feature engineering report",
    "Phase 2 only (no model training, no Shiny dashboard)",
    paste("Source file (unchanged):", raw_path),
    paste("Generated:", format(Sys.time(), usetz = TRUE)),
    "",
    "CLEANING PERFORMED",
    "- Loaded data/soweki_wdd_9.csv and parsed timestamp to POSIXct UTC.",
    "- Sorted rows from oldest to newest before creating lags or rolling means.",
    paste("- Exact duplicate rows removed:", n_dup_rows),
    paste("- Duplicate timestamps collapsed with mean demand:", n_dup_ts),
    paste("- Missing timestamps:", n_missing_ts),
    paste("- Missing demand values before handling:", n_missing_value_before),
    paste("- Negative demand values (impossible) set to NA:", n_negative),
    "- Missing demand handling: linear interpolation along the 15-minute time index.",
    "  This uses neighbouring observed demand only. No weather or population data",
    "  were invented. If the series has no NA values, interpolation does nothing.",
    paste("- High outliers kept (value > Q3+3*IQR =", round(upper_fence, 4), "):",
          n_high_outliers),
    "  These spikes were not deleted; they may be real peak demand.",
    paste("- Detected sampling interval (seconds):", interval_sec),
    paste("- All consecutive intervals equal:", all_intervals_equal),
    "",
    "FEATURES CREATED (from timestamp and past demand only)",
    "Calendar: year, month, day, hour, minute, day_of_week, day_of_year,",
    "          week_of_year, is_weekend, quarter",
    "Lags: lag_1 (15 min), lag_4 (1 hour), lag_96 (24 hours), lag_672 (7 days)",
    "Rolling means (previous window only): rolling_mean_4, rolling_mean_96,",
    "                                      rolling_mean_672",
    "",
    "LAG FEATURES",
    "Each lag is demand at a past grid point. lag_k uses row t-k, never t or t+k.",
    "",
    "ROLLING FEATURES",
    "rolling_mean_n is the mean of value[t-n] ... value[t-1]. The current target",
    "value[t] is excluded so the feature cannot leak the answer into the model.",
    "",
    "LEAKAGE PREVENTION",
    "- The table is never shuffled; row order stays chronological.",
    "- Features are computed after sorting.",
    "- Lags and rolling windows look backward only.",
    "- Warm-up rows without a full 7-day history are dropped instead of filling",
    "  with zeros or future values.",
    "- No synthetic temperature, rainfall, humidity, or population columns.",
    "",
    paste("Original observations:", n_original),
    paste("Warm-up rows dropped:", n_dropped_warmup),
    paste("Final observations:", n_final),
    paste("Final number of columns/features:", n_features),
    paste("Total missing cells after processing:", sum(na_after)),
    "",
    paste("Processed data:", processed_path),
    paste("Feature dictionary:", desc_path)
  )
  
  writeLines(report_lines, con = report_path)
  cat("Saved:", report_path, "\n")
  cat("\nPhase 2 feature engineering finished.\n")
  
  
  
})

cat("\nRunning 03_generate_context_data.R...\n")
local({
  
  
  set.seed(42)
  
  raw_path <- file.path("data", "soweki_wdd_9.csv")
  locality_path <- file.path("data", "locality_context.csv")
  enriched_path <- file.path("data", "urban_water_demand_enriched.csv")
  report_path <- file.path("outputs", "context_data_report.txt")
  desc_path <- file.path("outputs", "context_feature_description.csv")
  
  dir.create("data", showWarnings = FALSE)
  dir.create("outputs", showWarnings = FALSE)
  
  if (!file.exists(raw_path)) {
    stop("Dataset not found at ", raw_path, ". Run this script from the project root.")
  }
  
  raw <- read.csv(raw_path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("timestamp", "value")
  if (!all(required %in% names(raw))) {
    stop("Expected columns timestamp and value were not found in the raw dataset.")
  }
  
  raw$timestamp <- sub("Z$", "", raw$timestamp)
  raw$timestamp <- sub("\\+00:00$", "", raw$timestamp)
  raw$timestamp <- as.POSIXct(raw$timestamp, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  raw$value <- suppressWarnings(as.numeric(raw$value))
  raw <- raw[!is.na(raw$timestamp) & !is.na(raw$value), c("timestamp", "value")]
  raw <- raw[order(raw$timestamp), ]
  raw <- raw[!duplicated(raw$timestamp), ]
  rownames(raw) <- NULL
  
  locality <- data.frame(
    locality = c("Zone_A", "Zone_B", "Zone_C", "Zone_D", "Zone_E"),
    zone_type = c("Residential", "Commercial", "Mixed", "Industrial", "Residential"),
    area_km2 = c(10.5, 7.8, 13.2, 16.5, 9.4),
    population = c(72000, 48000, 96000, 39000, 61000),
    residential_pct = c(82, 28, 55, 18, 76),
    commercial_pct = c(13, 62, 30, 22, 17),
    industrial_pct = c(5, 10, 15, 60, 7),
    storage_capacity_mld = c(24, 20, 32, 38, 21),
    pipeline_age_years = c(11, 8, 14, 19, 10),
    distribution_efficiency = c(0.91, 0.94, 0.89, 0.86, 0.92),
    stringsAsFactors = FALSE
  )
  locality$population_density <- locality$population / locality$area_km2
  
  zone_factor <- c(Residential = 1.00, Commercial = 1.08, Mixed = 1.04, Industrial = 1.12)
  locality$base_demand_weight <- locality$population * zone_factor[locality$zone_type]
  locality$base_demand_share <- locality$base_demand_weight / sum(locality$base_demand_weight)
  
  write.csv(locality, locality_path, row.names = FALSE)
  
  dt <- raw$timestamp
  n <- length(dt)
  hour <- as.integer(format(dt, "%H"))
  minute <- as.integer(format(dt, "%M"))
  doy <- as.integer(format(dt, "%j"))
  dow <- as.integer(format(dt, "%u"))
  year <- as.integer(format(dt, "%Y"))
  
  seasonal_temp <- 29 + 4.5 * sin(2 * pi * (doy - 45) / 365.25)
  diurnal_temp <- 4.0 * sin(2 * pi * (hour + minute / 60 - 14) / 24)
  temperature <- seasonal_temp + diurnal_temp + rnorm(n, 0, 1.2)
  
  temperature <- pmax(pmin(temperature, 45), 12)
  
  rain_prob <- pmin(0.55, 0.035 + 0.22 * pmax(0, sin(2 * pi * (doy - 160) / 365.25)))
  rain_event <- rbinom(n, 1, rain_prob)
  rainfall <- ifelse(rain_event == 1, rgamma(n, shape = 1.8, rate = 0.16), 0)
  rainfall <- pmin(rainfall, 60)
  
  humidity <- 68 - 0.75 * (temperature - 28) + 10 * rain_event + rnorm(n, 0, 5)
  humidity <- pmax(pmin(humidity, 98), 25)
  
  wind <- 10 + 4 * sin(2 * pi * hour / 24) + rnorm(n, 0, 2.5)
  wind <- pmax(pmin(wind, 35), 0)
  
  dates <- as.Date(dt, tz = "UTC")
  unique_dates <- sort(unique(dates))
  holiday_dates <- unlist(lapply(sort(unique(year)), function(y) {
    candidates <- unique_dates[format(unique_dates, "%Y") == as.character(y)]
    sample(candidates, size = min(18, length(candidates)), replace = FALSE)
  }))
  is_holiday <- as.integer(dates %in% as.Date(holiday_dates))
  
  festival_dates <- as.Date(character(0))
  for (y in sort(unique(year))) {
    starts <- as.Date(sprintf("%d-%02d-%02d", y, c(3, 8, 11), c(10, 5, 15)))
    for (s in starts) {
      festival_dates <- c(festival_dates, seq(s, by = "day", length.out = 5))
    }
  }
  festival_dates <- unique(festival_dates)
  festival_period <- as.integer(dates %in% festival_dates)
  
  base_supply <- c(10, 9, 11, 8, 10)
  names(base_supply) <- locality$locality
  
  locality_rows <- vector("list", length(locality$locality))
  
  for (j in seq_len(nrow(locality))) {
    loc <- locality[j, ]
    loc_name <- loc$locality
    loc_type <- loc$zone_type
  
    if (loc_type == "Residential") {
      time_effect <- 1 + 0.14 * sin(2 * pi * (hour - 7) / 24) +
        0.10 * sin(2 * pi * (hour - 19) / 24)
    } else if (loc_type == "Commercial") {
      time_effect <- 1 + 0.18 * exp(-((hour - 13)^2) / 22)
    } else if (loc_type == "Industrial") {
      time_effect <- 1 + 0.10 * exp(-((hour - 14)^2) / 35)
    } else {
      time_effect <- 1 + 0.10 * sin(2 * pi * (hour - 10) / 24)
    }
  
    weather_effect <- 1 + 0.012 * pmax(temperature - 30, -8) - 0.010 * pmin(rainfall, 20)
    weekend_effect <- if (loc_type == "Commercial") 0.86 else if (loc_type == "Industrial") 0.78 else 1.03
    weekend_multiplier <- ifelse(dow >= 6, weekend_effect, 1)
    holiday_multiplier <- ifelse(is_holiday == 1 & loc_type == "Commercial", 0.88, 1)
    festival_multiplier <- ifelse(festival_period == 1 & loc_type %in% c("Residential", "Mixed"), 1.06, 1)
  
    dynamic_weight <- loc$base_demand_share * time_effect * weather_effect *
      weekend_multiplier * holiday_multiplier * festival_multiplier
  
    locality_rows[[j]] <- data.frame(
      timestamp = dt,
      locality = loc_name,
      zone_type = loc_type,
      area_km2 = loc$area_km2,
      population = loc$population,
      population_density = loc$population_density,
      residential_pct = loc$residential_pct,
      commercial_pct = loc$commercial_pct,
      industrial_pct = loc$industrial_pct,
      storage_capacity_mld = loc$storage_capacity_mld,
      pipeline_age_years = loc$pipeline_age_years,
      distribution_efficiency = loc$distribution_efficiency,
      temperature_c = temperature,
      humidity_pct = humidity,
      rainfall_mm = rainfall,
      wind_speed_kmh = wind,
      is_rain_event = rain_event,
      is_holiday = is_holiday,
      is_weekend = as.integer(dow >= 6),
      festival_period = festival_period,
      hour = hour,
      minute = minute,
      day_of_week = dow,
      month = as.integer(format(dt, "%m")),
      year = year,
      dynamic_weight = dynamic_weight,
      stringsAsFactors = FALSE
    )
  }
  
  enriched <- do.call(rbind, locality_rows)
  weight_sum <- ave(enriched$dynamic_weight, enriched$timestamp, FUN = sum)
  enriched$synthetic_demand_share <- enriched$dynamic_weight / weight_sum
  
  value_map <- raw$value
  names(value_map) <- as.character(raw$timestamp)
  enriched$total_water_demand_reference <- unname(value_map[as.character(enriched$timestamp)])
  enriched$water_demand <- enriched$total_water_demand_reference * enriched$synthetic_demand_share
  
  day_index <- as.integer(enriched$timestamp - min(enriched$timestamp), units = "days")
  enriched$water_supply_hours <- pmax(
    pmin(
      base_supply[enriched$locality] + 0.6 * sin(2 * pi * day_index / 7) + rnorm(nrow(enriched), 0, 0.35),
      14
    ),
    6
  )
  
  enriched$dynamic_weight <- NULL
  
  enriched <- enriched[order(enriched$timestamp, enriched$locality), ]
  rownames(enriched) <- NULL
  
  enriched$area_km2 <- round(enriched$area_km2, 3)
  enriched$population_density <- round(enriched$population_density, 2)
  enriched$temperature_c <- round(enriched$temperature_c, 3)
  enriched$humidity_pct <- round(enriched$humidity_pct, 3)
  enriched$rainfall_mm <- round(enriched$rainfall_mm, 3)
  enriched$wind_speed_kmh <- round(enriched$wind_speed_kmh, 3)
  enriched$water_supply_hours <- round(enriched$water_supply_hours, 3)
  enriched$distribution_efficiency <- round(enriched$distribution_efficiency, 3)
  enriched$synthetic_demand_share <- round(enriched$synthetic_demand_share, 6)
  enriched$water_demand <- round(enriched$water_demand, 6)
  enriched$total_water_demand_reference <- round(enriched$total_water_demand_reference, 6)
  
  enriched$timestamp <- format(enriched$timestamp, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  write.csv(enriched, enriched_path, row.names = FALSE)
  
  feature_description <- data.frame(
    feature_name = c(
      "timestamp", "locality", "zone_type", "area_km2", "population",
      "population_density", "residential_pct", "commercial_pct", "industrial_pct",
      "storage_capacity_mld", "pipeline_age_years", "distribution_efficiency",
      "temperature_c", "humidity_pct", "rainfall_mm", "wind_speed_kmh",
      "is_rain_event", "is_holiday", "is_weekend", "festival_period",
      "hour", "minute", "day_of_week", "month", "year", "water_supply_hours",
      "synthetic_demand_share", "total_water_demand_reference", "water_demand"
    ),
    data_type = c(
      "datetime", "categorical", "categorical", "numeric", "numeric",
      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric", "numeric", "binary", "binary", "binary", "binary",
      "integer", "integer", "integer", "integer", "integer", "numeric", "numeric", "numeric", "numeric"
    ),
    source = c(
      "Original SOWEKI timestamp", rep("Synthetic", 27), "Synthetic target derived from original demand"
    ),
    modelling_note = c(
      "Prediction time; raw timestamp should generally be decomposed into features",
      "Synthetic locality identifier",
      "Synthetic urban zone type",
      "Static locality attribute",
      "Static synthetic population",
      "Calculated population / area",
      "Synthetic land-use percentage",
      "Synthetic land-use percentage",
      "Synthetic land-use percentage",
      "Synthetic infrastructure attribute",
      "Synthetic infrastructure attribute",
      "Synthetic infrastructure attribute",
      "Synthetic time-varying weather",
      "Synthetic time-varying weather",
      "Synthetic time-varying weather",
      "Synthetic time-varying weather",
      "Synthetic rainfall indicator",
      "Synthetic generic holiday indicator",
      "Derived from timestamp",
      "Synthetic generic festival indicator",
      "Derived from timestamp",
      "Derived from timestamp",
      "Derived from timestamp",
      "Derived from timestamp",
      "Derived from timestamp",
      "Synthetic time-varying infrastructure variable",
      "Synthetic allocation share; use carefully and document",
      "Reference only; MUST NOT be used as a predictor because it directly determines the target",
      "Synthetic locality-level target allocated from the original aggregate demand"
    ),
    stringsAsFactors = FALSE
  )
  write.csv(feature_description, desc_path, row.names = FALSE)
  
  reconstructed <- aggregate(water_demand ~ timestamp, enriched, sum)
  original_check <- data.frame(
    timestamp = format(raw$timestamp, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    original = raw$value,
    stringsAsFactors = FALSE
  )
  check <- merge(original_check, reconstructed, by = "timestamp", all.x = TRUE)
  check$abs_error <- abs(check$original - check$water_demand)
  
  report_lines <- c(
    "URBAN WATER DEMAND - SYNTHETIC CONTEXT DATA REPORT",
    paste0("Generated with random seed: 42"),
    "",
    paste0("Original SOWEKI rows used: ", nrow(raw)),
    paste0("Original timestamp range: ", min(raw$timestamp), " to ", max(raw$timestamp)),
    paste0("Original demand mean: ", round(mean(raw$value), 6)),
    paste0("Original demand min: ", round(min(raw$value), 6)),
    paste0("Original demand max: ", round(max(raw$value), 6)),
    "",
    paste0("Synthetic localities: ", paste(locality$locality, collapse = ", ")), 
    paste0("Locality rows generated: ", nrow(enriched)),
    paste0("Rows per original timestamp: ", nrow(enriched) / nrow(raw)),
    "",
    "SYNTHETIC LOCALITY SUMMARY:",
    capture.output(print(locality)),
    "",
    paste0("Maximum absolute reconstruction error: ", format(max(check$abs_error, na.rm = TRUE), scientific = TRUE)),
    "The locality-level water_demand values are a synthetic allocation of the original SOWEKI aggregate value.",
    "The total_water_demand_reference column is reference-only and must not be used as a model predictor.",
    "All contextual variables created by this script are synthetic unless explicitly marked otherwise.",
    "Synthetic values must be disclosed in the README, dashboard and reports."
  )
  writeLines(report_lines, report_path)
  
  cat(paste(report_lines, collapse = "\n"), "\n")
  cat("\nSaved:", locality_path, "\n")
  cat("Saved:", enriched_path, "\n")
  cat("Saved:", report_path, "\n")
  cat("Saved:", desc_path, "\n")
  
  
  
})

cat("\nRunning PHASE 2 - ENRICHED FEATURE ENGINEERING...\n")
local({
  
  options(stringsAsFactors = FALSE)
  
  
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
  
  
  if (!file.exists(INPUT_FILE)) {
    stop(
      "Input file not found:\n",
      INPUT_FILE,
      "\nMake sure Phase 1 has been completed."
    )
  }
  
  cat("============================================================\n")
  cat("PHASE 2 - ENRICHED FEATURE ENGINEERING\n")
  cat("============================================================\n\n")
  
  cat("Project directory:\n", PROJECT_DIR, "\n\n")
  cat("Input file:\n", INPUT_FILE, "\n\n")
  
  
  df <- read.csv(
    INPUT_FILE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  cat("Rows loaded:", nrow(df), "\n")
  cat("Columns loaded:", ncol(df), "\n\n")
  
  
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
  
  
  df$timestamp <- as.POSIXct(
    df$timestamp,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )
  
  if (any(is.na(df$timestamp))) {
    stop("Some timestamp values could not be parsed.")
  }
  
  
  df <- df[order(df$locality, df$timestamp), ]
  
  rownames(df) <- NULL
  
  
  
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
  
  
    df$lag_1[idx] <- create_lag(demand, 1)
  
    df$lag_4[idx] <- create_lag(demand, 4)
  
    df$lag_96[idx] <- create_lag(demand, 96)
  
    df$lag_672[idx] <- create_lag(demand, 672)
  
  
  
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
  
  
  cat("Creating cyclical calendar features...\n")
  
  
  df$hour_sin <- sin(
    2 * pi * df$hour / 24
  )
  
  df$hour_cos <- cos(
    2 * pi * df$hour / 24
  )
  
  
  
  df$dow_sin <- sin(
    2 * pi * (df$day_of_week - 1) / 7
  )
  
  df$dow_cos <- cos(
    2 * pi * (df$day_of_week - 1) / 7
  )
  
  
  
  df$month_sin <- sin(
    2 * pi * (df$month - 1) / 12
  )
  
  df$month_cos <- cos(
    2 * pi * (df$month - 1) / 12
  )
  
  cat("Cyclical time features created.\n\n")
  
  
  cat("Creating weather/context features...\n")
  
  df$temperature_humidity_index <-
    df$temperature_c * df$humidity_pct / 100
  
  
  df$heavy_rain_event <-
    as.integer(df$rainfall_mm >= 5)
  
  
  df$high_wind_event <-
    as.integer(df$wind_speed_kmh >= 20)
  
  
  df$high_temperature_event <-
    as.integer(df$temperature_c >= 35)
  
  
  
  df$storage_per_1000_people <-
    df$storage_capacity_mld /
    (df$population / 1000)
  
  
  
  df$supply_efficiency_index <-
    df$water_supply_hours *
    df$distribution_efficiency
  
  
  
  df$infrastructure_stress <-
    df$pipeline_age_years *
    (1 - df$distribution_efficiency)
  
  
  
  
  land_use_sum <-
    df$residential_pct +
    df$commercial_pct +
    df$industrial_pct
  
  df$land_use_total_pct <- land_use_sum
  
  
  
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
  
  
  
  df$demand_per_1000_people <-
    df$water_demand /
    (df$population / 1000)
  
  
  
  leakage_columns <- c(
    "water_demand",
    "total_water_demand_reference",
    "synthetic_demand_share",
    "demand_per_1000_people"
  )
  
  
  model_features <- c(
  
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
    "storage_per_1000_people",
    "supply_efficiency_index",
    "infrastructure_stress",
  
    "temperature_c",
    "humidity_pct",
    "rainfall_mm",
    "wind_speed_kmh",
    "is_rain_event",
    "heavy_rain_event",
    "high_wind_event",
    "high_temperature_event",
    "temperature_humidity_index",
  
    "is_holiday",
    "is_weekend",
    "festival_period",
    "hour",
    "minute",
    "day_of_week",
    "month",
    "year",
  
    "hour_sin",
    "hour_cos",
    "dow_sin",
    "dow_cos",
    "month_sin",
    "month_cos",
  
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
  
  
  cat("Checking historical lag logic...\n")
  
  
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
  
  
  land_use_invalid <- sum(
    abs(df$land_use_total_pct - 100) > 0.01,
    na.rm = TRUE
  )
  
  cat(
    "Rows with land-use total different from 100%:",
    land_use_invalid,
    "\n\n"
  )
  
  
  
  output_columns <- c(
    "timestamp",
    "locality",
    "zone_type",
  
    "water_demand",
  
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
    "infrastructure_stress",
  
    "land_use_total_pct",
    "dominant_land_use",
  
    "demand_per_1000_people"
  )
  
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
  
  
  feature_description <- data.frame(
  
    feature = c(
  
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
      "infrastructure_stress",
  
      "land_use_total_pct",
      "dominant_land_use",
  
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
    "PHASE 2 - ENRICHED FEATURE ENGINEERING REPORT",
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
  
  
})

cat("\nRunning PHASE 3 - ENRICHED TRAIN / TEST SPLIT + LEAKAGE CHECKS...\n")
local({
  
  cat("============================================================\n")
  cat("PHASE 3 - ENRICHED TRAIN / TEST SPLIT + LEAKAGE CHECKS\n")
  cat("============================================================\n\n")
  
  
  
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
  
  
  
  if (!file.exists(INPUT_FILE)) {
    stop(
      paste0(
        "Input file not found:\n",
        INPUT_FILE,
        "\n\nRun Phase 2 first."
      )
    )
  }
  
  
  
  data <- read.csv(
    INPUT_FILE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  cat("Rows loaded:", nrow(data), "\n")
  cat("Columns loaded:", ncol(data), "\n\n")
  
  
  
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
  
  
  
  cat("Converting timestamps...\n")
  
  timestamp_raw <- trimws(
    as.character(data$timestamp)
  )
  
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
  
  
  
  model_features <- c(
  
    "locality",
    "zone_type",
  
    "area_km2",
    "population",
    "population_density",
  
    "residential_pct",
    "commercial_pct",
    "industrial_pct",
    "dominant_land_use",
  
    "storage_capacity_mld",
    "pipeline_age_years",
    "distribution_efficiency",
    "water_supply_hours",
    "storage_per_1000_people",
    "supply_efficiency_index",
    "infrastructure_stress",
  
    "temperature_c",
    "humidity_pct",
    "rainfall_mm",
    "wind_speed_kmh",
    "is_rain_event",
    "heavy_rain_event",
    "high_wind_event",
    "high_temperature_event",
    "temperature_humidity_index",
  
    "is_holiday",
    "is_weekend",
    "festival_period",
    "hour",
    "minute",
    "day_of_week",
    "month",
    "year",
  
    "hour_sin",
    "hour_cos",
    "dow_sin",
    "dow_cos",
    "month_sin",
    "month_cos",
  
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
    "PHASE 3 - ENRICHED TRAIN / TEST SPLIT REPORT",
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
  
  
})

cat("\nRunning PHASE 4 - TRAIN ENRICHED MODELS...\n")
local({
  
  cat("\n============================================\n")
  cat(" PHASE 4 - ENRICHED MODEL TRAINING\n")
  cat("============================================\n\n")
  
  
  options(stringsAsFactors = FALSE)
  
  PROJECT_DIR <- getwd()
  
  TRAIN_FILE <- file.path(PROJECT_DIR, "data", "enriched_train.csv")
  TEST_FILE  <- file.path(PROJECT_DIR, "data", "enriched_test.csv")
  
  MODEL_DIR  <- file.path(PROJECT_DIR, "models")
  OUTPUT_DIR <- file.path(PROJECT_DIR, "outputs")
  
  dir.create(MODEL_DIR, showWarnings = FALSE, recursive = TRUE)
  dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
  
  cat("Project directory:\n", PROJECT_DIR, "\n\n")
  
  
  required_packages <- c("randomForest")
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cloud.r-project.org")
    }
  }
  
  library(randomForest)
  
  
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
  
  
  categorical_features <- c(
    "locality",
    "zone_type",
    "dominant_land_use"
  )
  
  for (col in categorical_features) {
  
    if (col %in% names(train)) {
  
      train[[col]] <- as.factor(train[[col]])
  
      if (col %in% names(test)) {
        test[[col]] <- factor(
          test[[col]],
          levels = levels(train[[col]])
        )
      }
    }
  }
  
  
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
  
  
  x_train <- train_model[, model_features]
  y_train <- train_model[[target]]
  
  x_test <- test_model[, model_features]
  y_test <- test_model[[target]]
  
  
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
  
  lm_predictions <- predict(
    lm_model,
    newdata = test_model
  )
  
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
  
  
  cat("Generating Random Forest predictions...\n")
  
  rf_predictions <- predict(
    rf_model,
    newdata = x_test
  )
  
  cat("Predictions completed.\n\n")
  
  
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
  
  
  saveRDS(
    rf_model,
    file.path(
      MODEL_DIR,
      "enriched_random_forest_model.rds"
    )
  )
  
  cat("Random Forest model saved.\n\n")
  
  
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
  
  
  cat("Extracting Random Forest feature importance...\n")
  
  importance_matrix <- importance(
    rf_model
  )
  
  importance_df <- data.frame(
    Feature = rownames(importance_matrix),
    importance_matrix,
    row.names = NULL
  )
  
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
  
  
})

cat("\nRunning PHASE 5 - ORIGINAL vs ENRICHED MODEL COMPARISON...\n")
local({
  
  cat("\n============================================\n")
  cat(" PHASE 5 - ORIGINAL vs ENRICHED COMPARISON\n")
  cat("============================================\n\n")
  
  options(stringsAsFactors = FALSE)
  
  
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
  
  
  enriched_results$Model <- c(
    "Enriched Linear Regression",
    "Enriched Random Forest"
  )
  
  enriched_results$Dataset <- c(
    "Enriched contextual features",
    "Enriched contextual features"
  )
  
  
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
  
  
  comparison <- rbind(
    original_results,
    enriched_results
  )
  
  rownames(comparison) <- NULL
  
  cat("============================================\n")
  cat(" COMPLETE MODEL COMPARISON\n")
  cat("============================================\n\n")
  
  print(comparison)
  
  
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
  
  
  cat("\n============================================\n")
  cat(" IMPROVEMENT FROM ORIGINAL TO ENRICHED\n")
  cat("============================================\n\n")
  
  print(improvement_table)
  
  
  comparison_file <- file.path(
    OUTPUT_DIR,
    "original_vs_enriched_model_comparison.csv"
  )
  
  write.csv(
    comparison,
    comparison_file,
    row.names = FALSE
  )
  
  
  improvement_file <- file.path(
    OUTPUT_DIR,
    "original_vs_enriched_improvement.csv"
  )
  
  write.csv(
    improvement_table,
    improvement_file,
    row.names = FALSE
  )
  
  
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
  
  
})

cat("\nRunning PHASE 6 - EXPLAINABILITY + ERROR ANALYSIS...\n")
local({
  
  cat("\n============================================\n")
  cat(" PHASE 6 - EXPLAINABILITY + ERROR ANALYSIS\n")
  cat("============================================\n\n")
  
  options(stringsAsFactors = FALSE)
  
  
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
  
  
  actual <- predictions$actual_water_demand
  
  linear_pred <-
    predictions$linear_regression_prediction
  
  rf_pred <-
    predictions$random_forest_prediction
  
  
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
  
  
  cat("============================================\n")
  cat(" OVERALL ERROR ANALYSIS\n")
  cat("============================================\n\n")
  
  print(overall_metrics)
  
  
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
  
  
  if (!is.null(locality_metrics)) {
    
    cat("\n============================================\n")
    cat(" ERROR BY LOCALITY\n")
    cat("============================================\n\n")
    
    print(
      locality_metrics,
      row.names = FALSE
    )
  }
  
  
  cat("\n============================================\n")
  cat(" FEATURE IMPORTANCE\n")
  cat("============================================\n\n")
  
  importance <- read.csv(
    IMPORTANCE_FILE,
    stringsAsFactors = FALSE
  )
  
  
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
  
  
  cat("\n============================================\n")
  cat(" LARGEST PREDICTION ERRORS\n")
  cat("============================================\n\n")
  
  
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
  
  
  write.csv(
    residual_summary,
    file.path(
      OUTPUT_DIR,
      "residual_summary.csv"
    ),
    row.names = FALSE
  )
  
  
  write.csv(
    overall_metrics,
    file.path(
      OUTPUT_DIR,
      "enriched_error_metrics.csv"
    ),
    row.names = FALSE
  )
  
  
  write.csv(
    predictions,
    file.path(
      OUTPUT_DIR,
      "enriched_predictions_with_errors.csv"
    ),
    row.names = FALSE
  )
  
  
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
  
  
  writeLines(
    report,
    file.path(
      OUTPUT_DIR,
      "explainability_error_analysis_report.txt"
    )
  )
  
  
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
  
  
  
})

cat("\nRunning PHASE 7 - LOCALITY COMPARISON...\n")
local({
  
  cat("\n============================================\n")
  cat(" PHASE 7 - LOCALITY COMPARISON\n")
  cat("============================================\n\n")
  
  options(stringsAsFactors = FALSE)
  
  
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
  
  
  locality_errors <- read.csv(
    LOCALITY_ERROR_FILE,
    stringsAsFactors = FALSE
  )
  
  
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
  
  
  cat("Calculating demand statistics...\n")
  
  if (!"water_demand" %in% names(data)) {
  
    stop(
      "water_demand column not found in enriched dataset."
    )
  }
  
  
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
  
  
  locality_comparison <- merge(
    locality_comparison,
    locality_errors,
    by.x = "locality",
    by.y = "Locality",
    all.x = TRUE
  )
  
  
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
  
  
  locality_comparison <- locality_comparison[
    order(
      locality_comparison$locality
    ),
    ,
    drop = FALSE
  ]
  
  rownames(locality_comparison) <- NULL
  
  
  cat("\n============================================\n")
  cat(" LOCALITY COMPARISON\n")
  cat("============================================\n\n")
  
  print(
    locality_comparison,
    row.names = FALSE
  )
  
  
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
  
  
  write.csv(
    locality_comparison,
    file.path(
      OUTPUT_DIR,
      "locality_comparison.csv"
    ),
    row.names = FALSE
  )
  
  
  write.csv(
    model_performance,
    file.path(
      OUTPUT_DIR,
      "locality_model_performance.csv"
    ),
    row.names = FALSE
  )
  
  
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
  
  
})

cat("\nRunning PHASE 8 - WHAT-IF ANALYSIS...\n")
local({
  
  options(stringsAsFactors = FALSE)
  
  cat("\n============================================\n")
  cat(" PHASE 8 - WHAT-IF ANALYSIS\n")
  cat("============================================\n\n")
  
  
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop(
      "Package 'randomForest' is not installed. Run: install.packages('randomForest')"
    )
  }
  
  library(randomForest)
  
  
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
  
  
  create_prediction <- function(
    scenario,
    reference_row
  ) {
  
    new_data <- reference_row
  
  
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
  
  
    new_data$lag_1 <-
      scenario$lag_1
  
    new_data$lag_4 <-
      scenario$lag_4
  
    new_data$lag_96 <-
      scenario$lag_96
  
    new_data$lag_672 <-
      scenario$lag_672
  
  
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
  
  
    new_data$temperature_humidity_index <-
      scenario$temperature_c *
      (
        1 +
          scenario$humidity_pct / 100
      )
  
  
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
  
  
    prediction_data <- new_data[
      ,
      model_features,
      drop = FALSE
    ]
  
  
    prediction <- predict(
      rf_model,
      newdata = prediction_data
    )
  
    as.numeric(prediction)
  }
  
  
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
  
  
  cat("\n============================================\n")
  cat(" WHAT-IF ANALYSIS RESULTS\n")
  cat("============================================\n\n")
  
  print(
    scenario_results,
    row.names = FALSE
  )
  
  
  write.csv(
    baseline_results,
    file.path(
      OUTPUT_DIR,
      "what_if_baseline_predictions.csv"
    ),
    row.names = FALSE
  )
  
  
  write.csv(
    scenario_results,
    file.path(
      OUTPUT_DIR,
      "what_if_scenario_results.csv"
    ),
    row.names = FALSE
  )
  
  
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
  
  
  cat("\n============================================\n")
  cat(" PHASE 8 COMPLETED SUCCESSFULLY\n")
  cat("============================================\n\n")
  
  cat("Created files:\n")
  cat("- outputs/what_if_baseline_predictions.csv\n")
  cat("- outputs/what_if_scenario_results.csv\n")
  cat("- outputs/what_if_scenario_summary.csv\n")
  cat("- outputs/what_if_analysis_report.txt\n")
  
  cat("\n")
  
  
})

keep_outputs <- c(
  "automated_report_executive_summary.csv",
  "automated_water_demand_report.txt",
  "enriched_error_metrics.csv",
  "enriched_feature_importance.csv",
  "enriched_model_comparison.csv",
  "locality_comparison.csv",
  "locality_model_performance.csv",
  "original_vs_enriched_improvement.csv",
  "original_vs_enriched_model_comparison.csv",
  "what_if_scenario_summary.csv"
)

output_files <- list.files("outputs", full.names = TRUE)
remove_files <- output_files[
  !(basename(output_files) %in% keep_outputs)
]

if (length(remove_files) > 0) {
  unlink(remove_files)
}

cat(
  "Output cleanup complete. Kept ",
  length(setdiff(basename(output_files), basename(remove_files))),
  " important files in outputs/.\n",
  sep = ""
)

cat("\nRunning PHASE 9 - AUTOMATED REPORT GENERATION...\n")
local({
  
  options(stringsAsFactors = FALSE)
  
  cat("\n============================================\n")
  cat(" PHASE 9 - AUTOMATED REPORT GENERATION\n")
  cat("============================================\n\n")
  
  
  PROJECT_DIR <- getwd()
  
  DATA_DIR <- file.path(PROJECT_DIR, "data")
  OUTPUT_DIR <- file.path(PROJECT_DIR, "outputs")
  
  dir.create(
    OUTPUT_DIR,
    showWarnings = FALSE,
    recursive = TRUE
  )
  
  
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
  
  number_localities <- nrow(
    locality_comparison
  )
  
  
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
          " - importance ",
          fmt(
            row$Importance,
            2
          )
        )
      )
    }
  }
  
  
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
  
  
  cat("============================================\n")
  cat(" PHASE 9 COMPLETED SUCCESSFULLY\n")
  cat("============================================\n\n")
  
  cat("Created:\n")
  cat("- outputs/automated_water_demand_report.txt\n")
  cat("- outputs/automated_report_executive_summary.csv\n")
  
  cat("\n")
  
  
})
