# =============================================================================
# 02_feature_engineering.R
# Urban Water Demand Prediction Using R
# Phase 2: clean the SOWEKI series and add calendar / lag / rolling features
#
# Run from the project folder (urban-water-demand):
#   & "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" R/02_feature_engineering.R
#
# Reads:  data/soweki_wdd_9.csv          (never overwritten)
# Writes: data/water_demand_processed.csv
#         outputs/feature_description.csv
#         outputs/demand_by_hour.png
#         outputs/demand_by_day_of_week.png
#         outputs/demand_distribution.png
#         outputs/feature_engineering_report.txt
#
# Why chronological order matters:
#   Water demand at time t depends on recent past demand. Lags and rolling
#   means must look backward only. Shuffling would mix future values into
#   "past" features (data leakage) and would break the 15-minute time grid.
# =============================================================================

# Skip when source()'d (Shiny auto-loads every file in R/).
if (sys.nframe() > 0) {
  return(invisible(NULL))
}

library(ggplot2)

# Relative paths (Windows-safe; run from the project root)
raw_path <- file.path("data", "soweki_wdd_9.csv")
processed_path <- file.path("data", "water_demand_processed.csv")
desc_path <- file.path("outputs", "feature_description.csv")
plot_hour_path <- file.path("outputs", "demand_by_hour.png")
plot_dow_path <- file.path("outputs", "demand_by_day_of_week.png")
plot_dist_path <- file.path("outputs", "demand_distribution.png")
report_path <- file.path("outputs", "feature_engineering_report.txt")

if (!file.exists(raw_path)) {
  stop("Dataset not found at ", raw_path, ". Run this script from the project root.")
}
dir.create("outputs", showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)

# Mean of the previous n observations only (excludes the current row).
# Implemented with cumsum so it stays fast on ~70,000 rows and never uses t+1.
previous_rollmean <- function(x, n) {
  x <- as.numeric(x)
  n_obs <- length(x)
  cs <- c(0, cumsum(x))
  out <- rep(NA_real_, n_obs)
  if (n_obs > n) {
    t <- (n + 1):n_obs
    # cs[t] - cs[t - n] = x[(t - n)] + ... + x[(t - 1)]
    out[t] <- (cs[t] - cs[t - n]) / n
  }
  out
}

# Linear interpolation along the regular 15-minute index (safety net).
# rule = 2 copies the nearest observed endpoint if NA sits at the edge.
fill_na_linear <- function(x) {
  i_ok <- which(!is.na(x))
  if (length(i_ok) == 0L || length(i_ok) == length(x)) {
    return(x)
  }
  i_na <- which(is.na(x))
  x[i_na] <- stats::approx(i_ok, x[i_ok], xout = i_na, rule = 2)$y
  x
}

# -----------------------------------------------------------------------------
# Load original CSV (read-only)
# -----------------------------------------------------------------------------
raw <- read.csv(raw_path, stringsAsFactors = FALSE)
n_original <- nrow(raw)

ts_char <- as.character(raw$timestamp)
datetime <- as.POSIXct(sub("\\+00:00$", "", ts_char),
                       format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
value <- suppressWarnings(as.numeric(raw$value))

df <- data.frame(datetime = datetime, value = value, stringsAsFactors = FALSE)

# Sort oldest -> newest before any lag / rolling calculation
df <- df[order(df$datetime), ]
rownames(df) <- NULL

n_dup_rows <- sum(duplicated(df))
df <- df[!duplicated(df), ]

# Duplicate timestamps: keep one row per time using the mean demand
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

# Impossible values: demand cannot be negative. Set those to NA, then interpolate.
n_negative <- sum(!is.na(df$value) & df$value < 0)
df$value[!is.na(df$value) & df$value < 0] <- NA_real_

# Suspicious high spikes (Tukey outer fence). Reported, not removed: they may
# be real peak demand.
q1 <- stats::quantile(df$value, 0.25, na.rm = TRUE)
q3 <- stats::quantile(df$value, 0.75, na.rm = TRUE)
iqr <- q3 - q1
upper_fence <- as.numeric(q3 + 3 * iqr)
n_high_outliers <- sum(!is.na(df$value) & df$value > upper_fence)

n_na_to_fill <- sum(is.na(df$value))
# Chosen missing-value method: linear interpolation on the 15-minute grid.
# Reason: the series is regular and local in time; interpolation uses only
# neighbouring observed demand, not future-engineered features and not
# invented weather/population variables. If nothing is missing, this is a no-op.
if (n_na_to_fill > 0) {
  df$value <- fill_na_linear(df$value)
}

n_missing_value_after_fill <- sum(is.na(df$value))

# Sampling interval (seconds) on the sorted unique timestamps
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

# -----------------------------------------------------------------------------
# Calendar features from timestamp only (no external variables)
# -----------------------------------------------------------------------------
df$year <- as.integer(format(df$datetime, "%Y"))
df$month <- as.integer(format(df$datetime, "%m"))
df$day <- as.integer(format(df$datetime, "%d"))
df$hour <- as.integer(format(df$datetime, "%H"))
df$minute <- as.integer(format(df$datetime, "%M"))
# Monday = 1 ... Sunday = 7
df$day_of_week <- as.integer(format(df$datetime, "%u"))
df$day_of_year <- as.integer(format(df$datetime, "%j"))
# ISO week number
df$week_of_year <- as.integer(format(df$datetime, "%V"))
df$is_weekend <- as.integer(df$day_of_week >= 6)
df$quarter <- as.integer((df$month - 1L) %/% 3L + 1L)

# -----------------------------------------------------------------------------
# Lag features: strictly previous observations (no future, no current)
# 15-minute sampling: 1 step = 15 min, 4 = 1 hour, 96 = 24 hours, 672 = 7 days
# -----------------------------------------------------------------------------
n_obs <- nrow(df)
lag_shift <- function(x, k) {
  c(rep(NA_real_, k), x[seq_len(n_obs - k)])
}

df$lag_1 <- lag_shift(df$value, 1L)     # previous 15 minutes
df$lag_4 <- lag_shift(df$value, 4L)     # previous 1 hour
df$lag_96 <- lag_shift(df$value, 96L)   # previous 24 hours
df$lag_672 <- lag_shift(df$value, 672L) # previous 7 days

# Rolling means of the previous window only (current row excluded)
df$rolling_mean_4 <- previous_rollmean(df$value, 4L)
df$rolling_mean_96 <- previous_rollmean(df$value, 96L)
df$rolling_mean_672 <- previous_rollmean(df$value, 672L)

# Drop the warm-up rows that cannot have a 7-day lag / 7-day rolling mean
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

# Save processed data. as.character() keeps UTC timestamps readable on Windows.
out <- df
out$datetime <- format(df$datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC")
utils::write.csv(out, processed_path, row.names = FALSE)
cat("\nSaved:", processed_path, "\n")

# -----------------------------------------------------------------------------
# Feature dictionary for the viva
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# Exploratory plots (processed data only)
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# Short report
# -----------------------------------------------------------------------------
report_lines <- c(
  "SOWEKI water demand — feature engineering report",
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
