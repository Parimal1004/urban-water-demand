# =============================================================================
# 01_data_understanding.R
# Urban Water Demand Prediction Using R
# Phase 1: inspect the SOWEKI dataset (no ML, no dashboard, no synthetic data)
#
# Run from the project folder (urban-water-demand):
#   & "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" R/01_data_understanding.R
#
# Reads:  data/soweki_wdd_9.csv  (original file is never overwritten)
# Writes: outputs/water_demand_over_time.png
#         outputs/data_quality_report.txt
# =============================================================================

# Skip when source()'d (Shiny auto-loads every file in R/).
if (sys.nframe() > 0) {
  return(invisible(NULL))
}

# ggplot2 is used for the time-series plot
library(ggplot2)

# Relative paths from the project root (Windows-safe)
data_path <- file.path("data", "soweki_wdd_9.csv")
plot_path <- file.path("outputs", "water_demand_over_time.png")
report_path <- file.path("outputs", "data_quality_report.txt")

if (!file.exists(data_path)) {
  stop("Dataset not found at ", data_path,
       ". Place soweki_wdd_9.csv in the data folder and run from the project root.")
}

dir.create("outputs", showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Load the original CSV (do not modify it)
# -----------------------------------------------------------------------------
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

# Missing values per column
missing_counts <- colSums(is.na(raw) | raw == "")
cat("\n=== Missing values (NA or empty string) ===\n")
print(missing_counts)

# Duplicate full rows
n_dup_rows <- sum(duplicated(raw))
cat("\nDuplicate full rows:", n_dup_rows, "\n")

# -----------------------------------------------------------------------------
# Convert timestamp to POSIXct (UTC; original strings include +00:00)
# -----------------------------------------------------------------------------
# Strip the timezone suffix so as.POSIXct can parse ISO-8601 on Windows R
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

# -----------------------------------------------------------------------------
# Timestamp order
# -----------------------------------------------------------------------------
# Compare each row to the next; TRUE means the series is non-decreasing
is_ordered <- all(diff(as.numeric(datetime)) >= 0, na.rm = TRUE)
n_out_of_order <- sum(diff(as.numeric(datetime)) < 0, na.rm = TRUE)

cat("\n=== Timestamp order ===\n")
cat("Timestamps are non-decreasing:", is_ordered, "\n")
cat("Number of backward steps:", n_out_of_order, "\n")

# -----------------------------------------------------------------------------
# Sampling interval between consecutive rows (in the file order)
# -----------------------------------------------------------------------------
row_diffs_sec <- as.numeric(diff(datetime), units = "secs")
row_diffs_sec <- row_diffs_sec[!is.na(row_diffs_sec)]

# Typical interval: median of positive gaps (ignores 0-second duplicates)
positive_diffs <- row_diffs_sec[row_diffs_sec > 0]
median_interval_sec <- if (length(positive_diffs) > 0) {
  as.numeric(median(positive_diffs))
} else {
  NA_real_
}

# Frequency table of the most common gaps (helps viva explanation)
gap_table <- sort(table(row_diffs_sec), decreasing = TRUE)
cat("\n=== Consecutive-row time intervals (seconds) ===\n")
cat("Median positive interval (seconds):", median_interval_sec, "\n")
cat("Most common intervals (top 10):\n")
print(head(gap_table, 10))

# -----------------------------------------------------------------------------
# Missing timestamps vs a regular grid at the detected interval
# -----------------------------------------------------------------------------
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

# Extra large gaps (bigger than 1.5 x typical interval) after sorting
sorted_dt <- sort(datetime[!is.na(datetime)])
sorted_diffs <- as.numeric(diff(sorted_dt), units = "secs")
large_gap_count <- if (!is.na(median_interval_sec)) {
  sum(sorted_diffs > 1.5 * median_interval_sec)
} else {
  NA_integer_
}
cat("Gaps larger than 1.5 x typical interval (sorted series):", large_gap_count, "\n")

# -----------------------------------------------------------------------------
# Basic plot: water demand over time
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# Concise data-quality report
# -----------------------------------------------------------------------------
interval_minutes <- if (!is.na(median_interval_sec)) median_interval_sec / 60 else NA

report_lines <- c(
  "SOWEKI water demand — data quality report",
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
