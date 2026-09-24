# =============================================================================
# 03_generate_context_data.R
# Urban Water Demand Prediction Using R
# Phase 1: create reproducible synthetic locality/context data
#
# IMPORTANT:
#   - data/soweki_wdd_9.csv is NEVER overwritten.
#   - Original SOWEKI demand remains the reference aggregate demand.
#   - Synthetic locality/context variables are explicitly labelled as synthetic.
#   - The enriched target `water_demand` is a synthetic locality-level
#     allocation of the original SOWEKI `value`; it is NOT an observed
#     locality-level measurement.
#
# Run from the project root:
#   & "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" R/03_generate_context_data.R
#
# Writes:
#   data/locality_context.csv
#   data/urban_water_demand_enriched.csv
#   outputs/context_data_report.txt
#   outputs/context_feature_description.csv
# =============================================================================

if (sys.nframe() > 0) {
  return(invisible(NULL))
}

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

# Parse SOWEKI UTC timestamps.
raw$timestamp <- sub("Z$", "", raw$timestamp)
raw$timestamp <- sub("\\+00:00$", "", raw$timestamp)
raw$timestamp <- as.POSIXct(raw$timestamp, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
raw$value <- suppressWarnings(as.numeric(raw$value))
raw <- raw[!is.na(raw$timestamp) & !is.na(raw$value), c("timestamp", "value")]
raw <- raw[order(raw$timestamp), ]
raw <- raw[!duplicated(raw$timestamp), ]
rownames(raw) <- NULL

# -----------------------------------------------------------------------------
# 1. Synthetic locality table
# These are illustrative zones, not real measured localities.
# -----------------------------------------------------------------------------
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

# Base demand share is population weighted, then gently adjusted by land use.
zone_factor <- c(Residential = 1.00, Commercial = 1.08, Mixed = 1.04, Industrial = 1.12)
locality$base_demand_weight <- locality$population * zone_factor[locality$zone_type]
locality$base_demand_share <- locality$base_demand_weight / sum(locality$base_demand_weight)

write.csv(locality, locality_path, row.names = FALSE)

# -----------------------------------------------------------------------------
# 2. Time-varying synthetic context
# -----------------------------------------------------------------------------
dt <- raw$timestamp
n <- length(dt)
hour <- as.integer(format(dt, "%H"))
minute <- as.integer(format(dt, "%M"))
doy <- as.integer(format(dt, "%j"))
dow <- as.integer(format(dt, "%u"))
year <- as.integer(format(dt, "%Y"))

# Seasonal + daily temperature pattern. Synthetic, not observed weather.
seasonal_temp <- 29 + 4.5 * sin(2 * pi * (doy - 45) / 365.25)
diurnal_temp <- 4.0 * sin(2 * pi * (hour + minute / 60 - 14) / 24)
temperature <- seasonal_temp + diurnal_temp + rnorm(n, 0, 1.2)

temperature <- pmax(pmin(temperature, 45), 12)

# Rainfall: sparse events with stronger seasonal probability.
rain_prob <- pmin(0.55, 0.035 + 0.22 * pmax(0, sin(2 * pi * (doy - 160) / 365.25)))
rain_event <- rbinom(n, 1, rain_prob)
rainfall <- ifelse(rain_event == 1, rgamma(n, shape = 1.8, rate = 0.16), 0)
rainfall <- pmin(rainfall, 60)

# Humidity loosely inversely related to temperature and higher during rain.
humidity <- 68 - 0.75 * (temperature - 28) + 10 * rain_event + rnorm(n, 0, 5)
humidity <- pmax(pmin(humidity, 98), 25)

# Wind speed in km/h.
wind <- 10 + 4 * sin(2 * pi * hour / 24) + rnorm(n, 0, 2.5)
wind <- pmax(pmin(wind, 35), 0)

# Synthetic calendar effects. Random reproducible holidays are intentionally
# generic; they are not claims about any real-world holiday calendar.
dates <- as.Date(dt, tz = "UTC")
unique_dates <- sort(unique(dates))
holiday_dates <- unlist(lapply(sort(unique(year)), function(y) {
  candidates <- unique_dates[format(unique_dates, "%Y") == as.character(y)]
  sample(candidates, size = min(18, length(candidates)), replace = FALSE)
}))
is_holiday <- as.integer(dates %in% as.Date(holiday_dates))

# Three generic synthetic festival windows per year.
festival_dates <- as.Date(character(0))
for (y in sort(unique(year))) {
  starts <- as.Date(sprintf("%d-%02d-%02d", y, c(3, 8, 11), c(10, 5, 15)))
  for (s in starts) {
    festival_dates <- c(festival_dates, seq(s, by = "day", length.out = 5))
  }
}
festival_dates <- unique(festival_dates)
festival_period <- as.integer(dates %in% festival_dates)

# A synthetic supply schedule: locality-specific baseline with small daily noise.
# It is constrained to a realistic 6-14 hour range for this experimental dataset.
base_supply <- c(10, 9, 11, 8, 10)
names(base_supply) <- locality$locality

# -----------------------------------------------------------------------------
# 3. Allocate each original aggregate demand observation across localities.
# This preserves the original SOWEKI total at each timestamp while creating a
# synthetic locality-level target for comparative modelling.
# -----------------------------------------------------------------------------
locality_rows <- vector("list", length(locality$locality))

for (j in seq_len(nrow(locality))) {
  loc <- locality[j, ]
  loc_name <- loc$locality
  loc_type <- loc$zone_type

  # Demand pattern modifiers are synthetic assumptions used only to create a
  # coherent locality-level simulation.
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

  # Build all locality weights at the same timestamp later; here store the
  # locality-specific numerator.
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

# Combine and normalize locality weights within every timestamp.
enriched <- do.call(rbind, locality_rows)
weight_sum <- ave(enriched$dynamic_weight, enriched$timestamp, FUN = sum)
enriched$synthetic_demand_share <- enriched$dynamic_weight / weight_sum

# Original SOWEKI value is retained as an aggregate reference column only.
# It MUST NOT be used as a predictor in later models because it directly
# determines the synthetic locality-level target.
value_map <- raw$value
names(value_map) <- as.character(raw$timestamp)
enriched$total_water_demand_reference <- unname(value_map[as.character(enriched$timestamp)])
enriched$water_demand <- enriched$total_water_demand_reference * enriched$synthetic_demand_share

# Synthetic supply hours vary slightly by day but remain locality-specific.
day_index <- as.integer(enriched$timestamp - min(enriched$timestamp), units = "days")
enriched$water_supply_hours <- pmax(
  pmin(
    base_supply[enriched$locality] + 0.6 * sin(2 * pi * day_index / 7) + rnorm(nrow(enriched), 0, 0.35),
    14
  ),
  6
)

# Remove helper columns from the final modelling dataset.
enriched$dynamic_weight <- NULL

# Sort consistently.
enriched <- enriched[order(enriched$timestamp, enriched$locality), ]
rownames(enriched) <- NULL

# Round contextual variables for readable CSV output; retain sufficient
# precision for modelling.
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

# -----------------------------------------------------------------------------
# 4. Feature dictionary
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# 5. Validation/report
# -----------------------------------------------------------------------------
# Check that the five locality demands sum back to the original SOWEKI value
# at each timestamp (within floating point rounding tolerance).
reconstructed <- aggregate(water_demand ~ timestamp, enriched, sum)
original_check <- data.frame(
  timestamp = format(raw$timestamp, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  original = raw$value,
  stringsAsFactors = FALSE
)
check <- merge(original_check, reconstructed, by = "timestamp", all.x = TRUE)
check$abs_error <- abs(check$original - check$water_demand)

report_lines <- c(
  "URBAN WATER DEMAND — SYNTHETIC CONTEXT DATA REPORT",
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
