# =============================================================================
# app.R
# Explainable Urban Water Demand Forecasting and Decision-Support System
# =============================================================================

needed <- c("shiny", "shinydashboard", "plotly", "DT", "randomForest")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) stop("Missing R packages: ", paste(missing, collapse = ", "))

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(randomForest)

options(shiny.autoload.r = FALSE)

# ---- Paths ------------------------------------------------------------------
project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

model_path <- file.path(project_dir, "models", "enriched_random_forest_model.rds")
data_path <- file.path(project_dir, "data", "enriched_deployment.csv")
locality_path <- file.path(project_dir, "data", "locality_context.csv")

required_files <- c(model_path, data_path, locality_path)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Required files not found: ", paste(missing_files, collapse = ", "))
}

rf_model <- readRDS(model_path)
enriched <- read.csv(data_path, stringsAsFactors = FALSE, check.names = FALSE)
locality_context <- read.csv(locality_path, stringsAsFactors = FALSE, check.names = FALSE)

enriched$timestamp <- as.POSIXct(enriched$timestamp, tz = "UTC")
enriched <- enriched[order(enriched$locality, enriched$timestamp), ]

# SOWEKI demand is in m³/h. Contextual locality variables are synthetic
# experimental features and are not measurements of real locality demand.
DEMAND_UNIT <- "m³/h"

zones <- sort(unique(enriched$locality))
zone_types <- unique(enriched[, c("locality", "zone_type")])
zone_type_map <- setNames(zone_types$zone_type, zone_types$locality)

# 47 leakage-safe predictors used in Phase 4.
model_features <- c(
  "locality", "zone_type", "area_km2", "population", "population_density",
  "residential_pct", "commercial_pct", "industrial_pct", "storage_capacity_mld",
  "pipeline_age_years", "distribution_efficiency", "temperature_c", "humidity_pct",
  "rainfall_mm", "wind_speed_kmh", "is_rain_event", "is_holiday", "is_weekend",
  "festival_period", "hour", "minute", "day_of_week", "month", "year",
  "water_supply_hours", "lag_1", "lag_4", "lag_96", "lag_672", "rolling_mean_4",
  "rolling_mean_96", "rolling_mean_672", "rolling_sd_96", "rolling_sd_672",
  "hour_sin", "hour_cos", "dow_sin", "dow_cos", "month_sin", "month_cos",
  "temperature_humidity_index", "heavy_rain_event", "high_wind_event",
  "high_temperature_event", "storage_per_1000_people", "supply_efficiency_index",
  "infrastructure_stress", "land_use_total_pct", "dominant_land_use"
)

# ---- Helpers ----------------------------------------------------------------
num <- function(x, default = 0) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.finite(x), x, default)
}

clamp <- function(x, lo, hi) pmax(lo, pmin(hi, x))

format_demand <- function(x) sprintf("%.3f %s", as.numeric(x), DEMAND_UNIT)

safe_mean <- function(x) if (length(x) == 0 || all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

safe_sd <- function(x) if (sum(is.finite(x)) < 2) NA_real_ else sd(x, na.rm = TRUE)

get_zone_info <- function(zone) {
  x <- locality_context[locality_context$locality == zone, , drop = FALSE]
  if (nrow(x) == 0) x <- enriched[enriched$locality == zone, , drop = FALSE]
  x[1, , drop = FALSE]
}

# Retrieve historical SOWEKI-style demand values from the locality's historical
# series when available. These are the values the model expects for lag inputs.
get_historical_lags <- function(zone, target_ts) {
  z <- enriched[enriched$locality == zone & enriched$timestamp < target_ts, , drop = FALSE]
  if (nrow(z) == 0) return(c(lag_1 = NA, lag_4 = NA, lag_96 = NA, lag_672 = NA))
  z <- z[order(z$timestamp, decreasing = TRUE), , drop = FALSE]
  get_n <- function(n) if (nrow(z) >= n) z$water_demand[n] else NA_real_
  c(lag_1 = get_n(1), lag_4 = get_n(4), lag_96 = get_n(96), lag_672 = get_n(672))
}

build_prediction_row <- function(zone, pred_date, pred_time, temperature, humidity,
                                 rainfall, wind_speed, supply_hours, storage_capacity,
                                 pipeline_age, efficiency, holiday, weekend, festival,
                                 lag_1, lag_4, lag_96, lag_672) {
  info <- get_zone_info(zone)
  ts <- as.POSIXct(paste(pred_date, pred_time), format = "%Y-%m-%d %H:%M", tz = "UTC")
  hr <- as.integer(format(ts, "%H"))
  minute <- as.integer(format(ts, "%M"))
  dow <- as.integer(format(ts, "%u"))
  month <- as.integer(format(ts, "%m"))
  year <- as.integer(format(ts, "%Y"))

  pop <- num(info$population[1], 1)
  area <- num(info$area_km2[1], 1)
  residential <- num(info$residential_pct[1], 0)
  commercial <- num(info$commercial_pct[1], 0)
  industrial <- num(info$industrial_pct[1], 0)
  land_total <- residential + commercial + industrial
  dominant <- c(Residential = residential, Commercial = commercial, Industrial = industrial)
  dominant <- names(which.max(dominant))

  lag_values <- c(num(lag_1), num(lag_4), num(lag_96), num(lag_672))
  rolling4 <- mean(c(lag_values[1], lag_values[2]), na.rm = TRUE)
  rolling96 <- mean(c(lag_values[1], lag_values[2], lag_values[3]), na.rm = TRUE)
  rolling672 <- mean(lag_values, na.rm = TRUE)
  if (!is.finite(rolling4)) rolling4 <- 0
  if (!is.finite(rolling96)) rolling96 <- 0
  if (!is.finite(rolling672)) rolling672 <- 0

  # With four user-visible lags, use a conservative proxy for rolling SDs.
  rolling_sd96 <- safe_sd(lag_values[c(1, 2, 3)])
  rolling_sd672 <- safe_sd(lag_values)
  if (!is.finite(rolling_sd96)) rolling_sd96 <- 0
  if (!is.finite(rolling_sd672)) rolling_sd672 <- 0

  rainfall <- max(0, num(rainfall))
  humidity <- clamp(num(humidity), 0, 100)
  temperature <- num(temperature)
  wind_speed <- max(0, num(wind_speed))
  supply_hours <- clamp(num(supply_hours), 0, 24)
  efficiency <- clamp(num(efficiency), 0, 100)
  storage_capacity <- max(0, num(storage_capacity))
  pipeline_age <- max(0, num(pipeline_age))

  is_rain <- as.integer(rainfall >= 5)
  heavy_rain <- as.integer(rainfall >= 10)
  high_wind <- as.integer(wind_speed >= 25)
  high_temp <- as.integer(temperature >= 35)

  out <- data.frame(
    locality = zone,
    zone_type = as.character(info$zone_type[1]),
    area_km2 = area,
    population = pop,
    population_density = pop / area,
    residential_pct = residential,
    commercial_pct = commercial,
    industrial_pct = industrial,
    storage_capacity_mld = storage_capacity,
    pipeline_age_years = pipeline_age,
    distribution_efficiency = efficiency,
    temperature_c = temperature,
    humidity_pct = humidity,
    rainfall_mm = rainfall,
    wind_speed_kmh = wind_speed,
    is_rain_event = is_rain,
    is_holiday = as.integer(holiday),
    is_weekend = as.integer(weekend),
    festival_period = as.integer(festival),
    hour = hr,
    minute = minute,
    day_of_week = dow,
    month = month,
    year = year,
    water_supply_hours = supply_hours,
    lag_1 = lag_values[1], lag_4 = lag_values[2], lag_96 = lag_values[3], lag_672 = lag_values[4],
    rolling_mean_4 = rolling4, rolling_mean_96 = rolling96, rolling_mean_672 = rolling672,
    rolling_sd_96 = rolling_sd96, rolling_sd_672 = rolling_sd672,
    hour_sin = sin(2 * pi * hr / 24), hour_cos = cos(2 * pi * hr / 24),
    dow_sin = sin(2 * pi * dow / 7), dow_cos = cos(2 * pi * dow / 7),
    month_sin = sin(2 * pi * month / 12), month_cos = cos(2 * pi * month / 12),
    temperature_humidity_index = temperature * (1 + humidity / 100),
    heavy_rain_event = heavy_rain,
    high_wind_event = high_wind,
    high_temperature_event = high_temp,
    storage_per_1000_people = storage_capacity / max(pop / 1000, 1e-9),
    supply_efficiency_index = supply_hours * efficiency / 100,
    infrastructure_stress = (1 - efficiency / 100) + pipeline_age / 20,
    land_use_total_pct = land_total,
    dominant_land_use = dominant,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Match training factor levels where available.
  if ("locality" %in% names(rf_model$forest$xlevels)) {
    out$locality <- factor(out$locality, levels = rf_model$forest$xlevels$locality)
  }
  if ("zone_type" %in% names(rf_model$forest$xlevels)) {
    out$zone_type <- factor(out$zone_type, levels = rf_model$forest$xlevels$zone_type)
  }
  if ("dominant_land_use" %in% names(rf_model$forest$xlevels)) {
    out$dominant_land_use <- factor(out$dominant_land_use, levels = rf_model$forest$xlevels$dominant_land_use)
  }

  out[, model_features, drop = FALSE]
}

predict_row <- function(row) {
  as.numeric(predict(rf_model, newdata = row))
}

classify_demand <- function(pred, historical) {
  ref <- historical[is.finite(historical)]
  if (length(ref) < 3) {
    if (pred <= 1) {
      return("Low")
    } else if (pred <= 2) {
      return("Moderate")
    } else {
      return("High")
    }
  }
  q1 <- quantile(ref, 0.33, na.rm = TRUE)
  q2 <- quantile(ref, 0.67, na.rm = TRUE)
  if (pred <= q1) "Low" else if (pred <= q2) "Moderate" else if (pred <= max(ref, na.rm = TRUE)) "High" else "Very High"
}

# ---- UI ---------------------------------------------------------------------

ui <- dashboardPage(
  skin = "black", 
  dashboardHeader(title = "AquaSense"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Forecast Demand", tabName = "predict", icon = icon("tint")),
      menuItem("Demand Insights", tabName = "insights", icon = icon("lightbulb")),
      menuItem("What-If Analysis", tabName = "whatif", icon = icon("sliders-h")),
      menuItem("Zone Comparison", tabName = "compare", icon = icon("exchange-alt")),
      menuItem("Prediction History", tabName = "history", icon = icon("history")),
      menuItem("My Report", tabName = "report", icon = icon("file-alt"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;800&display=swap');
      
      body, h1, h2, h3, h4, h5, h6 { 
        font-family: 'Inter', sans-serif !important; 
      }
      
      .content-wrapper, .right-side { 
        background-color: #f8fafc; 
      }
      
      /* Global Input Tweaks to Prevent Scrolling */
      .shiny-input-container { margin-bottom: 12px !important; }
      .control-label { font-size: 13px; color: #475569; font-weight: 600; margin-bottom: 4px; }
      .form-group { margin-bottom: 0 !important; }
      hr { border-top: 1px solid #e2e8f0; margin-top: 12px; margin-bottom: 15px; }

      /* Header & Logo styling */
      .skin-black .main-header .navbar, .skin-black .main-header .logo { 
        background: linear-gradient(90deg, #0f172a, #1e293b) !important;
        color: white !important;
      }
      .skin-black .main-header .logo { font-weight: 800; letter-spacing: 1px; border-right: none;}
      .skin-black .main-header .navbar .sidebar-toggle:hover { background-color: rgba(255,255,255,0.1) !important; color: white !important; }
      
      /* Sidebar */
      .skin-black .sidebar-menu > li.active > a, .skin-black .sidebar-menu > li:hover > a {
        background-color: #1e293b !important;
        border-left-color: #4f46e5 !important;
      }

      /* Hero Section */
      .home-hero { 
        background: linear-gradient(135deg, #0f172a 0%, #1e1b4b 50%, #312e81 100%); 
        color: white; 
        padding: 50px 40px; 
        border-radius: 20px; 
        margin-bottom: 25px; 
        box-shadow: 0 10px 30px rgba(0,0,0,0.15);
        position: relative;
        overflow: hidden;
      }
      .home-hero h1 { font-size: 48px; font-weight: 800; margin: 0 0 10px; letter-spacing: -1px; }
      .home-hero p { font-size: 20px; opacity: 0.9; margin-bottom: 25px; font-weight: 300; }
      
      /* Cards */
      .feature-card { 
        background: white; 
        border-radius: 16px; 
        padding: 25px; 
        min-height: 160px; 
        box-shadow: 0 4px 20px rgba(0,0,0,0.03); 
        margin-bottom: 20px;
        border: 1px solid #f1f5f9;
        transition: all 0.3s cubic-bezier(0.25, 0.8, 0.25, 1);
      }
      .feature-card:hover { 
        transform: translateY(-5px); 
        box-shadow: 0 12px 25px rgba(79, 70, 229, 0.08); 
      }
      .feature-card h3 { margin-top: 10px; color: #1e293b; font-weight: 800; }
      .feature-icon { font-size: 36px; color: #4f46e5; margin-bottom: 10px; }
      
      /* KPIs */
      .kpi-card { 
        background: linear-gradient(145deg, #ffffff, #f8fafc); 
        border-radius: 16px; 
        padding: 20px; 
        border-left: 5px solid #4f46e5; 
        margin-bottom: 20px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.03);
        transition: transform 0.2s ease;
      }
      .kpi-card:hover { transform: scale(1.02); }
      .kpi-value { font-size: 30px; font-weight: 800; color: #1e293b; margin: 8px 0; }
      .kpi-label { color: #64748b; font-size: 11px; font-weight: 800; text-transform: uppercase; letter-spacing: 1px; }
      
      /* Typography */
      .section-title { color: #0f172a; font-weight: 800; margin-bottom: 20px; font-size: 30px; letter-spacing: -0.5px;}
      .unit-note { color: #94a3b8; font-size: 12px; font-style: italic; }
      .subsection-title { font-size: 14px; font-weight: 800; color: #312e81; text-transform: uppercase; letter-spacing: 0.5px; margin: 5px 0 10px; }
      
      /* Box containers */
      .box { 
        border-radius: 16px; 
        border-top: none !important; 
        box-shadow: 0 5px 20px rgba(0,0,0,0.04) !important; 
      }
      .box-header { 
        border-top-left-radius: 16px; 
        border-top-right-radius: 16px; 
        padding: 15px 20px;
      }
      .box.box-primary .box-header { background: linear-gradient(90deg, #4f46e5, #6366f1) !important; color: white; }
      .box.box-info .box-header { background: linear-gradient(90deg, #334155, #475569) !important; color: white; }
      .box-title { font-weight: 600; letter-spacing: 0.5px; }
      
      /* Insights */
      .insight-box { 
        padding: 20px; 
        border-radius: 14px; 
        background: #f5f3ff; 
        border-left: 5px solid #6366f1; 
        margin-bottom: 15px;
        transition: all 0.3s ease;
      }
      .insight-box:hover { background: #ede9fe; }
      .insight-box h4 { font-weight: 800; color: #3730a3; margin-top: 0; }
      
      /* Buttons */
      .predict-btn, .btn-primary { 
        background: linear-gradient(90deg, #4f46e5, #3b82f6); 
        border: none; 
        border-radius: 30px; 
        font-weight: 800; 
        letter-spacing: 1px; 
        padding: 12px 25px; 
        width: 100%;
        transition: all 0.3s ease; 
        box-shadow: 0 4px 15px rgba(79, 70, 229, 0.3); 
        margin-top: 10px;
      }
      .predict-btn:hover, .btn-primary:hover { 
        background: linear-gradient(90deg, #3b82f6, #4f46e5); 
        transform: translateY(-2px); 
        box-shadow: 0 6px 20px rgba(79, 70, 229, 0.4); 
      }
      .btn-light { 
        background: white; 
        color: #4f46e5; 
        border-radius: 30px; 
        font-weight: 800; 
        padding: 12px 35px; 
        box-shadow: 0 4px 15px rgba(0,0,0,0.1); 
        transition: all 0.3s; 
      }
      .btn-light:hover { 
        background: #f8fafc; 
        transform: translateY(-2px); 
        color: #3b82f6; 
        box-shadow: 0 8px 25px rgba(0,0,0,0.2);
      }
      
      /* Form Controls */
      .form-control { 
        border-radius: 8px; 
        border: 1px solid #cbd5e1; 
        padding: 6px 12px; 
        height: auto;
        transition: all 0.2s ease;
      }
      .form-control:focus { 
        border-color: #6366f1; 
        box-shadow: 0 0 0 3px rgba(99, 102, 241, 0.15); 
      }
      .selectize-input { 
        border-radius: 8px !important; 
        padding: 8px 12px !important; 
        border: 1px solid #cbd5e1 !important;
      }
      .selectize-input.focus {
        border-color: #6366f1 !important; 
        box-shadow: 0 0 0 3px rgba(99, 102, 241, 0.15) !important; 
      }
      
      .report-box { background: white; padding: 25px; border-radius: 16px; border: 1px solid #f1f5f9; }
      
      /* Grid Layouts for Data Display */
      .report-canvas {
        background: white; 
        padding: 35px; 
        border-radius: 16px; 
        box-shadow: 0 10px 30px rgba(0,0,0,0.03); 
        width: 100%; 
        border: 1px solid #e2e8f0;
      }
      .grid-params {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(140px, 1fr));
        gap: 15px;
        background: #f8fafc;
        padding: 18px;
        border-radius: 12px;
        border: 1px solid #e2e8f0;
      }
      .grid-param-item { display: flex; flex-direction: column; }
      .grid-param-label { color: #64748b; font-size: 11px; text-transform: uppercase; letter-spacing: 0.5px; font-weight: 700; margin-bottom: 2px; }
      .grid-param-value { color: #1e293b; font-size: 15px; font-weight: 700; }
    "))),
    tabItems(
      # HOME ------------------------------------------------------------------
      tabItem("home",
        div(class="home-hero",
          h1(icon("tint"), " AquaSense"),
          p("Urban Water Demand Intelligence & Decision Support"),
          h4(style="font-weight: 400; opacity: 0.85; margin-bottom: 25px;", "Predict • Understand • Compare • Plan"),
          actionButton("start_analysis", "START ANALYSIS", icon=icon("arrow-right"), class="btn-lg btn-light")
        ),
        fluidRow(
          column(3, div(class="feature-card", div(class="feature-icon", icon("chart-line")), h3("Forecast Demand"), p("Estimate future water demand in m³/h for a selected locality and time."))),
          column(3, div(class="feature-card", div(class="feature-icon", icon("lightbulb")), h3("Demand Insights"), p("Turn a prediction into a simple explanation using historical patterns and operating conditions."))),
          column(3, div(class="feature-card", div(class="feature-icon", icon("exchange-alt")), h3("Compare Zones"), p("Select two zones and compare their historical water-demand behaviour."))),
          column(3, div(class="feature-card", div(class="feature-icon", icon("sliders-h")), h3("Scenario Planning"), p("Explore how changes in weather, supply and infrastructure inputs affect the model prediction.")))
        ),
        fluidRow(
          column(12, div(class="feature-card", style="min-height: auto;",
            h3("What makes AquaSense different?"),
            tags$ul(style="font-size: 16px; line-height: 1.8; color: #475569;",
              tags$li("Uses historical demand together with locality, weather, calendar and infrastructure context."),
              tags$li("Shows every water-demand value with its unit: m³/h."),
              tags$li("Provides interpretable demand insights instead of exposing model internals only."),
              tags$li("Generates a user-specific report from the selected prediction scenario."),
              tags$li(strong("Data note: "), "locality and contextual attributes in this prototype are synthetic experimental features; the original SOWEKI source data is kept unchanged.")
            )
          ))
        )
      ),

      # PREDICT ---------------------------------------------------------------
      tabItem("predict",
        h2("Forecast Water Demand", class="section-title"),
        p(style="color: #64748b; font-size: 16px; margin-bottom: 20px;", "Enter the scenario conditions. Historical demand inputs are expressed in m³/h."),
        fluidRow(
          column(5,
            box(width=12, title="Prediction Scenario", status="primary", solidHeader=TRUE,
              fluidRow(
                column(12, selectInput("zone", "Locality", choices=zones, selected=zones[1]))
              ),
              fluidRow(
                column(6, dateInput("pred_date", "Prediction date", value=as.Date(max(enriched$timestamp, na.rm=TRUE)), format="yyyy-mm-dd")),
                column(6, selectInput("pred_time", "Time (UTC)", choices=sprintf("%02d:%02d", rep(0:23, each=4), rep(c(0,15,30,45),24)), selected="08:00"))
              ),
              tags$hr(),
              h5(class="subsection-title", "Weather Settings"),
              fluidRow(
                column(6, numericInput("temperature", "Temp (°C)", 29, step=.1)),
                column(6, numericInput("humidity", "Humidity (%)", 68, min=0, max=100, step=.1))
              ),
              fluidRow(
                column(6, numericInput("rainfall", "Rainfall (mm)", 0, min=0, step=.1)),
                column(6, numericInput("wind_speed", "Wind (km/h)", 10, min=0, step=.1))
              ),
              tags$hr(),
              h5(class="subsection-title", "Infrastructure"),
              fluidRow(
                column(6, numericInput("supply_hours", "Supply (hrs/day)", 10, min=0, max=24, step=.1)),
                column(6, numericInput("storage_capacity", "Storage (MLD)", 24, min=0, step=.1))
              ),
              fluidRow(
                column(6, numericInput("pipeline_age", "Pipe Age (yrs)", 11, min=0, step=.1)),
                column(6, numericInput("efficiency", "Efficiency (%)", 91, min=0, max=100, step=.1))
              ),
              tags$hr(),
              fluidRow(
                column(4, checkboxInput("holiday", "Holiday", FALSE)),
                column(4, checkboxInput("weekend", "Weekend", FALSE)),
                column(4, checkboxInput("festival", "Festival", FALSE))
              ),
              tags$hr(),
              h5(class="subsection-title", "Historical Inputs (m³/h)"),
              fluidRow(
                column(6, numericInput("lag_1", "15 mins ago", NA, min=0, step=.001)),
                column(6, numericInput("lag_4", "1 hour ago", NA, min=0, step=.001))
              ),
              fluidRow(
                column(6, numericInput("lag_96", "24 hrs ago", NA, min=0, step=.001)),
                column(6, numericInput("lag_672", "7 days ago", NA, min=0, step=.001))
              ),
              actionButton("predict_btn", "PREDICT DEMAND", class="btn-primary predict-btn", icon=icon("calculator"))
            )
          ),
          column(7,
            box(width=12, title="Prediction Result", status="primary", solidHeader=TRUE, uiOutput("prediction_result")),
            box(width=12, title="Scenario Snapshot", status="info", solidHeader=TRUE, uiOutput("scenario_grid_ui"))
          )
        )
      ),

      # INSIGHTS --------------------------------------------------------------
      tabItem("insights",
        h2("Demand Insights", class="section-title"),
        p(style="color: #64748b; font-size: 16px; margin-bottom: 20px;", "A practical interpretation of the latest prediction — not model internals."),
        uiOutput("insight_content")
      ),

      # WHAT IF ----------------------------------------------------------------
      tabItem("whatif",
        h2("What-If Analysis", class="section-title"),
        p(style="color: #64748b; font-size: 16px; margin-bottom: 20px;", "Change one operating condition at a time and compare the resulting RF prediction with the current baseline."),
        fluidRow(
          column(4, box(width=12, title="Scenario Configurations", status="primary", solidHeader=TRUE,
            selectInput("whatif_scenario", "Adjust Scenario", choices=c("Higher Temperature"="temp", "Heavy Rainfall"="rain", "Reduced Water Supply"="supply", "Improved Efficiency"="eff", "Older Pipeline"="pipeline")),
            tags$br(),
            actionButton("run_whatif", "RUN SCENARIO", class="btn-primary", icon=icon("play"))
          )),
          column(8, box(width=12, title="Scenario Result", status="info", solidHeader=TRUE, uiOutput("whatif_result")))
        )
      ),

      # COMPARISON ------------------------------------------------------------
      tabItem("compare",
        h2("Zone Comparison", class="section-title"),
        p(style="color: #64748b; font-size: 16px; margin-bottom: 20px;", "Select two localities to compare their historical demand. Locality-level allocation is synthetic experimental data."),
        fluidRow(
          column(6, box(width=12, status="primary", solidHeader=FALSE, selectInput("zone1", "Select Zone 1", choices=zones, selected=zones[1]))),
          column(6, box(width=12, status="primary", solidHeader=FALSE, selectInput("zone2", "Select Zone 2", choices=zones, selected=zones[min(2,length(zones))])))
        ),
        fluidRow(
          column(12, box(width=12, title="Historical Demand Comparison", status="primary", solidHeader=TRUE, plotlyOutput("zone_plot", height="380px")))
        ),
        fluidRow(column(12, box(width=12, title="Zone Statistics Overview", status="info", solidHeader=TRUE, DTOutput("zone_table"))))
      ),

      # HISTORY ----------------------------------------------------------------
      tabItem("history",
        h2("Prediction History", class="section-title"),
        p(style="color: #64748b; font-size: 16px; margin-bottom: 20px;", "Predictions generated during this browser session."),
        div(class="report-canvas", style="padding: 20px;", 
            DTOutput("history_table")
        )
      ),

      # REPORT -----------------------------------------------------------------
      tabItem("report",
        h2("My Report", class="section-title"),
        p(style="color: #64748b; font-size: 16px; margin-bottom: 20px;", "A user-specific report based on the latest prediction scenario. This is not the overall academic project report."),
        fluidRow(
          column(12,
            uiOutput("report_preview"),
            tags$div(style="margin-top: 20px; text-align: left;",
              downloadButton("download_report", "DOWNLOAD REPORT", class="btn-primary", style="width: auto; padding: 12px 30px;")
            )
          )
        )
      )
    )
  )
)

# ---- Server -----------------------------------------------------------------

server <- function(input, output, session) {
  rv <- reactiveValues(prediction=NULL, row=NULL, history=data.frame())

  observeEvent(input$start_analysis, updateTabItems(session, "tabs", "predict"))

  observeEvent(input$zone, {
    ts <- as.POSIXct(paste(input$pred_date, input$pred_time), format="%Y-%m-%d %H:%M", tz="UTC")
    l <- get_historical_lags(input$zone, ts)
    updateNumericInput(session, "lag_1", value=ifelse(is.na(l[1]), NA, round(l[1], 3)))
    updateNumericInput(session, "lag_4", value=ifelse(is.na(l[2]), NA, round(l[2], 3)))
    updateNumericInput(session, "lag_96", value=ifelse(is.na(l[3]), NA, round(l[3], 3)))
    updateNumericInput(session, "lag_672", value=ifelse(is.na(l[4]), NA, round(l[4], 3)))
  }, ignoreInit=FALSE)

  observeEvent(input$pred_date, {
    if (!is.null(input$zone)) {
      ts <- as.POSIXct(paste(input$pred_date, input$pred_time), format="%Y-%m-%d %H:%M", tz="UTC")
      l <- get_historical_lags(input$zone, ts)
      updateNumericInput(session, "lag_1", value=ifelse(is.na(l[1]), NA, round(l[1], 3)))
      updateNumericInput(session, "lag_4", value=ifelse(is.na(l[2]), NA, round(l[2], 3)))
      updateNumericInput(session, "lag_96", value=ifelse(is.na(l[3]), NA, round(l[3], 3)))
      updateNumericInput(session, "lag_672", value=ifelse(is.na(l[4]), NA, round(l[4], 3)))
    }
  })

  observeEvent(input$predict_btn, {
    req(input$zone, input$pred_date, input$pred_time)
    row <- build_prediction_row(input$zone, input$pred_date, input$pred_time,
      input$temperature, input$humidity, input$rainfall, input$wind_speed,
      input$supply_hours, input$storage_capacity, input$pipeline_age,
      input$efficiency, input$holiday, input$weekend, input$festival,
      input$lag_1, input$lag_4, input$lag_96, input$lag_672)
    pred <- tryCatch(predict_row(row), error=function(e) e)
    if (inherits(pred, "error")) {
      showNotification(paste("Prediction error:", pred$message), type="error", duration=8)
      return()
    }
    hist <- enriched$water_demand[enriched$locality == input$zone]
    level <- classify_demand(pred, hist)
    rv$prediction <- list(value=pred, level=level, zone=input$zone, timestamp=paste(input$pred_date,input$pred_time), row=row)
    rv$row <- row
    rv$history <- rbind(rv$history, data.frame(Time=paste(input$pred_date,input$pred_time), Locality=input$zone, Predicted_Demand_m3_h=round(pred,3), Demand_Level=level))
    updateTabItems(session, "tabs", "predict")
  })

  output$prediction_result <- renderUI({
    req(rv$prediction)
    p <- rv$prediction
    info <- get_zone_info(p$zone)
    hist <- enriched$water_demand[enriched$locality == p$zone]
    recent <- tail(hist[is.finite(hist)], 96)
    delta <- p$value - safe_mean(recent)
    delta_text <- if (is.finite(delta)) paste0(ifelse(delta >= 0, "+", ""), sprintf("%.3f", delta), " m³/h vs recent avg") else "Recent average unavailable"
    
    div(style="padding-bottom: 5px;",
      fluidRow(
        column(4, div(class="kpi-card", style="padding: 15px;", div(class="kpi-label", "Predicted Demand"), div(class="kpi-value", format_demand(p$value)), p(style="font-weight: 600; color: #4f46e5; margin:0;", paste("Status:", p$level)))),
        column(4, div(class="kpi-card", style="padding: 15px;", div(class="kpi-label", "Locality"), div(class="kpi-value", style="font-size: 24px;", p$zone), p(style="margin:0; color:#64748b;", zone_type_map[[p$zone]]))),
        column(4, div(class="kpi-card", style="padding: 15px;", div(class="kpi-label", "Recent Avg Compare"), div(class="kpi-value", style=ifelse(delta>0, "color:#e11d48; font-size:20px;", "color:#059669; font-size:20px;"), delta_text), p(style="margin:0; color:#64748b; font-size:12px;", "vs. latest 96 observations")))
      ),
      tags$hr(style="border-top: 1px solid #e2e8f0; margin: 5px 0 15px;"),
      p(style="font-size: 15px; color: #475569; margin: 0;", if (is.finite(delta) && delta > 0) "The prediction is above the recent locality average." else if (is.finite(delta) && delta < 0) "The prediction is below the recent locality average." else "The prediction is close to the recent locality average.")
    )
  })

  output$scenario_grid_ui <- renderUI({
    req(rv$prediction)
    r <- rv$prediction$row
    HTML(paste0(
      "<div class=\"grid-params\" style=\"padding: 10px; background: transparent; border: none;\">",
        "<div class=\"grid-param-item\"><span class=\"grid-param-label\">Temp</span><span class=\"grid-param-value\">", r$temperature_c, " °C</span></div>",
        "<div class=\"grid-param-item\"><span class=\"grid-param-label\">Humidity</span><span class=\"grid-param-value\">", r$humidity_pct, "%</span></div>",
        "<div class=\"grid-param-item\"><span class=\"grid-param-label\">Rainfall</span><span class=\"grid-param-value\">", r$rainfall_mm, " mm</span></div>",
        "<div class=\"grid-param-item\"><span class=\"grid-param-label\">Wind</span><span class=\"grid-param-value\">", r$wind_speed_kmh, " km/h</span></div>",
        "<div class=\"grid-param-item\"><span class=\"grid-param-label\">Supply</span><span class=\"grid-param-value\">", r$water_supply_hours, " hrs/d</span></div>",
        "<div class=\"grid-param-item\"><span class=\"grid-param-label\">Storage</span><span class=\"grid-param-value\">", r$storage_capacity_mld, " MLD</span></div>",
        "<div class=\"grid-param-item\"><span class=\"grid-param-label\">Pipe Age</span><span class=\"grid-param-value\">", r$pipeline_age_years, " yrs</span></div>",
        "<div class=\"grid-param-item\"><span class=\"grid-param-label\">Efficiency</span><span class=\"grid-param-value\">", r$distribution_efficiency, "%</span></div>",
        "<div class=\"grid-param-item\"><span class=\"grid-param-label\">Lag 1 (15m)</span><span class=\"grid-param-value\">", format_demand(r$lag_1), "</span></div>",
        "<div class=\"grid-param-item\"><span class=\"grid-param-label\">Lag 4 (1h)</span><span class=\"grid-param-value\">", format_demand(r$lag_4), "</span></div>",
        "<div class=\"grid-param-item\"><span class=\"grid-param-label\">Lag 96 (24h)</span><span class=\"grid-param-value\">", format_demand(r$lag_96), "</span></div>",
        "<div class=\"grid-param-item\"><span class=\"grid-param-label\">Lag 672 (7d)</span><span class=\"grid-param-value\">", format_demand(r$lag_672), "</span></div>",
      "</div>"
    ))
  })

  output$insight_content <- renderUI({
    req(rv$prediction)
    p <- rv$prediction
    r <- p$row
    z <- enriched[enriched$locality == p$zone, , drop=FALSE]
    recent <- tail(z$water_demand[is.finite(z$water_demand)], 96)
    recent_mean <- safe_mean(recent)
    change <- if (is.finite(recent_mean)) 100*(p$value-recent_mean)/max(abs(recent_mean),1e-9) else NA
    trend_text <- if (!is.finite(change)) "Recent comparison is unavailable." else if (change > 5) paste0("Demand is approximately ",sprintf("%.1f",change),"% above the recent average.") else if (change < -5) paste0("Demand is approximately ",sprintf("%.1f",abs(change)),"% below the recent average.") else "Demand is close to the recent average."
    alert <- if (p$level %in% c("High","Very High")) "Higher-demand conditions are indicated for this scenario." else "No high-demand flag is triggered by the current scenario."
    fluidRow(
      column(4, div(class="kpi-card", div(class="kpi-label","Predicted demand"), div(class="kpi-value",format_demand(p$value)), p(style="font-weight:600; color:#4f46e5; margin:0;", p$level))),
      column(4, div(class="kpi-card", div(class="kpi-label","Recent Average"), div(class="kpi-value",format_demand(recent_mean)), p(style="margin:0; color:#64748b;", "Latest 96 obs."))),
      column(4, div(class="kpi-card", div(class="kpi-label","Change vs Average"), div(class="kpi-value",style=ifelse(change>0, "color:#e11d48;", "color:#059669;"), ifelse(is.finite(change),paste0(sprintf("%+.1f",change),"%"),"N/A")), p(style="margin:0; color:#64748b;", "Against recent avg"))),
      column(12, div(class="insight-box", h4(icon("chart-line"), " Demand Pattern"), p(style="font-size: 16px;", trend_text))),
      column(6, div(class="insight-box", h4(icon("cloud-sun"), " Weather Conditions"), p(style="font-size: 15px;", paste0("Temperature: ",r$temperature_c," °C; humidity: ",r$humidity_pct,"%; rainfall: ",r$rainfall_mm," mm; wind: ",r$wind_speed_kmh," km/h.")))),
      column(6, div(class="insight-box", h4(icon("tint"), " Supply & Infrastructure"), p(style="font-size: 15px;", paste0("Supply: ",r$water_supply_hours," hours/day; efficiency: ",r$distribution_efficiency,"%; pipeline age: ",r$pipeline_age_years," years; storage: ",r$storage_capacity_mld," MLD.")))),
      column(12, div(class="insight-box", style=ifelse(p$level %in% c("High","Very High"), "background:#fff1f2; border-left-color:#e11d48;", "background:#f5f3ff;"), h4(icon("exclamation-triangle"), " Demand Status"), p(style="font-size: 16px; font-weight: 600;", alert), p(class="unit-note","This is an interpretation of the model output, not an operational safety threshold.")))
    )
  })

  observeEvent(input$run_whatif, {
    req(rv$prediction)
    r <- rv$prediction$row
    make <- function(type) {
      rr <- r
      if (type=="temp") rr$temperature_c <- rr$temperature_c + 5
      if (type=="rain") { rr$rainfall_mm <- rr$rainfall_mm + 20; rr$humidity_pct <- clamp(rr$humidity_pct + 5,0,100); rr$heavy_rain_event <- 1; rr$is_rain_event <- 1 }
      if (type=="supply") rr$water_supply_hours <- clamp(rr$water_supply_hours - 3,0,24)
      if (type=="eff") { rr$distribution_efficiency <- clamp(rr$distribution_efficiency + 5,0,100); rr$supply_efficiency_index <- rr$water_supply_hours*rr$distribution_efficiency/100; rr$infrastructure_stress <- (1-rr$distribution_efficiency/100)+rr$pipeline_age_years/20 }
      if (type=="pipeline") { rr$pipeline_age_years <- rr$pipeline_age_years + 5; rr$infrastructure_stress <- (1-rr$distribution_efficiency/100)+rr$pipeline_age_years/20 }
      predict_row(rr)
    }
    base <- rv$prediction$value
    scen <- make(input$whatif_scenario)
    label <- switch(input$whatif_scenario,temp="+5 °C temperature",rain="+20 mm rainfall and +5% humidity",supply="-3 hours/day water supply",eff="+5 percentage points efficiency",pipeline="+5 years pipeline age")
    rv$whatif <- data.frame(Scenario=c("Baseline",label), Predicted_Demand_m3_h=c(base,scen), Change_m3_h=c(0,scen-base), Change_percent=c(0,100*(scen-base)/max(abs(base),1e-9)))
  })

  output$whatif_result <- renderUI({
    req(rv$whatif)
    x <- rv$whatif
    div(class="report-box",
      h4(style="font-weight: 800; color: #312e81;", "Prediction impact"),
      tags$ul(style="font-size: 16px; line-height: 1.8; color: #475569;", 
              tags$li(paste0("Baseline: ",format_demand(x$Predicted_Demand_m3_h[1]))), 
              tags$li(paste0(x$Scenario[2],": ",format_demand(x$Predicted_Demand_m3_h[2]))), 
              tags$li(strong(paste0("Change: ",sprintf("%+.3f",x$Change_m3_h[2])," m³/h (",sprintf("%+.1f",x$Change_percent[2]),"%)")))
      ),
      tags$hr(style="border-top: 1px solid #e2e8f0;"),
      DTOutput("whatif_table")
    )
  })
  output$whatif_table <- renderDT({ req(rv$whatif); datatable(transform(rv$whatif, Predicted_Demand_m3_h=sprintf("%.3f m³/h",Predicted_Demand_m3_h), Change_m3_h=sprintf("%+.3f m³/h",Change_m3_h), Change_percent=sprintf("%+.1f%%",Change_percent)), options=list(dom="t", pageLength=5)) })

  zone_stats <- reactive({
    req(input$zone1,input$zone2)
    zz <- c(input$zone1,input$zone2)
    do.call(rbind,lapply(zz,function(z){v<-enriched$water_demand[enriched$locality==z]; data.frame(Locality=z,Mean_m3_h=safe_mean(v),Median_m3_h=median(v,na.rm=TRUE),Min_m3_h=min(v,na.rm=TRUE),Max_m3_h=max(v,na.rm=TRUE),SD_m3_h=safe_sd(v))}))
  })
  output$zone_table <- renderDT({ datatable(transform(zone_stats(), Mean_m3_h=sprintf("%.3f m³/h",Mean_m3_h), Median_m3_h=sprintf("%.3f m³/h",Median_m3_h), Min_m3_h=sprintf("%.3f m³/h",Min_m3_h), Max_m3_h=sprintf("%.3f m³/h",Max_m3_h), SD_m3_h=sprintf("%.3f m³/h",SD_m3_h)), colnames=c("Locality","Mean demand","Median demand","Minimum demand","Maximum demand","Demand SD"), options=list(dom="t",pageLength=5)) })
  output$zone_plot <- renderPlotly({
    s <- zone_stats()
    plot_ly(s,x=~Locality,y=~Mean_m3_h,type="bar",text=~sprintf("%.3f m³/h",Mean_m3_h),textposition="auto", marker=list(color=c("#4f46e5", "#334155"))) %>% 
      layout(yaxis=list(title="Average water demand (m³/h)"),xaxis=list(title="Locality"), plot_bgcolor="rgba(0,0,0,0)", paper_bgcolor="rgba(0,0,0,0)")
  })

  output$history_table <- renderDT({
    h <- rv$history
    if (nrow(h)==0) h <- data.frame(Message="No predictions yet. Run a prediction from Forecast Demand.")
    datatable(h, options=list(pageLength=10, scrollX=TRUE, dom='t'))
  })

  output$report_preview <- renderUI({
    req(rv$prediction)
    p <- rv$prediction
    r <- p$row
    recent <- enriched$water_demand[enriched$locality == p$zone]
    recent_mean <- safe_mean(tail(recent[is.finite(recent)], 96))

    HTML(paste0(
      "<div class=\"report-canvas\">",
      
        "<!-- Header -->",
        "<h2 style=\"color:#1e293b; font-weight:800; margin-top:0; margin-bottom: 5px; font-size: 32px;\">AquaSense Demand Report</h2>",
        "<p style=\"color:#64748b; border-bottom: 2px solid #f1f5f9; padding-bottom:20px; font-size: 15px;\">",
        "<b>Locality:</b> ", p$zone, " &nbsp;|&nbsp; ",
        "<b>Zone type:</b> ", as.character(r$zone_type), " &nbsp;|&nbsp; ",
        "<b>Prediction time:</b> ", p$timestamp, " UTC</p>",
        
        "<!-- Prediction Highlight -->",
        "<div style=\"margin: 30px 0;\">",
          "<h4 style=\"font-weight:700; color:#64748b; text-transform:uppercase; font-size:13px;\">Predicted Demand</h4>",
          "<h1 style=\"color:#4f46e5; font-weight:800; margin: 10px 0 15px; font-size: 48px; line-height: 1;\">", format_demand(p$value), "</h1>",
          "<p style=\"font-size: 16px;\"><b>Demand Status:</b> <span style=\"background:#e0e7ff; padding:6px 12px; border-radius:6px; color:#3730a3; font-weight:700;\">", p$level, "</span></p>",
        "</div>",
        
        "<!-- Selected Conditions Grid -->",
        "<h4 style=\"font-weight:800; color:#1e293b; margin-bottom: 15px; font-size: 20px;\">Operating Conditions</h4>",
        "<div class=\"grid-params\">",
          "<div class=\"grid-param-item\"><span class=\"grid-param-label\">Temperature</span><span class=\"grid-param-value\">", r$temperature_c, " °C</span></div>",
          "<div class=\"grid-param-item\"><span class=\"grid-param-label\">Humidity</span><span class=\"grid-param-value\">", r$humidity_pct, "%</span></div>",
          "<div class=\"grid-param-item\"><span class=\"grid-param-label\">Rainfall</span><span class=\"grid-param-value\">", r$rainfall_mm, " mm</span></div>",
          "<div class=\"grid-param-item\"><span class=\"grid-param-label\">Wind Speed</span><span class=\"grid-param-value\">", r$wind_speed_kmh, " km/h</span></div>",
          "<div class=\"grid-param-item\"><span class=\"grid-param-label\">Supply Hours</span><span class=\"grid-param-value\">", r$water_supply_hours, " hrs/day</span></div>",
          "<div class=\"grid-param-item\"><span class=\"grid-param-label\">Storage Capacity</span><span class=\"grid-param-value\">", r$storage_capacity_mld, " MLD</span></div>",
          "<div class=\"grid-param-item\"><span class=\"grid-param-label\">Pipeline Age</span><span class=\"grid-param-value\">", r$pipeline_age_years, " years</span></div>",
          "<div class=\"grid-param-item\"><span class=\"grid-param-label\">Efficiency</span><span class=\"grid-param-value\">", r$distribution_efficiency, "%</span></div>",
        "</div>",
        
        "<!-- Historical Demand Grid -->",
        "<h4 style=\"font-weight:800; color:#1e293b; margin-top: 30px; margin-bottom: 15px; font-size: 20px;\">Historical Inputs</h4>",
        "<div class=\"grid-params\">",
          "<div class=\"grid-param-item\"><span class=\"grid-param-label\">15 Mins Ago</span><span class=\"grid-param-value\">", format_demand(r$lag_1), "</span></div>",
          "<div class=\"grid-param-item\"><span class=\"grid-param-label\">1 Hour Ago</span><span class=\"grid-param-value\">", format_demand(r$lag_4), "</span></div>",
          "<div class=\"grid-param-item\"><span class=\"grid-param-label\">24 Hours Ago</span><span class=\"grid-param-value\">", format_demand(r$lag_96), "</span></div>",
          "<div class=\"grid-param-item\"><span class=\"grid-param-label\">7 Days Ago</span><span class=\"grid-param-value\">", format_demand(r$lag_672), "</span></div>",
          "<div class=\"grid-param-item\"><span class=\"grid-param-label\" style=\"color:#4f46e5;\">Recent Average</span><span class=\"grid-param-value\" style=\"color:#4f46e5;\">", format_demand(recent_mean), "</span></div>",
        "</div>",
        
        "<!-- Interpretation -->",
        "<h4 style=\"font-weight:800; color:#1e293b; margin-top: 30px; margin-bottom: 10px; font-size: 20px;\">Interpretation</h4>",
        "<p style=\"font-size: 16px; color:#475569; background:#f8fafc; border-left:4px solid #6366f1; padding: 15px; border-radius: 4px;\">",
        ifelse(p$value > recent_mean,
               "The predicted demand is <b>above</b> the recent locality average, indicating potentially elevated consumption for this time block.",
               "The predicted demand is <b>at or below</b> the recent locality average, suggesting standard or low consumption patterns for this time block."),
        "</p>",
        
        "<!-- Footer Note -->",
        "<p class=\"unit-note\" style=\"margin-top:40px; text-align:center; padding-top: 20px; border-top: 1px dashed #cbd5e1;\">",
        "Demand is expressed in m³/h. Locality and context variables in this prototype are synthetic experimental features and should not be interpreted as measured real-world locality data.</p>",
        
      "</div>"
    ))
  })

  output$download_report <- downloadHandler(
    filename = function() {
      paste0("AquaSense_Water_Demand_Report_", Sys.Date(), ".html")
    },
    content = function(file) {
      req(rv$prediction)
      p <- rv$prediction
      r <- p$row
      recent <- enriched$water_demand[enriched$locality == p$zone]
      recent_mean <- safe_mean(tail(recent[is.finite(recent)], 96))

      interpretation <- if (p$value > recent_mean) {
        "The predicted demand is above the recent locality average."
      } else {
        "The predicted demand is at or below the recent locality average."
      }

      html <- paste0(
        "<!doctype html><html><head><meta charset=\"utf-8\">",
        "<title>AquaSense Report</title>",
        "<style>body{font-family:'Segoe UI', Arial, sans-serif;max-width:850px;margin:40px auto;line-height:1.6; color:#334155;}",
        "h1,h2{color:#1e293b; font-weight:800;}.box{border:1px solid #e2e8f0;padding:25px;",
        "border-radius:12px;margin:20px 0; box-shadow: 0 4px 6px rgba(0,0,0,0.02);}",
        ".highlight{color:#4f46e5;}</style></head><body>",
        "<h1>AquaSense — Water Demand Report</h1>",
        "<div class=\"box\"><h2>Prediction</h2><h1 style=\"font-size: 42px; margin: 10px 0;\" class=\"highlight\">", format_demand(p$value),
        "</h1><p><b>Demand Status:</b> <span style=\"background:#e0e7ff; padding:4px 8px; border-radius:4px; color:#3730a3; font-weight:600;\">", p$level,
        "</span></p><hr style=\"border:none; border-top:1px solid #f1f5f9; margin:15px 0;\"><p><b>Locality:</b> ", p$zone,
        "<br><b>Zone type:</b> ", as.character(r$zone_type),
        "<br><b>Prediction time:</b> ", p$timestamp, " UTC</p></div>",
        "<div class=\"box\"><h2>Scenario Conditions</h2><p style=\"background:#f8fafc; padding: 15px; border-radius: 8px;\">",
        "Temperature: <b>", r$temperature_c, " °C</b><br>",
        "Humidity: <b>", r$humidity_pct, "%</b><br>",
        "Rainfall: <b>", r$rainfall_mm, " mm</b><br>",
        "Wind speed: <b>", r$wind_speed_kmh, " km/h</b><br>",
        "Water supply: <b>", r$water_supply_hours, " hours/day</b><br>",
        "Storage capacity: <b>", r$storage_capacity_mld, " MLD</b><br>",
        "Pipeline age: <b>", r$pipeline_age_years, " years</b><br>",
        "Distribution efficiency: <b>", r$distribution_efficiency, "%</b></p></div>",
        "<div class=\"box\"><h2>Historical demand</h2><p>",
        "15 minutes ago: ", format_demand(r$lag_1), "<br>",
        "1 hour ago: ", format_demand(r$lag_4), "<br>",
        "24 hours ago: ", format_demand(r$lag_96), "<br>",
        "7 days ago: ", format_demand(r$lag_672), "</p>",
        "<p><b>Recent Average Demand: </b><span class=\"highlight\">", format_demand(recent_mean), "</span></p></div>",
        "<div class=\"box\"><h2>Interpretation</h2><p>", interpretation,
        "</p><p style=\"color:#64748b; font-size:12px; margin-top:20px;\">Demand values are expressed in m³/h. Locality/context variables are synthetic experimental features.</p></div>",
        "</body></html>"
      )

      writeLines(html, file, useBytes = TRUE)
    }
  )

}

shinyApp(ui, server)