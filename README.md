# 💧 AquaSense — Explainable Urban Water Demand Forecasting & Decision-Support System

**AquaSense** is an explainable machine-learning based urban water demand forecasting and decision-support system developed using **R, Random Forest, and Shiny**.

The system forecasts water demand using historical demand patterns, temporal features, weather conditions, infrastructure characteristics, and locality-level contextual features through an interactive dashboard.

## 🚀 Project Overview

Urban water demand varies with time, weather, population, infrastructure, holidays, and other contextual factors. AquaSense combines these factors into a machine-learning forecasting pipeline and presents the results through an interactive web dashboard.

### Key capabilities

* 📈 Water demand forecasting
* 🤖 Random Forest-based prediction
* 🔍 Explainability and feature analysis
* 📊 Historical demand insights
* 🌦️ Weather and environmental context
* 🏙️ Locality/zone comparison
* 🧪 What-if scenario analysis
* 📋 Prediction-based reporting
* 🌐 Interactive R Shiny dashboard

---

## 🖥️ AquaSense Dashboard

The application provides several interactive modules:

### 1. Forecast Demand

Users can select a locality and provide prediction conditions such as:

* Prediction date and time
* Temperature
* Humidity
* Rainfall
* Wind speed
* Water supply hours
* Storage capacity
* Pipeline age
* Distribution efficiency
* Holiday/weekend/festival conditions
* Historical demand lag values

The system generates a predicted water demand value in **m³/h**.

### 2. Demand Insights

Provides historical demand analysis and visualizations to understand demand patterns across the available zones.

### 3. What-If Analysis

Allows users to modify important environmental and operational conditions and examine how the predicted demand changes under different scenarios.

### 4. Zone Comparison

Compares demand characteristics and model-related metrics between two selected urban zones.

### 5. Prediction History

Provides access to previous prediction results generated during the dashboard session.

### 6. My Report

Generates a user-specific summary based on the selected prediction inputs and resulting forecast.

---

## 🧠 Machine Learning Pipeline

The project follows a complete data-science workflow:

```text
Raw SOWEKI Data
       ↓
Data Understanding
       ↓
Feature Engineering
       ↓
Contextual Feature Generation
       ↓
Time-Series Feature Engineering
       ↓
Train/Test Split
       ↓
Linear Regression + Random Forest
       ↓
Model Evaluation
       ↓
Explainability & Error Analysis
       ↓
What-If Analysis
       ↓
AquaSense Shiny Dashboard
```

---

## 📊 Feature Engineering

The enriched dataset contains temporal, environmental, infrastructure, demographic, and lag-based features.

Important feature groups include:

### Temporal Features

* Hour
* Minute
* Day of week
* Month
* Year
* Weekend indicator
* Holiday indicator
* Festival period

### Historical Demand Features

* `lag_1`
* `lag_4`
* `lag_96`
* `lag_672`
* Rolling mean features
* Rolling standard deviation features

The lag values represent historical demand observations at different time intervals.

### Weather Features

* Temperature
* Humidity
* Rainfall
* Wind speed
* Rain-event indicator
* Heavy-rain indicator
* High-wind indicator
* High-temperature indicator

### Infrastructure Features

* Storage capacity
* Pipeline age
* Distribution efficiency
* Water supply hours
* Infrastructure stress
* Supply efficiency index

### Locality Features

* Population
* Population density
* Area
* Residential percentage
* Commercial percentage
* Industrial percentage
* Land-use information

### Cyclical Features

The project also uses cyclical transformations for:

* Hour
* Day of week
* Month

These help represent the repeating nature of temporal patterns.

---

## 🤖 Model Performance

Two regression approaches were evaluated on the enriched feature set.

| Model             |    MAE |   RMSE |     R² |
| ----------------- | -----: | -----: | -----: |
| Linear Regression | 0.1007 | 0.1834 | 0.9057 |
| Random Forest     | 0.0994 | 0.1807 | 0.9084 |

The Random Forest model was selected for the interactive forecasting application based on the evaluation metrics obtained during the project experiments.

The trained model is stored as:

```text
models/enriched_random_forest_model.rds
```

---

## 🔍 Explainability

AquaSense is designed as an **explainable forecasting system**, rather than simply producing a prediction.

The project includes:

* Feature importance analysis
* Prediction error analysis
* Locality-level performance analysis
* Largest-error investigation
* What-if scenario analysis
* Model comparison

Generated analysis files are stored under:

```text
outputs/
```

---

## 🗂️ Project Structure

```text
urban-water-demand/
│
├── app.R
├── README.md
├── requirements.R
├── .Rprofile
├── .gitignore
├── Dockerfile
│
├── data/
│   ├── enriched_deployment.csv
│   ├── locality_context.csv
│   ├── soweki_wdd_9.csv
│   └── urban_water_demand_enriched.csv
│
├── models/
│   └── enriched_random_forest_model.rds
│
├── R/
│   ├── 01_data_understanding.R
│   ├── 02_feature_engineering.R
│   ├── 03_generate_context_data.R
│   ├── 04_enriched_feature_engineering.R
│   ├── 05_enriched_split.R
│   ├── 06_train_enriched_models.R
│   ├── 07_compare_original_enriched.R
│   ├── 08_explainability_error_analysis.R
│   ├── 09_locality_comparison.R
│   ├── 10_what_if_analysis.R
│   └── 11_generate_report.R
│
└── outputs/
    ├── enriched_error_metrics.csv
    ├── enriched_feature_description.csv
    ├── enriched_feature_importance.csv
    ├── enriched_model_comparison.csv
    ├── explainability_error_analysis_report.txt
    ├── locality_comparison.csv
    ├── locality_error_metrics.csv
    ├── locality_model_performance.csv
    ├── what_if_scenario_results.csv
    └── ...
```

---

## 🛠️ Technology Stack

| Technology         | Purpose                           |
| ------------------ | --------------------------------- |
| **R**              | Data science and machine learning |
| **Random Forest**  | Demand forecasting                |
| **Shiny**          | Interactive web application       |
| **shinydashboard** | Dashboard interface               |
| **Plotly**         | Interactive visualizations        |
| **DT**             | Interactive data tables           |
| **ggplot2**        | Data visualization                |
| **dplyr / tidyr**  | Data manipulation                 |
| **caret**          | Machine-learning workflow         |
| **Metrics**        | Model evaluation                  |
| **Git / GitHub**   | Version control                   |
| **Docker**         | Application deployment            |

---

## ⚙️ Local Installation

### Prerequisites

Install:

* R 4.6.1
* Git
* Optional: Docker for containerized deployment

Clone the repository:

```bash
git clone https://github.com/Parimal1004/urban-water-demand.git
cd urban-water-demand
```

### Install R dependencies

From the project directory:

```powershell
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" requirements.R
```

### Run AquaSense

```powershell
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" -e "options(shiny.autoload.r=FALSE); shiny::runApp('.', port=3850)"
```

Then open:

```text
http://127.0.0.1:3850
```

---

## 🐳 Docker Deployment

The repository includes a Docker configuration for deployment platforms that support Docker-based services.

Build the image:

```bash
docker build -t aquasense .
```

Run the container:

```bash
docker run -p 10000:10000 -e PORT=10000 aquasense
```

The application is configured to listen on:

```text
0.0.0.0:$PORT
```

---

## 🌐 Deployment

The project is configured for deployment as a Docker-based web service.

The current deployment workflow uses:

```text
GitHub
   ↓
Dockerfile
   ↓
R 4.6.1
   ↓
Shiny
   ↓
AquaSense Dashboard
```

---

## 📦 Deployment Dataset

The complete enriched dataset is used during the research and model-development pipeline.

Because GitHub has individual-file size constraints, the deployed dashboard uses a compact historical dataset:

```text
data/enriched_deployment.csv
```

This deployment dataset contains:

* **43,665 rows**
* **51 columns**
* Approximately **16.65 MB**

The full enriched feature dataset is retained separately from the deployed application because of its larger size.

---

## ⚠️ Data & Feature Disclosure

The project combines historical SOWEKI-style water-demand data with contextual locality features.

Some locality-level contextual variables are **synthetically generated experimental features**. They are included to demonstrate how demographic, infrastructure, land-use, and environmental context can be incorporated into a forecasting system.

They should **not be interpreted as measured real-world values for individual localities**.

The dashboard explicitly identifies these contextual variables as synthetic experimental features.

---

## 📈 Dataset Coverage

The enriched historical dataset covers:

```text
December 2023 → December 2025
```

with observations at approximately **15-minute intervals** across five zones:

```text
Zone_A
Zone_B
Zone_C
Zone_D
Zone_E
```

Each zone contains approximately **70,177 observations** in the complete enriched dataset.

---

## 🎯 Project Objectives

The main objectives of AquaSense are to:

1. Forecast urban water demand using machine learning.
2. Engineer meaningful temporal, environmental, infrastructure, and locality features.
3. Compare traditional regression with ensemble learning.
4. Analyze model errors and feature importance.
5. Provide interpretable forecasting results.
6. Support what-if operational analysis.
7. Compare demand characteristics across urban zones.
8. Deliver the complete workflow through an interactive dashboard.

---

## 🔮 Future Improvements

Potential future extensions include:

* Real-time IoT water-meter integration
* Live weather API integration
* More advanced time-series models
* XGBoost/LightGBM comparison
* SHAP-based local explanations
* Automated model retraining
* Real-time anomaly detection
* Water supply optimization
* Cloud database integration
* Authentication and role-based access
* Real-world locality infrastructure data

---

## 👨‍💻 Author

**Parimal Goud**

AI & Data Science Undergraduate
CBIT

GitHub: [@Parimal1004](https://github.com/Parimal1004)

---

## 📄 License

This project is intended for academic, educational, and experimental purposes.
