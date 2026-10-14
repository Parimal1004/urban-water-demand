# AquaSense: Explainable Urban Water Demand Forecasting

AquaSense is a machine-learning system that forecasts urban water demand and presents the results in an interactive dashboard. It is built in R using Random Forest and Shiny.

Live dashboard: https://urban-water-demand.onrender.com/

## Overview

Water demand changes with time of day, weather, population, infrastructure, and holidays. AquaSense brings these factors together in one forecasting pipeline and lets users explore predictions, compare zones, and test what-if scenarios.

## Dashboard Modules

- **Forecast Demand:** Choose a zone and enter conditions such as date and time, weather, supply hours, storage capacity, pipeline age, and recent demand values. The app returns a predicted demand in m³/h.
- **Demand Insights:** Historical demand patterns across zones.
- **What-If Analysis:** Change weather or operational inputs and see how the forecast responds.
- **Zone Comparison:** Compare demand behaviour and model metrics for two zones.
- **Prediction History:** Predictions made during the current session.
- **My Report:** A summary of the selected inputs and the resulting forecast.

## Approach

1. Clean and understand the raw SOWEKI water-demand data.
2. Engineer features: time, weather, infrastructure, locality context, lag values, and rolling statistics.
3. Split the data into training and test sets.
4. Train Linear Regression and Random Forest models and compare them.
5. Analyse errors and feature importance.
6. Serve the chosen model through a Shiny dashboard.

Hour, day of week, and month are encoded cyclically so the model can recognise repeating patterns. Lag features (`lag_1`, `lag_4`, `lag_96`, `lag_672`) capture demand at earlier time steps.

## Results

| Model             |    MAE |   RMSE |     R² |
| ----------------- | -----: | -----: | -----: |
| Linear Regression | 0.1007 | 0.1834 | 0.9057 |
| Random Forest     | 0.0994 | 0.1807 | 0.9084 |

Random Forest performed slightly better and was used in the dashboard. The trained model is saved at `models/enriched_random_forest_model.rds`.

## Explainability

The project goes beyond producing a single number. It includes feature importance, prediction error analysis, per-zone performance, a review of the largest errors, and what-if scenarios. Reports and tables are in `outputs/`, and plots are in `plots/`.

## Data

The data covers December 2023 to December 2025 at roughly 15-minute intervals across five zones (Zone_A to Zone_E), with about 70,000 observations per zone.

To stay within GitHub file-size limits, the deployed app uses a compact version of the dataset, `data/enriched_deployment.csv` (43,665 rows, 51 columns, about 16.65 MB).

**Note on synthetic features:** Some locality-level variables, such as demographics, infrastructure, and land use, are synthetically generated for experimentation. They show how contextual data can be added to a forecasting system and should not be treated as real measurements for any locality. The dashboard labels these variables as synthetic.

## Project Structure

```text
urban-water-demand/
├── app.R                  # Shiny application
├── R/pipeline.R           # Data processing and modelling pipeline
├── requirements.R         # R dependencies
├── Dockerfile
├── data/                  # Deployment dataset and locality context
├── models/                # Trained Random Forest model
├── plots/                 # Generated plots
└── outputs/               # Metrics, reports, and analysis tables
```

## Technology Stack

R, Shiny, shinydashboard, Plotly, DT, ggplot2, dplyr, tidyr, caret, Metrics, Docker, Git

## Running Locally

Requirements: R 4.6.1 and Git (Docker is optional).

```bash
git clone https://github.com/Parimal1004/urban-water-demand.git
cd urban-water-demand
Rscript requirements.R          # install dependencies
Rscript R/pipeline.R            # optional: rerun the full pipeline
Rscript -e "options(shiny.autoload.r=FALSE); shiny::runApp('.', port=3850)"
```

Then open http://127.0.0.1:3850. The dashboard only needs `enriched_deployment.csv` and `locality_context.csv`.

## Docker

```bash
docker build -t aquasense .
docker run -p 10000:10000 -e PORT=10000 aquasense
```

The app listens on `0.0.0.0:$PORT`. The live version is deployed on Render from this repository using the Dockerfile.

## Future Work

- Live weather and IoT water-meter integration
- XGBoost / LightGBM comparison and more advanced time-series models
- SHAP-based local explanations
- Automated retraining and anomaly detection
- Real locality infrastructure data to replace the synthetic features

## Author

Parimal Goud, AI and Data Science undergraduate, CBIT
GitHub: [@Parimal1004](https://github.com/Parimal1004)

## License

Intended for academic and educational use.
