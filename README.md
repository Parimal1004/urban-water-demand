# Urban Water Demand Prediction Using R

A beginner-friendly Data Science project that predicts **urban water demand in Million Litres per Day (MLD)** from historical demand and related environmental / time-based features.

This repository is being built **phase by phase**. Phase 1 only sets up folders, package requirements, and this README. Dataset generation, model training, and the Shiny dashboard come later.

## Project objective

Build a machine learning workflow in **R** that:

1. Uses historical urban water demand data plus simple environmental and calendar features.
2. Trains a CPU-only model (planned: Random Forest via `randomForest` and `caret`).
3. Reports prediction quality with standard regression metrics (for example RMSE, MAE, R-squared).
4. Later presents predictions and charts in a Shiny dashboard.

The target variable is **water demand in MLD**.

## Technology stack

| Item | Choice |
|------|--------|
| Language | R 4.6.1 |
| IDE | Cursor |
| OS | Windows 10/11 |
| Compute | CPU only (no GPU, no CUDA) |
| Shell | PowerShell |
| Not used | WSL, Ubuntu, Python for ML |

### Planned R packages (CRAN)

- **Data wrangling and dates:** `tidyverse`, `dplyr`, `tidyr`, `lubridate`
- **Plots:** `ggplot2`, `plotly`
- **Machine learning:** `caret`, `randomForest`, `Metrics`
- **Dashboard (later):** `shiny`, `shinydashboard`, `DT`

Install them with `requirements.R` (see below). `tidyverse` already includes `dplyr`, `tidyr`, `ggplot2`, and `lubridate`; they are listed separately so the viva explanation stays clear.

## Folder structure

```
urban-water-demand/
├── data/            # Datasets (not generated in Phase 1)
├── R/               # R scripts for cleaning, features, training
├── models/          # Saved model objects (later)
├── outputs/         # Plots, metrics tables, prediction files (later)
├── app.R            # Shiny app (later phase)
├── requirements.R   # Package installer
└── README.md        # This file
```

All scripts will use **relative paths** from the project folder (for example `file.path("data", "file.csv")`) so the project stays portable on Windows.

## Planned workflow (later phases)

1. **Setup (this phase):** folders + packages + README.
2. **Dataset:** create or load a historical water-demand table with date, demand (MLD), and environmental / calendar features.
3. **Explore and clean:** missing values, summaries, and simple plots.
4. **Feature engineering:** time features such as month, weekday, season, lags, and rolling averages if useful.
5. **Train / test split:** keep time order in mind (no random shuffle of the future into the past).
6. **Train models:** baseline plus Random Forest on CPU.
7. **Evaluate:** RMSE, MAE, and related metrics; save the best model under `models/`.
8. **Dashboard:** Shiny app (`app.R`) for interactive charts and predicted MLD.

## How to run Phase 1

Open PowerShell in this project folder (`urban-water-demand`).

`Rscript` may not be on your PATH. Use the R 4.6.1 executable:

```powershell
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" requirements.R
```

This installs any missing CRAN packages listed above. It does **not** create data, train models, or start Shiny.

## Viva talking points (short)

- **Problem:** cities need a forecast of daily water demand (MLD) for operations and planning.
- **Inputs:** past demand plus environment and calendar features (temperature, rainfall, day of week, season, and similar).
- **Method:** supervised regression in R; Random Forest is a strong, CPU-friendly first model.
- **Output:** predicted MLD and error metrics; later a dashboard for demonstration.

## Current status

**Phase 1 complete:** project folders, `requirements.R`, and this README.

**Not done yet:** dataset, training scripts, saved models, Shiny dashboard.
