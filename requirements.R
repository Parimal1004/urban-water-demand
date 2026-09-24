# =============================================================================
# requirements.R
# Urban Water Demand Prediction Using R
#
# Purpose:
#   Install the CRAN packages needed for this project.
#   Run this file once before building the rest of the pipeline.
#
# How to run (PowerShell, from the project folder):
#   & "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" requirements.R
#
# Notes:
#   - Uses CRAN-compatible packages for R 4.6.1
#   - CPU only (no GPU / CUDA packages)
#   - Windows-friendly; no Linux-only dependencies
# =============================================================================

# Use the default CRAN mirror if none is set
if (is.null(getOption("repos")) || getOption("repos")["CRAN"] == "@CRAN@") {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

# Packages used in this project:
#   tidyverse, dplyr, tidyr, ggplot2, lubridate  -> data wrangling and plots
#   caret, randomForest, Metrics                 -> training and evaluation
#   shiny, shinydashboard, plotly, DT            -> dashboard (app.R)
required_packages <- c(
  "tidyverse",
  "dplyr",
  "tidyr",
  "ggplot2",
  "lubridate",
  "caret",
  "randomForest",
  "Metrics",
  "shiny",
  "shinydashboard",
  "plotly",
  "DT"
)

# Install a package only if it is not already installed
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing: ", pkg)
    install.packages(pkg, dependencies = TRUE)
  } else {
    message("Already installed: ", pkg)
  }
}

# Loop through the list and install missing packages
for (pkg in required_packages) {
  install_if_missing(pkg)
}

message("Package setup is complete.")
message("Launch the dashboard from the project root with:")
message("& \"C:\\Program Files\\R\\R-4.6.1\\bin\\Rscript.exe\" -e \"options(shiny.autoload.r=FALSE); shiny::runApp('.')\"")
