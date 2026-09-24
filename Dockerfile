FROM rocker/shiny:4.6.1

WORKDIR /app

# System libraries required by the dashboard packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libgit2-dev \
    libicu-dev \
    libglpk-dev \
    make \
    g++ \
    && rm -rf /var/lib/apt/lists/*

# Install dashboard-specific packages not guaranteed by the base image
RUN R -e "options(repos=c(CRAN='https://cloud.r-project.org')); install.packages(c('shinydashboard','plotly','DT','randomForest'), dependencies=TRUE)"

# Verify the required packages are actually available
RUN R -e "stopifnot(requireNamespace('shiny', quietly=TRUE)); stopifnot(requireNamespace('shinydashboard', quietly=TRUE)); stopifnot(requireNamespace('plotly', quietly=TRUE)); stopifnot(requireNamespace('DT', quietly=TRUE)); stopifnot(requireNamespace('randomForest', quietly=TRUE))"

# Copy application files
COPY app.R /app/app.R
COPY data /app/data
COPY models /app/models

ENV PORT=10000

EXPOSE 10000

CMD ["sh", "-c", "R -e \"options(shiny.autoload.r=FALSE); shiny::runApp('/app', host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '10000')))\""]