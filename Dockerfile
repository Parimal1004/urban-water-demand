FROM rocker/r-ver:4.6.1

WORKDIR /app

# System libraries required by common CRAN/Shiny packages
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

# Install only the packages required by the dashboard
RUN R -e "options(repos=c(CRAN='https://cloud.r-project.org')); install.packages(c('shiny','shinydashboard','plotly','DT','randomForest'), dependencies=TRUE)"

# Copy the application
COPY app.R /app/app.R
COPY data /app/data
COPY models /app/models

# Vercel supplies PORT at runtime
ENV PORT=10000
EXPOSE 10000

CMD ["sh", "-c", "R -e \"options(shiny.autoload.r=FALSE); shiny::runApp('/app', host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '10000')))\""]
