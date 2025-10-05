# Use the rocker/shiny image with R and Shiny pre-installed
FROM rocker/shiny:latest

# System libraries for httr/jsonlite/ggplot2 and for shiny
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages needed
RUN R -e "install.packages(c('shiny', 'httr', 'jsonlite', 'ggplot2'), repos='https://cran.rstudio.com/')"

# Copy app files into the image
COPY . /srv/shiny-server/

# Expose the port (Render uses $PORT env var, but expose default shiny port)
EXPOSE 3838

# Run shiny app on 0.0.0.0 and port from env variable PORT or default 3838
CMD R -e "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', 3838)))"
