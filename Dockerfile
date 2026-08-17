# Use a modern, general-purpose R base image
FROM rocker/r-ver:4.4.1

# Install system dependencies required by some of your R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libglpk-dev \
    && rm -rf /var/lib/apt/lists/*

# Install the 'remotes' package, which can install local packages and their dependencies
RUN R -e "install.packages('remotes', repos = 'https://cran.rstudio.com/')"

# Copy your local package source code into the container
COPY . /usr/src/pbat_src

# Install all dependencies from the DESCRIPTION file, then install the package itself
# from the local source code we just copied into the image.
RUN R -e "remotes::install_local('/usr/src/pbat_src', dependencies = TRUE)"

# Expose the port that Cloud Run will listen on
EXPOSE 8080

# The command to run when the container starts
CMD ["R", "-e", "shiny::runApp(PbAT::run_app(), host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT')))"]