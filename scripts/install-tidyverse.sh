#!/bin/bash
# install-tidyverse.sh

# Stop on error
set -e

# --- Tidyverse Dependencies ---
# Install system dependencies required for building Tidyverse packages from source
# This is crucial for environments where binary packages are not available or are outdated.
echo "Updating package lists..."
sudo apt-get update

echo "Installing system dependencies for Tidyverse..."
# Common dependencies for packages like `readxl`, `dplyr`, `sf`, `xml2`, `curl`, etc.
sudo apt-get install -y \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libfontconfig1-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  libfreetype6-dev \
  libpng-dev \
  libtiff5-dev \
  libjpeg-dev \
  libudunits2-dev \
  libgdal-dev \
  libgeos-dev \
  libproj-dev \
  libcairo2-dev \
  libpq-dev # For RPostgres, often used with Tidyverse

# --- R packages ---
# Install or update the Tidyverse packages using R
# The `repos` option points to a reliable CRAN mirror.
echo "Installing/updating Tidyverse R packages..."
R -e "install.packages('tidyverse', repos = 'https://cloud.r-project.org/')"

# Optional: verify installation by listing some Tidyverse packages
# This is a simple check to see if the installation command ran.
echo "Verifying installation (listing installed dplyr version)..."
R -e "if ('tidyverse' %in% installed.packages()) { print(paste('Tidyverse version:', packageVersion('tidyverse'))) } else { stop('Tidyverse installation failed.') }"
R -e "if ('dplyr' %in% installed.packages()) { print(paste('dplyr version:', packageVersion('dplyr'))) } else { stop('dplyr (part of Tidyverse) not found.') }"


echo "Script finished. Tidyverse and its dependencies should be installed."
