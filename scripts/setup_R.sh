#!/bin/bash
# setup_R.sh

# Stop on error
set -e

# --- R Installation ---
# Update package lists
sudo apt-get update -qq

# Install dependencies for adding new repositories
sudo apt-get install -y --no-install-recommends software-properties-common dirmngr

# Add CRAN repository key
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc

# Add the CRAN repository for R
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"

# Install R base and essential development tools
# `r-base` includes R itself
# `r-base-dev` is needed for compiling R packages from source
sudo apt-get update
sudo apt-get install -y --no-install-recommends r-base r-base-dev

# Verify R installation
echo "Verifying R installation..."
R --version

# --- renv setup ---
# Install renv, the dependency management tool
# This allows for reproducible R environments
echo "Installing renv..."
R -e "install.packages('renv', repos = 'https://cloud.r-project.org/')"

# Optional: You could also have a step to run `renv::restore()`
# if a `renv.lock` file is present in the project.
# Example:
# if [ -f "renv.lock" ]; then
#   echo "Restoring R environment from renv.lock..."
#   R -e "renv::restore()"
# fi

echo "Script finished. R and renv are now set up."
