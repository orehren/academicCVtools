#!/bin/bash
# install-rstudio.sh

# Stop on error
set -e

# Function to check for command existence
command_exists() {
  command -v "$1" >/dev/null 2>&1
}

# --- RStudio ---
# Default version if not specified
RSTUDIO_VERSION=${1:-"2023.12.1+402"} # Example: "2023.09.1+494"
DEB_FILE="rstudio-${RSTUDIO_VERSION}-amd64.deb"
DOWNLOAD_URL="https://download1.rstudio.org/electron/jammy/amd64/${DEB_FILE}"

# Check if RStudio is already installed and at the correct version
# Note: rstudio --version is not a standard command. Checking existence is more reliable.
if command_exists rstudio; then
  echo "RStudio is already installed. Skipping re-installation."
  # A more advanced script could check the installed version, but this is often sufficient.
  # For example: dpkg-query -W -f='${Version}' rstudio
  exit 0
fi

echo "RStudio not found. Installing RStudio version ${RSTUDIO_VERSION}..."

# Clean up previous downloads if they exist
rm -f rstudio-*-amd64.deb

# --- Download ---
echo "Downloading RStudio from ${DOWNLOAD_URL}..."
curl -sSL -o "${DEB_FILE}" "${DOWNLOAD_URL}"
if [ $? -ne 0 ]; then
  echo "Failed to download RStudio. Please check the URL and your connection."
  exit 1
fi

# --- Install Dependencies ---
# RStudio has several dependencies. Gdebi is good at handling these, but if not
# available, we can use apt. `apt-get install -f` will also be used later.
echo "Updating package lists and installing dependencies..."
sudo apt-get update
sudo apt-get install -y libjpeg-dev libpng-dev libtiff-dev libssl-dev libcurl4-openssl-dev libxml2-dev

# --- Install RStudio ---
echo "Installing ${DEB_FILE}..."
# Use gdebi if available, as it handles dependencies automatically
if command_exists gdebi; then
  sudo gdebi -n "${DEB_FILE}"
else
  # Fallback to dpkg and then fix dependencies
  sudo dpkg -i "${DEB_FILE}" || sudo apt-get install -f -y
  # If dpkg failed, the above command should fix it. Retry for certainty.
  if ! dpkg -l | grep -q rstudio; then
      sudo dpkg -i "${DEB_FILE}"
  fi
fi
if [ $? -ne 0 ]; then
  echo "RStudio installation failed."
  exit 1
fi

# --- Cleanup ---
rm -f "${DEB_FILE}"

# --- Verify ---
echo "Verifying RStudio installation..."
if command_exists rstudio; then
  echo "RStudio installed successfully."
else
  echo "RStudio installation could not be verified."
  exit 1
fi

echo "Script finished."
