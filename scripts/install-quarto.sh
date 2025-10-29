#!/bin/bash
# install-quarto.sh

# Stop on error
set -e

# Function to check for command existence
command_exists() {
  command -v "$1" >/dev/null 2>&1
}

# Default Quarto version if not provided
QUARTO_VERSION=${1:-"1.4.553"} # Default to a recent, stable version

# Architecture detection
ARCH=$(uname -m)
case "$ARCH" in
  x86_64)
    QUARTO_ARCH="amd64"
    ;;
  aarch64 | arm64)
    QUARTO_ARCH="arm64"
    ;;
  *)
    echo "Unsupported architecture: $ARCH"
    exit 1
    ;;
esac

# Construct download URL
QUARTO_URL="https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-${QUARTO_ARCH}.deb"
DEB_FILE="quarto-${QUARTO_VERSION}-linux-${QUARTO_ARCH}.deb"

# Check if Quarto is already installed and at the correct version
if command_exists quarto && [[ $(quarto --version) == "${QUARTO_VERSION}" ]]; then
  echo "Quarto version ${QUARTO_VERSION} is already installed."
  exit 0
fi

echo "Quarto not found or wrong version. Installing/updating Quarto ${QUARTO_VERSION}..."

# Clean up previous downloads if they exist
rm -f quarto-*.deb

# Download the specified version of Quarto
echo "Downloading Quarto from ${QUARTO_URL}..."
curl -sSL -o "${DEB_FILE}" "${QUARTO_URL}"
if [ $? -ne 0 ]; then
  echo "Failed to download Quarto. Please check the URL and your connection."
  exit 1
fi

# Install the downloaded .deb file
echo "Installing ${DEB_FILE}..."
sudo dpkg -i "${DEB_FILE}"
if [ $? -ne 0 ]; then
  echo "Failed to install Quarto. Running apt-get install -f to fix potential dependencies..."
  # Attempt to fix broken dependencies, which is a common issue with dpkg
  sudo apt-get install -f -y
  # Retry installation
  sudo dpkg -i "${DEB_FILE}"
  if [ $? -ne 0 ]; then
    echo "Quarto installation failed even after attempting to fix dependencies."
    exit 1
  fi
fi

# Clean up the downloaded file
rm -f "${DEB_FILE}"

# Verify installation
echo "Verifying Quarto installation..."
if command_exists quarto; then
  INSTALLED_VERSION=$(quarto --version)
  echo "Quarto version ${INSTALLED_VERSION} installed successfully."
  # Optional: check if installed version matches requested version
  if [[ "$INSTALLED_VERSION" != "$QUARTO_VERSION" ]]; then
    echo "Warning: Installed version ($INSTALLED_VERSION) does not match requested version ($QUARTO_VERSION)."
  fi
else
  echo "Quarto installation could not be verified."
  exit 1
fi

echo "Script finished."
