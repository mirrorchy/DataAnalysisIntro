# ===================================================
# Package Installation Script for DataAnalysisIntro
# Code Dependencies
# ===================================================
# 
# This script installs all required R packages used in the 
# Code directory of the DataAnalysisIntro project.
# 
# Run this script before executing any of the Lec2 R files
# to ensure all dependencies are properly installed.
#
# Created: 2025-09-23
# ===================================================

cat("===================================================\n")
cat("Installing R packages for DataAnalysisIntro\n")
cat("===================================================\n\n")

# Function to check and install packages
install_if_missing <- function(package_name) {
  if (!require(package_name, character.only = TRUE, quietly = TRUE)) {
    cat(paste("Installing", package_name, "...\n"))
    install.packages(package_name, dependencies = TRUE)
    library(package_name, character.only = TRUE)
    cat(paste("✓", package_name, "installed successfully\n\n"))
  } else {
    cat(paste("✓", package_name, "already installed\n"))
  }
}

# List of all required packages identified from Code/Lec2/*.r files
required_packages <- c(
  # Data manipulation and processing
  "dplyr",        # Used in: PM25_Beijing_episode.r, PM25_Beijing_PM25_density.r
  "tidyr",        # Used in: PM25_Beijing_episode.r
  "magrittr",     # Used in: Jellybeans.r, Shipman_scatter_hist.r
  
  # Visualization packages
  "ggplot2",      # Used in: Jellybeans.r, PM25_Beijing_episode.r, Shipman_times.r, Shipman_scatter_hist.r
  "ggpubr",       # Used in: Jellybeans.r, Shipman_scatter_hist.r
  "ggExtra"       # Used in: Shipman_scatter_hist.r
)

cat("Installing required packages:\n")
cat("- dplyr: Data manipulation and transformation\n")
cat("- tidyr: Data tidying and reshaping\n") 
cat("- magrittr: Pipe operators and enhanced syntax\n")
cat("- ggplot2: Grammar of graphics for data visualization\n")
cat("- ggpubr: Publication-ready plots and statistical visualization\n")
cat("- ggExtra: Additional ggplot2 extensions (marginal plots, etc.)\n\n")

# Install all required packages
for (pkg in required_packages) {
  install_if_missing(pkg)
}

cat("\n===================================================\n")
cat("Package installation completed!\n")
cat("===================================================\n\n")

# Additional notes about the code files and their purposes
cat("Code files in Lec2 and their purposes:\n")
cat("- babyweight.r: Birth weight distribution analysis (base R only)\n")
cat("- babyweight_overall.r: Overall birth weight statistics (base R only)\n")
cat("- Jellybeans.r: Jellybean guess analysis with ggplot2 visualizations\n")
cat("- PM25_Beijing_episode.r: Beijing PM2.5 pollution episode analysis\n")
cat("- PM25_Beijing_PM25_cmp.r: PM2.5 comparison with weather variables (with kernel regression)\n")
cat("- PM25_Beijing_PM25_density.r: PM2.5 density distribution analysis\n")
cat("- PM25_Beijing_weather_cmp.r: Weather variable comparisons (base R only)\n")
cat("- Shipman_times.r: Shipman case time series analysis\n")
cat("- Shipman_scatter_hist.r: Shipman case scatter plot with marginal histograms\n\n")

cat("Note: Some files use only base R functions and require no additional packages.\n")
cat("The kernel regression in PM25_Beijing_PM25_cmp.r uses the built-in loess() function.\n\n")

cat("All packages are now ready for use!\n")
