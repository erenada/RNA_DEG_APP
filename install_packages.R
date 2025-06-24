# Package Installation Script for RNA-seq DEG Analysis App
# Author: Eren Ada, PhD
# Date: 06/06/2025

cat("Installing required packages for RNA-seq DEG Analysis App...\n")

# Check if BiocManager is installed
if (!require("BiocManager", quietly = TRUE)) {
  cat("Installing BiocManager...\n")
  install.packages("BiocManager")
}

# Essential packages (Sprint 1)
essential_cran <- c(
  "shiny",
  "shinythemes", 
  "shinyWidgets",
  "DT",
  "dplyr",
  "tidyr", 
  "readr",
  "ggplot2",
  "plotly"
)

essential_bioc <- c(
  "DESeq2",
  "SummarizedExperiment"
)

# Enhanced packages (Sprint 3-4)
enhanced_cran <- c(
  "shinycssloaders",
  "shinyalert",
  "openxlsx",
  "pheatmap",
  "RColorBrewer"
)

# Advanced packages (Sprint 5-6)
advanced_cran <- c(
  "parallel",
  "future",
  "promises",
  "matrixStats",
  "jsonlite",
  "rmarkdown"
)

advanced_bioc <- c(
  "IHW"
)

# Function to install CRAN packages
install_cran_packages <- function(packages, description) {
  cat(paste0("\n", description, ":\n"))
  
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat(paste0("  Installing ", pkg, "...\n"))
      tryCatch({
        install.packages(pkg, dependencies = TRUE)
        cat(paste0("  ✓ ", pkg, " installed successfully\n"))
      }, error = function(e) {
        cat(paste0("  ✗ Failed to install ", pkg, ": ", e$message, "\n"))
      })
    } else {
      cat(paste0("  ✓ ", pkg, " already installed\n"))
    }
  }
}

# Function to install Bioconductor packages
install_bioc_packages <- function(packages, description) {
  cat(paste0("\n", description, ":\n"))
  
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat(paste0("  Installing ", pkg, "...\n"))
      tryCatch({
        BiocManager::install(pkg, update = FALSE)
        cat(paste0("  ✓ ", pkg, " installed successfully\n"))
      }, error = function(e) {
        cat(paste0("  ✗ Failed to install ", pkg, ": ", e$message, "\n"))
      })
    } else {
      cat(paste0("  ✓ ", pkg, " already installed\n"))
    }
  }
}

# Install packages by category
cat("\n", paste(rep("=", 50), collapse = ""), "\n")
cat("ESSENTIAL PACKAGES (Required for Sprint 1)\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

install_cran_packages(essential_cran, "Essential CRAN packages")
install_bioc_packages(essential_bioc, "Essential Bioconductor packages")

# Ask user if they want to install additional packages
response <- readline(prompt = "\nInstall enhanced packages for future features? (y/n): ")
if (tolower(response) %in% c("y", "yes")) {
  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("ENHANCED PACKAGES (For Sprint 3-4)\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  
  install_cran_packages(enhanced_cran, "Enhanced CRAN packages")
  
  # Ask about advanced packages
  response2 <- readline(prompt = "\nInstall advanced packages for optimization? (y/n): ")
  if (tolower(response2) %in% c("y", "yes")) {
    cat("\n", paste(rep("=", 50), collapse = ""), "\n")
    cat("ADVANCED PACKAGES (For Sprint 5-6)\n")
    cat(paste(rep("=", 50), collapse = ""), "\n")
    
    install_cran_packages(advanced_cran, "Advanced CRAN packages")
    install_bioc_packages(advanced_bioc, "Advanced Bioconductor packages")
  }
}

# Final summary
cat("\n", paste(rep("=", 50), collapse = ""), "\n")
cat("INSTALLATION COMPLETE\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

cat("\nTo run the app:\n")
cat("1. Set working directory: setwd('/path/to/RNA_DEG_APP')\n")
cat("2. Run the app: shiny::runApp()\n")

cat("\nOr simply run: shiny::runApp('.')\n")

# Check R version
r_version <- R.Version()
cat(paste0("\nR version: ", r_version$major, ".", r_version$minor, "\n"))
if (as.numeric(paste0(r_version$major, ".", r_version$minor)) < 4.1) {
  cat("⚠️  WARNING: R version 4.1.0 or higher is recommended\n")
}

cat("\n🎉 Setup complete! You can now run the DEG analysis app.\n") 