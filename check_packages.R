# Package Checker for RNA-seq DEG Analysis App
# Author: Eren Ada, PhD
# Run this script first to check if all required packages are installed

cat("Checking required packages for RNA-seq DEG Analysis App...\n\n")

# Required packages
required_packages <- c(
  "shiny", "shinythemes", "shinyWidgets", "DT", 
  "dplyr", "tidyr", "readr", "ggplot2", "plotly", "tools"
)

bioc_packages <- c("DESeq2", "SummarizedExperiment")

# Check CRAN packages
cat("CRAN Packages:\n")
cat(paste(rep("=", 40), collapse = ""), "\n")

missing_cran <- c()
for (pkg in required_packages) {
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("✓ %-15s %s\n", pkg, "installed"))
  } else {
    cat(sprintf("✗ %-15s %s\n", pkg, "MISSING"))
    missing_cran <- c(missing_cran, pkg)
  }
}

# Check Bioconductor packages
cat("\nBioconductor Packages:\n")
cat(paste(rep("=", 40), collapse = ""), "\n")

missing_bioc <- c()
for (pkg in bioc_packages) {
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("✓ %-20s %s\n", pkg, "installed"))
  } else {
    cat(sprintf("✗ %-20s %s\n", pkg, "MISSING"))
    missing_bioc <- c(missing_bioc, pkg)
  }
}

# Summary
cat("\n", paste(rep("=", 50), collapse = ""), "\n")
cat("SUMMARY\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

if (length(missing_cran) == 0 && length(missing_bioc) == 0) {
  cat("🎉 All packages are installed! You can run the app with:\n")
  cat("   shiny::runApp()\n\n")
} else {
  cat("❌ Missing packages detected. To install:\n\n")
  
  if (length(missing_cran) > 0) {
    cat("CRAN packages:\n")
    cat("install.packages(c(", paste0('"', missing_cran, '"', collapse = ", "), "))\n\n")
  }
  
  if (length(missing_bioc) > 0) {
    cat("Bioconductor packages:\n")
    cat("if (!require('BiocManager', quietly = TRUE)) install.packages('BiocManager')\n")
    cat("BiocManager::install(c(", paste0('"', missing_bioc, '"', collapse = ", "), "))\n\n")
  }
  
  cat("Or run the automated installer:\n")
  cat("source('install_packages.R')\n\n")
}

# R version check
r_version <- R.Version()
version_string <- paste0(r_version$major, ".", r_version$minor)
current_version <- as.numeric(version_string)
cat("R Version: ", version_string, "\n")
if (!is.na(current_version) && current_version < 4.1) {
  cat("⚠️  WARNING: R version 4.1.0 or higher is recommended\n")
} else {
  cat("✓ R version meets requirements\n")
} 