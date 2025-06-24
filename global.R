# Global configuration for RNA-seq DEG Analysis App
# Author: Eren Ada, PhD
# This file is automatically loaded by Shiny before ui.R and server.R

# Load required packages
required_packages <- c(
  "shiny", "shinythemes", "shinyWidgets", "shinyjs", "DT", 
  "dplyr", "tidyr", "readr", "ggplot2", "plotly", "tools"
)

bioc_packages <- c("DESeq2", "SummarizedExperiment")

# Function to safely load packages
load_package <- function(pkg, is_bioc = FALSE) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    if (is_bioc) {
      stop(paste0("Bioconductor package '", pkg, "' is not installed.\n",
                  "Please run: BiocManager::install('", pkg, "')"))
    } else {
      stop(paste0("Package '", pkg, "' is not installed.\n",
                  "Please run: install.packages('", pkg, "')"))
    }
  }
}

# Load CRAN packages
cat("Loading required packages...\n")
suppressPackageStartupMessages({
  for (pkg in required_packages) {
    load_package(pkg)
  }
  
  for (pkg in bioc_packages) {
    load_package(pkg, is_bioc = TRUE)
  }
})

# Source utility functions
if (file.exists("R/utils_validation.R")) {
  source("R/utils_validation.R")
} else {
  stop("Could not find R/utils_validation.R file. Please ensure the R/ directory exists with utility functions.")
}

if (file.exists("R/utils_deseq2.R")) {
  source("R/utils_deseq2.R")
}

cat("✓ All packages and utilities loaded successfully!\n")
cat("✓ Ready to run the DEG analysis app!\n") 