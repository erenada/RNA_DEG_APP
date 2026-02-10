# Global configuration for RNA-seq DEG Analysis App
# Author: Eren Ada, PhD
# This file is automatically loaded by Shiny before ui.R and server.R

# Configure repositories to prefer Bioconductor when available
suppressWarnings({
  if (requireNamespace("BiocManager", quietly = TRUE)) {
    options(repos = BiocManager::repositories())
  }
})

# Set maximum upload size to 300 MB (default is 5 MB)
options(shiny.maxRequestSize = 300 * 1024^2)

# Load required packages
required_packages <- c(
  "shiny", "shinythemes", "shinyWidgets", "shinyjs", "shinycssloaders", "DT", 
  "dplyr", "tidyr", "readr", "ggplot2", "plotly", "tools", "openxlsx",
  "digest"  # For enrichment module cache keys
)

bioc_packages <- c(
  "DESeq2", "SummarizedExperiment",
  "clusterProfiler", "enrichplot"  # For enrichment analysis
  # Note: org.Mm.eg.db and org.Hs.eg.db are loaded conditionally based on user selection
)

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

if (file.exists("R/utils_enrichment.R")) {
  source("R/utils_enrichment.R")
}

if (file.exists("R/utils_visualization.R")) {
  source("R/utils_visualization.R")
}

# Utility function to load organism database conditionally
# Note: This function is called when user selects organism in Tab 1
get_orgdb <- function(organism) {
  if (organism == "Mouse") {
    pkg_name <- "org.Mm.eg.db"
    if (!require(pkg_name, character.only = TRUE, quietly = TRUE)) {
      stop(paste0("Mouse genome annotation package 'org.Mm.eg.db' is not installed.\n",
                  "Please run: BiocManager::install('org.Mm.eg.db')"))
    }
    return(org.Mm.eg.db::org.Mm.eg.db)
  } else if (organism == "Human") {
    pkg_name <- "org.Hs.eg.db"
    if (!require(pkg_name, character.only = TRUE, quietly = TRUE)) {
      stop(paste0("Human genome annotation package 'org.Hs.eg.db' is not installed.\n",
                  "Please run: BiocManager::install('org.Hs.eg.db')"))
    }
    return(org.Hs.eg.db::org.Hs.eg.db)
  } else {
    stop(paste0("Unsupported organism: ", organism, ". Please select 'Mouse' or 'Human'."))
  }
}

cat("✓ All packages and utilities loaded successfully!\n")
cat("✓ Ready to run the DEG analysis app!\n") 