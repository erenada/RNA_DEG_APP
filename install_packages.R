# Package Installation Script for RNA-seq DEG Analysis App
# Author: Eren Ada, PhD
# Date: 12/02/2025

cat("Installing required packages for RNA-seq DEG Analysis App...\n\n")

# Install BiocManager if needed
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

# CRAN packages
cran_packages <- c(
  "shiny",
  "shinythemes",
  "shinyWidgets",
  "shinyjs",
  "shinycssloaders",
  "DT",
  "dplyr",
  "tidyr",
  "readr",
  "ggplot2",
  "plotly",
  "openxlsx",
  "digest",
  "colourpicker",
  "jsonlite",
  "shinyalert",
  "pheatmap",
  "RColorBrewer",
  "testthat"
)

# Bioconductor packages
bioc_packages <- c(
  "DESeq2",
  "SummarizedExperiment",
  "clusterProfiler",
  "enrichplot",
  "EnhancedVolcano",
  "apeglm",
  "org.Mm.eg.db",
  "org.Hs.eg.db"
)

# Install CRAN packages
cat("Installing CRAN packages...\n")
for (pkg in cran_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, dependencies = TRUE)
  } else {
    cat(pkg, "already installed\n")
  }
}

# Install Bioconductor packages
cat("\nInstalling Bioconductor packages...\n")
for (pkg in bioc_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    BiocManager::install(pkg, update = FALSE, ask = FALSE)
  } else {
    cat(pkg, "already installed\n")
  }
}

cat("\nInstallation complete.\n")
