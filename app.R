# RNA-seq Differential Gene Expression Analysis App
# Author: Eren Ada, PhD
# Date: 06/06/2025

# Clear environment to avoid conflicts
rm(list = ls())

# Load global configuration and packages
source("global.R")

# Source UI and Server components
source("ui.R")
source("server.R")

# Create and run the application explicitly
deg_app <- shinyApp(ui = ui, server = server)

# Run the app when this file is sourced
if (interactive()) {
  runApp(deg_app)
} 