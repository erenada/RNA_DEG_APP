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

# Create and return the application object
shinyApp(ui = ui, server = server) 