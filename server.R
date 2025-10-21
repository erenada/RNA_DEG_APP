# Server for RNA-seq DEG Analysis App
# Author: Eren Ada, PhD
#
# All packages and utilities are loaded in global.R
# Server logic is organized into separate files for each tab

server <- function(input, output, session) {
  
  # =============================================================================
  # REACTIVE VALUES - Shared across all tabs
  # =============================================================================
  
  values <- reactiveValues(
    count_data = NULL,
    meta_data = NULL,
    validation_status = NULL,
    design_factors = NULL,
    design_formula = NULL,
    reference_levels = list(),
    contrasts = list(),
    analysis_completed = FALSE,
    # Tab 2 variables
    group1_samples = character(0),
    group2_samples = character(0),
    available_samples = character(0),
    created_contrasts = list(),
    filtered_samples = character(0),
    selected_filters = list(),
    filter_trigger = 0,
    # Project settings (Tab 1)
    organism = "Mouse",  # Default to Mouse
    id_type = "auto",    # Default to auto-detect
    orgdb = NULL,        # Will be loaded when organism is selected
    # Enrichment module (Tab 5)
    enrichment_results = list(),  # Cache for enrichment results
    # Volcano module (Tab 6)
    volcano_cache = list(),  # Cache for volcano plots
    uploaded_volcano_results = NULL  # Uploaded DEG results for volcano plots
  )
  
  # =============================================================================
  # LOAD TAB-SPECIFIC SERVER LOGIC
  # =============================================================================
  
  # Tab 1: Data Input & Validation
  source("server_modules/server_tab1_data_input.R", local = TRUE)
  
  # Tab 2: Experimental Design
  source("server_modules/server_tab2_experimental_design.R", local = TRUE)
  
  # Tab 3: Analysis Configuration
  source("server_modules/server_tab3_analysis_config.R", local = TRUE)
  
  # Tab 4: Results Display
  source("server_modules/server_tab4_results_display.R", local = TRUE)
  
  # Tab 5: Enrichment Analysis
  source("server_modules/server_tab5_enrichment.R", local = TRUE)
  
  # Tab 6: Volcano Plot Visualization
  source("server_modules/server_tab6_volcano.R", local = TRUE)
  
  # Tab 7: Documentation
  source("server_modules/server_tab7_documentation.R", local = TRUE)
  
}
