# UI for RNA-seq DEG Analysis App
# Author: Eren Ada, PhD

# All packages and utilities are loaded in global.R

# Source UI tab builders
source("ui/tab_input.R", local = TRUE)
source("ui/tab_design.R", local = TRUE)
source("ui/tab_config.R", local = TRUE)
source("ui/tab_results.R", local = TRUE)
source("ui/tab_enrichment.R", local = TRUE)
source("ui/tab_volcano.R", local = TRUE)
source("ui/tab_documentation.R", local = TRUE)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel(
    div(
      h1("RNA-seq Differential Gene Expression Analysis", 
         style = "color: #2C3E50; margin-bottom: 10px; text-align: center;"),
      div(
        style = "text-align: center; margin-bottom: 15px;",
        p(strong("Author: "), "Eren Ada, PhD", 
          style = "color: #34495E; margin-bottom: 2px; font-size: 14px;"),
        p("Harvard Medical School, Department of Immunology", 
          style = "color: #7F8C8D; margin-bottom: 2px; font-size: 12px;"),
        p(tags$a("GitHub: @erenada", 
                 href = "https://github.com/erenada", 
                 target = "_blank",
                 style = "color: #3498DB; text-decoration: none;"), 
          style = "margin-bottom: 0px; font-size: 12px;")
      )
    )
  ),
  
  # Add custom CSS and shinyjs
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    # Add jQuery UI for drag and drop
    tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.min.js"),
    tags$link(rel = "stylesheet", href = "https://code.jquery.com/ui/1.12.1/themes/ui-lightness/jquery-ui.css"),
    # Initialize Bootstrap popovers (tap/click; dismiss on blur)
    tags$script(HTML("$(function(){ $('body').popover({ selector: '[data-toggle=popover]', html: true, trigger: 'focus', container: 'body' }); });")),
    # Custom JavaScript files
    tags$script(src = "js/analysis_log.js"),
    tags$script(src = "js/design_drag_drop.js"),
    tags$script(src = "js/enrichment_controls.js"),
    tags$script(src = "js/volcano_controls.js")
  ),
  
  # Main tabset panel
  tabsetPanel(
    id = "main_tabs",
    type = "tabs",
    
    # Tab 1: Data Input
    build_input_tab(),
    
    # Tab 2: Experimental Design
    build_design_tab(),
    
    # Tab 3: Analysis Configuration (initially disabled)
    build_config_tab(),
    
    # Tab 4: Results
    build_results_tab(),
    
    # Tab 5: Enrichment Analysis
    build_enrichment_tab(),
    
    # Tab 6: Volcano Plot Visualization
    build_volcano_tab(),
    
    # Tab 7: Documentation
    build_documentation_tab()
  ),  # Close tabsetPanel
  
  # Footer
  hr(),
  fluidRow(
    column(12,
      p("RNA-seq DEG Analysis App | Built with Shiny and DESeq2",
        style = "text-align: center; color: #7F8C8D; font-size: 12px;")
    )
  )
) 
