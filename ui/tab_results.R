# Results Tab UI Builder
# Author: Eren Ada, PhD
#
# Returns the Results tabPanel

build_results_tab <- function() {
  tabPanel(
    title = "Results",
    value = "tab_results",
    icon = icon("chart-line"),
    
    br(),
    
    # Conditional display based on whether analysis has been run
    conditionalPanel(
      condition = "!output.results_available",
      div(
        style = "text-align: center; margin-top: 50px;",
        icon("info-circle", class = "fa-3x", style = "color: #3498DB;"),
        h3("No Analysis Results Yet", style = "color: #2C3E50; margin-top: 20px;"),
        p("Run the DESeq2 analysis from the Configuration tab to view results here.", 
          style = "color: #7F8C8D; font-size: 14px;")
      )
    ),
    
    # Results display when analysis is complete
    conditionalPanel(
      condition = "output.results_available",
      fluidRow(
        column(12,
          div(
            style = "text-align: center; margin-bottom: 20px;",
            h2("Differential Expression Results", style = "color: #2C3E50;"),
            p("Explore gene-level statistics for each contrast. Tables are sortable and searchable.", 
              style = "color: #7F8C8D; font-size: 14px;")
          ),
          hr()
        )
      ),
      
      # Dynamic tabbed interface for contrasts
      fluidRow(
        column(12,
          uiOutput("results_tabs_ui")
        )
      )
    )
  )
}

