# Documentation Tab UI Builder
# Author: Eren Ada, PhD
#
# Returns the Documentation tabPanel

build_documentation_tab <- function() {
  tabPanel(
    title = "Documentation",
    value = "tab_documentation",
    icon = icon("book"),
    
    br(),
    
    fluidRow(
      column(12,
        div(
          style = "text-align: center; margin-bottom: 30px;",
          h2("Documentation & Help", style = "color: #2C3E50;"),
          p("Comprehensive guides and references for using the RNA-seq DEG Analysis App", 
            style = "color: #7F8C8D; font-size: 14px;")
        )
      )
    ),
    
    fluidRow(
      # Quick Start Section
      column(6,
        wellPanel(
          style = "background-color: #f8f9fa; border-left: 4px solid #27AE60;",
          h4(icon("rocket"), " Quick Start", style = "color: #27AE60; margin-top: 0;"),
          p("New to the app? Start here for a 5-minute walkthrough.", 
            style = "color: #666; margin-bottom: 15px;"),
          
          div(
            style = "margin-bottom: 10px;",
            tags$a(
              href = "https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/quick_start_guide.md",
              target = "_blank",
              class = "btn btn-success btn-sm",
              style = "margin-right: 10px;",
              icon("external-link-alt"), " Quick Start Guide"
            )
          ),
          
          p(strong("What you'll learn:"), style = "font-size: 12px; color: #666; margin-top: 15px;"),
          tags$ul(
            style = "font-size: 12px; color: #666;",
            tags$li("File format requirements"),
            tags$li("Essential workflow steps"),
            tags$li("Common issues & solutions"),
            tags$li("Recommended defaults")
          )
        )
      ),
      
      # Main Documentation Section
      column(6,
        wellPanel(
          style = "background-color: #f8f9fa; border-left: 4px solid #3498DB;",
          h4(icon("book-open"), " Complete Documentation", style = "color: #3498DB; margin-top: 0;"),
          p("Detailed guides for each tab and feature.", 
            style = "color: #666; margin-bottom: 15px;"),
          
          div(
            style = "margin-bottom: 10px;",
            tags$a(
              href = "https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/README.md",
              target = "_blank",
              class = "btn btn-primary btn-sm",
              style = "margin-right: 10px;",
              icon("external-link-alt"), " Documentation Index"
            )
          ),
          
          p(strong("Includes:"), style = "font-size: 12px; color: #666; margin-top: 15px;"),
          tags$ul(
            style = "font-size: 12px; color: #666;",
            tags$li("Tab-by-tab guides"),
            tags$li("Troubleshooting sections"),
            tags$li("File structure reference"),
            tags$li("Common tasks lookup")
          )
        )
      )
    ),
    
    # Tab-by-Tab Guides Section
    fluidRow(
      column(12,
        wellPanel(
          style = "background-color: #f8f9fa; border-left: 4px solid #9B59B6;",
          h4(icon("list-ol"), " Step-by-Step Guides", style = "color: #9B59B6; margin-top: 0;"),
          p("Detailed documentation for each tab in the workflow order:", 
            style = "color: #666; margin-bottom: 20px;"),
          
          fluidRow(
            column(4,
              div(
                style = "margin-bottom: 15px;",
                h5("1. Data Input", style = "color: #2C3E50; margin-bottom: 5px;"),
                p("Upload count matrix and metadata", style = "font-size: 12px; color: #666; margin-bottom: 8px;"),
                tags$a(
                  href = "https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/01_data_input_tab.md",
                  target = "_blank",
                  class = "btn btn-outline-secondary btn-xs",
                  icon("external-link-alt"), " View Guide"
                )
              ),
              
              div(
                style = "margin-bottom: 15px;",
                h5("2. Experimental Design", style = "color: #2C3E50; margin-bottom: 5px;"),
                p("Create contrasts and add covariates", style = "font-size: 12px; color: #666; margin-bottom: 8px;"),
                tags$a(
                  href = "https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/02_experimental_design_tab.md",
                  target = "_blank",
                  class = "btn btn-outline-secondary btn-xs",
                  icon("external-link-alt"), " View Guide"
                )
              )
            ),
            
            column(4,
              div(
                style = "margin-bottom: 15px;",
                h5("3. DESeq2 Configuration", style = "color: #2C3E50; margin-bottom: 5px;"),
                p("Configure and run differential expression", style = "font-size: 12px; color: #666; margin-bottom: 8px;"),
                tags$a(
                  href = "https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/03_analysis_configuration_tab.md",
                  target = "_blank",
                  class = "btn btn-outline-secondary btn-xs",
                  icon("external-link-alt"), " View Guide"
                )
              ),
              
              div(
                style = "margin-bottom: 15px;",
                h5("4. Results Display", style = "color: #2C3E50; margin-bottom: 5px;"),
                p("Browse and export DE results", style = "font-size: 12px; color: #666; margin-bottom: 8px;"),
                tags$a(
                  href = "https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/04_results_display_tab.md",
                  target = "_blank",
                  class = "btn btn-outline-secondary btn-xs",
                  icon("external-link-alt"), " View Guide"
                )
              )
            ),
            
            column(4,
              div(
                style = "margin-bottom: 15px;",
                h5("5. Enrichment Analysis", style = "color: #2C3E50; margin-bottom: 5px;"),
                p("Run GO enrichment analysis", style = "font-size: 12px; color: #666; margin-bottom: 8px;"),
                tags$a(
                  href = "https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/05_enrichment_tab.md",
                  target = "_blank",
                  class = "btn btn-outline-secondary btn-xs",
                  icon("external-link-alt"), " View Guide"
                )
              ),
              
              div(
                style = "margin-bottom: 15px;",
                h5("6. Volcano Plot", style = "color: #2C3E50; margin-bottom: 5px;"),
                p("Create publication-ready plots", style = "font-size: 12px; color: #666; margin-bottom: 8px;"),
                tags$a(
                  href = "https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/06_volcano_tab.md",
                  target = "_blank",
                  class = "btn btn-outline-secondary btn-xs",
                  icon("external-link-alt"), " View Guide"
                )
              )
            )
          )
        )
      )
    ),
    
    # Additional Resources Section
    fluidRow(
      column(6,
        wellPanel(
          style = "background-color: #f8f9fa; border-left: 4px solid #E67E22;",
          h4(icon("database"), " Example Data", style = "color: #E67E22; margin-top: 0;"),
          p("Test the app with provided example datasets:", 
            style = "color: #666; margin-bottom: 15px;"),
          
          tags$ul(
            style = "font-size: 12px; color: #666;",
            tags$li(strong("Toy datasets: "), "toy_counts.csv, toy_metadata.csv (quick testing)"),
            tags$li(strong("Example datasets: "), "example_counts.csv, example_metadata.csv (full workflow)"),
            tags$li(strong("Location: "), code("example_data/", style = "background-color: #f1f1f1; padding: 2px 4px;"))
          ),
          
          p("These files demonstrate proper formatting and can be used to test all app features.",
            style = "font-size: 11px; color: #666; margin-top: 10px; font-style: italic;")
        )
      ),
      
      column(6,
        wellPanel(
          style = "background-color: #f8f9fa; border-left: 4px solid #E74C3C;",
          h4(icon("github"), " Source Code & Issues", style = "color: #E74C3C; margin-top: 0;"),
          p("Access the source code and report issues:", 
            style = "color: #666; margin-bottom: 15px;"),
          
          div(
            style = "margin-bottom: 10px;",
            tags$a(
              href = "https://github.com/erenada/RNA_DEG_APP",
              target = "_blank",
              class = "btn btn-outline-danger btn-sm",
              style = "margin-right: 10px; margin-bottom: 5px;",
              icon("github"), " GitHub Repository"
            ),
            tags$a(
              href = "https://github.com/erenada/RNA_DEG_APP/issues",
              target = "_blank",
              class = "btn btn-outline-danger btn-sm",
              style = "margin-bottom: 5px;",
              icon("bug"), " Report Issues"
            )
          ),
          
          p("Found a bug or have a feature request? Please open an issue on GitHub.",
            style = "font-size: 11px; color: #666; margin-top: 10px; font-style: italic;")
        )
      )
    ),
    
    # Footer note
    fluidRow(
      column(12,
        div(
          style = "text-align: center; margin-top: 30px; padding: 20px; background-color: #ecf0f1; border-radius: 5px;",
          p(icon("lightbulb"), strong(" Tip: "), "Bookmark the documentation links for quick reference during your analysis.",
            style = "color: #2C3E50; margin: 0; font-size: 13px;")
        )
      )
    )
  )
}
