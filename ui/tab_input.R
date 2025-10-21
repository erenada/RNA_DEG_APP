# Data Input Tab UI Builder
# Author: Eren Ada, PhD
#
# Returns the Data Input tabPanel

build_input_tab <- function() {
  tabPanel(
      title = "Data Input",
      value = "tab_input",
      icon = icon("upload"),
      
      br(),
      
      fluidRow(
        column(12,
          div(
            style = "text-align: center; margin-bottom: 20px;",
            h2("Upload Your Data", style = "color: #2C3E50;"),
            p("Upload your count matrix and sample metadata files to begin the analysis.", 
              style = "color: #7F8C8D; font-size: 14px;")
          )
        )
      ),
      
      fluidRow(
        # Count Matrix Upload
        column(6,
          wellPanel(
            style = "background-color: #f8f9fa; border-left: 4px solid #2980B9;",
            h4("Count Matrix", style = "color: #2980B9; margin-top: 0;"),
            fileInput(
              "count_file",
              "Choose Count Matrix File",
              accept = c(".csv", ".tsv", ".txt", ".xlsx")
            ),
            selectInput(
              "count_sep",
              "File Separator:",
              choices = list("Comma (,)" = ",", "Tab" = "\t", "Semicolon (;)" = ";"),
              selected = ","
            ),
            checkboxInput("count_header", "Header", value = TRUE),
            checkboxInput("count_rownames", "Row names", value = TRUE)
          )
        ),
        
        # Metadata Upload
        column(6,
          wellPanel(
            style = "background-color: #f8f9fa; border-left: 4px solid #2980B9;",
            h4("Sample Metadata", style = "color: #2980B9; margin-top: 0;"),
            fileInput(
              "meta_file",
              "Choose Metadata File",
              accept = c(".csv", ".tsv", ".txt", ".xlsx")
            ),
            selectInput(
              "meta_sep",
              "File Separator:",
              choices = list("Comma (,)" = ",", "Tab" = "\t", "Semicolon (;)" = ";"),
              selected = ","
            ),
            checkboxInput("meta_header", "Header", value = TRUE),
            checkboxInput("meta_rownames", "Row names", value = TRUE)
          )
        )
      ),
      
      # Project Settings Section
      fluidRow(
        column(12,
          wellPanel(
            style = "background-color: #f8f9fa; border-left: 4px solid #2980B9;",
            h4("Project Settings", style = "color: #2980B9; margin-top: 0;"),
            p("Configure organism and gene ID type for your analysis. These settings are used for enrichment analysis.",
              style = "color: #666; font-size: 12px; margin-bottom: 15px;"),
            
            fluidRow(
              column(4,
                radioButtons(
                  "organism",
                  "Organism:",
                  choices = c("Mouse" = "Mouse", "Human" = "Human"),
                  selected = "Mouse",
                  inline = FALSE
                ),
                p("Select the organism for your dataset.",
                  style = "font-size: 11px; color: #666; margin-top: -10px;")
              ),
              
              column(4,
                selectInput(
                  "id_type",
                  "Gene ID Type:",
                  choices = c(
                    "Auto-detect" = "auto",
                    "Gene Symbol" = "SYMBOL",
                    "Ensembl ID" = "ENSEMBL"
                  ),
                  selected = "auto"
                ),
                p("Specify the type of gene identifiers in your count matrix.",
                  style = "font-size: 11px; color: #666; margin-top: -10px;")
              ),
              
              column(4,
                div(
                  style = "margin-top: 25px; padding: 10px; background-color: #fff3cd; border-radius: 4px;",
                  icon("info-circle", style = "color: #856404;"),
                  span(" These settings apply to the entire project.",
                       style = "font-size: 11px; color: #856404; margin-left: 5px;")
                )
              )
            )
          )
        )
      ),
      
      # Data Preview Section
      fluidRow(
        column(12,
          h4("Data Preview", style = "color: #2C3E50;"),
          
          # Validation Status
          uiOutput("validation_status"),
          
          # Navigation Button
          br(),
          uiOutput("next_button"),
          
          # Data preview tabs
          tabsetPanel(
            id = "preview_tabs",
            tabPanel(
              "Count Matrix",
              br(),
              DT::dataTableOutput("count_preview")
            ),
            tabPanel(
              "Metadata",
              br(),
              DT::dataTableOutput("meta_preview")
            ),
            tabPanel(
              "Summary",
              br(),
              verbatimTextOutput("data_summary")
            )
          )
        )
      )
    )
}
