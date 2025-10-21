# Enrichment Analysis Tab UI Builder
# Author: Eren Ada, PhD
#
# Returns the Enrichment Analysis tabPanel

build_enrichment_tab <- function() {
  tabPanel(
    title = "Enrichment",
    value = "tab_enrichment",
    icon = icon("flask"),
    
    br(),
      
      # Page Header
      fluidRow(
        column(12,
          div(
            style = "text-align: center; margin-bottom: 20px;",
            h2("Gene Ontology Enrichment Analysis", style = "color: #2C3E50;"),
            p("Perform functional enrichment analysis to identify biological processes, molecular functions, and cellular components associated with your differentially expressed genes.",
              style = "color: #7F8C8D; font-size: 14px;")
          ),
          hr()
        )
      ),
      
      # CSV Upload Section
      fluidRow(
        column(12,
          wellPanel(
            style = "background-color: #f8f9fa; border-left: 4px solid #2980B9; margin-bottom: 20px;",
            h4("Import DEG Results", icon("upload"), style = "color: #2980B9; margin-top: 0;"),
            
            p("Upload pre-computed DEG results to run enrichment analysis without re-running DESeq2.",
              style = "font-size: 12px; color: #666; margin-bottom: 15px;"),
            
            fluidRow(
              column(6,
                fileInput(
                  "upload_deg_results",
                  "Upload DEG Results (CSV):",
                  accept = ".csv",
                  width = "100%",
                  buttonLabel = "Browse...",
                  placeholder = "No file selected",
                  multiple = TRUE
                ),
                p("Required columns: gene, log2FoldChange, padj, contrast",
                  style = "font-size: 10px; color: #666; margin-top: -10px;")
              ),
              column(3,
                br(),
                actionButton(
                  "clear_uploaded_results",
                  "Clear Uploaded Results",
                  icon = icon("times"),
                  class = "btn-outline-danger btn-sm btn-block",
                  style = "margin-top: 5px;"
                )
              ),
              column(3,
                br(),
                uiOutput("upload_status")
              )
            ),
            
            div(
              style = "background-color: #f0f8ff; padding: 10px; border-radius: 4px; margin-top: 10px;",
              icon("info-circle", style = "color: #3498DB;"),
              strong(" How it works:", style = "color: #2C3E50;"),
              br(),
              span("1. Upload one or more CSV files", style = "font-size: 11px; color: #555;"),
              br(),
              span("2. Contrasts will appear in the selector below", style = "font-size: 11px; color: #555;"),
              br(),
              span("3. Configure settings and click 'Run Selected Contrasts'", style = "font-size: 11px; color: #555;")
            )
          )
        )
      ),
      
      # Controls Section
      fluidRow(
        column(12,
          wellPanel(
            style = "background-color: #f8f9fa; border-left: 4px solid #2980B9;",
            h4("Analysis Settings", style = "color: #2C3E50; margin-top: 0;"),
            
            fluidRow(
              # Contrast Selection
              column(3,
                selectizeInput(
                  "enrich_contrast",
                  "Select Contrast(s):",
                  choices = NULL,  # Will be populated by server
                  multiple = TRUE,
                  options = list(
                    placeholder = 'Select one or more contrasts',
                    maxItems = 10
                  ),
                  width = "100%"
                ),
                p("Select multiple contrasts to run batch analysis",
                  style = "font-size: 10px; color: #666; margin-top: -10px;")
              ),
              
              # Ontology Selection
              column(3,
                checkboxGroupInput(
                  "enrich_ontology",
                  "GO Ontology:",
                  choices = c(
                    "Biological Process" = "BP",
                    "Molecular Function" = "MF",
                    "Cellular Component" = "CC"
                  ),
                  selected = "BP"
                )
              ),
              
              # Direction Selection
              column(3,
                radioButtons(
                  "enrich_direction",
                  "Gene Set:",
                  choices = c(
                    "Up-regulated" = "Up",
                    "Down-regulated" = "Down",
                    "All significant" = "All"
                  ),
                  selected = "Up"
                )
              ),
              
              # Action Buttons
              column(3,
                br(),
                actionButton(
                  "run_enrichment",
                  "Run Selected Contrasts",
                  icon = icon("play-circle"),
                  class = "btn-primary btn-block",
                  style = "margin-bottom: 10px;"
                ),
                actionButton(
                  "run_all_contrasts",
                  "Run All Contrasts",
                  icon = icon("forward"),
                  class = "btn-info btn-block btn-sm",
                  style = "margin-bottom: 10px;"
                ),
                actionButton(
                  "show_advanced_options",
                  "Advanced Options",
                  icon = icon("cog"),
                  class = "btn-outline-secondary btn-block btn-sm"
                )
              )
            ),
            
            # Advanced Options (Collapsible)
            div(
              id = "advanced_options_panel",
              style = "display: none; margin-top: 15px; padding-top: 15px; border-top: 1px solid #dee2e6;",
              
              h5("Advanced Parameters", style = "color: #2C3E50; margin-bottom: 15px;"),
              
              fluidRow(
                column(3,
                  numericInput(
                    "enrich_padj_cutoff",
                    "Significance Cutoff (padj):",
                    value = 0.05,
                    min = 0,
                    max = 1,
                    step = 0.01,
                    width = "100%"
                  )
                ),
                
                column(3,
                  numericInput(
                    "enrich_lfc_cutoff",
                    "Log2FC Threshold:",
                    value = 1,
                    min = 0,
                    max = 10,
                    step = 0.1,
                    width = "100%"
                  )
                ),
                
                column(3,
                  numericInput(
                    "enrich_pvalue_cutoff",
                    "GO p-value Cutoff:",
                    value = 0.05,
                    min = 0,
                    max = 1,
                    step = 0.01,
                    width = "100%"
                  )
                ),
                
                column(3,
                  numericInput(
                    "enrich_qvalue_cutoff",
                    "GO q-value Cutoff:",
                    value = 0.20,
                    min = 0,
                    max = 1,
                    step = 0.01,
                    width = "100%"
                  )
                )
              ),
              
              fluidRow(
                column(3,
                  numericInput(
                    "enrich_min_gs_size",
                    "Min Gene Set Size:",
                    value = 10,
                    min = 1,
                    max = 500,
                    step = 1,
                    width = "100%"
                  )
                ),
                
                column(3,
                  numericInput(
                    "enrich_max_gs_size",
                    "Max Gene Set Size:",
                    value = 500,
                    min = 10,
                    max = 5000,
                    step = 10,
                    width = "100%"
                  )
                ),
                
                column(3,
                  numericInput(
                    "enrich_simplify_cutoff",
                    "Simplify Cutoff:",
                    value = 0.70,
                    min = 0,
                    max = 1,
                    step = 0.05,
                    width = "100%"
                  ),
                  p("Semantic similarity threshold for term reduction",
                    style = "font-size: 10px; color: #666; margin-top: -10px;")
                ),
                
                column(3,
                  br(),
                  actionButton(
                    "clear_enrichment_cache",
                    "Clear Cache",
                    icon = icon("trash"),
                    class = "btn-outline-danger btn-sm btn-block"
                  )
                )
              )
            )
          )
        )
      ),
      
      # Results Section
      fluidRow(
        column(12,
          uiOutput("enrichment_results_panel")
        )
      )
  )
}

