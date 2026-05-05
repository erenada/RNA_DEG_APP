# Heatmap Tab UI Builder
# Author: Eren Ada, PhD
#
# Returns the Heatmap Visualization tabPanel. Supports two modes:
#   - Expression (genes x samples): backed by DESeq2 dds + VST/rlog/log2norm.
#   - Cross-contrast LFC (genes x contrasts): backed by computed DESeq2 results
#     or uploaded CSVs.
# UI scaffolding mirrors ui/tab_volcano.R and ui/tab_ma.R for consistency.

build_heatmap_tab <- function() {
  tabPanel(
    title = "Heatmap",
    value = "tab_heatmap",
    icon = icon("th"),
    
    br(),
    
    # Page Header
    fluidRow(
      column(12,
        div(
          style = "text-align: center; margin-bottom: 20px;",
          h2("Heatmap Visualization", style = "color: #2C3E50;"),
          p("Generate publication-ready expression and cross-contrast heatmaps with customizable parameters",
            style = "color: #7F8C8D; font-size: 14px;")
        )
      )
    ),
    
    # Main Content
    fluidRow(
      # Left Column: Controls (scrollable to keep plot visible)
      column(4,
        wellPanel(
          style = "background-color: #f8f9fa; border-left: 4px solid #2980B9; 
                   max-height: 85vh; overflow-y: auto; overflow-x: hidden;",
          
          # ========================================
          # Section 1: Data Source
          # ========================================
          h4("Data Source", style = "color: #2C3E50; margin-top: 0;"),
          
          # Mode selector
          radioButtons(
            "heatmap_mode",
            "Heatmap mode:",
            choices = c(
              "Expression (genes x samples)" = "expression",
              "Cross-contrast LFC (genes x contrasts)" = "lfc"
            ),
            selected = "expression",
            inline = FALSE
          ),
          
          # Expression mode: single contrast selector
          conditionalPanel(
            condition = "input.heatmap_mode == 'expression'",
            selectizeInput(
              "heatmap_contrast",
              "Select Contrast (drives gene set + dds):",
              choices = NULL,
              options = list(placeholder = 'Select a contrast'),
              width = "100%"
            )
          ),
          
          # LFC mode: multi-contrast selector
          conditionalPanel(
            condition = "input.heatmap_mode == 'lfc'",
            selectizeInput(
              "heatmap_contrasts",
              "Select Contrasts (heatmap columns):",
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = 'Select one or more contrasts',
                plugins = list("remove_button")
              ),
              width = "100%"
            )
          ),
          
          # CSV Upload Section (LFC mode only; expression requires dds)
          conditionalPanel(
            condition = "input.heatmap_mode == 'lfc'",
            div(
              style = "background-color: #fff9e6; padding: 10px; border-radius: 4px; margin-bottom: 15px;",
              h5(icon("upload"), " Import DEG Results", style = "color: #FF8C00; margin-top: 0;"),
              fileInput(
                "heatmap_upload_deg_results",
                "Upload CSV:",
                accept = ".csv",
                width = "100%",
                buttonLabel = "Browse...",
                placeholder = "No file selected"
              ),
              p("Required: gene, log2FoldChange, padj, contrast",
                style = "font-size: 10px; color: #666; margin-top: -10px;"),
              actionButton(
                "heatmap_clear_upload",
                "Clear Upload",
                icon = icon("times"),
                class = "btn-outline-danger btn-sm btn-block"
              ),
              br(),
              uiOutput("heatmap_upload_status")
            )
          ),
          
          hr(),
          
          # ========================================
          # Section 2: Basic Parameters
          # ========================================
          h4("Basic Parameters", style = "color: #2C3E50;"),
          
          # Gene selection method
          radioButtons(
            "heatmap_gene_method",
            "Gene selection method:",
            choices = c(
              "Top N by padj"          = "top_padj",
              "Top N by |Log2FC|"      = "top_lfc",
              "Threshold (padj/LFC)"   = "threshold",
              "Custom gene list"       = "custom"
            ),
            selected = "top_padj"
          ),
          
          # Top N input (top_padj or top_lfc)
          conditionalPanel(
            condition = "input.heatmap_gene_method == 'top_padj' || input.heatmap_gene_method == 'top_lfc'",
            numericInput(
              "heatmap_top_n",
              "Number of genes (N):",
              value = 50,
              min = 5,
              max = 500,
              step = 5,
              width = "100%"
            )
          ),
          
          # Threshold inputs
          conditionalPanel(
            condition = "input.heatmap_gene_method == 'threshold'",
            fluidRow(
              column(6,
                numericInput(
                  "heatmap_padj_cutoff",
                  "Adjusted p-value cutoff:",
                  value = 0.05,
                  min = 0,
                  max = 1,
                  step = 0.01,
                  width = "100%"
                )
              ),
              column(6,
                numericInput(
                  "heatmap_lfc_cutoff",
                  "|Log2FC| cutoff:",
                  value = 1.0,
                  min = 0,
                  max = 10,
                  step = 0.1,
                  width = "100%"
                )
              )
            ),
            conditionalPanel(
              condition = "input.heatmap_mode == 'lfc'",
              radioButtons(
                "heatmap_threshold_quantifier",
                "Apply threshold:",
                choices = c(
                  "In ANY selected contrast" = "any",
                  "In ALL selected contrasts" = "all"
                ),
                selected = "any",
                inline = FALSE
              )
            )
          ),
          
          # Custom gene list inputs
          conditionalPanel(
            condition = "input.heatmap_gene_method == 'custom'",
            textAreaInput(
              "heatmap_gene_list_paste",
              "Paste gene names:",
              value = "",
              rows = 4,
              width = "100%",
              placeholder = "GENE1, GENE2, GENE3 (comma, semicolon, whitespace, or newline separated)"
            ),
            fileInput(
              "heatmap_gene_list_upload",
              "Or upload a list (.txt or .csv):",
              accept = c(".txt", ".csv"),
              width = "100%",
              buttonLabel = "Browse...",
              placeholder = "No file selected"
            )
          ),
          
          # Sample subset (expression mode only)
          conditionalPanel(
            condition = "input.heatmap_mode == 'expression'",
            radioButtons(
              "heatmap_sample_scope",
              "Samples to display:",
              choices = c(
                "Samples in selected contrast" = "contrast",
                "All samples in dds" = "all"
              ),
              selected = "contrast",
              inline = FALSE
            )
          ),
          
          # Custom Title
          fluidRow(
            column(8,
              textInput(
                "heatmap_title",
                "Custom plot title (leave empty for default):",
                value = "",
                placeholder = "Enter custom title...",
                width = "100%"
              )
            ),
            column(4,
              selectInput(
                "heatmap_title_align",
                "Title alignment:",
                choices = c("Left" = "left", "Center" = "center", "Right" = "right"),
                selected = "center",
                width = "100%"
              )
            )
          ),
          
          hr(),
          
          # Advanced Options Toggle Button
          actionButton(
            "show_heatmap_advanced",
            HTML('<i class="fa fa-cog"></i> Advanced Options'),
            class = "btn-info btn-block",
            style = "margin-bottom: 10px;"
          ),
          
          # Advanced Options Panel (Initially Hidden)
          div(
            id = "heatmap_advanced_panel",
            style = "display: none;",
            
            h4("Advanced Options", style = "color: #2C3E50; margin-top: 15px;"),
            
            # ========================================
            # 1. HEATMAP LAYOUT & TRANSFORMATION
            # ========================================
            h5("Heatmap Layout & Transformation", style = "color: #555; font-weight: 600;"),
            
            # Transformation (expression mode only)
            conditionalPanel(
              condition = "input.heatmap_mode == 'expression'",
              p(strong("Transformation"), style = "color: #666; margin-top: 10px; margin-bottom: 5px;"),
              radioButtons(
                "heatmap_transform",
                label = NULL,
                choices = c(
                  "VST (fast, recommended)" = "vst",
                  "rlog (slower, smaller datasets)" = "rlog",
                  "log2(normalized + 1)" = "log2norm"
                ),
                selected = "vst"
              )
            ),
            
            # Scaling
            p(strong("Scaling"), style = "color: #666; margin-top: 10px; margin-bottom: 5px;"),
            radioButtons(
              "heatmap_scale",
              label = NULL,
              choices = c(
                "None" = "none",
                "Z-score by row (gene)" = "row",
                "Z-score by column (sample)" = "column"
              ),
              selected = "row"
            ),
            p(em("Note: LFC mode forces scaling to None."),
              style = "font-size: 11px; color: #888;"),
            
            # Clustering
            p(strong("Clustering"), style = "color: #666; margin-top: 10px; margin-bottom: 5px;"),
            fluidRow(
              column(6,
                checkboxInput(
                  "heatmap_cluster_rows",
                  "Cluster rows (genes)",
                  value = TRUE
                )
              ),
              column(6,
                checkboxInput(
                  "heatmap_cluster_cols",
                  "Cluster columns",
                  value = TRUE
                )
              )
            ),
            
            fluidRow(
              column(6,
                selectInput(
                  "heatmap_distance",
                  "Distance:",
                  choices = c(
                    "Euclidean" = "euclidean",
                    "Correlation" = "correlation",
                    "Manhattan" = "manhattan"
                  ),
                  selected = "euclidean",
                  width = "100%"
                )
              ),
              column(6,
                selectInput(
                  "heatmap_linkage",
                  "Linkage:",
                  choices = c(
                    "Complete" = "complete",
                    "Ward.D2" = "ward.D2",
                    "Average" = "average",
                    "Single" = "single"
                  ),
                  selected = "complete",
                  width = "100%"
                )
              )
            ),
            
            hr(),
            
            # ========================================
            # 2. CELL DISPLAY & LABELS
            # ========================================
            h5("Cell Display & Labels", style = "color: #555; font-weight: 600; margin-top: 15px;"),
            
            fluidRow(
              column(6,
                checkboxInput(
                  "heatmap_show_rownames",
                  "Show row labels (genes)",
                  value = TRUE
                )
              ),
              column(6,
                checkboxInput(
                  "heatmap_show_colnames",
                  "Show column labels",
                  value = TRUE
                )
              )
            ),
            
            fluidRow(
              column(6,
                numericInput(
                  "heatmap_fontsize_row",
                  "Row label size:",
                  value = 8,
                  min = 4,
                  max = 24,
                  step = 1,
                  width = "100%"
                )
              ),
              column(6,
                numericInput(
                  "heatmap_fontsize_col",
                  "Column label size:",
                  value = 10,
                  min = 4,
                  max = 24,
                  step = 1,
                  width = "100%"
                )
              )
            ),
            
            fluidRow(
              column(6,
                numericInput(
                  "heatmap_cellwidth",
                  "Cell width (NA = auto):",
                  value = NA,
                  width = "100%"
                )
              ),
              column(6,
                numericInput(
                  "heatmap_cellheight",
                  "Cell height (NA = auto):",
                  value = NA,
                  width = "100%"
                )
              )
            ),
            
            fluidRow(
              column(6,
                numericInput(
                  "heatmap_treeheight_row",
                  "Row dendrogram height:",
                  value = 30,
                  min = 0,
                  max = 200,
                  step = 5,
                  width = "100%"
                )
              ),
              column(6,
                numericInput(
                  "heatmap_treeheight_col",
                  "Column dendrogram height:",
                  value = 30,
                  min = 0,
                  max = 200,
                  step = 5,
                  width = "100%"
                )
              )
            ),
            
            fluidRow(
              column(6,
                colourpicker::colourInput(
                  "heatmap_border_color",
                  "Cell border:",
                  value = "#FFFFFF00",
                  showColour = "background",
                  allowTransparent = TRUE
                )
              ),
              column(6,
                colourpicker::colourInput(
                  "heatmap_na_color",
                  "NA cell color:",
                  value = "#DDDDDD",
                  showColour = "background"
                )
              )
            ),
            
            hr(),
            
            # ========================================
            # 3. COLOR SCALE & RANGE
            # ========================================
            h5("Color Scale & Range", style = "color: #555; font-weight: 600; margin-top: 15px;"),
            
            selectInput(
              "heatmap_palette",
              "Palette:",
              choices = c(
                "RdBu (diverging)" = "RdBu",
                "RdYlBu (diverging)" = "RdYlBu",
                "Spectral (diverging)" = "Spectral",
                "Viridis (sequential)" = "Viridis",
                "Plasma (sequential)" = "Plasma"
              ),
              selected = "RdBu",
              width = "100%"
            ),
            
            checkboxInput(
              "heatmap_palette_reverse",
              "Reverse palette",
              value = FALSE
            ),
            
            p(strong("Manual color range (NA = auto)"), style = "color: #666; margin-top: 10px; margin-bottom: 5px;"),
            p(em("LFC mode auto-defaults to a symmetric range around zero."),
              style = "font-size: 11px; color: #888; margin-bottom: 10px;"),
            fluidRow(
              column(6,
                numericInput(
                  "heatmap_zlim_min",
                  "Min:",
                  value = NA,
                  width = "100%"
                )
              ),
              column(6,
                numericInput(
                  "heatmap_zlim_max",
                  "Max:",
                  value = NA,
                  width = "100%"
                )
              )
            ),
            
            hr(),
            
            # ========================================
            # 4. LEGEND & ANNOTATIONS
            # ========================================
            h5("Legend & Annotations", style = "color: #555; font-weight: 600; margin-top: 15px;"),
            
            checkboxInput(
              "heatmap_show_legend",
              "Show value legend",
              value = TRUE
            ),
            
            # Annotation tracks (expression mode only)
            conditionalPanel(
              condition = "input.heatmap_mode == 'expression'",
              p(strong("Column annotation tracks"), style = "color: #666; margin-top: 10px; margin-bottom: 5px;"),
              checkboxInput(
                "heatmap_show_anno",
                "Enable column annotations",
                value = TRUE
              ),
              conditionalPanel(
                condition = "input.heatmap_show_anno == true",
                selectizeInput(
                  "heatmap_anno_factors",
                  "Annotation factors (from metadata):",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(
                    placeholder = 'Pick metadata columns to display as color bars',
                    plugins = list("remove_button")
                  ),
                  width = "100%"
                ),
                checkboxInput(
                  "heatmap_show_annotation_legend",
                  "Show annotation legend",
                  value = TRUE
                )
              )
            ),
            
            hr(),
            
            # ========================================
            # 5. EXPORT SETTINGS
            # ========================================
            h5("Export Settings", style = "color: #555; font-weight: 600; margin-top: 15px;"),
            fluidRow(
              column(4,
                numericInput(
                  "heatmap_plot_width",
                  "Width (in):",
                  value = 10,
                  min = 4,
                  max = 24,
                  step = 1,
                  width = "100%"
                )
              ),
              column(4,
                numericInput(
                  "heatmap_plot_height",
                  "Height (in):",
                  value = 8,
                  min = 4,
                  max = 24,
                  step = 1,
                  width = "100%"
                )
              ),
              column(4,
                numericInput(
                  "heatmap_plot_dpi",
                  "DPI:",
                  value = 300,
                  min = 72,
                  max = 600,
                  step = 50,
                  width = "100%"
                )
              )
            )
          )
        )
      ),
      
      # Right Column: Display Area
      column(8,
        conditionalPanel(
          condition = "output.heatmap_available",
          
          # Data source banner
          uiOutput("heatmap_data_source_info"),
          
          # Counts / dimension summary
          wellPanel(
            style = "background-color: #f0f8ff; border-left: 4px solid #3498DB;",
            uiOutput("heatmap_summary")
          ),
          
          # Plot Output with spinner
          wellPanel(
            shinycssloaders::withSpinner(
              plotOutput("heatmap_plot", height = "600px"),
              type = 6,
              color = "#667eea"
            )
          ),
          
          # Download buttons
          fluidRow(
            column(4,
              downloadButton(
                "download_heatmap_pdf",
                "Download PDF",
                icon = icon("file-pdf"),
                class = "btn-primary btn-block"
              )
            ),
            column(4,
              downloadButton(
                "download_heatmap_png",
                "Download PNG",
                icon = icon("file-image"),
                class = "btn-primary btn-block"
              )
            ),
            column(4,
              downloadButton(
                "download_heatmap_params",
                "Download Parameters",
                icon = icon("file-code"),
                class = "btn-info btn-block"
              )
            )
          )
        ),
        
        # Empty State
        conditionalPanel(
          condition = "!output.heatmap_available",
          wellPanel(
            style = "text-align: center; padding: 60px 20px; background-color: #f8f9fa;",
            icon("th", style = "font-size: 72px; color: #BDC3C7; margin-bottom: 20px;"),
            h3("No Data Available", style = "color: #7F8C8D;"),
            uiOutput("heatmap_empty_message")
          )
        )
      )
    ),
    
    # Matrix Preview Section (full width below the main content)
    conditionalPanel(
      condition = "output.heatmap_available",
      fluidRow(
        column(12,
          wellPanel(
            style = "background-color: #f8f9fa; border-left: 4px solid #667eea;",
            h4("Heatmap Data Matrix", style = "color: #2C3E50; margin-top: 0;"),
            
            fluidRow(
              column(8,
                p(em("Numeric values used in the heatmap above. Sortable and filterable."),
                  style = "color: #666; font-size: 12px;")
              ),
              column(4,
                div(
                  style = "text-align: right;",
                  downloadButton(
                    "download_heatmap_matrix_csv",
                    "Export Matrix (CSV)",
                    icon = icon("file-csv"),
                    class = "btn-outline-primary btn-sm"
                  )
                )
              )
            ),
            
            br(),
            
            DT::dataTableOutput("heatmap_matrix_preview")
          )
        )
      )
    )
  )  # Close Heatmap tabPanel
}
