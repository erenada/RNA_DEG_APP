# Volcano Plot Tab UI Builder
# Author: Eren Ada, PhD
#
# Returns the Volcano Plot Visualization tabPanel

build_volcano_tab <- function() {
  tabPanel(
    title = "Volcano Plot",
    value = "tab_volcano",
    icon = icon("chart-area"),
    
    br(),
    
    # Page Header
    fluidRow(
      column(12,
        div(
          style = "text-align: center; margin-bottom: 20px;",
          h2("Volcano Plot Visualization", style = "color: #2C3E50;"),
          p("Generate publication-ready volcano plots with customizable parameters",
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
          
          # Section 1: Data Source
          h4("Data Source", style = "color: #2C3E50; margin-top: 0;"),
          
          # Contrast Selector
          selectizeInput(
            "volcano_contrast",
            "Select Contrast:",
            choices = NULL,
            options = list(
              placeholder = 'Select a contrast'
            ),
            width = "100%"
          ),
          
          # CSV Upload Section
          div(
            style = "background-color: #fff9e6; padding: 10px; border-radius: 4px; margin-bottom: 15px;",
            h5(icon("upload"), " Import DEG Results", style = "color: #FF8C00; margin-top: 0;"),
            fileInput(
              "volcano_upload_deg_results",
              "Upload CSV:",
              accept = ".csv",
              width = "100%",
              buttonLabel = "Browse...",
              placeholder = "No file selected"
            ),
            p("Required: gene, log2FoldChange, pvalue, padj, contrast",
              style = "font-size: 10px; color: #666; margin-top: -10px;"),
            actionButton(
              "volcano_clear_upload",
              "Clear Upload",
              icon = icon("times"),
              class = "btn-outline-danger btn-sm btn-block"
            ),
            br(),
            uiOutput("volcano_upload_status")
          ),
          
          hr(),
          
          # Section 2: Basic Parameters
          h4("Basic Parameters", style = "color: #2C3E50;"),
          
          fluidRow(
            column(6,
              numericInput(
                "volcano_padj_cutoff",
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
                "volcano_lfc_cutoff",
                "|Log2FC| cutoff:",
                value = 1.0,
                min = 0,
                max = 10,
                step = 0.1,
                width = "100%"
              )
            )
          ),
          
          fluidRow(
            column(6,
              numericInput(
                "volcano_top_n",
                "Top N labels per direction:",
                value = 20,
                min = 0,
                max = 100,
                step = 5,
                width = "100%"
              )
            ),
            column(6,
              br(),
              checkboxInput(
                "volcano_use_shrunk",
                "Use shrunk Log2FC (if available)",
                value = TRUE
              )
            )
          ),
          
          # Custom Title
          fluidRow(
            column(8,
              textInput(
                "volcano_custom_title",
                "Custom plot title (leave empty for contrast name):",
                value = "",
                placeholder = "Enter custom title...",
                width = "100%"
              )
            ),
            column(4,
              selectInput(
                "volcano_title_align",
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
            "show_volcano_advanced",
            HTML('<i class="fa fa-cog"></i> Advanced Options'),
            class = "btn-info btn-block",
            style = "margin-bottom: 10px;"
          ),
          
          # Advanced Options Panel (Initially Hidden)
          div(
            id = "volcano_advanced_panel",
            style = "display: none;",
            
            h4("Advanced Options", style = "color: #2C3E50; margin-top: 15px;"),
            
            # ========================================
            # 1. PLOT APPEARANCE
            # ========================================
            h5("Plot Appearance", style = "color: #555; font-weight: 600;"),
            
            # Point Styling
            p(strong("Point Styling"), style = "color: #666; margin-top: 10px; margin-bottom: 5px;"),
            
            fluidRow(
              column(4,
                numericInput(
                  "volcano_point_size",
                  "Size:",
                  value = 1.5,
                  min = 0.1,
                  max = 5,
                  step = 0.1,
                  width = "100%"
                )
              ),
              column(4,
                selectInput(
                  "volcano_point_shape",
                  "Shape:",
                  choices = c(
                    "Circle" = "19",
                    "Square" = "15",
                    "Triangle" = "17",
                    "Diamond" = "18",
                    "Plus" = "3",
                    "Cross" = "4"
                  ),
                  selected = "19",
                  width = "100%"
                )
              ),
              column(4,
                numericInput(
                  "volcano_alpha",
                  "Transparency:",
                  value = 0.6,
                  min = 0.1,
                  max = 1,
                  step = 0.1,
                  width = "100%"
                )
              )
            ),
            
            # Colors
            p(strong("Colors"), style = "color: #666; margin-top: 10px; margin-bottom: 5px;"),
            
            fluidRow(
              column(4,
                colourpicker::colourInput(
                  "volcano_color_up",
                  "Up-regulated:",
                  value = "#CD0000",
                  showColour = "background"
                )
              ),
              column(4,
                colourpicker::colourInput(
                  "volcano_color_down",
                  "Down-regulated:",
                  value = "#4169E1",
                  showColour = "background"
                )
              ),
              column(4,
                colourpicker::colourInput(
                  "volcano_color_ns",
                  "Non-significant:",
                  value = "#999999",
                  showColour = "background"
                )
              )
            ),
            
            fluidRow(
              column(12,
                colourpicker::colourInput(
                  "volcano_color_selected",
                  "Selected genes (from table):",
                  value = "#FF6600",
                  showColour = "background"
                )
              )
            ),
            
            # Grid Lines
            p(strong("Grid Lines"), style = "color: #666; margin-top: 10px; margin-bottom: 5px;"),
            
            fluidRow(
              column(6,
                checkboxInput(
                  "volcano_show_major_grid",
                  "Major grid lines",
                  value = TRUE
                )
              ),
              column(6,
                checkboxInput(
                  "volcano_show_minor_grid",
                  "Minor grid lines",
                  value = FALSE
                )
              )
            ),
            
            hr(),
            
            # ========================================
            # 2. GENE LABELING
            # ========================================
            h5("Gene Labeling", style = "color: #555; font-weight: 600; margin-top: 15px;"),
            
            # Label Styling
            p(strong("Label Styling"), style = "color: #666; margin-top: 10px; margin-bottom: 5px;"),
            
            fluidRow(
              column(6,
                numericInput(
                  "volcano_label_size",
                  "Label size:",
                  value = 3.0,
                  min = 1,
                  max = 8,
                  step = 0.5,
                  width = "100%"
                )
              ),
              column(6,
                numericInput(
                  "volcano_max_overlaps",
                  "Max overlaps:",
                  value = 50,
                  min = 0,
                  max = 200,
                  step = 10,
                  width = "100%"
                )
              )
            ),
            
            fluidRow(
              column(6,
                checkboxInput(
                  "volcano_boxed_labels",
                  "Boxed labels",
                  value = TRUE
                )
              ),
              column(6,
                checkboxInput(
                  "volcano_draw_connectors",
                  "Draw connectors",
                  value = TRUE
                )
              )
            ),
            
            conditionalPanel(
              condition = "input.volcano_draw_connectors == true",
              numericInput(
                "volcano_connector_width",
                "Connector width:",
                value = 0.3,
                min = 0.1,
                max = 2,
                step = 0.1,
                width = "100%"
              )
            ),
            
            # Custom Labels
            p(strong("Custom Labels"), style = "color: #666; margin-top: 10px; margin-bottom: 5px;"),
            
            textAreaInput(
              "volcano_force_labels",
              "Specific genes to label (comma or newline separated):",
              value = "",
              rows = 3,
              width = "100%",
              placeholder = "GENE1, GENE2, GENE3"
            ),
            
            checkboxInput(
              "volcano_custom_labels_only",
              "Show only custom labels (disable automatic top N labeling)",
              value = FALSE
            ),
            
            hr(),
            
            # ========================================
            # 3. TEXT ELEMENTS
            # ========================================
            h5("Text Elements", style = "color: #555; font-weight: 600; margin-top: 15px;"),
            
            # Subtitle
            p(strong("Subtitle"), style = "color: #666; margin-top: 10px; margin-bottom: 5px;"),
            
            checkboxInput(
              "volcano_show_subtitle",
              "Show subtitle",
              value = TRUE
            ),
            
            conditionalPanel(
              condition = "input.volcano_show_subtitle",
              
              textInput(
                "volcano_custom_subtitle",
                "Custom subtitle (leave empty for auto-generated):",
                value = "",
                placeholder = "Up: X | Down: Y",
                width = "100%"
              ),
              
              fluidRow(
                column(6,
                  checkboxInput(
                    "volcano_show_percentage",
                    "Show percentage significant",
                    value = FALSE
                  )
                ),
                column(6,
                  selectInput(
                    "volcano_subtitle_align",
                    "Alignment:",
                    choices = c("Left" = "left", "Center" = "center", "Right" = "right"),
                    selected = "center",
                    width = "100%"
                  )
                )
              )
            ),
            
            # Caption
            p(strong("Caption"), style = "color: #666; margin-top: 10px; margin-bottom: 5px;"),
            
            checkboxInput(
              "volcano_show_caption",
              "Show caption",
              value = TRUE
            ),
            
            conditionalPanel(
              condition = "input.volcano_show_caption",
              
              textAreaInput(
                "volcano_custom_caption",
                "Custom caption (leave empty for auto-generated):",
                value = "",
                rows = 2,
                placeholder = "FDR < 0.05, |Log2FC| > 1.0",
                width = "100%"
              ),
              
              selectInput(
                "volcano_caption_align",
                "Alignment:",
                choices = c("Left" = "left", "Center" = "center", "Right" = "right"),
                selected = "center",
                width = "100%"
              )
            ),
            
            hr(),
            
            # ========================================
            # 4. THRESHOLDS & AXES
            # ========================================
            h5("Thresholds & Axes", style = "color: #555; font-weight: 600; margin-top: 15px;"),
            
            # Threshold Lines
            p(strong("Threshold Lines"), style = "color: #666; margin-top: 10px; margin-bottom: 5px;"),
            
            checkboxInput(
              "volcano_show_threshold_lines",
              "Show threshold lines",
              value = TRUE
            ),
            
            conditionalPanel(
              condition = "input.volcano_show_threshold_lines == true",
              selectInput(
                "volcano_threshold_style",
                "Line style:",
                choices = c(
                  "Solid" = "solid",
                  "Dashed" = "dashed",
                  "Dotted" = "dotted",
                  "Dot-dash" = "dotdash"
                ),
                selected = "dashed",
                width = "100%"
              )
            ),
            
            # Axis Limits
            p(strong("Axis Limits"), style = "color: #666; margin-top: 10px; margin-bottom: 5px;"),
            
            checkboxInput(
              "volcano_fix_axes",
              "Fix axis limits (for comparing multiple contrasts)",
              value = FALSE
            ),
            
            conditionalPanel(
              condition = "input.volcano_fix_axes == true",
              p("Set custom axis ranges to maintain consistent scales across plots",
                style = "font-size: 11px; color: #666; margin-bottom: 10px;"),
              fluidRow(
                column(6,
                  numericInput(
                    "volcano_xlim_min",
                    "X-axis min (log2FC):",
                    value = NA,
                    width = "100%"
                  )
                ),
                column(6,
                  numericInput(
                    "volcano_xlim_max",
                    "X-axis max (log2FC):",
                    value = NA,
                    width = "100%"
                  )
                )
              ),
              fluidRow(
                column(6,
                  numericInput(
                    "volcano_ylim_min",
                    "Y-axis min (-log10 p):",
                    value = 0,
                    width = "100%"
                  )
                ),
                column(6,
                  numericInput(
                    "volcano_ylim_max",
                    "Y-axis max (-log10 p):",
                    value = NA,
                    width = "100%"
                  )
                )
              )
            ),
            
            hr(),
            
            # ========================================
            # 5. LEGEND & ANNOTATIONS
            # ========================================
            h5("Legend & Annotations", style = "color: #555; font-weight: 600; margin-top: 15px;"),
            
            # Legend
            p(strong("Legend"), style = "color: #666; margin-top: 10px; margin-bottom: 5px;"),
            
            selectInput(
              "volcano_legend_position",
              "Legend position:",
              choices = c("right", "left", "top", "bottom", "none"),
              selected = "right",
              width = "100%"
            ),
            
            # Annotations
            p(strong("Annotations"), style = "color: #666; margin-top: 10px; margin-bottom: 5px;"),
            
            checkboxInput(
              "volcano_show_counts_on_plot",
              "Show gene counts on plot",
              value = FALSE
            ),
            
            hr(),
            
            # ========================================
            # 6. EXPORT SETTINGS
            # ========================================
            h5("Export Settings", style = "color: #555; font-weight: 600; margin-top: 15px;"),
            fluidRow(
              column(4,
                numericInput(
                  "volcano_plot_width",
                  "Width (in):",
                  value = 8,
                  min = 4,
                  max = 20,
                  step = 1,
                  width = "100%"
                )
              ),
              column(4,
                numericInput(
                  "volcano_plot_height",
                  "Height (in):",
                  value = 6,
                  min = 4,
                  max = 20,
                  step = 1,
                  width = "100%"
                )
              ),
              column(4,
                numericInput(
                  "volcano_plot_dpi",
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
          condition = "output.volcano_available",
          
          # Upload Status Indicator
          uiOutput("volcano_data_source_info"),
          
          # Counts Summary
          wellPanel(
            style = "background-color: #f0f8ff; border-left: 4px solid #3498DB;",
            uiOutput("volcano_counts_summary")
          ),
          
          # Plot Output
          wellPanel(
            shinycssloaders::withSpinner(
              plotOutput("volcano_plot", height = "600px"),
              type = 6,
              color = "#667eea"
            )
          ),
          
          # Download Buttons
          fluidRow(
            column(4,
              downloadButton(
                "download_volcano_pdf",
                "Download PDF",
                icon = icon("file-pdf"),
                class = "btn-primary btn-block"
              )
            ),
            column(4,
              downloadButton(
                "download_volcano_png",
                "Download PNG",
                icon = icon("file-image"),
                class = "btn-primary btn-block"
              )
            ),
            column(4,
              downloadButton(
                "download_volcano_params",
                "Download Parameters",
                icon = icon("file-code"),
                class = "btn-info btn-block"
              )
            )
          )
        ),
        
        # Empty State
        conditionalPanel(
          condition = "!output.volcano_available",
          wellPanel(
            style = "text-align: center; padding: 60px 20px; background-color: #f8f9fa;",
            icon("chart-scatter", style = "font-size: 72px; color: #BDC3C7; margin-bottom: 20px;"),
            h3("No Contrast Selected", style = "color: #7F8C8D;"),
            p("Select a contrast from the dropdown or upload DEG results to generate a volcano plot.",
              style = "color: #95A5A6; font-size: 14px;")
          )
        )
      )
    ),
    
    # Gene List Table Section (full width below the main content)
    conditionalPanel(
      condition = "output.volcano_available",
      fluidRow(
        column(12,
          wellPanel(
            style = "background-color: #f8f9fa; border-left: 4px solid #667eea;",
            h4("Gene List", style = "color: #2C3E50; margin-top: 0;"),
            
            fluidRow(
              column(6,
                radioButtons(
                  "volcano_show_genes",
                  "Display:",
                  choices = c(
                    "Significant genes only" = "significant",
                    "All genes" = "all"
                  ),
                  selected = "significant",
                  inline = TRUE
                )
              ),
              column(6,
                div(
                  style = "text-align: right; margin-top: 25px;",
                  downloadButton(
                    "download_volcano_genes_csv",
                    "Export Table (CSV)",
                    icon = icon("file-csv"),
                    class = "btn-outline-primary btn-sm"
                  )
                )
              )
            ),
            
            br(),
            
            DT::dataTableOutput("volcano_gene_table")
          )
        )
      )
    )
  )  # Close Volcano tabPanel
}

