# UI for RNA-seq DEG Analysis App
# Author: Eren Ada, PhD

# All packages and utilities are loaded in global.R

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel(
    div(
      h1("RNA-seq Differential Gene Expression Analysis", 
         style = "color: #2C3E50; margin-bottom: 5px;"),
      h4("Powered by DESeq2", 
         style = "color: #7F8C8D; margin-top: 0px; font-weight: normal;")
    )
  ),
  
  # Add custom CSS and shinyjs
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    # Add jQuery UI for drag and drop
    tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.min.js"),
    tags$link(rel = "stylesheet", href = "https://code.jquery.com/ui/1.12.1/themes/ui-lightness/jquery-ui.css")
  ),
  
  # Main tabset panel
  tabsetPanel(
    id = "main_tabs",
    type = "tabs",
    
    # Tab 1: Data Input
    tabPanel(
      title = "Data Input",
      value = "tab_input",
      icon = icon("upload"),
      
      br(),
      
      fluidRow(
        column(12,
          h3("Upload Your Data", style = "color: #2C3E50;"),
          p("Upload your count matrix and sample metadata files to begin the analysis.")
        )
      ),
      
      fluidRow(
        # Count Matrix Upload
        column(6,
          wellPanel(
            h4("Count Matrix", style = "color: #2980B9;"),
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
            h4("Sample Metadata", style = "color: #2980B9;"),
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
    ),
    
    # Tab 2: Experimental Design
    tabPanel(
      title = "Experimental Design",
      value = "tab_design",
      icon = icon("project-diagram"),
      
      br(),
      
      fluidRow(
        column(12,
          h3("Experimental Design Builder", style = "color: #2C3E50;"),
          p("Create comparison groups by dragging samples into different groups for differential expression analysis.")
        )
      ),
      
      # Add CSS for drag and drop
      tags$head(
        tags$style(HTML("
          .sample-list {
            border: 2px dashed #ddd;
            border-radius: 5px;
            padding: 15px;
            margin: 10px 0;
            min-height: 100px;
            background-color: #f9f9f9;
          }
          .sample-item {
            background-color: #e3f2fd;
            border: 1px solid #2196f3;
            border-radius: 4px;
            padding: 6px 10px;
            margin: 3px;
            display: inline-block;
            cursor: move;
            user-select: none;
            min-width: 80px;
            text-align: center;
            font-size: 13px;
          }
          .sample-item:hover {
            background-color: #bbdefb;
          }
          .group-container {
            border: 2px solid #ddd;
            border-radius: 8px;
            padding: 15px;
            margin: 10px 0;
            background-color: white;
          }
          .group-1 { border-color: #4caf50; }
          .group-2 { border-color: #ff9800; }
          .drop-zone {
            min-height: 80px;
            border: 2px dashed #ccc;
            border-radius: 5px;
            padding: 10px;
            margin: 5px 0;
            background-color: #fafafa;
          }
          .drop-zone.dragover {
            border-color: #2196f3;
            background-color: #e3f2fd;
          }
        "))
      ),
      
      fluidRow(
        # Available Samples
        column(4,
          wellPanel(
            h4("Available Samples", style = "color: #2980B9;"),
            p("Filter and select samples to drag into comparison groups."),
            
            # Sample filtering controls
            fluidRow(
              column(12,
                h5("Filter Samples:", style = "margin-bottom: 10px;"),
                uiOutput("metadata_filters")
              )
            ),
            
            # Sample count and info
            uiOutput("filtered_sample_info"),
            
            # Filtered samples list
            div(
              id = "available_samples",
              class = "sample-list",
              style = "max-height: 400px; overflow-y: auto;",
              uiOutput("available_samples_list")
            ),
            
            # Quick actions
            br(),
            div(
              actionButton("show_all_samples", "Show All", class = "btn-outline-secondary btn-sm"),
              style = "text-align: center;"
            )
          )
        ),
        
        # Comparison Groups
        column(8,
          wellPanel(
            h4("Comparison Groups", style = "color: #2980B9;"),
            p("Create comparison groups by dragging samples. Each group should contain biologically similar samples."),
            
            fluidRow(
              # Group 1
              column(6,
                div(
                  class = "group-container group-1",
                  h5("Focus Group (Numerator)", style = "color: #4caf50; text-align: center;"),
                  textInput("group1_name", "Group Name:", value = "Treatment", placeholder = "e.g., Treatment, Condition"),
                  div(
                    id = "group1_samples",
                    class = "drop-zone",
                    style = "min-height: 120px;",
                    uiOutput("group1_samples_list")
                  )
                )
              ),
              
              # Group 2
              column(6,
                div(
                  class = "group-container group-2",
                  h5("Background/Control Group (Denominator)", style = "color: #ff9800; text-align: center;"),
                  textInput("group2_name", "Group Name:", value = "Control", placeholder = "e.g., Control, Baseline"),
                  div(
                    id = "group2_samples",
                    class = "drop-zone",
                    style = "min-height: 120px;",
                    uiOutput("group2_samples_list")
                  )
                )
              )
            ),
            
            # Contrast preview and controls
            br(),
            div(
              style = "text-align: center; padding: 15px; background-color: #f8f9fa; border-radius: 5px;",
              h5("Contrast Preview:", style = "margin-bottom: 10px;"),
              uiOutput("contrast_preview"),
              br(),
              div(
                style = "margin-bottom: 15px;",
                textInput(
                  "contrast_factor_name",
                  "Factor Name:",
                  value = "contrast_group",
                  placeholder = "e.g., treatment_condition, experimental_group",
                  width = "300px"
                ),
                div(
                  style = "font-size: 11px; color: #666; margin-top: -10px;",
                  "This will be the factor name in the DESeq2 design formula"
                )
              ),
              actionButton(
                "create_contrast",
                "Create Contrast",
                icon = icon("plus"),
                class = "btn-success btn-lg",
                disabled = TRUE
              ),
              actionButton(
                "clear_groups",
                "Clear All",
                icon = icon("eraser"),
                class = "btn-secondary",
                style = "margin-left: 10px;"
              )
            )
          )
        )
      ),
      
      # Created Contrasts Section
      fluidRow(
        column(12,
          wellPanel(
            h4("Created Contrasts", style = "color: #2980B9;"),
            uiOutput("created_contrasts_list"),
            
            # Navigation buttons
            br(),
            div(
              style = "text-align: center;",
              actionButton(
                "back_to_input",
                "Back: Data Input",
                icon = icon("arrow-left"),
                class = "btn-secondary",
                style = "margin-right: 10px;"
              ),
              uiOutput("design_next_button")
            )
          )
        )
      ),
      
      # JavaScript for drag and drop functionality
      tags$script(HTML("
        // Wait for both jQuery UI and Shiny to be ready
        $(document).ready(function() {
          // Ensure jQuery UI is loaded
          if (typeof $.ui === 'undefined') {
            setTimeout(arguments.callee, 100);
            return;
          }
          
          // Make samples draggable
          function makeDraggable() {
            $('.sample-item').draggable({
              revert: 'invalid',
              helper: 'clone',
              cursor: 'move',
              zIndex: 1000
            });
          }
          
          // Make drop zones droppable  
          function makeDroppable() {
            $('#group1_samples, #group2_samples, #available_samples').droppable({
              accept: '.sample-item',
              tolerance: 'pointer',
              over: function(event, ui) {
                $(this).addClass('dragover');
              },
              out: function(event, ui) {
                $(this).removeClass('dragover');
              },
              drop: function(event, ui) {
                $(this).removeClass('dragover');
                var sampleId = ui.draggable.data('sample-id');
                var targetZone = $(this).attr('id');
                
                // Send to Shiny
                Shiny.setInputValue('sample_moved', {
                  sample: sampleId,
                  target: targetZone,
                  timestamp: Date.now()
                });
              }
            });
          }
          
          // Initialize drag and drop
          function initializeDragDrop() {
            makeDraggable();
            makeDroppable();
          }
          
          // Initial setup
          initializeDragDrop();
          
          // Re-initialize when specific outputs update
          $(document).on('shiny:value', function(event) {
            if (event.target.id === 'available_samples_list' || 
                event.target.id === 'group1_samples_list' || 
                event.target.id === 'group2_samples_list') {
              setTimeout(initializeDragDrop, 100);
            }
          });
        });
      "))
    ),
    
    # Tab 3: Analysis Configuration (initially disabled)
    tabPanel(
      title = "DESeq2 Configuration",
      value = "tab_config",
      icon = icon("cogs"),
      
      br(),
      h3("Coming Soon - DESeq2 Parameter Configuration")
    ),
    
    # Tab 4: Results (initially disabled)
    tabPanel(
      title = "Results",
      value = "tab_results",
      icon = icon("chart-line"),
      
      br(),
      h3("Coming Soon - Analysis Results")
    ),
    
    # Tab 5: Export (initially disabled)
    tabPanel(
      title = "Export",
      value = "tab_export",
      icon = icon("download"),
      
      br(),
      h3("Coming Soon - Export Results")
    )
  ),
  
  # Footer
  hr(),
  fluidRow(
    column(12,
      p("RNA-seq DEG Analysis App | Built with Shiny and DESeq2",
        style = "text-align: center; color: #7F8C8D; font-size: 12px;")
    )
  )
) 