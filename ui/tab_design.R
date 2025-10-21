# Experimental Design Tab UI Builder
# Author: Eren Ada, PhD
#
# Returns the Experimental Design tabPanel

build_design_tab <- function() {
  tabPanel(
      title = "Experimental Design",
      value = "tab_design",
      icon = icon("project-diagram"),
      
      br(),
      
      fluidRow(
        column(12,
          div(
            style = "text-align: center; margin-bottom: 20px;",
            h2("Experimental Design Builder", style = "color: #2C3E50;"),
            p("Create comparison groups by dragging samples into different groups for differential expression analysis.",
              style = "color: #7F8C8D; font-size: 14px;")
          )
        )
      ),
      
      # Best Practices Info Box
      fluidRow(
        column(12,
          div(
            style = "background-color: #f0f8ff; border-left: 4px solid #2980B9; padding: 12px; margin-bottom: 20px; border-radius: 4px;",
            icon("info-circle", style = "color: #2980B9;"),
            strong(" Design Notes:", style = "color: #2C3E50; margin-left: 5px;"),
            br(),
            span("Single-factor design (~ group). Set control as reference level. Aim for ≥3 biological replicates per group. Avoid confounding with batch.",
                 style = "font-size: 12px; color: #666; margin-left: 20px;"),
            tags$span(
              icon("question-circle"),
              style = "color: #2980B9; cursor: pointer; margin-left: 8px;",
              tabindex = "0",
              `data-toggle` = "popover",
              `data-placement` = "right",
              title = "Best Practices Details",
              `data-content` = "DESeq2 compares numerator vs denominator (log2FC = numerator/denominator). Put the control as reference. Prefer balanced replicates (≥3/group). Avoid perfect confounding (e.g., all controls in one batch). See DESeq2 vignette: Note on factor levels; Multi-factor designs."
            )
          )
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
                    style = "font-size: 11px; color: #666; margin-top: -8px; margin-bottom: 6px; text-align: center;",
                    "Group of interest (numerator). Prefer ≥3 biological replicates.",
                    tags$span(
                      icon("info-circle"),
                      style = "color: #3498DB; cursor: pointer; margin-left: 6px;",
                      tabindex = "0",
                      `data-toggle` = "popover",
                      `data-placement` = "right",
                      title = "Numerator group",
                      `data-content` = "Positive log2FC indicates higher expression in this group relative to the reference (denominator). Ensure biological comparability; avoid mixing batches within groups."
                    )
                  ),
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
                    style = "font-size: 11px; color: #666; margin-top: -8px; margin-bottom: 6px; text-align: center;",
                    "Reference/baseline (denominator). This determines the sign of log2FC.",
                    tags$span(
                      icon("info-circle"),
                      style = "color: #3498DB; cursor: pointer; margin-left: 6px;",
                      tabindex = "0",
                      `data-toggle` = "popover",
                      `data-placement` = "right",
                      title = "Reference level",
                      `data-content` = "DESeq2 orders factor levels as c(reference, numerator). Setting control as reference yields interpretable signs. See DESeq2 vignette: Note on factor levels."
                    )
                  ),
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
                    "This will be the factor name in the DESeq2 design formula",
                    tags$span(
                      icon("info-circle"),
                      style = "color: #3498DB; cursor: pointer; margin-left: 6px;",
                      tabindex = "0",
                      `data-toggle` = "popover",
                      `data-placement` = "right",
                      title = "Factor name",
                      `data-content` = "Use a clear alphanumeric name. It appears in coefficient names (resultsNames) and in contrast specifications."
                    )
                  )
                ),
              
              # Advanced: Add Covariates (collapsible panel)
              br(),
              div(
                style = "margin-bottom: 15px;",
                tags$a(
                  id = "toggle_covariates",
                  href = "#covariates_panel",
                  `data-toggle` = "collapse",
                  class = "btn btn-outline-secondary btn-sm",
                  style = "text-decoration: none;",
                  icon("cog"),
                  span(" Advanced: Add Covariates", style = "margin-left: 5px;"),
                  tags$span(
                    icon("info-circle"),
                    style = "color: #3498DB; cursor: pointer; margin-left: 8px;",
                    tabindex = "0",
                    `data-toggle` = "popover",
                    `data-placement` = "right",
                    title = "Covariates",
                    `data-content` = "Add covariates to control for batch, time, or subject effects. The factor of interest is tested while adjusting for these. Covariates are additive only (no interactions)."
                  )
                ),
                
                # Collapsible panel content
                div(
                  id = "covariates_panel",
                  class = "collapse",
                  style = "margin-top: 15px; padding: 15px; background-color: #f9f9f9; border: 1px solid #ddd; border-radius: 5px;",
                  
                  fluidRow(
                    column(6,
                      selectizeInput(
                        "covariates",
                        "Select Covariates:",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(
                          placeholder = 'Select metadata columns',
                          maxItems = 4
                        )
                      ),
                      div(
                        style = "font-size: 11px; color: #666; margin-top: -10px;",
                        "Select metadata columns to include as covariates. Categorical covariates will have reference levels set; numeric covariates are included as-is.",
                        tags$span(
                          icon("info-circle"),
                          style = "color: #3498DB; cursor: pointer; margin-left: 6px;",
                          tabindex = "0",
                          `data-toggle` = "popover",
                          `data-placement` = "right",
                          title = "Covariate selection",
                          `data-content` = "Choose columns from your metadata to control for unwanted variation. Ensure covariates are not perfectly confounded with your contrast groups."
                        )
                      )
                    ),
                    column(6,
                      uiOutput("covariate_reference_controls"),
                      br(),
                      actionButton(
                        "reset_covariates",
                        "Reset to Simple Design",
                        icon = icon("undo"),
                        class = "btn-outline-warning btn-sm"
                      )
                    )
                  ),
                  
                  hr(),
                  
                  # Design formula preview
                  div(
                    style = "background-color: #fff; padding: 10px; border-radius: 4px; border: 1px solid #ddd;",
                    uiOutput("design_formula_preview")
                  ),
                  
                  # Design warnings
                  uiOutput("design_warnings")
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
      
      # Contrast Import/Export Section
      fluidRow(
        column(12,
          wellPanel(
            style = "background-color: #f0f8ff; border-left: 4px solid #3498DB;",
            h4("Contrast Management", style = "color: #2980B9; margin-top: 0;"),
            p("Save your contrasts to a file or load previously saved contrasts.",
              style = "color: #666; font-size: 12px; margin-bottom: 15px;"),
            fluidRow(
              column(6,
                fileInput(
                  "import_contrasts_file",
                  "Import Contrasts from CSV:",
                  accept = ".csv",
                  width = "100%",
                  buttonLabel = "Browse...",
                  placeholder = "No file selected"
                ),
                p("Upload a CSV file with saved contrast definitions",
                  style = "font-size: 10px; color: #666; margin-top: -10px;")
              ),
              column(6,
                br(),
                downloadButton(
                  "export_contrasts",
                  "Export Contrasts to CSV",
                  icon = icon("download"),
                  class = "btn-primary",
                  style = "margin-top: 5px;"
                ),
                p("Download all created contrasts as CSV file",
                  style = "font-size: 10px; color: #666; margin-top: 5px;")
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
      )
    )
}
