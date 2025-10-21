# DESeq2 Configuration Tab UI Builder
# Author: Eren Ada, PhD
#
# Returns the DESeq2 Configuration tabPanel

build_config_tab <- function() {
  tabPanel(
      title = "DESeq2 Configuration",
      value = "tab_config",
      icon = icon("cogs"),
      
      br(),
      
      # Configuration Header
      fluidRow(
        column(12,
          div(
            style = "text-align: center; margin-bottom: 20px;",
            h2("DESeq2 Analysis Configuration", style = "color: #2C3E50;"),
            p("Configure parameters for differential expression analysis", 
              style = "color: #7F8C8D; font-size: 14px;")
          )
        )
      ),
      
      # Main Configuration Panel
      fluidRow(
        column(12,
          wellPanel(
            style = "background-color: #f8f9fa; border-left: 4px solid #2980B9;",
            
            # Test Type Section
            fluidRow(
              column(6,
                h4("Statistical Test", style = "color: #2980B9; margin-bottom: 15px;"),
                radioButtons(
                  "test_type",
                  "Test Type:",
                  choices = list(
                    "Wald test (pairwise comparisons)" = "Wald",
                    "Likelihood Ratio Test (LRT)" = "LRT"
                  ),
                  selected = "Wald",
                  width = "100%"
                ),
                div(
                  style = "font-size: 11px; color: #666; margin-top: -10px;",
                  "Use Wald for standard pairwise contrasts; use LRT to test multiple coefficients jointly via a reduced model.",
                  tags$span(
                    icon("info-circle"),
                    style = "color: #3498DB; cursor: pointer; margin-left: 6px;",
                    tabindex = "0",
                    `data-toggle` = "popover",
                    `data-placement` = "right",
                    title = "Test type",
                    `data-content` = "Wald tests single coefficients; LRT compares a full vs reduced design to ask if removed terms explain variation. Shrinkage applies to Wald, not LRT. For time-series or multi-factor omnibus tests, prefer LRT."
                  )
                ),
                conditionalPanel(
                  condition = "input.test_type == 'LRT'",
                  textInput(
                    "lrt_reduced_formula",
                    "Reduced Formula:",
                    value = "~ 1",
                    placeholder = "e.g., ~ 1 or ~ batch",
                    width = "100%"
                  ),
                  div(
                    style = "font-size: 11px; color: #666; margin-top: -10px;",
                    "For LRT: specify the reduced model formula (terms to test)"
                  )
                )
              ),
              
              # Significance Thresholds
              column(6,
                h4("Significance Thresholds", style = "color: #2980B9; margin-bottom: 15px;"),
                numericInput(
                  "alpha_level",
                  "Alpha Level (FDR cutoff):",
                  value = 0.05,
                  min = 0.001,
                  max = 0.2,
                  step = 0.01,
                  width = "100%"
                ),
                div(
                  style = "font-size: 11px; color: #666; margin-top: -10px;",
                  "Controls adjusted p-value cutoff; typical values 0.1 or 0.05.",
                  tags$span(
                    icon("info-circle"),
                    style = "color: #3498DB; cursor: pointer; margin-left: 6px;",
                    tabindex = "0",
                    `data-toggle` = "popover",
                    `data-placement` = "right",
                    title = "Alpha (FDR)",
                    `data-content` = "results(dds, alpha=...) tunes independent filtering to maximize rejections at this FDR. 0.1 is a common default in the vignette; 0.05 is also used."
                  )
                ),
                numericInput(
                  "lfc_threshold",
                  "Log2 Fold Change Threshold:",
                  value = 0,
                  min = 0,
                  max = 5,
                  step = 0.1,
                  width = "100%"
                ),
                div(
                  style = "font-size: 11px; color: #666; margin-top: -10px;",
                  "Minimum absolute log2FC for significance",
                  tags$span(
                    icon("info-circle"),
                    style = "color: #3498DB; cursor: pointer; margin-left: 6px;",
                    tabindex = "0",
                    `data-toggle` = "popover",
                    `data-placement` = "right",
                    title = "LFC threshold",
                    `data-content` = "Magnitude filter after testing. For thresholded Wald tests, DESeq2 supports lfcThreshold with altHypothesis in results(). Otherwise, filter by absolute LFC downstream."
                  )
                )
              )
            ),
            
            hr(),
            
            # Log Fold Change Shrinkage
            fluidRow(
              column(6,
                h4("Log Fold Change Shrinkage", style = "color: #2980B9; margin-bottom: 15px;"),
                checkboxInput(
                  "apply_lfc_shrinkage",
                  "Apply LFC shrinkage",
                  value = TRUE,
                  width = "100%"
                ),
                conditionalPanel(
                  condition = "input.test_type == 'LRT'",
                  div(
                    style = "font-size: 11px; color: #666; margin-top: -10px;",
                    "Note: Shrinkage is not applied with LRT."
                  )
                ),
                conditionalPanel(
                  condition = "input.apply_lfc_shrinkage",
                  selectInput(
                    "shrinkage_type",
                    "Shrinkage Method:",
                    choices = list(
                      "apeglm (recommended)" = "apeglm",
                      "ashr (alternative)" = "ashr", 
                      "normal (basic)" = "normal"
                    ),
                    selected = "apeglm",
                    width = "100%"
                  ),
                  div(
                    style = "font-size: 11px; color: #666; margin-top: -10px;",
                    "apeglm is recommended for better LFC estimates",
                    tags$span(
                      icon("info-circle"),
                      style = "color: #3498DB; cursor: pointer; margin-left: 6px;",
                      tabindex = "0",
                      `data-toggle` = "popover",
                      `data-placement` = "right",
                      title = "Shrinkage guidance",
                      `data-content` = "Use shrinkage for effect-size estimation, ranking, and plots. apeglm preserves large effects; ashr is an alternative, especially with numeric contrasts. Do not interpret shrunken p-values; rely on unshrunken tests."
                    ),
                    br(),
                    span("Note: Robust coefficient matching with ashr fallback", 
                         style = "color: #2E8B57; font-weight: bold;")
                  )
                )
              ),
              
              # Multiple Testing Correction
              column(6,
                h4("Multiple Testing Correction", style = "color: #2980B9; margin-bottom: 15px;"),
                selectInput(
                  "p_adjust_method",
                  "Adjustment Method:",
                  choices = list(
                    "Benjamini-Hochberg (FDR)" = "BH",
                    "Bonferroni" = "bonferroni",
                    "Benjamini-Yekutieli" = "BY",
                    "Holm" = "holm"
                  ),
                  selected = "BH",
                  width = "100%"
                ),
                div(
                  style = "font-size: 11px; color: #666; margin-top: -10px;",
                  "Benjamini-Hochberg controls false discovery rate",
                  tags$span(
                    icon("info-circle"),
                    style = "color: #3498DB; cursor: pointer; margin-left: 6px;",
                    tabindex = "0",
                    `data-toggle` = "popover",
                    `data-placement` = "right",
                    title = "P-value adjustment",
                    `data-content` = "BH is recommended for RNA-seq FDR control. Change only for specific benchmarking or methods work."
                  )
                ),
                br(),
                selectInput(
                  "dispersion_fit_type",
                  "Dispersion Estimation:",
                  choices = list(
                    "Parametric (default)" = "parametric",
                    "Local regression" = "local", 
                    "Mean of gene-wise" = "mean"
                  ),
                  selected = "parametric",
                  width = "100%"
                ),
                div(
                  style = "font-size: 11px; color: #666; margin-top: -10px;",
                  "Method for fitting dispersion-mean relationship",
                  tags$span(
                    icon("info-circle"),
                    style = "color: #3498DB; cursor: pointer; margin-left: 6px;",
                    tabindex = "0",
                    `data-toggle` = "popover",
                    `data-placement` = "right",
                    title = "Dispersion fit",
                    `data-content` = "Parametric works for most datasets. If the fit is inadequate, use local. Mean is a fallback. Inspect dispersion plots to diagnose."
                  )
                )
              )
            ),
            
            hr(),
            
            # Pre-filtering Options
            fluidRow(
              column(12,
                h4("Pre-filtering Options", style = "color: #2980B9; margin-bottom: 15px;"),
                
                fluidRow(
                  column(4,
                    checkboxInput(
                      "apply_prefiltering",
                      "Apply pre-filtering",
                      value = FALSE,
                      width = "100%"
                    )
                  ),
                  column(4,
                    conditionalPanel(
                      condition = "input.apply_prefiltering",
                      numericInput(
                        "min_count_threshold",
                        "Minimum Count:",
                        value = 10,
                        min = 1,
                        max = 100,
                        step = 1,
                        width = "100%"
                      )
                    )
                  ),
                  column(4,
                    conditionalPanel(
                      condition = "input.apply_prefiltering",
                      numericInput(
                        "min_samples_threshold",
                        "Min Samples:",
                        value = 3,
                        min = 1,
                        max = 20,
                        step = 1,
                        width = "100%"
                      )
                    )
                  )
                ),
                
                conditionalPanel(
                  condition = "input.apply_prefiltering",
                  div(
                    style = "font-size: 11px; color: #666; margin-top: 5px;",
                    "Keep genes with >= minimum count in >= minimum samples",
                    tags$span(
                      icon("info-circle"),
                      style = "color: #3498DB; cursor: pointer; margin-left: 6px;",
                      tabindex = "0",
                      `data-toggle` = "popover",
                      `data-placement` = "right",
                      title = "Pre-filtering",
                      `data-content` = "Optional step to reduce memory and runtime; helpful for visualization. DESeq2 still performs independent filtering during results()."
                    )
                  )
                )
              )
            )
          ),
          # DESeq2 automatic filtering/outlier note
          div(
            style = "background-color: #f0f8ff; border-left: 4px solid #3498DB; padding: 10px; margin-top: 10px; border-radius: 4px;",
            icon("info-circle", style = "color: #3498DB;"),
            span(
              " DESeq2 applies independent filtering and Cook's distance outlier handling by default.",
              style = "font-size: 12px; color: #2C3E50; margin-left: 6px;"
            )
          )
        )
      ),
      
      # Configuration Summary and Controls
      fluidRow(
        column(12,
          wellPanel(
            style = "background-color: #f8f9fa; border-left: 4px solid #2980B9;",
            h4("Configuration Summary", style = "color: #2980B9; margin-top: 0;"),
            uiOutput("config_summary"),
            
            br(),
            div(
              style = "text-align: center;",
              actionButton(
                "back_to_design",
                "Back: Sample Design",
                icon = icon("arrow-left"),
                class = "btn-secondary",
                style = "margin-right: 10px;"
              ),
              actionButton(
                "run_analysis",
                "Run DESeq2 Analysis",
                icon = icon("play"),
                class = "btn-success btn-lg",
                disabled = TRUE
              )
            )
          )
        )
      ),
      
      # Analysis Completion Message (shown after analysis)
      conditionalPanel(
        condition = "output.show_results_panel",
        fluidRow(
          column(12,
            wellPanel(
              style = "background-color: #f8f9fa; border-left: 4px solid #2980B9; text-align: center;",
              h4("Analysis Completed Successfully", style = "color: #2980B9; margin-bottom: 15px;"),
              p(uiOutput("analysis_completion_message", inline = TRUE), style = "color: #666; font-size: 14px;"),
              br(),
              div(
                style = "margin-top: 10px;",
                actionButton(
                  "go_to_results",
                  "View Results",
                  icon = icon("arrow-right"),
                  class = "btn-success btn-lg"
                ),
                downloadButton(
                  "download_results",
                  "Download Results",
                  icon = icon("download"),
                  class = "btn-outline-success",
                  style = "margin-left: 10px;"
                )
              )
            )
          )
        )
      )
    )
}
