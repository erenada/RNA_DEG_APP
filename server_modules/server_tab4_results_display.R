# Server logic for Tab 4: Results Display
# Author: Eren Ada, PhD
#
# This file contains all server-side logic for displaying analysis results

# =============================================================================
# TAB 4: RESULTS DISPLAY
# =============================================================================

# Control results availability for conditional panels
output$results_available <- reactive({
  !is.null(values$deseq_results) && length(values$deseq_results) > 0
})
outputOptions(output, "results_available", suspendWhenHidden = FALSE)

# Dynamic tabbed interface for results
output$results_tabs_ui <- renderUI({
  req(values$deseq_results)
  
  contrast_names <- names(values$deseq_results)
  
  if (length(contrast_names) == 0) {
    return(div(
      style = "text-align: center; margin-top: 50px;",
      p("No contrast results available.", style = "color: #7F8C8D;")
    ))
  }
  
  # Create tab panels for each contrast
  tab_panels <- lapply(contrast_names, function(contrast_name) {
    safe_name <- gsub("[^A-Za-z0-9]", "_", contrast_name)
    tabPanel(
      title = contrast_name,
      value = paste0("results_", contrast_name),
      
      br(),
      
      # Summary statistics panel for this contrast
      wellPanel(
        style = "background-color: #F8F9FA; border-left: 4px solid #2196F3;",
        h4(paste("Contrast:", contrast_name), style = "color: #2980B9; margin-top: 0;"),
        uiOutput(paste0("results_summary_", safe_name))
      ),
      
      # Proceed to Enrichment button
      div(
        style = "text-align: center; margin-top: 15px; margin-bottom: 15px;",
        actionButton(
          paste0("proceed_to_enrichment_", safe_name),
          "Proceed to Enrichment Analysis",
          icon = icon("arrow-right"),
          class = "btn-primary",
          style = "padding: 10px 30px; font-size: 16px;"
        ),
        br(),
        p("Perform GO enrichment analysis on significant genes from this contrast",
          style = "color: #666; font-size: 12px; margin-top: 10px; font-style: italic;")
      ),
      
      br(),
      
      # Filter and Export controls
      fluidRow(
        column(6,
          div(
            style = "margin-bottom: 10px;",
            radioButtons(
              paste0("show_genes_", safe_name),
              "Display:",
              choices = c(
                "Significant genes only" = "significant",
                "All genes" = "all"
              ),
              selected = "significant",
              inline = TRUE
            )
          )
        ),
        column(6,
          div(
            style = "margin-bottom: 10px; text-align: right;",
            # All genes exports
            downloadButton(
              paste0("download_csv_all_", safe_name),
              "Export All (CSV)",
              icon = icon("file-csv"),
              class = "btn-outline-primary btn-sm",
              style = "margin-right: 5px;"
            ),
            downloadButton(
              paste0("download_excel_all_", safe_name),
              "Export All (Excel)",
              icon = icon("file-excel"),
              class = "btn-outline-success btn-sm",
              style = "margin-right: 10px;"
            ),
            # Significant genes only exports
            downloadButton(
              paste0("download_csv_sig_", safe_name),
              "Export Significant (CSV)",
              icon = icon("file-csv"),
              class = "btn-primary btn-sm",
              style = "margin-right: 5px;"
            ),
            downloadButton(
              paste0("download_excel_sig_", safe_name),
              "Export Significant (Excel)",
              icon = icon("file-excel"),
              class = "btn-success btn-sm"
            )
          )
        )
      ),
      
      # Results table
      div(
        style = "margin-top: 10px;",
        DT::dataTableOutput(paste0("results_table_", safe_name))
      )
    )
  })
  
  # Create the tabset panel with all contrast tabs
  do.call(tabsetPanel, c(
    list(id = "results_contrast_tabs", type = "tabs"),
    tab_panels
  ))
})

# Render results tables and summaries for each contrast
observe({
  req(values$deseq_results)
  
  contrast_names <- names(values$deseq_results)
  
  lapply(contrast_names, function(contrast_name) {
    local({
      my_contrast <- contrast_name
      safe_name <- gsub("[^A-Za-z0-9]", "_", my_contrast)
      
      # Render summary for this contrast
      output[[paste0("results_summary_", safe_name)]] <- renderUI({
        req(values$analysis_summary[[my_contrast]])
        
        summary_data <- values$analysis_summary[[my_contrast]]
        
        fluidRow(
          column(6,
            div(
              style = "font-size: 14px;",
              div(
                strong("Comparison: "),
                span(paste0(summary_data$group1, " (n=", summary_data$group1_samples, ") vs ", 
                           summary_data$group2, " (n=", summary_data$group2_samples, ")"),
                     style = "color: #34495E;")
              ),
              div(
                style = "margin-top: 8px;",
                strong("Total Genes: "),
                span(summary_data$total_genes, style = "color: #2980B9; font-weight: bold;")
              )
            )
          ),
          column(6,
            div(
              style = "font-size: 14px;",
              div(
                strong("Total Genes: "),
                span(summary_data$total_genes, style = "color: #7F8C8D; font-weight: bold;"),
                span(" | ", style = "color: #BDC3C7;"),
                strong("Significant: "),
                span(summary_data$significant_genes, style = "color: #E67E22; font-weight: bold;"),
                span(" | ", style = "color: #BDC3C7;"),
                strong("Up: "),
                span(summary_data$upregulated, style = "color: #27AE60; font-weight: bold;"),
                span(" | ", style = "color: #BDC3C7;"),
                strong("Down: "),
                span(summary_data$downregulated, style = "color: #E74C3C; font-weight: bold;")
              ),
              div(
                style = "margin-top: 8px; font-size: 12px; color: #7F8C8D; font-style: italic;",
                icon("info-circle"),
                " Table shows significant genes by default. Use 'Display' option above to show all genes."
              )
            )
          )
        )
      })
      
      # CSV download handler - ALL GENES
      output[[paste0("download_csv_all_", safe_name)]] <- downloadHandler(
        filename = function() {
          paste0(my_contrast, "_all_genes_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        },
        content = function(file) {
          results_df <- values$deseq_results[[my_contrast]]$results
          
          # Get analysis parameters
          alpha <- values$overall_summary$analysis_parameters$alpha_level
          lfc_threshold <- values$overall_summary$analysis_parameters$lfc_threshold
          
          # Add direction column using filtering approach
          # Filter by: padj < alpha AND abs(log2FoldChange) > threshold
          if ("padj" %in% colnames(results_df)) {
            if (lfc_threshold > 0) {
              # Apply magnitude filter
              results_df$direction <- ifelse(
                results_df$padj < alpha & !is.na(results_df$padj) & 
                  abs(results_df$log2FoldChange) > lfc_threshold,
                ifelse(
                  results_df$log2FoldChange > 0,
                  "Up",
                  "Down"
                ),
                "NS"
              )
            } else {
              # No magnitude filter, just significance
              results_df$direction <- ifelse(
                results_df$padj < alpha & !is.na(results_df$padj),
                ifelse(
                  results_df$log2FoldChange > 0,
                  "Up",
                  "Down"
                ),
                "NS"
              )
            }
          } else {
            # If padj doesn't exist, use threshold-based classification
            results_df$direction <- ifelse(
              results_df$log2FoldChange > lfc_threshold,
              "Up",
              ifelse(
                results_df$log2FoldChange < -lfc_threshold,
                "Down",
                "NS"
              )
            )
          }
          
          # Reorder columns
          base_cols <- c("gene", "direction", "baseMean", "log2FoldChange", "lfcSE")
          optional_cols <- c("stat", "pvalue", "padj", "contrast")
          available_optional <- optional_cols[optional_cols %in% colnames(results_df)]
          export_cols <- c(base_cols, available_optional)
          
          results_export <- results_df[, export_cols, drop = FALSE]
          write.csv(results_export, file, row.names = FALSE)
        }
      )
      
      # Excel download handler - ALL GENES
      output[[paste0("download_excel_all_", safe_name)]] <- downloadHandler(
        filename = function() {
          paste0(my_contrast, "_all_genes_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
        },
        content = function(file) {
          results_df <- values$deseq_results[[my_contrast]]$results
          
          # Get analysis parameters
          alpha <- values$overall_summary$analysis_parameters$alpha_level
          lfc_threshold <- values$overall_summary$analysis_parameters$lfc_threshold
          
          # Add direction column using filtering approach
          # Filter by: padj < alpha AND abs(log2FoldChange) > threshold
          if ("padj" %in% colnames(results_df)) {
            if (lfc_threshold > 0) {
              # Apply magnitude filter
              results_df$direction <- ifelse(
                results_df$padj < alpha & !is.na(results_df$padj) & 
                  abs(results_df$log2FoldChange) > lfc_threshold,
                ifelse(
                  results_df$log2FoldChange > 0,
                  "Up",
                  "Down"
                ),
                "NS"
              )
            } else {
              # No magnitude filter, just significance
              results_df$direction <- ifelse(
                results_df$padj < alpha & !is.na(results_df$padj),
                ifelse(
                  results_df$log2FoldChange > 0,
                  "Up",
                  "Down"
                ),
                "NS"
              )
            }
          } else {
            # If padj doesn't exist, use threshold-based classification
            results_df$direction <- ifelse(
              results_df$log2FoldChange > lfc_threshold,
              "Up",
              ifelse(
                results_df$log2FoldChange < -lfc_threshold,
                "Down",
                "NS"
              )
            )
          }
          
          # Reorder columns
          base_cols <- c("gene", "direction", "baseMean", "log2FoldChange", "lfcSE")
          optional_cols <- c("stat", "pvalue", "padj", "contrast")
          available_optional <- optional_cols[optional_cols %in% colnames(results_df)]
          export_cols <- c(base_cols, available_optional)
          
          results_export <- results_df[, export_cols, drop = FALSE]
          
          # Create Excel workbook
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "Results")
          openxlsx::writeData(wb, "Results", results_export)
          
          # Add some formatting
          header_style <- openxlsx::createStyle(
            textDecoration = "bold",
            border = "bottom",
            fontSize = 11
          )
          openxlsx::addStyle(wb, "Results", header_style, rows = 1, cols = 1:ncol(results_export), gridExpand = TRUE)
          
          # Save workbook
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        }
      )
      
      # CSV download handler - SIGNIFICANT GENES ONLY
      output[[paste0("download_csv_sig_", safe_name)]] <- downloadHandler(
        filename = function() {
          paste0(my_contrast, "_significant_genes_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        },
        content = function(file) {
          results_df <- values$deseq_results[[my_contrast]]$results
          
          # Get analysis parameters
          alpha <- values$overall_summary$analysis_parameters$alpha_level
          lfc_threshold <- values$overall_summary$analysis_parameters$lfc_threshold
          
          # Add direction column using filtering approach
          if ("padj" %in% colnames(results_df)) {
            if (lfc_threshold > 0) {
              results_df$direction <- ifelse(
                results_df$padj < alpha & !is.na(results_df$padj) & 
                  abs(results_df$log2FoldChange) > lfc_threshold,
                ifelse(results_df$log2FoldChange > 0, "Up", "Down"),
                "NS"
              )
            } else {
              results_df$direction <- ifelse(
                results_df$padj < alpha & !is.na(results_df$padj),
                ifelse(results_df$log2FoldChange > 0, "Up", "Down"),
                "NS"
              )
            }
          } else {
            results_df$direction <- ifelse(
              results_df$log2FoldChange > lfc_threshold,
              "Up",
              ifelse(results_df$log2FoldChange < -lfc_threshold, "Down", "NS")
            )
          }
          
          # Filter for significant genes only
          results_df <- results_df[results_df$direction != "NS", ]
          
          # Determine columns for export
          base_cols <- c("gene", "direction", "baseMean", "log2FoldChange", "lfcSE")
          optional_cols <- c("stat", "pvalue", "padj")
          available_optional <- optional_cols[optional_cols %in% colnames(results_df)]
          export_cols <- c(base_cols, available_optional)
          
          results_export <- results_df[, export_cols, drop = FALSE]
          write.csv(results_export, file, row.names = FALSE)
          
          showNotification(
            paste0("Exported ", nrow(results_export), " significant genes"),
            type = "message",
            duration = 3
          )
        }
      )
      
      # Excel download handler - SIGNIFICANT GENES ONLY
      output[[paste0("download_excel_sig_", safe_name)]] <- downloadHandler(
        filename = function() {
          paste0(my_contrast, "_significant_genes_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
        },
        content = function(file) {
          results_df <- values$deseq_results[[my_contrast]]$results
          
          # Get analysis parameters
          alpha <- values$overall_summary$analysis_parameters$alpha_level
          lfc_threshold <- values$overall_summary$analysis_parameters$lfc_threshold
          
          # Add direction column using filtering approach
          if ("padj" %in% colnames(results_df)) {
            if (lfc_threshold > 0) {
              results_df$direction <- ifelse(
                results_df$padj < alpha & !is.na(results_df$padj) & 
                  abs(results_df$log2FoldChange) > lfc_threshold,
                ifelse(results_df$log2FoldChange > 0, "Up", "Down"),
                "NS"
              )
            } else {
              results_df$direction <- ifelse(
                results_df$padj < alpha & !is.na(results_df$padj),
                ifelse(results_df$log2FoldChange > 0, "Up", "Down"),
                "NS"
              )
            }
          } else {
            results_df$direction <- ifelse(
              results_df$log2FoldChange > lfc_threshold,
              "Up",
              ifelse(results_df$log2FoldChange < -lfc_threshold, "Down", "NS")
            )
          }
          
          # Filter for significant genes only
          results_df <- results_df[results_df$direction != "NS", ]
          
          # Determine columns for export
          base_cols <- c("gene", "direction", "baseMean", "log2FoldChange", "lfcSE")
          optional_cols <- c("stat", "pvalue", "padj")
          available_optional <- optional_cols[optional_cols %in% colnames(results_df)]
          export_cols <- c(base_cols, available_optional)
          
          results_export <- results_df[, export_cols, drop = FALSE]
          
          # Create Excel workbook
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "Significant Genes")
          openxlsx::writeData(wb, "Significant Genes", results_export)
          
          # Add some formatting
          header_style <- openxlsx::createStyle(
            textDecoration = "bold",
            border = "bottom",
            fontSize = 11
          )
          openxlsx::addStyle(wb, "Significant Genes", header_style, rows = 1, cols = 1:ncol(results_export), gridExpand = TRUE)
          
          # Save workbook
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
          
          showNotification(
            paste0("Exported ", nrow(results_export), " significant genes"),
            type = "message",
            duration = 3
          )
        }
      )
      
      # Render results table for this contrast
      output[[paste0("results_table_", safe_name)]] <- DT::renderDataTable({
        req(values$deseq_results[[my_contrast]])
        req(values$overall_summary)
        
        results_df <- values$deseq_results[[my_contrast]]$results
        
        # Get analysis parameters for direction calculation
        alpha <- values$overall_summary$analysis_parameters$alpha_level
        lfc_threshold <- values$overall_summary$analysis_parameters$lfc_threshold
        
        # Add direction column using filtering approach
        # Filter by: padj < alpha AND abs(log2FoldChange) > threshold
        if ("padj" %in% colnames(results_df)) {
          if (lfc_threshold > 0) {
            # Apply magnitude filter
            results_df$direction <- ifelse(
              results_df$padj < alpha & !is.na(results_df$padj) & 
                abs(results_df$log2FoldChange) > lfc_threshold,
              ifelse(
                results_df$log2FoldChange > 0,
                "Up",
                "Down"
              ),
              "NS"
            )
          } else {
            # No magnitude filter, just significance
            results_df$direction <- ifelse(
              results_df$padj < alpha & !is.na(results_df$padj),
              ifelse(
                results_df$log2FoldChange > 0,
                "Up",
                "Down"
              ),
              "NS"
            )
          }
        } else {
          # If padj doesn't exist, use threshold-based classification
          results_df$direction <- ifelse(
            results_df$log2FoldChange > lfc_threshold,
            "Up",
            ifelse(
              results_df$log2FoldChange < -lfc_threshold,
              "Down",
              "NS"
            )
          )
        }
        
        # Filter based on user selection
        filter_option <- input[[paste0("show_genes_", safe_name)]]
        if (is.null(filter_option)) filter_option <- "significant"  # Default
        
        if (filter_option == "significant") {
          # Show only significant genes (not NS)
          results_df_filtered <- results_df[results_df$direction != "NS", ]
          message(paste("Showing significant genes:", nrow(results_df_filtered), "out of", nrow(results_df)))
        } else {
          # Show all genes
          results_df_filtered <- results_df
          message(paste("Showing all genes:", nrow(results_df_filtered)))
        }
        
        # Determine which columns are available (apeglm shrinkage removes stat column)
        base_cols <- c("gene", "direction", "baseMean", "log2FoldChange", "lfcSE")
        optional_cols <- c("stat", "pvalue", "padj")
        available_optional <- optional_cols[optional_cols %in% colnames(results_df_filtered)]
        display_cols <- c(base_cols, available_optional)
        
        results_display <- results_df_filtered[, display_cols, drop = FALSE]
        
        # Round numeric columns for cleaner display
        results_display$baseMean <- round(results_display$baseMean, 2)
        results_display$log2FoldChange <- round(results_display$log2FoldChange, 3)
        results_display$lfcSE <- round(results_display$lfcSE, 3)
        
        # Round optional columns if they exist
        if ("stat" %in% colnames(results_display)) {
          results_display$stat <- round(results_display$stat, 3)
        }
        if ("pvalue" %in% colnames(results_display)) {
          results_display$pvalue <- formatC(results_display$pvalue, format = "e", digits = 2)
        }
        if ("padj" %in% colnames(results_display)) {
          results_display$padj <- formatC(results_display$padj, format = "e", digits = 2)
        }
        
        # Find padj column index for sorting (if it exists)
        padj_col_idx <- which(colnames(results_display) == "padj") - 1  # 0-indexed for DT
        sort_order <- if(length(padj_col_idx) > 0) list(list(padj_col_idx, 'asc')) else list(list(0, 'asc'))
        
        DT::datatable(
          results_display,
          options = list(
            scrollX = TRUE,
            scrollY = "500px",
            pageLength = 25,
            lengthMenu = c(10, 25, 50, 100),
            order = sort_order,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel'),
            searchHighlight = TRUE
          ),
          rownames = FALSE,
          filter = 'top',
          class = 'cell-border stripe hover',
          caption = paste0("DESeq2 Results: ", my_contrast)
        )
      })
    })
  })
})

# =============================================================================
# PROCEED TO ENRICHMENT NAVIGATION
# =============================================================================

# Handle "Proceed to Enrichment" button clicks for each contrast
observe({
  req(values$deseq_results)
  
  contrast_names <- names(values$deseq_results)
  
  lapply(contrast_names, function(contrast_name) {
    safe_name <- gsub("[^A-Za-z0-9]", "_", contrast_name)
    button_id <- paste0("proceed_to_enrichment_", safe_name)
    
    observeEvent(input[[button_id]], {
      # Navigate to Enrichment tab
      updateTabsetPanel(session, "main_tabs", selected = "tab_enrichment")
      
      # Pre-select this contrast in enrichment tab (use selectizeInput for multi-select support)
      updateSelectizeInput(session, "enrich_contrast", selected = contrast_name)
      
      # Show notification
      showNotification(
        paste0("Navigated to Enrichment Analysis for: ", contrast_name),
        type = "message",
        duration = 3
      )
      
      message(paste("User navigated to Enrichment tab for contrast:", contrast_name))
    })
  })
})
