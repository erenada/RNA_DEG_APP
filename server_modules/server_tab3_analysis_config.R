# Server logic for Tab 3: Analysis Configuration
# Author: Eren Ada, PhD
#
# This file contains all server-side logic for DESeq2 configuration and execution

# =============================================================================
# TAB 3: ANALYSIS CONFIGURATION
# =============================================================================

# Configuration summary
output$config_summary <- renderUI({
  req(input$test_type, input$alpha_level, input$lfc_threshold, input$p_adjust_method)
  
  # Build configuration summary
  summary_items <- list()
  
  # Test type
  test_desc <- switch(input$test_type,
    "Wald" = "Wald test (pairwise comparisons)",
    "LRT" = paste0(
      "Likelihood Ratio Test (reduced: ",
      if (is.null(input$lrt_reduced_formula) || !nzchar(trimws(input$lrt_reduced_formula))) "~ 1" else input$lrt_reduced_formula,
      ")"
    )
  )
  summary_items <- append(summary_items, list(
    div(strong("Test Type: "), span(test_desc, style = "color: #2980B9;"))
  ))
  
  # Significance thresholds
  summary_items <- append(summary_items, list(
    div(strong("Alpha Level: "), span(input$alpha_level, style = "color: #27AE60;")),
    div(strong("LFC Threshold: "), span(input$lfc_threshold, style = "color: #27AE60;"))
  ))
  
  # LFC shrinkage
  if (input$test_type == "LRT") {
    shrinkage_desc <- "No (disabled for LRT)"
  } else if (input$apply_lfc_shrinkage) {
    shrinkage_desc <- paste0("Yes (", input$shrinkage_type, ")")
  } else {
    shrinkage_desc <- "No"
  }
  summary_items <- append(summary_items, list(
    div(strong("LFC Shrinkage: "), span(shrinkage_desc, style = "color: #E67E22;"))
  ))
  
  # Multiple testing correction
  summary_items <- append(summary_items, list(
    div(strong("P-value Adjustment: "), span(input$p_adjust_method, style = "color: #8E44AD;")),
    div(strong("Dispersion Estimation: "), span(input$dispersion_fit_type, style = "color: #9B59B6;"))
  ))
  
  # Pre-filtering
  if (input$apply_prefiltering) {
    prefilter_desc <- paste0("Yes (≥", input$min_count_threshold, " counts in ≥", input$min_samples_threshold, " samples)")
  } else {
    prefilter_desc <- "No"
  }
  summary_items <- append(summary_items, list(
    div(strong("Pre-filtering: "), span(prefilter_desc, style = "color: #16A085;"))
  ))
  
  # Contrasts info
  num_contrasts <- length(values$created_contrasts)
  summary_items <- append(summary_items, list(
    br(),
    div(strong("Contrasts to Analyze: "), span(num_contrasts, style = "color: #C0392B; font-size: 16px; font-weight: bold;"))
  ))
  
  if (num_contrasts > 0) {
    contrast_names <- names(values$created_contrasts)
    summary_items <- append(summary_items, list(
      div(style = "margin-top: 5px; font-size: 12px; color: #666;",
          paste("•", contrast_names, collapse = " "))
    ))
  }
  
  do.call(div, summary_items)
})

# Enable/disable run analysis button based on validation
observe({
  has_contrasts <- length(values$created_contrasts) > 0
  has_data <- !is.null(values$count_data) && !is.null(values$metadata)
  
  # Basic validation for required parameters
  valid_config <- !is.null(input$test_type) && 
                 !is.null(input$alpha_level) && 
                 input$alpha_level > 0 && input$alpha_level <= 1 &&
                 !is.null(input$lfc_threshold) && 
                 input$lfc_threshold >= 0
  
  # Additional validation for LRT
  if (!is.null(input$test_type) && input$test_type == "LRT") {
    valid_config <- valid_config && 
                   !is.null(input$lrt_reduced_formula) && 
                   nchar(trimws(input$lrt_reduced_formula)) > 0
  }
  
  # Additional validation for pre-filtering
  if (!is.null(input$apply_prefiltering) && input$apply_prefiltering) {
    valid_config <- valid_config && 
                   !is.null(input$min_count_threshold) && 
                   input$min_count_threshold > 0 &&
                   !is.null(input$min_samples_threshold) && 
                   input$min_samples_threshold > 0
  }
  
  can_run <- has_contrasts && has_data && valid_config
  
  updateActionButton(session, "run_analysis", 
                    label = if (can_run) "Run DESeq2 Analysis" else "Run DESeq2 Analysis (Configure First)")
  
  shinyjs::toggleState("run_analysis", condition = can_run)
})

# Back to design tab
observeEvent(input$back_to_design, {
  updateTabsetPanel(session, "main_tabs", selected = "tab_design")
})

# DESeq2 Analysis Execution
observeEvent(input$run_analysis, {
  req(length(values$created_contrasts) > 0)
  req(values$count_data, values$metadata)
  
  # Disable the button during analysis
  shinyjs::disable("run_analysis")
  
  # Show progress with detailed steps
  withProgress(message = 'Running DESeq2 Analysis...', value = 0, {
    
    tryCatch({
      # Step 1: Data preparation and validation
      incProgress(0.1, detail = "Preparing data...")
      
      # Validate data integrity
      if (nrow(values$count_data) == 0) {
        stop("Count data is empty")
      }
      if (nrow(values$metadata) == 0) {
        stop("Metadata is empty")
      }
      
      # Ensure count data is numeric and contains no NAs
      count_matrix <- as.matrix(values$count_data)
      if (any(is.na(count_matrix))) {
        stop("Count data contains missing values")
      }
      if (!is.numeric(count_matrix)) {
        stop("Count data must be numeric")
      }
      
      # Round to integers for DESeq2
      count_matrix <- round(count_matrix)
      rownames(count_matrix) <- rownames(values$count_data)
      
      # Initialize results storage
      values$deseq_results <- list()
      values$analysis_summary <- list()
      
      # Step 2: Process each contrast
      incProgress(0.1, detail = "Processing contrasts...")
      
      total_contrasts <- length(values$created_contrasts)
      contrast_progress <- 0.6 / total_contrasts  # Reserve 60% for contrast processing
      
      for (contrast_name in names(values$created_contrasts)) {
        contrast_info <- values$created_contrasts[[contrast_name]]
        
        incProgress(contrast_progress * 0.2, detail = paste("Analyzing:", contrast_name))
        
        # Get samples for this contrast
        contrast_samples <- c(contrast_info$group1_samples, contrast_info$group2_samples)
        
        # Filter count data and metadata for this contrast
        contrast_counts <- count_matrix[, contrast_samples, drop = FALSE]
        contrast_metadata <- values$metadata[contrast_samples, , drop = FALSE]
        
        # Add the contrast factor to metadata
        contrast_metadata[[contrast_info$design_analysis$design_strategy$factor_name]] <- 
          factor(c(rep(contrast_info$group1_name, length(contrast_info$group1_samples)),
                  rep(contrast_info$group2_name, length(contrast_info$group2_samples))),
                 levels = c(contrast_info$group2_name, contrast_info$group1_name))  # Reference level first
        
        incProgress(contrast_progress * 0.1, detail = paste("Creating DESeq object for:", contrast_name))
        
        # Create DESeqDataSet
        design_formula <- as.formula(contrast_info$design_analysis$design_strategy$design_formula)
        
        dds <- DESeqDataSetFromMatrix(
          countData = contrast_counts,
          colData = contrast_metadata,
          design = design_formula
        )
        
        # Pre-filtering if enabled
        if (input$apply_prefiltering) {
          incProgress(contrast_progress * 0.1, detail = paste("Pre-filtering genes for:", contrast_name))
          keep <- rowSums(counts(dds) >= input$min_count_threshold) >= input$min_samples_threshold
          dds <- dds[keep, ]
          message("Pre-filtering: kept ", sum(keep), " out of ", length(keep), " genes")
        }
        
        incProgress(contrast_progress * 0.2, detail = paste("Running DESeq2 for:", contrast_name))
        # Log start of contrast
        session$sendCustomMessage("appendLog", list(text = paste0("[", contrast_name, "] Starting DESeq2 run (", input$test_type, ")")))
        
        # Run DESeq2 with configured parameters
        if (input$test_type == "LRT") {
          reduced_formula <- as.formula(input$lrt_reduced_formula)
          dds <- DESeq(dds, 
                      test = "LRT", 
                      reduced = reduced_formula,
                      fitType = input$dispersion_fit_type)
          session$sendCustomMessage("appendLog", list(text = paste0("[", contrast_name, "] LRT run with reduced formula: ", deparse(reduced_formula))))
        } else {
          dds <- DESeq(dds, 
                      test = "Wald",
                      fitType = input$dispersion_fit_type)
          session$sendCustomMessage("appendLog", list(text = paste0("[", contrast_name, "] Wald run completed")))
        }
        
        incProgress(contrast_progress * 0.2, detail = paste("Extracting results for:", contrast_name))
        
        # Extract results
        contrast_spec <- c(contrast_info$design_analysis$design_strategy$factor_name,
                         contrast_info$group1_name,
                         contrast_info$group2_name)
        
        # Use filtering approach: test for any change (lfcThreshold=0), then filter by magnitude
        res <- results(dds, 
                      contrast = contrast_spec,
                      alpha = input$alpha_level,
                      pAdjustMethod = input$p_adjust_method)
        session$sendCustomMessage("appendLog", list(text = paste0("[", contrast_name, "] Results extracted for contrast: ", paste(contrast_spec, collapse = ", "))))
        
        # Apply LFC shrinkage if enabled and test is Wald (apeglm only; no fallback)
        if (input$apply_lfc_shrinkage && input$test_type != "LRT") {
          incProgress(contrast_progress * 0.1, detail = paste("Applying LFC shrinkage for:", contrast_name))
          
          # Use our robust shrinkage function (filtering approach: no threshold in shrinkage)
          shrinkage_result <- apply_lfc_shrinkage(
            dds = dds,
            contrast = contrast_spec,
            shrinkage_type = input$shrinkage_type
          )
          # Log shrinkage outcome
          if (isTRUE(shrinkage_result$shrinkage_applied)) {
            session$sendCustomMessage("appendLog", list(text = paste0("[", contrast_name, "] Shrinkage applied (", shrinkage_result$shrinkage_method, ") coef=", if (!is.null(shrinkage_result$coefficient_used)) shrinkage_result$coefficient_used else "NA")))
          } else {
            session$sendCustomMessage("appendLog", list(text = paste0("[", contrast_name, "] Shrinkage not applied: ", if (!is.null(shrinkage_result$note)) shrinkage_result$note else "no note")))
          }
          
          res_shrunk <- shrinkage_result$shrunk
          shrinkage_info <- list(
            applied = isTRUE(shrinkage_result$shrinkage_applied),
            method = shrinkage_result$shrinkage_method,
            coefficient = if (!is.null(shrinkage_result$coefficient_used)) shrinkage_result$coefficient_used else NA,
            note = if (!is.null(shrinkage_result$note)) shrinkage_result$note else NULL,
            available_coefficients = shrinkage_result$available_coefficients
          )
        } else {
          res_shrunk <- res
          shrinkage_info <- list(applied = FALSE, method = "none", note = if (input$test_type == "LRT") "Disabled for LRT" else NULL)
        }
        
        # Convert to data frame and add metadata
        res_df <- as.data.frame(res_shrunk)
        res_df$gene <- rownames(res_df)
        res_df$contrast <- contrast_name
        
        # Calculate summary statistics using filtering approach
        # Filter by: padj < alpha AND abs(log2FoldChange) > threshold
        total_genes <- nrow(res_df)
        
        if (input$lfc_threshold > 0) {
          # Apply magnitude filter
          sig_genes <- sum(res_df$padj < input$alpha_level & 
                          abs(res_df$log2FoldChange) > input$lfc_threshold, na.rm = TRUE)
          up_genes <- sum(res_df$padj < input$alpha_level & 
                         res_df$log2FoldChange > input$lfc_threshold, na.rm = TRUE)
          down_genes <- sum(res_df$padj < input$alpha_level & 
                           res_df$log2FoldChange < -input$lfc_threshold, na.rm = TRUE)
        } else {
          # No magnitude filter, just significance
          sig_genes <- sum(res_df$padj < input$alpha_level, na.rm = TRUE)
          up_genes <- sum(res_df$padj < input$alpha_level & 
                         res_df$log2FoldChange > 0, na.rm = TRUE)
          down_genes <- sum(res_df$padj < input$alpha_level & 
                           res_df$log2FoldChange < 0, na.rm = TRUE)
        }
        
        # Store results
          values$deseq_results[[contrast_name]] <- list(
          results = res_df,
          dds = dds,
          unshrunken_results = as.data.frame(res),
          shrinkage_info = shrinkage_info,
          contrast_info = contrast_info,
          analysis_params = list(
            test_type = input$test_type,
            alpha = input$alpha_level,
            lfc_threshold = input$lfc_threshold,
            shrinkage_type = if(input$apply_lfc_shrinkage && input$test_type != "LRT") input$shrinkage_type else "none",
            p_adjust_method = input$p_adjust_method,
            dispersion_fit_type = input$dispersion_fit_type,
            prefiltering = input$apply_prefiltering,
            min_count_threshold = if(input$apply_prefiltering) input$min_count_threshold else NA,
            min_samples_threshold = if(input$apply_prefiltering) input$min_samples_threshold else NA
          )
        )
        
        # Store summary
        values$analysis_summary[[contrast_name]] <- list(
          contrast_name = contrast_name,
          total_genes = total_genes,
          significant_genes = sig_genes,
          upregulated = up_genes,
          downregulated = down_genes,
          group1 = contrast_info$group1_name,
          group2 = contrast_info$group2_name,
          group1_samples = length(contrast_info$group1_samples),
          group2_samples = length(contrast_info$group2_samples)
        )
        
        incProgress(contrast_progress * 0.1, detail = paste("Completed:", contrast_name))
      }
      
      # Step 3: Create overall summary
      incProgress(0.1, detail = "Creating analysis summary...")
      
      # Calculate total statistics
      total_contrasts <- length(values$analysis_summary)
      total_sig_genes <- sum(sapply(values$analysis_summary, function(x) x$significant_genes))
      
      values$overall_summary <- list(
        analysis_date = Sys.time(),
        total_contrasts = total_contrasts,
        total_significant_genes = total_sig_genes,
        analysis_parameters = list(
          test_type = input$test_type,
          alpha_level = input$alpha_level,
          lfc_threshold = input$lfc_threshold,
          shrinkage_applied = input$apply_lfc_shrinkage,
          shrinkage_type = if(input$apply_lfc_shrinkage) input$shrinkage_type else "none",
          p_adjust_method = input$p_adjust_method,
          dispersion_fit_type = input$dispersion_fit_type,
          prefiltering_applied = input$apply_prefiltering
        )
      )
      
      # Step 4: Success notification and navigation
      incProgress(0.1, detail = "Analysis completed successfully!")
      
      showNotification(
        paste0("DESeq2 analysis completed successfully! ",
              "Analyzed ", total_contrasts, " contrasts with ",
              total_sig_genes, " total significant genes."),
        type = "message",
        duration = 8
      )
      
      # Enable results display
      values$analysis_completed <- TRUE
      
    }, error = function(e) {
      # Error handling
      error_msg <- paste("DESeq2 analysis failed:", e$message)
      showNotification(error_msg, type = "error", duration = 10)
      message("DESeq2 analysis error: ", e$message)
      
      # Store error information
      values$analysis_error <- list(
        error_message = e$message,
        error_time = Sys.time(),
        error_details = toString(e)
      )
    })
    
  })
  
  # Re-enable the button
  shinyjs::enable("run_analysis")
})

# Control results panel visibility
output$show_results_panel <- reactive({
  !is.null(values$analysis_completed) && values$analysis_completed
})
outputOptions(output, "show_results_panel", suspendWhenHidden = FALSE)

# Simple completion message
output$analysis_completion_message <- renderUI({
  req(values$overall_summary)
  
  total_contrasts <- values$overall_summary$total_contrasts
  total_sig <- values$overall_summary$total_significant_genes
  
  span(
    paste0("Analyzed ", total_contrasts, " contrast", 
           if(total_contrasts > 1) "s" else "", 
           " with ", total_sig, " total significant gene", 
           if(total_sig != 1) "s" else "", ".")
  )
})

# Navigate to Results tab
observeEvent(input$go_to_results, {
  updateTabsetPanel(session, "main_tabs", selected = "tab_results")
})

# Download results handler
output$download_results <- downloadHandler(
  filename = function() {
    paste0("DESeq2_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
  },
  content = function(file) {
    req(values$deseq_results)
    
    # For now, create a simple CSV export
    # Create combined results
    all_results <- do.call(rbind, lapply(names(values$deseq_results), function(contrast_name) {
      res_df <- values$deseq_results[[contrast_name]]$results
      res_df$contrast <- contrast_name
      return(res_df)
    }))
    
    # Write to CSV
    write.csv(all_results, file, row.names = FALSE)
  }
)

