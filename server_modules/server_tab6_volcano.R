# Server logic for Tab 6: Volcano Plot Visualization
# Author: Eren Ada, PhD
# Date: 10/20/2025
#
# This file contains all server-side logic for volcano plot generation

# =============================================================================
# TAB 6: VOLCANO PLOT VISUALIZATION
# =============================================================================

# =============================================================================
# PRE-CONDITION CHECKS & AVAILABILITY
# =============================================================================

# Control volcano tab availability for conditional panels
output$volcano_available <- reactive({
  # Check if a contrast is selected AND data is available
  req(input$volcano_contrast)
  
  # Check if data exists for the selected contrast
  has_uploaded <- !is.null(values$uploaded_volcano_results) && 
                  input$volcano_contrast %in% names(values$uploaded_volcano_results)
  
  has_computed <- !is.null(values$deseq_results) && 
                  input$volcano_contrast %in% names(values$deseq_results)
  
  return(has_uploaded || has_computed)
})
outputOptions(output, "volcano_available", suspendWhenHidden = FALSE)

# =============================================================================
# CSV UPLOAD FOR PRE-COMPUTED DEG RESULTS
# =============================================================================

# Handle DEG results CSV upload for volcano plot
observeEvent(input$volcano_upload_deg_results, {
  req(input$volcano_upload_deg_results)
  
  tryCatch({
    
    message("\n=== UPLOADING DEG RESULTS FOR VOLCANO PLOT ===")
    
    # Initialize uploaded results list if needed
    if (is.null(values$uploaded_volcano_results)) {
      values$uploaded_volcano_results <- list()
    }
    
    file_path <- input$volcano_upload_deg_results$datapath
    file_name <- input$volcano_upload_deg_results$name
    
    message(paste("Processing file:", file_name))
    
    # Read CSV
    uploaded_data <- read.csv(file_path, stringsAsFactors = FALSE)
    message(paste("  Read", nrow(uploaded_data), "rows"))
    
    # Validate required columns
    required_cols <- c("gene", "log2FoldChange", "pvalue", "padj", "contrast")
    missing_cols <- setdiff(required_cols, colnames(uploaded_data))
    
    if (length(missing_cols) > 0) {
      showNotification(
        paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
        type = "error",
        duration = 10
      )
      return(NULL)
    }
    
    # Validate data types
    if (!is.numeric(uploaded_data$log2FoldChange)) {
      showNotification("log2FoldChange must be numeric", type = "error", duration = 10)
      return(NULL)
    }
    
    if (!is.numeric(uploaded_data$pvalue)) {
      showNotification("pvalue must be numeric", type = "error", duration = 10)
      return(NULL)
    }
    
    if (!is.numeric(uploaded_data$padj)) {
      showNotification("padj must be numeric", type = "error", duration = 10)
      return(NULL)
    }
    
    # Get unique contrasts in this file
    contrasts_in_file <- unique(uploaded_data$contrast)
    message(paste("  Found", length(contrasts_in_file), "contrast(s):",
                  paste(contrasts_in_file, collapse = ", ")))
    
    # Store data by contrast
    for (contrast_name in contrasts_in_file) {
      contrast_data <- uploaded_data[uploaded_data$contrast == contrast_name, ]
      values$uploaded_volcano_results[[contrast_name]] <- contrast_data
      message(paste("  Stored", nrow(contrast_data), "genes for contrast:", contrast_name))
    }
    
    # Update contrast selector to include uploaded contrasts
    all_contrasts <- unique(c(
      names(values$deseq_results),
      names(values$uploaded_volcano_results)
    ))
    
    updateSelectizeInput(
      session,
      "volcano_contrast",
      choices = all_contrasts,
      selected = contrasts_in_file[1]  # Auto-select first uploaded contrast
    )
    
    showNotification(
      paste("Successfully uploaded", length(contrasts_in_file), "contrast(s)"),
      type = "message",
      duration = 5
    )
    
    message("=== UPLOAD COMPLETE ===\n")
    
  }, error = function(e) {
    message(paste("ERROR uploading volcano DEG results:", e$message))
    showNotification(
      paste("Error uploading file:", e$message),
      type = "error",
      duration = 10
    )
  })
})

# Clear uploaded volcano results
observeEvent(input$volcano_clear_upload, {
  values$uploaded_volcano_results <- NULL
  
  # Update contrast selector to only show computed results
  if (!is.null(values$deseq_results)) {
    updateSelectizeInput(
      session,
      "volcano_contrast",
      choices = names(values$deseq_results),
      selected = NULL
    )
  } else {
    updateSelectizeInput(
      session,
      "volcano_contrast",
      choices = NULL,
      selected = NULL
    )
  }
  
  showNotification(
    "Uploaded results cleared",
    type = "message",
    duration = 3
  )
  
  message("Cleared uploaded volcano results")
})

# =============================================================================
# CONTRAST SELECTOR POPULATION
# =============================================================================

# Update contrast selector when DESeq2 results are available
observe({
  # Get all available contrasts from both computed and uploaded results
  computed_contrasts <- if (!is.null(values$deseq_results)) {
    names(values$deseq_results)
  } else {
    character(0)
  }
  
  uploaded_contrasts <- if (!is.null(values$uploaded_volcano_results)) {
    names(values$uploaded_volcano_results)
  } else {
    character(0)
  }
  
  all_contrasts <- unique(c(computed_contrasts, uploaded_contrasts))
  
  if (length(all_contrasts) > 0) {
    updateSelectizeInput(
      session,
      "volcano_contrast",
      choices = all_contrasts
    )
  }
})

# =============================================================================
# UPLOAD STATUS INDICATOR
# =============================================================================

output$volcano_upload_status <- renderUI({
  if (!is.null(values$uploaded_volcano_results) && 
      length(values$uploaded_volcano_results) > 0) {
    div(
      style = "background-color: #d4edda; border: 1px solid #c3e6cb; 
               border-radius: 4px; padding: 8px; margin-top: 5px;",
      icon("check-circle", style = "color: #28a745;"),
      span(paste(length(values$uploaded_volcano_results), "contrast(s) uploaded"),
           style = "color: #155724; font-size: 11px; margin-left: 5px;")
    )
  } else {
    div(
      style = "background-color: #f8f9fa; border: 1px solid #dee2e6; 
               border-radius: 4px; padding: 8px; margin-top: 5px;",
      icon("info-circle", style = "color: #6c757d;"),
      span("No uploaded data",
           style = "color: #6c757d; font-size: 11px; margin-left: 5px;")
    )
  }
})

# =============================================================================
# DATA SOURCE INFO DISPLAY
# =============================================================================

output$volcano_data_source_info <- renderUI({
  req(input$volcano_contrast)
  
  # Determine data source
  is_uploaded <- !is.null(values$uploaded_volcano_results) && 
                 input$volcano_contrast %in% names(values$uploaded_volcano_results)
  
  if (is_uploaded) {
    div(
      style = "background-color: #fff3cd; border-left: 4px solid #ffc107; 
               padding: 10px; margin-bottom: 15px; border-radius: 4px;",
      icon("upload", style = "color: #856404;"),
      strong(" Data Source: ", style = "color: #856404;"),
      span("Uploaded CSV", style = "color: #856404;")
    )
  } else {
    # Check if shrinkage was applied
    use_shrunk <- input$volcano_use_shrunk
    has_shrunk <- !is.null(values$deseq_results[[input$volcano_contrast]]$results)
    
    shrinkage_text <- if (use_shrunk && has_shrunk) {
      "Computed (Shrunk LFC)"
    } else {
      "Computed (Unshrunk LFC)"
    }
    
    div(
      style = "background-color: #d1ecf1; border-left: 4px solid #17a2b8; 
               padding: 10px; margin-bottom: 15px; border-radius: 4px;",
      icon("calculator", style = "color: #0c5460;"),
      strong(" Data Source: ", style = "color: #0c5460;"),
      span(shrinkage_text, style = "color: #0c5460;")
    )
  }
})

# =============================================================================
# MAIN VOLCANO PLOT GENERATION
# =============================================================================

# Initialize persistent storage for selected genes from the table
observe({
  if (is.null(values$volcano_selected_genes)) {
    values$volcano_selected_genes <- character(0)
  }
})

# Reset selections when contrast changes
observeEvent(input$volcano_contrast, {
  values$volcano_selected_genes <- character(0)
})

# Persist selected genes from the table so re-renders don't clear them
observeEvent(input$volcano_gene_table_rows_selected, {
  gene_data <- volcano_gene_data()
  if (is.null(gene_data) || nrow(gene_data) == 0) return(NULL)
  idx <- input$volcano_gene_table_rows_selected
  
  # Get new selection
  new_selection <- if (!is.null(idx) && length(idx) > 0) {
    as.character(gene_data$gene[idx])
  } else {
    character(0)
  }
  
  # Only update if selection actually changed
  current_selection <- if (!is.null(values$volcano_selected_genes)) {
    values$volcano_selected_genes
  } else {
    character(0)
  }
  
  if (!identical(sort(new_selection), sort(current_selection))) {
    values$volcano_selected_genes <- new_selection
  }
}, ignoreInit = TRUE)

# Reactive to get volcano plot data and parameters
volcano_plot_reactive <- reactive({
  req(input$volcano_contrast)
  
  # Get data source
  data_source <- get_volcano_data_source(
    contrast = input$volcano_contrast,
    use_shrunk = input$volcano_use_shrunk,
    values = values
  )
  
  # Check if data is available
  if (is.null(data_source$data) || nrow(data_source$data) == 0) {
    return(NULL)
  }
  
  # Parse force labels
  force_labels <- parse_force_labels(input$volcano_force_labels)
  
  # Get selected genes persisted across re-renders
  # CRITICAL: This must be accessed inside the reactive to create dependency
  selected_genes <- if (!is.null(values$volcano_selected_genes)) {
    values$volcano_selected_genes
  } else {
    character(0)
  }
  
  # Combine force labels with selected genes from table
  all_force_labels <- unique(c(force_labels, selected_genes))
  
  # Determine plot title
  plot_title <- if (!is.null(input$volcano_custom_title) && 
                    nchar(trimws(input$volcano_custom_title)) > 0) {
    input$volcano_custom_title
  } else {
    input$volcano_contrast
  }
  
  # Build parameters list
  params <- list(
    padj_cutoff = input$volcano_padj_cutoff,
    lfc_cutoff = input$volcano_lfc_cutoff,
    top_n_labels = input$volcano_top_n,
    point_size = input$volcano_point_size,
    label_size = input$volcano_label_size,
    alpha = input$volcano_alpha,
    boxed_labels = input$volcano_boxed_labels,
    draw_connectors = input$volcano_draw_connectors,
    connector_width = input$volcano_connector_width,
    max_overlaps = input$volcano_max_overlaps,
    legend_position = input$volcano_legend_position,
    force_labels = all_force_labels,
    selected_genes = selected_genes,  # Pass selected genes separately for orange highlighting
    custom_labels_only = input$volcano_custom_labels_only,
    fix_axes = input$volcano_fix_axes,
    xlim_min = input$volcano_xlim_min,
    xlim_max = input$volcano_xlim_max,
    ylim_min = input$volcano_ylim_min,
    ylim_max = input$volcano_ylim_max,
    contrast_name = plot_title,
    title_align = input$volcano_title_align,
    
    # Text customization
    show_subtitle = input$volcano_show_subtitle,
    custom_subtitle = input$volcano_custom_subtitle,
    show_percentage = input$volcano_show_percentage,
    subtitle_align = input$volcano_subtitle_align,
    show_caption = input$volcano_show_caption,
    custom_caption = input$volcano_custom_caption,
    caption_align = input$volcano_caption_align,
    
    # Colors
    color_up = input$volcano_color_up,
    color_down = input$volcano_color_down,
    color_ns = input$volcano_color_ns,
    color_selected = input$volcano_color_selected,
    
    # Point styling
    point_shape = as.numeric(input$volcano_point_shape),
    
    # Threshold lines
    show_threshold_lines = input$volcano_show_threshold_lines,
    threshold_style = input$volcano_threshold_style,
    
    # Annotations
    show_counts_on_plot = input$volcano_show_counts_on_plot,
    show_major_grid = input$volcano_show_major_grid,
    show_minor_grid = input$volcano_show_minor_grid
  )
  
  # Generate cache key
  cache_key <- generate_cache_key_volcano(input$volcano_contrast, params)
  
  # Check cache - selected genes are part of the cache key, so this works correctly
  if (!is.null(values$volcano_cache) && cache_key %in% names(values$volcano_cache)) {
    message(paste("Using cached volcano plot for:", input$volcano_contrast))
    return(values$volcano_cache[[cache_key]])
  }
  
  # Generate new plot
  message(paste("Generating volcano plot for:", input$volcano_contrast))
  
  result <- tryCatch({
    generate_volcano_plot(data_source$data, params)
  }, error = function(e) {
    message(paste("ERROR generating volcano plot:", e$message))
    showNotification(
      paste("Error generating plot:", e$message),
      type = "error",
      duration = 10
    )
    return(NULL)
  })
  
  # Cache the result
  if (!is.null(result)) {
    if (is.null(values$volcano_cache)) {
      values$volcano_cache <- list()
    }
    values$volcano_cache[[cache_key]] <- result
    message("Cached volcano plot")
  }
  
  return(result)
})

# Render volcano plot
output$volcano_plot <- renderPlot({
  plot_result <- volcano_plot_reactive()
  req(plot_result)
  
  plot_result$plot
}, height = 600)

# =============================================================================
# COUNTS SUMMARY
# =============================================================================

output$volcano_counts_summary <- renderUI({
  plot_result <- volcano_plot_reactive()
  req(plot_result)
  
  stats <- plot_result$stats
  
  div(
    h4("Differential Expression Summary", style = "color: #2C3E50; margin-top: 0;"),
    fluidRow(
      column(3,
        div(
          style = "text-align: center; padding: 10px; background-color: #ffebee; 
                   border-radius: 4px;",
          h3(stats$n_up, style = "color: #c62828; margin: 5px 0;"),
          p("Up-regulated", style = "color: #666; margin: 0; font-size: 12px;")
        )
      ),
      column(3,
        div(
          style = "text-align: center; padding: 10px; background-color: #e3f2fd; 
                   border-radius: 4px;",
          h3(stats$n_down, style = "color: #1565c0; margin: 5px 0;"),
          p("Down-regulated", style = "color: #666; margin: 0; font-size: 12px;")
        )
      ),
      column(3,
        div(
          style = "text-align: center; padding: 10px; background-color: #f3e5f5; 
                   border-radius: 4px;",
          h3(stats$n_sig, style = "color: #6a1b9a; margin: 5px 0;"),
          p("Total Significant", style = "color: #666; margin: 0; font-size: 12px;")
        )
      ),
      column(3,
        div(
          style = "text-align: center; padding: 10px; background-color: #f1f8e9; 
                   border-radius: 4px;",
          h3(stats$n_total, style = "color: #558b2f; margin: 5px 0;"),
          p("Total Genes", style = "color: #666; margin: 0; font-size: 12px;")
        )
      )
    ),
    br(),
    p(
      paste0(
        "Labeled: ", stats$n_labeled_up, " up, ", stats$n_labeled_down, " down"
      ),
      style = "color: #666; font-size: 12px; margin: 5px 0;"
    ),
    if (stats$n_force_labeled > 0) {
      p(
        paste0(
          "Custom labels: ", stats$n_force_found, " of ", stats$n_force_labeled, " found"
        ),
        style = "color: #666; font-size: 12px; margin: 5px 0;"
      )
    },
    if (stats$all_padj_na) {
      div(
        style = "background-color: #fff3cd; border-left: 4px solid #ffc107; 
                 padding: 8px; margin-top: 10px; border-radius: 4px;",
        icon("exclamation-triangle", style = "color: #856404;"),
        span(" Note: All adjusted p-values are NA. Using raw p-values for visualization.",
             style = "color: #856404; font-size: 11px; margin-left: 5px;")
      )
    }
  )
})

# =============================================================================
# DOWNLOAD HANDLERS
# =============================================================================

# Download volcano plot as PDF
output$download_volcano_pdf <- downloadHandler(
  filename = function() {
    req(input$volcano_contrast)
    safe_name <- sanitize_filename(input$volcano_contrast)
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    paste0(safe_name, "_Volcano_", timestamp, ".pdf")
  },
  content = function(file) {
    req(input$volcano_contrast)
    
    plot_result <- volcano_plot_reactive()
    req(plot_result)
    
    # Save to download location
    ggsave(
      filename = file,
      plot = plot_result$plot,
      width = input$volcano_plot_width,
      height = input$volcano_plot_height,
      dpi = input$volcano_plot_dpi,
      device = "pdf"
    )
    
    # Also save to results directory
    safe_name <- sanitize_filename(input$volcano_contrast)
    results_dir <- file.path("results", "volcano", safe_name)
    dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
    
    results_file <- file.path(results_dir, paste0(safe_name, "_Volcano.pdf"))
    ggsave(
      filename = results_file,
      plot = plot_result$plot,
      width = input$volcano_plot_width,
      height = input$volcano_plot_height,
      dpi = input$volcano_plot_dpi,
      device = "pdf"
    )
    
    showNotification(
      paste("Volcano plot saved to:", results_file),
      type = "message",
      duration = 5
    )
    
    message(paste("Downloaded volcano plot PDF for:", input$volcano_contrast))
  }
)

# Download volcano plot as PNG
output$download_volcano_png <- downloadHandler(
  filename = function() {
    req(input$volcano_contrast)
    safe_name <- sanitize_filename(input$volcano_contrast)
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    paste0(safe_name, "_Volcano_", timestamp, ".png")
  },
  content = function(file) {
    req(input$volcano_contrast)
    
    plot_result <- volcano_plot_reactive()
    req(plot_result)
    
    # Save to download location
    ggsave(
      filename = file,
      plot = plot_result$plot,
      width = input$volcano_plot_width,
      height = input$volcano_plot_height,
      dpi = input$volcano_plot_dpi,
      device = "png"
    )
    
    # Also save to results directory
    safe_name <- sanitize_filename(input$volcano_contrast)
    results_dir <- file.path("results", "volcano", safe_name)
    dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
    
    results_file <- file.path(results_dir, paste0(safe_name, "_Volcano.png"))
    ggsave(
      filename = results_file,
      plot = plot_result$plot,
      width = input$volcano_plot_width,
      height = input$volcano_plot_height,
      dpi = input$volcano_plot_dpi,
      device = "png"
    )
    
    showNotification(
      paste("Volcano plot saved to:", results_file),
      type = "message",
      duration = 5
    )
    
    message(paste("Downloaded volcano plot PNG for:", input$volcano_contrast))
  }
)

# Download parameters as JSON
output$download_volcano_params <- downloadHandler(
  filename = function() {
    req(input$volcano_contrast)
    safe_name <- sanitize_filename(input$volcano_contrast)
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    paste0(safe_name, "_Volcano_params_", timestamp, ".json")
  },
  content = function(file) {
    req(input$volcano_contrast)
    
    # Get data source info
    data_source <- get_volcano_data_source(
      contrast = input$volcano_contrast,
      use_shrunk = input$volcano_use_shrunk,
      values = values
    )
    
    # Build parameters object
    params_export <- list(
      contrast = input$volcano_contrast,
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      data_source = data_source$source_type,
      shrinkage_applied = data_source$shrinkage_applied,
      thresholds = list(
        padj_cutoff = input$volcano_padj_cutoff,
        lfc_cutoff = input$volcano_lfc_cutoff
      ),
      labeling = list(
        top_n_per_direction = input$volcano_top_n,
        force_labels = parse_force_labels(input$volcano_force_labels),
        custom_labels_only = input$volcano_custom_labels_only
      ),
      aesthetics = list(
        point_size = input$volcano_point_size,
        label_size = input$volcano_label_size,
        alpha = input$volcano_alpha,
        boxed_labels = input$volcano_boxed_labels,
        draw_connectors = input$volcano_draw_connectors,
        connector_width = input$volcano_connector_width,
        max_overlaps = input$volcano_max_overlaps,
        legend_position = input$volcano_legend_position
      ),
      text_customization = list(
        custom_subtitle = input$volcano_custom_subtitle,
        show_percentage = input$volcano_show_percentage,
        custom_caption = input$volcano_custom_caption
      ),
      colors = list(
        up_regulated = input$volcano_color_up,
        down_regulated = input$volcano_color_down,
        non_significant = input$volcano_color_ns
      ),
      point_styling = list(
        point_shape = input$volcano_point_shape
      ),
      threshold_lines = list(
        show = input$volcano_show_threshold_lines,
        style = input$volcano_threshold_style
      ),
      annotations = list(
        show_counts_on_plot = input$volcano_show_counts_on_plot,
        show_major_grid = input$volcano_show_major_grid,
        show_minor_grid = input$volcano_show_minor_grid
      ),
      axes = list(
        fix_axes = input$volcano_fix_axes,
        xlim = if (input$volcano_fix_axes) c(input$volcano_xlim_min, input$volcano_xlim_max) else NULL,
        ylim = if (input$volcano_fix_axes) c(input$volcano_ylim_min, input$volcano_ylim_max) else NULL
      ),
      export_settings = list(
        width_inches = input$volcano_plot_width,
        height_inches = input$volcano_plot_height,
        dpi = input$volcano_plot_dpi
      )
    )
    
    # Write JSON
    writeLines(
      jsonlite::toJSON(params_export, pretty = TRUE, auto_unbox = TRUE),
      file
    )
    
    showNotification(
      "Parameters exported successfully",
      type = "message",
      duration = 3
    )
    
    message(paste("Downloaded volcano plot parameters for:", input$volcano_contrast))
  }
)

# =============================================================================
# GENE LIST TABLE
# =============================================================================

# Reactive to get filtered gene data for table
volcano_gene_data <- reactive({
  req(input$volcano_contrast)
  
  # Get data source
  data_source <- get_volcano_data_source(
    contrast = input$volcano_contrast,
    use_shrunk = input$volcano_use_shrunk,
    values = values
  )
  
  # Check if data is available
  if (is.null(data_source$data) || nrow(data_source$data) == 0) {
    return(NULL)
  }
  
  gene_data <- data_source$data
  
  # Add regulation direction column
  gene_data$regulation <- ifelse(
    !is.na(gene_data$padj) & 
    gene_data$padj < input$volcano_padj_cutoff & 
    gene_data$log2FoldChange > input$volcano_lfc_cutoff,
    "Up",
    ifelse(
      !is.na(gene_data$padj) & 
      gene_data$padj < input$volcano_padj_cutoff & 
      gene_data$log2FoldChange < -input$volcano_lfc_cutoff,
      "Down",
      "NS"
    )
  )
  
  # Filter based on user selection
  if (input$volcano_show_genes == "significant") {
    gene_data <- gene_data[gene_data$regulation != "NS", ]
  }
  
  # Select and order columns
  display_cols <- c("gene", "log2FoldChange", "pvalue", "padj", "regulation")
  
  # Add optional columns if they exist
  if ("baseMean" %in% colnames(gene_data)) {
    display_cols <- c("gene", "baseMean", "log2FoldChange", "pvalue", "padj", "regulation")
  }
  if ("lfcSE" %in% colnames(gene_data)) {
    display_cols <- c(display_cols[1:(length(display_cols)-1)], "lfcSE", "regulation")
  }
  
  # Filter to available columns
  display_cols <- display_cols[display_cols %in% colnames(gene_data)]
  
  gene_data <- gene_data[, display_cols, drop = FALSE]
  
  # Sort by absolute log2FoldChange (descending)
  gene_data <- gene_data[order(abs(gene_data$log2FoldChange), decreasing = TRUE), ]
  
  return(gene_data)
})

# Render gene table
output$volcano_gene_table <- DT::renderDataTable({
  gene_data <- volcano_gene_data()
  req(gene_data)
  
  DT::datatable(
    gene_data,
    options = list(
      pageLength = 25,
      scrollX = TRUE,
      scrollY = "400px",
      scrollCollapse = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel'),
      columnDefs = list(
        list(className = 'dt-center', targets = '_all')
      )
    ),
    rownames = FALSE,
    class = 'cell-border stripe hover',
    filter = 'top',
    selection = list(mode = 'multiple', target = 'row')
  ) %>%
    DT::formatRound(
      columns = 'log2FoldChange',
      digits = 4
    ) %>%
    DT::formatSignif(
      columns = c('pvalue', 'padj'),
      digits = 3  # Show 3 significant figures (e.g., 1.23e-50)
    ) %>%
    DT::formatRound(
      columns = if ('baseMean' %in% colnames(gene_data)) 'baseMean' else NULL,
      digits = 2
    ) %>%
    DT::formatRound(
      columns = if ('lfcSE' %in% colnames(gene_data)) 'lfcSE' else NULL,
      digits = 4
    ) %>%
    DT::formatStyle(
      'regulation',
      backgroundColor = DT::styleEqual(
        c('Up', 'Down', 'NS'),
        c('#ffebee', '#e3f2fd', '#f5f5f5')
      ),
      fontWeight = 'bold',
      color = DT::styleEqual(
        c('Up', 'Down', 'NS'),
        c('#c62828', '#1565c0', '#757575')
      )
    )
})

# Download gene table as CSV
output$download_volcano_genes_csv <- downloadHandler(
  filename = function() {
    req(input$volcano_contrast)
    safe_name <- sanitize_filename(input$volcano_contrast)
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filter_type <- if (input$volcano_show_genes == "significant") "significant" else "all"
    paste0(safe_name, "_genes_", filter_type, "_", timestamp, ".csv")
  },
  content = function(file) {
    gene_data <- volcano_gene_data()
    req(gene_data)
    
    write.csv(gene_data, file, row.names = FALSE)
    
    showNotification(
      "Gene list exported successfully",
      type = "message",
      duration = 3
    )
    
    message(paste("Downloaded volcano gene list for:", input$volcano_contrast))
  }
)

