# Server logic for Tab 7: MA Plot Visualization
# Author: Eren Ada, PhD
# Date: 04/22/2026
#
# This file contains all server-side logic for MA plot generation.
# The structure mirrors the Volcano tab (server_tab6_volcano.R); the MA tab
# adds two controls: ma_x_transform (log10/log2 baseMean) and
# ma_show_zero_line. The CSV upload validator additionally requires a
# non-negative numeric "baseMean" column.

# =============================================================================
# TAB 7: MA PLOT VISUALIZATION
# =============================================================================

# =============================================================================
# PRE-CONDITION CHECKS & AVAILABILITY
# =============================================================================

# Control MA tab availability for conditional panels
output$ma_available <- reactive({
  # Check if a contrast is selected AND data is available
  req(input$ma_contrast)
  
  # Check if data exists for the selected contrast
  has_uploaded <- !is.null(values$uploaded_ma_results) && 
                  input$ma_contrast %in% names(values$uploaded_ma_results)
  
  has_computed <- !is.null(values$deseq_results) && 
                  input$ma_contrast %in% names(values$deseq_results)
  
  return(has_uploaded || has_computed)
})
outputOptions(output, "ma_available", suspendWhenHidden = FALSE)

# =============================================================================
# CSV UPLOAD FOR PRE-COMPUTED DEG RESULTS
# =============================================================================

# Handle DEG results CSV upload for MA plot
observeEvent(input$ma_upload_deg_results, {
  req(input$ma_upload_deg_results)
  
  tryCatch({
    
    message("\n=== UPLOADING DEG RESULTS FOR MA PLOT ===")
    
    # Initialize uploaded results list if needed
    if (is.null(values$uploaded_ma_results)) {
      values$uploaded_ma_results <- list()
    }
    
    file_path <- input$ma_upload_deg_results$datapath
    file_name <- input$ma_upload_deg_results$name
    
    message(paste("Processing file:", file_name))
    
    # Read CSV
    uploaded_data <- read.csv(file_path, stringsAsFactors = FALSE)
    message(paste("  Read", nrow(uploaded_data), "rows"))
    
    # Validate required columns (MA requires baseMean in addition to volcano's set)
    required_cols <- c("gene", "baseMean", "log2FoldChange", "pvalue", "padj", "contrast")
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
    if (!is.numeric(uploaded_data$baseMean)) {
      showNotification("baseMean must be numeric", type = "error", duration = 10)
      return(NULL)
    }
    
    if (any(uploaded_data$baseMean < 0, na.rm = TRUE)) {
      showNotification("baseMean must be non-negative", type = "error", duration = 10)
      return(NULL)
    }
    
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
      values$uploaded_ma_results[[contrast_name]] <- contrast_data
      message(paste("  Stored", nrow(contrast_data), "genes for contrast:", contrast_name))
    }
    
    # Update contrast selector to include uploaded contrasts
    all_contrasts <- unique(c(
      names(values$deseq_results),
      names(values$uploaded_ma_results)
    ))
    
    updateSelectizeInput(
      session,
      "ma_contrast",
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
    message(paste("ERROR uploading MA DEG results:", e$message))
    showNotification(
      paste("Error uploading file:", e$message),
      type = "error",
      duration = 10
    )
  })
})

# Clear uploaded MA results
observeEvent(input$ma_clear_upload, {
  values$uploaded_ma_results <- NULL
  
  # Update contrast selector to only show computed results
  if (!is.null(values$deseq_results)) {
    updateSelectizeInput(
      session,
      "ma_contrast",
      choices = names(values$deseq_results),
      selected = NULL
    )
  } else {
    updateSelectizeInput(
      session,
      "ma_contrast",
      choices = NULL,
      selected = NULL
    )
  }
  
  showNotification(
    "Uploaded results cleared",
    type = "message",
    duration = 3
  )
  
  message("Cleared uploaded MA results")
})

# =============================================================================
# CONTRAST SELECTOR POPULATION
# =============================================================================

# Update contrast selector when DESeq2 results are available
observe({
  computed_contrasts <- if (!is.null(values$deseq_results)) {
    names(values$deseq_results)
  } else {
    character(0)
  }
  
  uploaded_contrasts <- if (!is.null(values$uploaded_ma_results)) {
    names(values$uploaded_ma_results)
  } else {
    character(0)
  }
  
  all_contrasts <- unique(c(computed_contrasts, uploaded_contrasts))
  
  if (length(all_contrasts) > 0) {
    updateSelectizeInput(
      session,
      "ma_contrast",
      choices = all_contrasts
    )
  }
})

# =============================================================================
# UPLOAD STATUS INDICATOR
# =============================================================================

output$ma_upload_status <- renderUI({
  if (!is.null(values$uploaded_ma_results) && 
      length(values$uploaded_ma_results) > 0) {
    div(
      style = "background-color: #d4edda; border: 1px solid #c3e6cb; 
               border-radius: 4px; padding: 8px; margin-top: 5px;",
      icon("check-circle", style = "color: #28a745;"),
      span(paste(length(values$uploaded_ma_results), "contrast(s) uploaded"),
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

output$ma_data_source_info <- renderUI({
  req(input$ma_contrast)
  
  # Determine data source
  is_uploaded <- !is.null(values$uploaded_ma_results) && 
                 input$ma_contrast %in% names(values$uploaded_ma_results)
  
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
    use_shrunk <- input$ma_use_shrunk
    has_shrunk <- !is.null(values$deseq_results[[input$ma_contrast]]$results)
    
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
# MAIN MA PLOT GENERATION
# =============================================================================

# Initialize persistent storage for selected genes from the table
observe({
  if (is.null(values$ma_selected_genes)) {
    values$ma_selected_genes <- character(0)
  }
})

# Reset selections when contrast changes
observeEvent(input$ma_contrast, {
  values$ma_selected_genes <- character(0)
})

# Persist selected genes from the table so re-renders don't clear them
observeEvent(input$ma_gene_table_rows_selected, {
  gene_data <- ma_gene_data()
  if (is.null(gene_data) || nrow(gene_data) == 0) return(NULL)
  idx <- input$ma_gene_table_rows_selected
  
  new_selection <- if (!is.null(idx) && length(idx) > 0) {
    as.character(gene_data$gene[idx])
  } else {
    character(0)
  }
  
  current_selection <- if (!is.null(values$ma_selected_genes)) {
    values$ma_selected_genes
  } else {
    character(0)
  }
  
  if (!identical(sort(new_selection), sort(current_selection))) {
    values$ma_selected_genes <- new_selection
  }
}, ignoreInit = TRUE)

# Reactive to get MA plot data and parameters
ma_plot_reactive <- reactive({
  req(input$ma_contrast)
  
  # Get data source
  data_source <- get_ma_data_source(
    contrast = input$ma_contrast,
    use_shrunk = input$ma_use_shrunk,
    values = values
  )
  
  # Check if data is available
  if (is.null(data_source$data) || nrow(data_source$data) == 0) {
    return(NULL)
  }
  
  # Parse force labels
  force_labels <- parse_force_labels(input$ma_force_labels)
  
  # Get selected genes persisted across re-renders
  selected_genes <- if (!is.null(values$ma_selected_genes)) {
    values$ma_selected_genes
  } else {
    character(0)
  }
  
  # Combine force labels with selected genes from table
  all_force_labels <- unique(c(force_labels, selected_genes))
  
  # Determine plot title
  plot_title <- if (!is.null(input$ma_custom_title) && 
                    nchar(trimws(input$ma_custom_title)) > 0) {
    input$ma_custom_title
  } else {
    input$ma_contrast
  }
  
  # Build parameters list
  params <- list(
    padj_cutoff = input$ma_padj_cutoff,
    lfc_cutoff = input$ma_lfc_cutoff,
    top_n_labels = input$ma_top_n,
    point_size = input$ma_point_size,
    label_size = input$ma_label_size,
    alpha = input$ma_alpha,
    boxed_labels = input$ma_boxed_labels,
    draw_connectors = input$ma_draw_connectors,
    connector_width = input$ma_connector_width,
    max_overlaps = input$ma_max_overlaps,
    legend_position = input$ma_legend_position,
    force_labels = all_force_labels,
    selected_genes = selected_genes,
    custom_labels_only = input$ma_custom_labels_only,
    fix_axes = input$ma_fix_axes,
    xlim_min = input$ma_xlim_min,
    xlim_max = input$ma_xlim_max,
    ylim_min = input$ma_ylim_min,
    ylim_max = input$ma_ylim_max,
    contrast_name = plot_title,
    title_align = input$ma_title_align,
    
    # Text customization
    show_subtitle = input$ma_show_subtitle,
    custom_subtitle = input$ma_custom_subtitle,
    show_percentage = input$ma_show_percentage,
    subtitle_align = input$ma_subtitle_align,
    show_caption = input$ma_show_caption,
    custom_caption = input$ma_custom_caption,
    caption_align = input$ma_caption_align,
    
    # Colors
    color_up = input$ma_color_up,
    color_down = input$ma_color_down,
    color_ns = input$ma_color_ns,
    color_selected = input$ma_color_selected,
    
    # Point styling
    point_shape = as.numeric(input$ma_point_shape),
    
    # Threshold / reference lines
    show_threshold_lines = input$ma_show_threshold_lines,
    threshold_style = input$ma_threshold_style,
    show_zero_line = input$ma_show_zero_line,
    
    # Annotations
    show_counts_on_plot = input$ma_show_counts_on_plot,
    show_major_grid = input$ma_show_major_grid,
    show_minor_grid = input$ma_show_minor_grid,
    
    # MA-specific
    x_transform = input$ma_x_transform
  )
  
  # Generate cache key
  cache_key <- generate_cache_key_ma(input$ma_contrast, params)
  
  # Check cache
  if (!is.null(values$ma_cache) && cache_key %in% names(values$ma_cache)) {
    message(paste("Using cached MA plot for:", input$ma_contrast))
    return(values$ma_cache[[cache_key]])
  }
  
  # Generate new plot
  message(paste("Generating MA plot for:", input$ma_contrast))
  
  result <- tryCatch({
    generate_ma_plot(data_source$data, params)
  }, error = function(e) {
    message(paste("ERROR generating MA plot:", e$message))
    showNotification(
      paste("Error generating plot:", e$message),
      type = "error",
      duration = 10
    )
    return(NULL)
  })
  
  # Cache the result
  if (!is.null(result)) {
    if (is.null(values$ma_cache)) {
      values$ma_cache <- list()
    }
    values$ma_cache[[cache_key]] <- result
    message("Cached MA plot")
  }
  
  return(result)
})

# Render MA plot
output$ma_plot <- renderPlot({
  plot_result <- ma_plot_reactive()
  req(plot_result)
  
  plot_result$plot
}, height = 600)

# =============================================================================
# COUNTS SUMMARY
# =============================================================================

output$ma_counts_summary <- renderUI({
  plot_result <- ma_plot_reactive()
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

# Download MA plot as PDF
output$download_ma_pdf <- downloadHandler(
  filename = function() {
    req(input$ma_contrast)
    safe_name <- sanitize_filename(input$ma_contrast)
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    paste0(safe_name, "_MA_", timestamp, ".pdf")
  },
  content = function(file) {
    req(input$ma_contrast)
    
    plot_result <- ma_plot_reactive()
    req(plot_result)
    
    # Save to download location
    ggsave(
      filename = file,
      plot = plot_result$plot,
      width = input$ma_plot_width,
      height = input$ma_plot_height,
      dpi = input$ma_plot_dpi,
      device = "pdf"
    )
    
    # Also save to results directory
    safe_name <- sanitize_filename(input$ma_contrast)
    results_dir <- file.path("results", "ma", safe_name)
    dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
    
    results_file <- file.path(results_dir, paste0(safe_name, "_MA.pdf"))
    ggsave(
      filename = results_file,
      plot = plot_result$plot,
      width = input$ma_plot_width,
      height = input$ma_plot_height,
      dpi = input$ma_plot_dpi,
      device = "pdf"
    )
    
    showNotification(
      paste("MA plot saved to:", results_file),
      type = "message",
      duration = 5
    )
    
    message(paste("Downloaded MA plot PDF for:", input$ma_contrast))
  }
)

# Download MA plot as PNG
output$download_ma_png <- downloadHandler(
  filename = function() {
    req(input$ma_contrast)
    safe_name <- sanitize_filename(input$ma_contrast)
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    paste0(safe_name, "_MA_", timestamp, ".png")
  },
  content = function(file) {
    req(input$ma_contrast)
    
    plot_result <- ma_plot_reactive()
    req(plot_result)
    
    # Save to download location
    ggsave(
      filename = file,
      plot = plot_result$plot,
      width = input$ma_plot_width,
      height = input$ma_plot_height,
      dpi = input$ma_plot_dpi,
      device = "png"
    )
    
    # Also save to results directory
    safe_name <- sanitize_filename(input$ma_contrast)
    results_dir <- file.path("results", "ma", safe_name)
    dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
    
    results_file <- file.path(results_dir, paste0(safe_name, "_MA.png"))
    ggsave(
      filename = results_file,
      plot = plot_result$plot,
      width = input$ma_plot_width,
      height = input$ma_plot_height,
      dpi = input$ma_plot_dpi,
      device = "png"
    )
    
    showNotification(
      paste("MA plot saved to:", results_file),
      type = "message",
      duration = 5
    )
    
    message(paste("Downloaded MA plot PNG for:", input$ma_contrast))
  }
)

# Download parameters as JSON
output$download_ma_params <- downloadHandler(
  filename = function() {
    req(input$ma_contrast)
    safe_name <- sanitize_filename(input$ma_contrast)
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    paste0(safe_name, "_MA_params_", timestamp, ".json")
  },
  content = function(file) {
    req(input$ma_contrast)
    
    # Get data source info
    data_source <- get_ma_data_source(
      contrast = input$ma_contrast,
      use_shrunk = input$ma_use_shrunk,
      values = values
    )
    
    # Build parameters object
    params_export <- list(
      contrast = input$ma_contrast,
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      data_source = data_source$source_type,
      shrinkage_applied = data_source$shrinkage_applied,
      thresholds = list(
        padj_cutoff = input$ma_padj_cutoff,
        lfc_cutoff = input$ma_lfc_cutoff
      ),
      labeling = list(
        top_n_per_direction = input$ma_top_n,
        force_labels = parse_force_labels(input$ma_force_labels),
        custom_labels_only = input$ma_custom_labels_only
      ),
      aesthetics = list(
        point_size = input$ma_point_size,
        label_size = input$ma_label_size,
        alpha = input$ma_alpha,
        boxed_labels = input$ma_boxed_labels,
        draw_connectors = input$ma_draw_connectors,
        connector_width = input$ma_connector_width,
        max_overlaps = input$ma_max_overlaps,
        legend_position = input$ma_legend_position,
        x_transform = input$ma_x_transform,
        show_zero_line = input$ma_show_zero_line
      ),
      text_customization = list(
        custom_subtitle = input$ma_custom_subtitle,
        show_percentage = input$ma_show_percentage,
        custom_caption = input$ma_custom_caption
      ),
      colors = list(
        up_regulated = input$ma_color_up,
        down_regulated = input$ma_color_down,
        non_significant = input$ma_color_ns
      ),
      point_styling = list(
        point_shape = input$ma_point_shape
      ),
      threshold_lines = list(
        show = input$ma_show_threshold_lines,
        style = input$ma_threshold_style
      ),
      annotations = list(
        show_counts_on_plot = input$ma_show_counts_on_plot,
        show_major_grid = input$ma_show_major_grid,
        show_minor_grid = input$ma_show_minor_grid
      ),
      axes = list(
        fix_axes = input$ma_fix_axes,
        xlim = if (input$ma_fix_axes) c(input$ma_xlim_min, input$ma_xlim_max) else NULL,
        ylim = if (input$ma_fix_axes) c(input$ma_ylim_min, input$ma_ylim_max) else NULL
      ),
      export_settings = list(
        width_inches = input$ma_plot_width,
        height_inches = input$ma_plot_height,
        dpi = input$ma_plot_dpi
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
    
    message(paste("Downloaded MA plot parameters for:", input$ma_contrast))
  }
)

# =============================================================================
# GENE LIST TABLE
# =============================================================================

# Reactive to get filtered gene data for table
ma_gene_data <- reactive({
  req(input$ma_contrast)
  
  # Get data source
  data_source <- get_ma_data_source(
    contrast = input$ma_contrast,
    use_shrunk = input$ma_use_shrunk,
    values = values
  )
  
  if (is.null(data_source$data) || nrow(data_source$data) == 0) {
    return(NULL)
  }
  
  gene_data <- data_source$data
  
  # Add regulation direction column
  gene_data$regulation <- ifelse(
    !is.na(gene_data$padj) & 
    gene_data$padj < input$ma_padj_cutoff & 
    gene_data$log2FoldChange > input$ma_lfc_cutoff,
    "Up",
    ifelse(
      !is.na(gene_data$padj) & 
      gene_data$padj < input$ma_padj_cutoff & 
      gene_data$log2FoldChange < -input$ma_lfc_cutoff,
      "Down",
      "NS"
    )
  )
  
  # Filter based on user selection
  if (input$ma_show_genes == "significant") {
    gene_data <- gene_data[gene_data$regulation != "NS", ]
  }
  
  # Select and order columns (baseMean first if present, to emphasize MA context)
  display_cols <- c("gene", "baseMean", "log2FoldChange", "pvalue", "padj", "regulation")
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
output$ma_gene_table <- DT::renderDataTable({
  gene_data <- ma_gene_data()
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
      digits = 3
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
output$download_ma_genes_csv <- downloadHandler(
  filename = function() {
    req(input$ma_contrast)
    safe_name <- sanitize_filename(input$ma_contrast)
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filter_type <- if (input$ma_show_genes == "significant") "significant" else "all"
    paste0(safe_name, "_genes_", filter_type, "_", timestamp, ".csv")
  },
  content = function(file) {
    gene_data <- ma_gene_data()
    req(gene_data)
    
    write.csv(gene_data, file, row.names = FALSE)
    
    showNotification(
      "Gene list exported successfully",
      type = "message",
      duration = 3
    )
    
    message(paste("Downloaded MA gene list for:", input$ma_contrast))
  }
)
