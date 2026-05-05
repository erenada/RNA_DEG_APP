# Server logic for Tab 8: Heatmap Visualization
# Author: Eren Ada, PhD
#
# This module owns all reactives, observers, and outputs for the Heatmap tab.
# It supports two modes:
#   - Expression heatmap (genes x samples): backed by DESeq2 dds + transform.
#   - Cross-contrast LFC heatmap (genes x contrasts): backed by DESeq2 results
#     or uploaded CSVs.
# All input/output IDs are namespaced with the "heatmap_" prefix to avoid
# collisions with other tabs.

# =============================================================================
# TAB 8: HEATMAP VISUALIZATION
# =============================================================================

# -----------------------------------------------------------------------------
# AVAILABILITY GATE (drives output.heatmap_available conditionalPanels)
# -----------------------------------------------------------------------------

output$heatmap_available <- reactive({
  mode <- input$heatmap_mode
  if (is.null(mode)) return(FALSE)
  
  if (mode == "expression") {
    if (is.null(input$heatmap_contrast) || nchar(input$heatmap_contrast) == 0) {
      return(FALSE)
    }
    has_dds <- !is.null(values$deseq_results) &&
               input$heatmap_contrast %in% names(values$deseq_results) &&
               !is.null(values$deseq_results[[input$heatmap_contrast]]$dds)
    return(isTRUE(has_dds))
  }
  
  if (mode == "lfc") {
    selected <- input$heatmap_contrasts
    if (is.null(selected) || length(selected) == 0) return(FALSE)
    
    pipeline_names <- if (!is.null(values$deseq_results)) names(values$deseq_results) else character(0)
    upload_names   <- if (!is.null(values$uploaded_heatmap_results)) names(values$uploaded_heatmap_results) else character(0)
    available      <- unique(c(pipeline_names, upload_names))
    
    return(any(selected %in% available))
  }
  
  FALSE
})
outputOptions(output, "heatmap_available", suspendWhenHidden = FALSE)


# -----------------------------------------------------------------------------
# CSV UPLOAD FOR PRE-COMPUTED DEG RESULTS (LFC mode)
# -----------------------------------------------------------------------------

observeEvent(input$heatmap_upload_deg_results, {
  req(input$heatmap_upload_deg_results)
  
  tryCatch({
    message("\n=== UPLOADING DEG RESULTS FOR HEATMAP ===")
    
    if (is.null(values$uploaded_heatmap_results)) {
      values$uploaded_heatmap_results <- list()
    }
    
    file_path <- input$heatmap_upload_deg_results$datapath
    file_name <- input$heatmap_upload_deg_results$name
    message(paste("Processing file:", file_name))
    
    uploaded_data <- read.csv(file_path, stringsAsFactors = FALSE)
    message(paste("  Read", nrow(uploaded_data), "rows"))
    
    required_cols <- c("gene", "log2FoldChange", "padj", "contrast")
    missing_cols <- setdiff(required_cols, colnames(uploaded_data))
    
    if (length(missing_cols) > 0) {
      showNotification(
        paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
        type = "error", duration = 10
      )
      return(NULL)
    }
    
    if (!is.numeric(uploaded_data$log2FoldChange)) {
      showNotification("log2FoldChange must be numeric", type = "error", duration = 10)
      return(NULL)
    }
    if (!is.numeric(uploaded_data$padj)) {
      showNotification("padj must be numeric", type = "error", duration = 10)
      return(NULL)
    }
    
    contrasts_in_file <- unique(uploaded_data$contrast)
    message(paste("  Found", length(contrasts_in_file), "contrast(s):",
                  paste(contrasts_in_file, collapse = ", ")))
    
    for (contrast_name in contrasts_in_file) {
      contrast_data <- uploaded_data[uploaded_data$contrast == contrast_name, ]
      values$uploaded_heatmap_results[[contrast_name]] <- contrast_data
      message(paste("  Stored", nrow(contrast_data), "genes for contrast:", contrast_name))
    }
    
    showNotification(
      paste("Successfully uploaded", length(contrasts_in_file), "contrast(s)"),
      type = "message", duration = 5
    )
    message("=== UPLOAD COMPLETE ===\n")
    
  }, error = function(e) {
    message(paste("ERROR uploading heatmap DEG results:", e$message))
    showNotification(
      paste("Error uploading file:", e$message),
      type = "error", duration = 10
    )
  })
})

observeEvent(input$heatmap_clear_upload, {
  values$uploaded_heatmap_results <- NULL
  showNotification("Uploaded results cleared", type = "message", duration = 3)
  message("Cleared uploaded heatmap results")
})


# -----------------------------------------------------------------------------
# CONTRAST SELECTOR POPULATION (mode-aware)
# -----------------------------------------------------------------------------

observe({
  computed_contrasts <- if (!is.null(values$deseq_results)) names(values$deseq_results) else character(0)
  uploaded_contrasts <- if (!is.null(values$uploaded_heatmap_results)) names(values$uploaded_heatmap_results) else character(0)
  
  expression_choices <- computed_contrasts  # expression mode requires dds (pipeline-only)
  lfc_choices        <- unique(c(computed_contrasts, uploaded_contrasts))
  
  updateSelectizeInput(session, "heatmap_contrast", choices = expression_choices)
  updateSelectizeInput(session, "heatmap_contrasts", choices = lfc_choices)
})


# -----------------------------------------------------------------------------
# ANNOTATION-FACTOR CHOICES (expression mode)
# -----------------------------------------------------------------------------

observe({
  req(input$heatmap_mode == "expression")
  contrast_name <- input$heatmap_contrast
  if (is.null(contrast_name) || nchar(contrast_name) == 0) return()
  if (is.null(values$deseq_results) || !(contrast_name %in% names(values$deseq_results))) return()
  
  dds <- values$deseq_results[[contrast_name]]$dds
  if (is.null(dds)) return()
  
  factor_choices <- tryCatch(
    eligible_annotation_columns(dds),
    error = function(e) character(0)
  )
  
  current_selection <- input$heatmap_anno_factors
  preserved_selection <- intersect(current_selection, factor_choices)
  
  updateSelectizeInput(
    session,
    "heatmap_anno_factors",
    choices  = factor_choices,
    selected = preserved_selection
  )
})


# -----------------------------------------------------------------------------
# CACHE INVALIDATION (when pipeline produces fresh results)
# -----------------------------------------------------------------------------

observeEvent(values$deseq_results, {
  values$heatmap_cache <- list()
  values$vst_cache <- list()
  values$rlog_cache <- list()
  message("Heatmap caches cleared (deseq_results changed)")
}, ignoreInit = TRUE, ignoreNULL = FALSE)


# -----------------------------------------------------------------------------
# CACHED TRANSFORMATION HELPER
# -----------------------------------------------------------------------------

# Returns a transformed expression matrix (genes x samples) for the given
# contrast, computing it on first access and caching by transform type.
get_transformed_matrix <- function(contrast_name, transform) {
  if (is.null(values$deseq_results) || !(contrast_name %in% names(values$deseq_results))) {
    return(NULL)
  }
  dds <- values$deseq_results[[contrast_name]]$dds
  if (is.null(dds)) return(NULL)
  
  if (transform == "vst") {
    if (is.null(values$vst_cache[[contrast_name]])) {
      message(paste("Computing VST for contrast:", contrast_name))
      values$vst_cache[[contrast_name]] <- compute_transformed_matrix(dds, "vst")
    }
    return(values$vst_cache[[contrast_name]])
  } else if (transform == "rlog") {
    if (is.null(values$rlog_cache[[contrast_name]])) {
      message(paste("Computing rlog for contrast:", contrast_name))
      values$rlog_cache[[contrast_name]] <- compute_transformed_matrix(dds, "rlog")
    }
    return(values$rlog_cache[[contrast_name]])
  } else {
    return(compute_transformed_matrix(dds, "log2norm"))
  }
}


# -----------------------------------------------------------------------------
# UPLOAD STATUS INDICATOR
# -----------------------------------------------------------------------------

output$heatmap_upload_status <- renderUI({
  if (!is.null(values$uploaded_heatmap_results) &&
      length(values$uploaded_heatmap_results) > 0) {
    div(
      style = "background-color: #d4edda; border: 1px solid #c3e6cb; 
               border-radius: 4px; padding: 8px; margin-top: 5px;",
      icon("check-circle", style = "color: #28a745;"),
      span(paste(length(values$uploaded_heatmap_results), "contrast(s) uploaded"),
           style = "color: #155724; font-size: 11px; margin-left: 5px;")
    )
  } else {
    div(
      style = "background-color: #f8f9fa; border: 1px solid #dee2e6; 
               border-radius: 4px; padding: 8px; margin-top: 5px;",
      icon("info-circle", style = "color: #6c757d;"),
      span("No uploaded data", style = "color: #6c757d; font-size: 11px; margin-left: 5px;")
    )
  }
})


# -----------------------------------------------------------------------------
# EMPTY-STATE MESSAGE
# -----------------------------------------------------------------------------

output$heatmap_empty_message <- renderUI({
  mode <- input$heatmap_mode
  if (is.null(mode)) return(p("Select a mode to begin.", style = "color: #95A5A6;"))
  
  pipeline_n <- if (!is.null(values$deseq_results)) length(values$deseq_results) else 0
  upload_n   <- if (!is.null(values$uploaded_heatmap_results)) length(values$uploaded_heatmap_results) else 0
  
  if (mode == "expression") {
    if (pipeline_n == 0) {
      return(p("Expression mode requires pipeline-computed results (it needs the DESeq2 dds object). Run the analysis on the Configuration tab first, or switch to Cross-contrast LFC mode to use uploaded CSVs.",
               style = "color: #95A5A6; font-size: 14px;"))
    }
    return(p("Select a contrast from the dropdown to view its expression heatmap.",
             style = "color: #95A5A6; font-size: 14px;"))
  }
  
  if (mode == "lfc") {
    if (pipeline_n == 0 && upload_n == 0) {
      return(p("Run the pipeline or upload DEG results to enable the cross-contrast LFC heatmap.",
               style = "color: #95A5A6; font-size: 14px;"))
    }
    return(p("Select one or more contrasts from the dropdown to view the cross-contrast LFC heatmap.",
             style = "color: #95A5A6; font-size: 14px;"))
  }
  
  p("No data available.", style = "color: #95A5A6;")
})


# -----------------------------------------------------------------------------
# DATA SOURCE BANNER
# -----------------------------------------------------------------------------

output$heatmap_data_source_info <- renderUI({
  mode <- input$heatmap_mode
  if (is.null(mode)) return(NULL)
  
  if (mode == "expression") {
    req(input$heatmap_contrast)
    div(
      style = "background-color: #d1ecf1; border-left: 4px solid #17a2b8; 
               padding: 10px; margin-bottom: 15px; border-radius: 4px;",
      icon("calculator", style = "color: #0c5460;"),
      strong(" Data Source: ", style = "color: #0c5460;"),
      span(paste0("Pipeline (computed) - contrast '", input$heatmap_contrast, "'"),
           style = "color: #0c5460;")
    )
  } else {
    selected <- input$heatmap_contrasts
    if (is.null(selected) || length(selected) == 0) return(NULL)
    
    sources <- vapply(selected, function(cn) {
      uploaded_first <- !is.null(values$uploaded_heatmap_results) &&
                        cn %in% names(values$uploaded_heatmap_results)
      if (uploaded_first) "uploaded" else "computed"
    }, character(1))
    
    n_uploaded <- sum(sources == "uploaded")
    n_computed <- sum(sources == "computed")
    
    msg <- paste0(
      "Cross-contrast LFC across ", length(selected), " contrast(s) - ",
      n_computed, " computed, ", n_uploaded, " uploaded"
    )
    
    div(
      style = "background-color: #d1ecf1; border-left: 4px solid #17a2b8; 
               padding: 10px; margin-bottom: 15px; border-radius: 4px;",
      icon("layer-group", style = "color: #0c5460;"),
      strong(" Data Source: ", style = "color: #0c5460;"),
      span(msg, style = "color: #0c5460;")
    )
  }
})


# -----------------------------------------------------------------------------
# MAIN PLOT REACTIVE
# -----------------------------------------------------------------------------

heatmap_plot_reactive <- reactive({
  mode <- input$heatmap_mode
  req(mode %in% c("expression", "lfc"))
  
  # Resolve gene-selection params with NA-safe defaults
  top_n        <- if (isTruthy(input$heatmap_top_n)) input$heatmap_top_n else 50
  padj_cutoff  <- if (isTruthy(input$heatmap_padj_cutoff)) input$heatmap_padj_cutoff else 0.05
  lfc_cutoff   <- if (isTruthy(input$heatmap_lfc_cutoff)) input$heatmap_lfc_cutoff else 1.0
  
  custom_genes <- character(0)
  if (isTRUE(input$heatmap_gene_method == "custom")) {
    upload_path <- if (!is.null(input$heatmap_gene_list_upload)) {
      input$heatmap_gene_list_upload$datapath
    } else NULL
    custom_genes <- parse_heatmap_gene_list(
      text = input$heatmap_gene_list_paste,
      file_path = upload_path
    )
  }
  
  # Numeric advanced params with isTruthy guards
  fontsize_row    <- if (isTruthy(input$heatmap_fontsize_row)) input$heatmap_fontsize_row else 8
  fontsize_col    <- if (isTruthy(input$heatmap_fontsize_col)) input$heatmap_fontsize_col else 10
  cellwidth       <- if (!is.null(input$heatmap_cellwidth) && !is.na(input$heatmap_cellwidth)) input$heatmap_cellwidth else NA_real_
  cellheight      <- if (!is.null(input$heatmap_cellheight) && !is.na(input$heatmap_cellheight)) input$heatmap_cellheight else NA_real_
  treeheight_row  <- if (isTruthy(input$heatmap_treeheight_row)) input$heatmap_treeheight_row else 30
  treeheight_col  <- if (isTruthy(input$heatmap_treeheight_col)) input$heatmap_treeheight_col else 30
  zlim_min        <- if (!is.null(input$heatmap_zlim_min) && !is.na(input$heatmap_zlim_min)) input$heatmap_zlim_min else NA_real_
  zlim_max        <- if (!is.null(input$heatmap_zlim_max) && !is.na(input$heatmap_zlim_max)) input$heatmap_zlim_max else NA_real_
  
  # Plot title
  plot_title <- if (!is.null(input$heatmap_title) && nchar(trimws(input$heatmap_title)) > 0) {
    input$heatmap_title
  } else if (mode == "expression") {
    input$heatmap_contrast
  } else {
    "Cross-contrast LFC heatmap"
  }
  
  # Common style params
  base_params <- list(
    cluster_rows           = isTRUE(input$heatmap_cluster_rows),
    cluster_cols           = isTRUE(input$heatmap_cluster_cols),
    distance               = input$heatmap_distance %||% "euclidean",
    linkage                = input$heatmap_linkage %||% "complete",
    palette                = input$heatmap_palette %||% "RdBu",
    palette_reverse        = isTRUE(input$heatmap_palette_reverse),
    zlim_min               = zlim_min,
    zlim_max               = zlim_max,
    show_rownames          = isTRUE(input$heatmap_show_rownames),
    show_colnames          = isTRUE(input$heatmap_show_colnames),
    fontsize_row           = fontsize_row,
    fontsize_col           = fontsize_col,
    cellwidth              = cellwidth,
    cellheight             = cellheight,
    treeheight_row         = treeheight_row,
    treeheight_col         = treeheight_col,
    show_legend            = isTRUE(input$heatmap_show_legend),
    show_annotation_legend = isTRUE(input$heatmap_show_annotation_legend),
    border_color           = if (!is.null(input$heatmap_border_color)) input$heatmap_border_color else NA,
    na_color               = input$heatmap_na_color %||% "#DDDDDD",
    title                  = plot_title
  )
  
  # ---- EXPRESSION MODE ----
  if (mode == "expression") {
    contrast_name <- input$heatmap_contrast
    req(contrast_name)
    if (is.null(values$deseq_results) || !(contrast_name %in% names(values$deseq_results))) {
      return(NULL)
    }
    contrast_pkg <- values$deseq_results[[contrast_name]]
    if (is.null(contrast_pkg$dds) || is.null(contrast_pkg$results)) return(NULL)
    
    transform <- input$heatmap_transform %||% "vst"
    expr_matrix_full <- get_transformed_matrix(contrast_name, transform)
    if (is.null(expr_matrix_full)) return(NULL)
    
    samples <- resolve_heatmap_samples(
      dds = contrast_pkg$dds,
      contrast_info = contrast_pkg$contrast_info,
      sample_scope = input$heatmap_sample_scope %||% "contrast"
    )
    expr_matrix <- expr_matrix_full[, samples, drop = FALSE]
    
    gene_set <- select_heatmap_genes(
      method = input$heatmap_gene_method %||% "top_padj",
      mode = "expression",
      params = list(
        top_n                = top_n,
        padj_cutoff          = padj_cutoff,
        lfc_cutoff           = lfc_cutoff,
        custom_genes         = custom_genes
      ),
      results_df = contrast_pkg$results
    )
    
    annotation_df <- NULL
    if (isTRUE(input$heatmap_show_anno) && length(input$heatmap_anno_factors) > 0) {
      annotation_df <- tryCatch(
        build_heatmap_annotations(contrast_pkg$dds, input$heatmap_anno_factors),
        error = function(e) NULL
      )
      if (!is.null(annotation_df)) {
        annotation_df <- annotation_df[rownames(annotation_df) %in% colnames(expr_matrix), , drop = FALSE]
      }
    }
    
    expr_params <- modifyList(base_params, list(
      scale = input$heatmap_scale %||% "row",
      transform = transform,
      sample_scope = input$heatmap_sample_scope %||% "contrast",
      gene_method = input$heatmap_gene_method %||% "top_padj",
      contrast_name = contrast_name
    ))
    
    cache_key <- generate_cache_key_heatmap("expression", expr_params, gene_set)
    if (!is.null(values$heatmap_cache[[cache_key]])) {
      message(paste("Using cached heatmap (expression):", contrast_name))
      return(values$heatmap_cache[[cache_key]])
    }
    
    message(paste("Generating expression heatmap for:", contrast_name))
    result <- tryCatch(
      generate_heatmap_expression(
        expr_matrix = expr_matrix,
        gene_set = gene_set,
        params = expr_params,
        annotation_df = annotation_df
      ),
      error = function(e) {
        message(paste("ERROR generating expression heatmap:", e$message))
        showNotification(paste("Error generating heatmap:", e$message),
                         type = "error", duration = 10)
        list(plot = NULL, matrix = NULL, n_genes = 0, warnings = e$message)
      }
    )
    
    result$mode <- "expression"
    result$params <- expr_params
    result$gene_set <- gene_set
    result$contrast_name <- contrast_name
    
    if (!is.null(result$plot)) {
      values$heatmap_cache[[cache_key]] <- result
    }
    
    return(result)
  }
  
  # ---- LFC MODE ----
  if (mode == "lfc") {
    selected <- input$heatmap_contrasts
    req(length(selected) >= 1)
    
    matrices <- get_heatmap_lfc_matrices(selected, values)
    if (is.null(matrices$lfc_matrix) || nrow(matrices$lfc_matrix) == 0) return(NULL)
    
    quantifier <- input$heatmap_threshold_quantifier %||% "any"
    
    gene_set <- select_heatmap_genes(
      method = input$heatmap_gene_method %||% "top_padj",
      mode = "lfc",
      params = list(
        top_n                = top_n,
        padj_cutoff          = padj_cutoff,
        lfc_cutoff           = lfc_cutoff,
        threshold_quantifier = quantifier,
        custom_genes         = custom_genes
      ),
      lfc_matrix = matrices$lfc_matrix,
      padj_matrix = matrices$padj_matrix
    )
    
    lfc_params <- modifyList(base_params, list(
      scale = "none",
      gene_method = input$heatmap_gene_method %||% "top_padj",
      threshold_quantifier = quantifier,
      contrasts = selected
    ))
    
    cache_key <- generate_cache_key_heatmap("lfc", lfc_params, gene_set)
    if (!is.null(values$heatmap_cache[[cache_key]])) {
      message(paste("Using cached heatmap (LFC) for", length(selected), "contrast(s)"))
      return(values$heatmap_cache[[cache_key]])
    }
    
    message(paste("Generating LFC heatmap for", length(selected), "contrast(s)"))
    result <- tryCatch(
      generate_heatmap_lfc(
        lfc_matrix = matrices$lfc_matrix,
        gene_set = gene_set,
        params = lfc_params
      ),
      error = function(e) {
        message(paste("ERROR generating LFC heatmap:", e$message))
        showNotification(paste("Error generating heatmap:", e$message),
                         type = "error", duration = 10)
        list(plot = NULL, matrix = NULL, n_genes = 0, warnings = e$message)
      }
    )
    
    result$mode <- "lfc"
    result$params <- lfc_params
    result$gene_set <- gene_set
    result$contrasts <- selected
    
    if (!is.null(result$plot)) {
      values$heatmap_cache[[cache_key]] <- result
    }
    
    return(result)
  }
  
  NULL
})


# -----------------------------------------------------------------------------
# RENDER PLOT
# -----------------------------------------------------------------------------

output$heatmap_plot <- renderPlot({
  pr <- heatmap_plot_reactive()
  req(pr)
  req(!is.null(pr$plot))
  
  grid::grid.newpage()
  grid::grid.draw(pr$plot$gtable)
}, height = 600)


# -----------------------------------------------------------------------------
# SUMMARY PANEL (genes shown, dim, warnings)
# -----------------------------------------------------------------------------

output$heatmap_summary <- renderUI({
  pr <- heatmap_plot_reactive()
  
  if (is.null(pr) || is.null(pr$matrix)) {
    return(div(
      icon("exclamation-circle", style = "color: #e67e22;"),
      span(" No data to display. Adjust filters or selections.",
           style = "color: #34495e;")
    ))
  }
  
  n_genes  <- nrow(pr$matrix)
  n_cols   <- ncol(pr$matrix)
  col_label <- if (isTRUE(pr$mode == "expression")) "samples" else "contrasts"
  
  dim_block <- div(
    h4("Heatmap Summary", style = "color: #2C3E50; margin-top: 0;"),
    fluidRow(
      column(4,
        div(
          style = "text-align: center; padding: 10px; background-color: #ffffff; 
                   border-radius: 4px; border: 1px solid #ddd;",
          h3(n_genes, style = "color: #2C3E50; margin: 5px 0;"),
          p("Genes shown", style = "color: #666; margin: 0; font-size: 12px;")
        )
      ),
      column(4,
        div(
          style = "text-align: center; padding: 10px; background-color: #ffffff; 
                   border-radius: 4px; border: 1px solid #ddd;",
          h3(n_cols, style = "color: #2C3E50; margin: 5px 0;"),
          p(col_label, style = "color: #666; margin: 0; font-size: 12px;")
        )
      ),
      column(4,
        div(
          style = "text-align: center; padding: 10px; background-color: #ffffff; 
                   border-radius: 4px; border: 1px solid #ddd;",
          h3(toupper(pr$mode), style = "color: #2C3E50; margin: 5px 0;"),
          p("Mode", style = "color: #666; margin: 0; font-size: 12px;")
        )
      )
    )
  )
  
  warn_block <- NULL
  if (length(pr$warnings) > 0) {
    warn_block <- div(
      style = "background-color: #fff3cd; border-left: 4px solid #ffc107; 
               padding: 8px; margin-top: 10px; border-radius: 4px;",
      icon("exclamation-triangle", style = "color: #856404;"),
      lapply(pr$warnings, function(msg) {
        div(span(msg, style = "color: #856404; font-size: 11px; margin-left: 5px;"))
      })
    )
  }
  
  tagList(dim_block, warn_block)
})


# -----------------------------------------------------------------------------
# DOWNLOAD HANDLERS
# -----------------------------------------------------------------------------

# Helper to render a heatmap to a graphics device file
.render_heatmap_to_device <- function(file, plot_obj, width_in, height_in, dpi, device) {
  if (device == "pdf") {
    grDevices::pdf(file = file, width = width_in, height = height_in)
  } else if (device == "png") {
    grDevices::png(file = file, width = width_in, height = height_in,
                   units = "in", res = dpi)
  } else {
    stop("Unsupported device: ", device)
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  grid::grid.draw(plot_obj$gtable)
}

# PDF
output$download_heatmap_pdf <- downloadHandler(
  filename = function() {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    pr <- heatmap_plot_reactive()
    base <- if (!is.null(pr) && isTRUE(pr$mode == "expression") && !is.null(pr$contrast_name)) {
      sanitize_filename(pr$contrast_name)
    } else {
      "heatmap_lfc"
    }
    paste0(base, "_heatmap_", timestamp, ".pdf")
  },
  content = function(file) {
    pr <- heatmap_plot_reactive()
    req(pr); req(pr$plot)
    
    width_in  <- if (isTruthy(input$heatmap_plot_width))  input$heatmap_plot_width  else 10
    height_in <- if (isTruthy(input$heatmap_plot_height)) input$heatmap_plot_height else 8
    dpi       <- if (isTruthy(input$heatmap_plot_dpi))    input$heatmap_plot_dpi    else 300
    
    .render_heatmap_to_device(file, pr$plot, width_in, height_in, dpi, "pdf")
    
    base <- if (isTRUE(pr$mode == "expression") && !is.null(pr$contrast_name)) {
      sanitize_filename(pr$contrast_name)
    } else {
      "lfc"
    }
    results_dir <- file.path("results", "heatmap", base)
    dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
    results_file <- file.path(results_dir, paste0(base, "_heatmap.pdf"))
    .render_heatmap_to_device(results_file, pr$plot, width_in, height_in, dpi, "pdf")
    
    showNotification(paste("Heatmap saved to:", results_file),
                     type = "message", duration = 5)
    message(paste("Downloaded heatmap PDF for:", base))
  }
)

# PNG
output$download_heatmap_png <- downloadHandler(
  filename = function() {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    pr <- heatmap_plot_reactive()
    base <- if (!is.null(pr) && isTRUE(pr$mode == "expression") && !is.null(pr$contrast_name)) {
      sanitize_filename(pr$contrast_name)
    } else {
      "heatmap_lfc"
    }
    paste0(base, "_heatmap_", timestamp, ".png")
  },
  content = function(file) {
    pr <- heatmap_plot_reactive()
    req(pr); req(pr$plot)
    
    width_in  <- if (isTruthy(input$heatmap_plot_width))  input$heatmap_plot_width  else 10
    height_in <- if (isTruthy(input$heatmap_plot_height)) input$heatmap_plot_height else 8
    dpi       <- if (isTruthy(input$heatmap_plot_dpi))    input$heatmap_plot_dpi    else 300
    
    .render_heatmap_to_device(file, pr$plot, width_in, height_in, dpi, "png")
    
    base <- if (isTRUE(pr$mode == "expression") && !is.null(pr$contrast_name)) {
      sanitize_filename(pr$contrast_name)
    } else {
      "lfc"
    }
    results_dir <- file.path("results", "heatmap", base)
    dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
    results_file <- file.path(results_dir, paste0(base, "_heatmap.png"))
    .render_heatmap_to_device(results_file, pr$plot, width_in, height_in, dpi, "png")
    
    showNotification(paste("Heatmap saved to:", results_file),
                     type = "message", duration = 5)
    message(paste("Downloaded heatmap PNG for:", base))
  }
)

# Parameters JSON
output$download_heatmap_params <- downloadHandler(
  filename = function() {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    pr <- heatmap_plot_reactive()
    base <- if (!is.null(pr) && isTRUE(pr$mode == "expression") && !is.null(pr$contrast_name)) {
      sanitize_filename(pr$contrast_name)
    } else {
      "heatmap_lfc"
    }
    paste0(base, "_heatmap_params_", timestamp, ".json")
  },
  content = function(file) {
    pr <- heatmap_plot_reactive()
    req(pr)
    
    params_export <- list(
      mode               = pr$mode,
      timestamp          = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      contrast           = pr$contrast_name,
      contrasts          = pr$contrasts,
      gene_method        = input$heatmap_gene_method,
      top_n              = input$heatmap_top_n,
      padj_cutoff        = input$heatmap_padj_cutoff,
      lfc_cutoff         = input$heatmap_lfc_cutoff,
      threshold_quantifier = input$heatmap_threshold_quantifier,
      sample_scope       = input$heatmap_sample_scope,
      transformation     = input$heatmap_transform,
      scaling            = input$heatmap_scale,
      clustering = list(
        cluster_rows = isTRUE(input$heatmap_cluster_rows),
        cluster_cols = isTRUE(input$heatmap_cluster_cols),
        distance     = input$heatmap_distance,
        linkage      = input$heatmap_linkage
      ),
      colors = list(
        palette         = input$heatmap_palette,
        palette_reverse = isTRUE(input$heatmap_palette_reverse),
        zlim_min        = input$heatmap_zlim_min,
        zlim_max        = input$heatmap_zlim_max,
        na_color        = input$heatmap_na_color,
        border_color    = input$heatmap_border_color
      ),
      display = list(
        show_rownames   = isTRUE(input$heatmap_show_rownames),
        show_colnames   = isTRUE(input$heatmap_show_colnames),
        fontsize_row    = input$heatmap_fontsize_row,
        fontsize_col    = input$heatmap_fontsize_col,
        cellwidth       = input$heatmap_cellwidth,
        cellheight      = input$heatmap_cellheight,
        treeheight_row  = input$heatmap_treeheight_row,
        treeheight_col  = input$heatmap_treeheight_col,
        show_legend     = isTRUE(input$heatmap_show_legend)
      ),
      annotations = list(
        show_anno              = isTRUE(input$heatmap_show_anno),
        anno_factors           = input$heatmap_anno_factors,
        show_annotation_legend = isTRUE(input$heatmap_show_annotation_legend)
      ),
      title = list(
        text  = input$heatmap_title,
        align = input$heatmap_title_align
      ),
      export_settings = list(
        width_inches  = input$heatmap_plot_width,
        height_inches = input$heatmap_plot_height,
        dpi           = input$heatmap_plot_dpi
      ),
      gene_set_size = if (!is.null(pr$gene_set)) length(pr$gene_set) else 0,
      n_genes_displayed = pr$n_genes
    )
    
    writeLines(
      jsonlite::toJSON(params_export, pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null"),
      file
    )
    
    showNotification("Parameters exported successfully", type = "message", duration = 3)
    message("Downloaded heatmap parameters")
  }
)


# -----------------------------------------------------------------------------
# DATA MATRIX TABLE + CSV DOWNLOAD
# -----------------------------------------------------------------------------

heatmap_matrix_for_table <- reactive({
  pr <- heatmap_plot_reactive()
  if (is.null(pr) || is.null(pr$matrix)) return(NULL)
  mat <- pr$matrix
  df <- data.frame(gene = rownames(mat), mat, check.names = FALSE, stringsAsFactors = FALSE)
  df
})

output$heatmap_matrix_preview <- DT::renderDataTable({
  df <- heatmap_matrix_for_table()
  req(df)
  
  numeric_cols <- setdiff(colnames(df), "gene")
  
  DT::datatable(
    df,
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
    selection = 'none'
  ) %>%
    DT::formatRound(columns = numeric_cols, digits = 4)
})

output$download_heatmap_matrix_csv <- downloadHandler(
  filename = function() {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    pr <- heatmap_plot_reactive()
    base <- if (!is.null(pr) && isTRUE(pr$mode == "expression") && !is.null(pr$contrast_name)) {
      sanitize_filename(pr$contrast_name)
    } else {
      "heatmap_lfc"
    }
    paste0(base, "_heatmap_matrix_", timestamp, ".csv")
  },
  content = function(file) {
    df <- heatmap_matrix_for_table()
    req(df)
    write.csv(df, file, row.names = FALSE)
    showNotification("Matrix exported successfully", type = "message", duration = 3)
  }
)
