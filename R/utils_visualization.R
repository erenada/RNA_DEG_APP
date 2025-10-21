# Volcano Plot Visualization Utilities
# Author: Eren Ada, PhD
# Date: 10/20/2025
#
# This module provides helper functions for generating publication-ready
# volcano plots using EnhancedVolcano package.

#' Generate Volcano Plot
#'
#' Creates a publication-ready volcano plot from DESeq2 results
#'
#' @param results_df Data frame with columns: gene, log2FoldChange, pvalue, padj, baseMean
#' @param params List of parameters including:
#'   - padj_cutoff: Adjusted p-value threshold (default: 0.05)
#'   - lfc_cutoff: Log2 fold change threshold (default: 1.0)
#'   - top_n_labels: Number of top genes to label per direction (default: 20)
#'   - point_size: Size of points (default: 1.5)
#'   - label_size: Size of gene labels (default: 3.0)
#'   - alpha: Transparency of points (default: 0.6)
#'   - boxed_labels: Whether to box labels (default: TRUE)
#'   - draw_connectors: Whether to draw connectors (default: TRUE)
#'   - connector_width: Width of connectors (default: 0.3)
#'   - max_overlaps: Maximum label overlaps (default: 50)
#'   - legend_position: Position of legend (default: "right")
#'   - force_labels: Character vector of genes to force label
#'   - fix_axes: Whether to fix axis limits (default: FALSE)
#'   - xlim_min, xlim_max, ylim_min, ylim_max: Axis limits if fix_axes = TRUE
#'   - contrast_name: Name of the contrast for title
#'   - use_pvalue_fallback: Use pvalue when padj is NA (default: TRUE)
#'
#' @return List with components:
#'   - plot: ggplot object
#'   - stats: List with n_up, n_down, n_total, n_sig, n_labeled_up, n_labeled_down
#'
#' @export
generate_volcano_plot <- function(results_df, params) {
  
  # Validate required packages
  if (!requireNamespace("EnhancedVolcano", quietly = TRUE)) {
    stop("EnhancedVolcano package is required. Install with: BiocManager::install('EnhancedVolcano')")
  }
  
  # Validate required columns
  required_cols <- c("gene", "log2FoldChange", "pvalue", "padj")
  missing_cols <- setdiff(required_cols, colnames(results_df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Set default parameters
  params <- modifyList(list(
    padj_cutoff = 0.05,
    lfc_cutoff = 1.0,
    top_n_labels = 20,
    point_size = 1.5,
    label_size = 3.0,
    alpha = 0.6,
    boxed_labels = TRUE,
    draw_connectors = TRUE,
    connector_width = 0.3,
    max_overlaps = 50,
    legend_position = "right",
    force_labels = character(0),
    selected_genes = character(0),  # Genes selected from table (highlighted in orange)
    custom_labels_only = FALSE,
    fix_axes = FALSE,
    contrast_name = "Contrast",
    title_align = "center",
    use_pvalue_fallback = TRUE,
    
    # Text customization
    show_subtitle = TRUE,
    custom_subtitle = "",
    show_percentage = FALSE,
    subtitle_align = "center",
    show_caption = TRUE,
    custom_caption = "",
    caption_align = "center",
    
    # Colors
    color_up = "#CD0000",      # red2
    color_down = "#4169E1",    # royalblue
    color_ns = "#999999",      # grey60
    color_selected = "#FF6600", # bright orange for table-selected genes
    
    # Point styling
    point_shape = 19,          # circle
    
    # Threshold lines
    show_threshold_lines = TRUE,
    threshold_style = "dashed",
    
    # Annotations
    show_counts_on_plot = FALSE,
    show_major_grid = TRUE,
    show_minor_grid = FALSE
  ), params)
  
  # Check if all padj are NA
  all_padj_na <- all(is.na(results_df$padj))
  
  # Use padj by default (standard DESeq2 practice), only use pvalue if padj is not available
  # This ensures Y-axis, threshold lines, and coloring all use the same p-value type
  y_col <- if (all_padj_na) {
    "pvalue"
  } else {
    "padj"
  }
  
  # Calculate label score for ranking genes using the same p-value type as Y-axis
  p_col_for_score <- results_df[[y_col]]
  results_df$label_score <- -log10(p_col_for_score) * abs(results_df$log2FoldChange)
  results_df$label_score[is.na(results_df$label_score)] <- 0
  
  # Identify significant genes using the same p-value type as Y-axis
  p_col_for_sig <- results_df[[y_col]]
  
  sig_up_mask <- !is.na(p_col_for_sig) & 
                 p_col_for_sig < params$padj_cutoff & 
                 results_df$log2FoldChange > params$lfc_cutoff
  
  sig_down_mask <- !is.na(p_col_for_sig) & 
                   p_col_for_sig < params$padj_cutoff & 
                   results_df$log2FoldChange < -params$lfc_cutoff
  
  # Count significant genes
  n_up <- sum(sig_up_mask, na.rm = TRUE)
  n_down <- sum(sig_down_mask, na.rm = TRUE)
  n_sig <- n_up + n_down
  n_total <- nrow(results_df)
  
  # Select genes to label
  if (params$custom_labels_only) {
    # Only use custom labels (ignore automatic top N selection)
    up_genes <- character(0)
    down_genes <- character(0)
    genes_to_label <- params$force_labels
  } else {
    # Select top genes to label based on label_score
    up_genes <- results_df %>%
      dplyr::filter(sig_up_mask) %>%
      dplyr::arrange(desc(label_score)) %>%
      dplyr::slice_head(n = params$top_n_labels) %>%
      dplyr::pull(gene)
    
    down_genes <- results_df %>%
      dplyr::filter(sig_down_mask) %>%
      dplyr::arrange(desc(label_score)) %>%
      dplyr::slice_head(n = params$top_n_labels) %>%
      dplyr::pull(gene)
    
    # Combine with custom labels
    genes_to_label <- unique(c(up_genes, down_genes, params$force_labels))
  }
  
  # Create labels column
  results_df$label <- ""
  label_mask <- results_df$gene %in% genes_to_label
  results_df$label[label_mask] <- results_df$gene[label_mask]
  
  # Count labeled genes
  n_labeled_up <- sum(results_df$gene %in% up_genes)
  n_labeled_down <- sum(results_df$gene %in% down_genes)
  n_force_labeled <- length(params$force_labels)
  n_force_found <- sum(params$force_labels %in% results_df$gene)
  
  # Create custom color values
  keyvals <- rep(params$color_ns, nrow(results_df))
  names(keyvals) <- rep('NS', nrow(results_df))
  
  # Color significant genes
  keyvals[sig_up_mask] <- params$color_up
  names(keyvals)[sig_up_mask] <- 'Up'
  
  keyvals[sig_down_mask] <- params$color_down
  names(keyvals)[sig_down_mask] <- 'Down'
  
  # Handle NA values
  na_mask <- is.na(results_df$padj) | is.na(results_df$log2FoldChange)
  keyvals[na_mask] <- params$color_ns
  names(keyvals)[na_mask] <- 'NS'
  
  # Highlight selected genes from table with distinct color
  # Note: We only change the color, not the name, so they don't appear as a separate legend entry
  if (!is.null(params$selected_genes) && length(params$selected_genes) > 0) {
    selected_mask <- results_df$gene %in% params$selected_genes
    keyvals[selected_mask] <- params$color_selected
    # Keep original names (Up/Down/NS) to avoid adding "Selected" to legend
  }
  
  # Create plot title and subtitle
  plot_title <- params$contrast_name
  
  # Generate subtitle (NULL if disabled)
  plot_subtitle <- if (!params$show_subtitle) {
    NULL
  } else if (!is.null(params$custom_subtitle) && 
             nchar(trimws(params$custom_subtitle)) > 0) {
    # Use custom subtitle
    if (params$show_percentage) {
      pct_sig <- round((n_sig / n_total) * 100, 1)
      paste0(params$custom_subtitle, " (", pct_sig, "% significant)")
    } else {
      params$custom_subtitle
    }
  } else {
    # Auto-generate subtitle
    base_subtitle <- sprintf('Up-regulated: %d | Down-regulated: %d', n_up, n_down)
    if (params$show_percentage) {
      pct_sig <- round((n_sig / n_total) * 100, 1)
      paste0(base_subtitle, " (", pct_sig, "% significant)")
    } else {
      base_subtitle
    }
  }
  
  # Create caption (NULL if disabled)
  plot_caption <- if (!params$show_caption) {
    NULL
  } else if (!is.null(params$custom_caption) && 
             nchar(trimws(params$custom_caption)) > 0) {
    params$custom_caption
  } else {
    # Auto-generate caption with appropriate p-value terminology
    p_label <- if (y_col == "padj") "FDR" else "P-value"
    
    base_caption <- sprintf(
      '%s < %.2f, |Log2FC| > %.1f\nTop %d genes per direction ranked by -log10(%s) * |Log2FC|',
      p_label,
      params$padj_cutoff, 
      params$lfc_cutoff, 
      params$top_n_labels,
      p_label
    )
    
    if (n_force_labeled > 0) {
      base_caption <- paste0(base_caption, sprintf('\n%d custom genes labeled', n_force_found))
    }
    
    # Add note when using pvalue fallback
    if (y_col == "pvalue") {
      base_caption <- paste0(base_caption, '\nNote: Using p-value (adjusted p-values not available)')
    }
    
    base_caption
  }
  
  # Calculate axis limits if not fixed
  xlim_vals <- if (params$fix_axes && !is.na(params$xlim_min) && !is.na(params$xlim_max)) {
    c(params$xlim_min, params$xlim_max)
  } else {
    lfc_range <- range(results_df$log2FoldChange, na.rm = TRUE)
    lfc_padding <- diff(lfc_range) * 0.1
    c(lfc_range[1] - lfc_padding, lfc_range[2] + lfc_padding)
  }
  
  ylim_vals <- if (params$fix_axes && !is.na(params$ylim_min) && !is.na(params$ylim_max)) {
    c(params$ylim_min, params$ylim_max)
  } else {
    # Calculate -log10 transformed p-values for ylim
    p_col <- results_df[[y_col]]
    p_finite <- p_col[is.finite(p_col) & p_col > 0]
    if (length(p_finite) > 0) {
      max_y <- -log10(min(p_finite)) * 1.1
      c(0, max_y)
    } else {
      c(0, 10)
    }
  }
  
  # Use uniform point shape for all points
  point_shapes <- params$point_shape
  
  # Determine threshold line parameters
  if (params$show_threshold_lines) {
    # Show threshold lines with custom style
    cutoff_line_type <- params$threshold_style
    cutoff_line_col <- "grey30"
    cutoff_line_width <- 0.5
  } else {
    # Hide threshold lines by setting them to blank
    cutoff_line_type <- "blank"
    cutoff_line_col <- "transparent"
    cutoff_line_width <- 0
  }
  
  # Dynamic Y-axis label based on which p-value type is being used
  ylab_text <- if (y_col == "padj") {
    bquote(~-Log[10]~adjusted~italic(P))
  } else {
    bquote(~-Log[10]~italic(P)~value)
  }
  
  # Create volcano plot using EnhancedVolcano
  volcano_plot <- EnhancedVolcano::EnhancedVolcano(
    results_df,
    lab = results_df$label,
    selectLab = genes_to_label,  # Force these genes to be labeled regardless of thresholds
    x = 'log2FoldChange',
    y = y_col,
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption,
    captionLabSize = 10,
    subtitleLabSize = 11,
    titleLabSize = 13,
    pCutoff = params$padj_cutoff,
    FCcutoff = params$lfc_cutoff,
    pointSize = params$point_size,
    labSize = params$label_size,
    colCustom = keyvals,
    colAlpha = params$alpha,
    legendPosition = params$legend_position,
    legendLabSize = 12,
    legendIconSize = 4.0,
    drawConnectors = params$draw_connectors,
    widthConnectors = params$connector_width,
    colConnectors = 'grey50',
    arrowheads = TRUE,
    max.overlaps = params$max_overlaps,
    maxoverlapsConnectors = params$max_overlaps,
    min.segment.length = 0.1,
    boxedLabels = params$boxed_labels,
    shape = point_shapes,
    cutoffLineType = cutoff_line_type,
    cutoffLineCol = cutoff_line_col,
    cutoffLineWidth = cutoff_line_width,
    gridlines.major = params$show_major_grid,
    gridlines.minor = params$show_minor_grid,
    border = 'partial',
    borderWidth = 0.5,
    borderColour = 'black',
    xlim = xlim_vals,
    ylim = ylim_vals,
    ylab = ylab_text,
    xlab = bquote(~Log[2]~fold~change),
    axisLabSize = 12
  )
  
  # Overlay selected genes with a distinct color to guarantee visibility,
  # independent of EnhancedVolcano's internal color grouping/legend logic
  if (!is.null(params$selected_genes) && length(params$selected_genes) > 0) {
    sel_mask <- results_df$gene %in% params$selected_genes
    if (any(sel_mask, na.rm = TRUE)) {
      sel_df <- results_df[sel_mask & is.finite(results_df[[y_col]]) & results_df[[y_col]] > 0, , drop = FALSE]
      if (nrow(sel_df) > 0) {
        sel_df$y_plot <- -log10(sel_df[[y_col]])
        volcano_plot <- volcano_plot +
          ggplot2::geom_point(
            data = sel_df,
            mapping = ggplot2::aes(x = log2FoldChange, y = y_plot),
            color = params$color_selected,
            size = params$point_size,
            alpha = params$alpha,
            inherit.aes = FALSE,
            show.legend = FALSE
          )
      }
    }
  }
  
  # Apply title alignment
  # Calculate alignment values (hjust)
  title_hjust <- switch(params$title_align,
                        "left" = 0,
                        "center" = 0.5,
                        "right" = 1,
                        0.5)  # default to center
  
  subtitle_hjust <- switch(params$subtitle_align,
                           "left" = 0,
                           "center" = 0.5,
                           "right" = 1,
                           0.5)  # default to center
  
  caption_hjust <- switch(params$caption_align,
                          "left" = 0,
                          "center" = 0.5,
                          "right" = 1,
                          0.5)  # default to center
  
  # Apply text alignment
  volcano_plot <- volcano_plot + 
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = title_hjust),
      plot.subtitle = ggplot2::element_text(hjust = subtitle_hjust),
      plot.caption = ggplot2::element_text(hjust = caption_hjust)
    )
  
  # Add count annotations on plot if requested
  if (params$show_counts_on_plot) {
    # Calculate position for text box (top-right corner)
    x_pos <- xlim_vals[2] * 0.95
    y_pos <- ylim_vals[2] * 0.95
    
    count_text <- sprintf("Up: %d\nDown: %d\nTotal: %d", n_up, n_down, n_sig)
    
    volcano_plot <- volcano_plot +
      ggplot2::annotate(
        "label",
        x = x_pos,
        y = y_pos,
        label = count_text,
        hjust = 1,
        vjust = 1,
        size = 3,
        fill = "white",
        alpha = 0.8,
        label.size = 0.5
      )
  }
  
  # Return plot and statistics
  return(list(
    plot = volcano_plot,
    stats = list(
      n_up = n_up,
      n_down = n_down,
      n_total = n_total,
      n_sig = n_sig,
      n_labeled_up = n_labeled_up,
      n_labeled_down = n_labeled_down,
      n_force_labeled = n_force_labeled,
      n_force_found = n_force_found,
      n_selected_requested = length(params$selected_genes),
      n_selected_found = sum(results_df$gene %in% params$selected_genes, na.rm = TRUE),
      all_padj_na = all_padj_na
    )
  ))
}


#' Generate Cache Key for Volcano Plot
#'
#' Creates a unique cache key based on contrast name and parameters
#'
#' @param contrast Character string of contrast name
#' @param params List of parameters used for plot generation
#'
#' @return Character string MD5 hash
#'
#' @export
generate_cache_key_volcano <- function(contrast, params) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("digest package is required. Install with: install.packages('digest')")
  }
  
  # Create a list combining contrast and params
  cache_input <- list(
    contrast = contrast,
    params = params
  )
  
  # Generate MD5 hash
  cache_key <- digest::digest(cache_input, algo = "md5")
  
  return(cache_key)
}


#' Parse Force Labels Input
#'
#' Parses user input for force labels, splitting by comma or newline
#'
#' @param text Character string with gene names separated by comma or newline
#'
#' @return Character vector of gene names (trimmed, non-empty)
#'
#' @export
parse_force_labels <- function(text) {
  if (is.null(text) || text == "" || nchar(trimws(text)) == 0) {
    return(character(0))
  }
  
  # Split by comma or newline
  genes <- unlist(strsplit(text, "[,\n]"))
  
  # Trim whitespace
  genes <- trimws(genes)
  
  # Remove empty strings
  genes <- genes[genes != ""]
  
  # Remove duplicates
  genes <- unique(genes)
  
  return(genes)
}


#' Get Volcano Data Source
#'
#' Resolves data source priority for volcano plot generation
#'
#' @param contrast Character string of contrast name
#' @param use_shrunk Logical, whether to use shrunk log2FC
#' @param values Reactive values object from Shiny server
#'
#' @return List with components:
#'   - data: Data frame with results
#'   - source_type: Character string ("uploaded", "computed_shrunk", "computed_unshrunk")
#'   - shrinkage_applied: Logical
#'
#' @export
get_volcano_data_source <- function(contrast, use_shrunk, values) {
  
  # Priority 1: Check for uploaded data
  if (!is.null(values$uploaded_volcano_results) && 
      contrast %in% names(values$uploaded_volcano_results)) {
    
    uploaded_data <- values$uploaded_volcano_results[[contrast]]
    
    # Check if shrinkage info is available in uploaded data
    shrinkage_applied <- if ("shrinkage_applied" %in% names(uploaded_data)) {
      uploaded_data$shrinkage_applied
    } else {
      NA
    }
    
    return(list(
      data = uploaded_data,
      source_type = "uploaded",
      shrinkage_applied = shrinkage_applied
    ))
  }
  
  # Priority 2 & 3: Check for computed results
  if (!is.null(values$deseq_results) && 
      contrast %in% names(values$deseq_results)) {
    
    contrast_results <- values$deseq_results[[contrast]]
    
    # Determine which results to use based on use_shrunk preference
    if (use_shrunk && !is.null(contrast_results$results)) {
      # Use shrunk results if available and requested
      return(list(
        data = contrast_results$results,
        source_type = "computed_shrunk",
        shrinkage_applied = TRUE
      ))
    } else if (!is.null(contrast_results$unshrunken_results)) {
      # Use unshrunken results
      return(list(
        data = contrast_results$unshrunken_results,
        source_type = "computed_unshrunk",
        shrinkage_applied = FALSE
      ))
    } else if (!is.null(contrast_results$results)) {
      # Fallback to shrunk results if unshrunken not available
      return(list(
        data = contrast_results$results,
        source_type = "computed_shrunk",
        shrinkage_applied = TRUE
      ))
    }
  }
  
  # No data found
  return(list(
    data = NULL,
    source_type = "none",
    shrinkage_applied = NA
  ))
}


#' Sanitize Filename
#'
#' Sanitizes a string for use as a filename
#'
#' @param text Character string to sanitize
#'
#' @return Character string with only alphanumeric and underscores
#'
#' @export
sanitize_filename <- function(text) {
  # Replace non-alphanumeric characters with underscore
  sanitized <- gsub("[^A-Za-z0-9]", "_", text)
  
  # Remove leading/trailing underscores
  sanitized <- gsub("^_+|_+$", "", sanitized)
  
  # Replace multiple consecutive underscores with single underscore
  sanitized <- gsub("_+", "_", sanitized)
  
  return(sanitized)
}

