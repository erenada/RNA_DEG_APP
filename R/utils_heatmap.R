# Heatmap Visualization Utilities
# Author: Eren Ada, PhD
#
# This module provides helper functions for the Heatmap tab. It supports two
# visualization modes:
#   1) Expression heatmap (genes x samples) backed by DESeq2's dds object and
#      VST/rlog/log2(normalized+1) transforms.
#   2) Cross-contrast LFC heatmap (genes x contrasts) backed by per-contrast
#      DESeq2 results data frames or uploaded CSVs.
# The pheatmap package is used as the rendering backend.


# =============================================================================
# DATA SOURCING
# =============================================================================

#' Compute Transformed Expression Matrix
#'
#' Computes a log-scale expression matrix from a DESeq2 dds object using one of
#' three standard transformations. This function is intentionally pure (no
#' Shiny dependencies) so callers can wrap it in their own caching layer.
#'
#' @param dds A DESeqDataSet object (post-DESeq()).
#' @param transform One of "vst", "rlog", or "log2norm".
#'
#' @return Numeric matrix with rownames = genes, colnames = samples.
#'
#' @export
compute_transformed_matrix <- function(dds, transform = c("vst", "rlog", "log2norm")) {
  transform <- match.arg(transform)
  
  if (!requireNamespace("DESeq2", quietly = TRUE)) {
    stop("DESeq2 package is required.")
  }
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    stop("SummarizedExperiment package is required.")
  }
  
  if (transform == "vst") {
    vst_obj <- DESeq2::vst(dds, blind = FALSE)
    return(SummarizedExperiment::assay(vst_obj))
  } else if (transform == "rlog") {
    rlog_obj <- DESeq2::rlog(dds, blind = FALSE)
    return(SummarizedExperiment::assay(rlog_obj))
  } else {
    log2(DESeq2::counts(dds, normalized = TRUE) + 1)
  }
}


#' Resolve Sample Subset for Expression Heatmap
#'
#' Returns the column indices (sample names) that should appear in the
#' expression heatmap based on user choice of sample scope.
#'
#' @param dds A DESeqDataSet object.
#' @param contrast_info List from values$deseq_results[[contrast]]$contrast_info
#'   containing group1_samples and group2_samples (character vectors).
#' @param sample_scope One of "contrast" (samples in the two contrast groups)
#'   or "all" (all samples in the dds).
#'
#' @return Character vector of sample names that exist in colnames(dds).
#'
#' @export
resolve_heatmap_samples <- function(dds, contrast_info, sample_scope = c("contrast", "all")) {
  sample_scope <- match.arg(sample_scope)
  available <- colnames(dds)
  
  if (sample_scope == "all" || is.null(contrast_info)) {
    return(available)
  }
  
  contrast_samples <- c(contrast_info$group1_samples, contrast_info$group2_samples)
  contrast_samples <- contrast_samples[contrast_samples %in% available]
  
  if (length(contrast_samples) == 0) {
    return(available)  # Fallback: avoid empty matrix
  }
  
  return(contrast_samples)
}


#' Build Cross-Contrast LFC and padj Matrices
#'
#' For a vector of contrast names, fetches per-contrast DESeq2 results from
#' either uploaded CSVs (preferred when present) or pipeline output, then
#' pivots them into two wide gene-by-contrast matrices: one of log2 fold
#' changes and one of adjusted p-values. Genes missing from a given contrast
#' appear as NA cells.
#'
#' @param contrasts Character vector of contrast names.
#' @param values Reactive values object from the Shiny server.
#'
#' @return List with components:
#'   - lfc_matrix: numeric matrix (genes x contrasts)
#'   - padj_matrix: numeric matrix (genes x contrasts)
#'   - sources: named character vector indicating "uploaded" or "computed"
#'     per contrast.
#'
#' @export
get_heatmap_lfc_matrices <- function(contrasts, values) {
  if (length(contrasts) == 0) {
    return(list(lfc_matrix = NULL, padj_matrix = NULL, sources = character(0)))
  }
  
  long_rows <- list()
  sources <- character(length(contrasts))
  names(sources) <- contrasts
  
  for (cname in contrasts) {
    df <- NULL
    
    if (!is.null(values$uploaded_heatmap_results) &&
        cname %in% names(values$uploaded_heatmap_results)) {
      df <- values$uploaded_heatmap_results[[cname]]
      sources[[cname]] <- "uploaded"
    } else if (!is.null(values$deseq_results) &&
               cname %in% names(values$deseq_results)) {
      df <- values$deseq_results[[cname]]$results
      sources[[cname]] <- "computed"
    } else {
      sources[[cname]] <- "missing"
      next
    }
    
    if (is.null(df) || nrow(df) == 0) next
    
    required_cols <- c("gene", "log2FoldChange", "padj")
    if (!all(required_cols %in% colnames(df))) {
      next
    }
    
    sub <- df[, c("gene", "log2FoldChange", "padj"), drop = FALSE]
    sub$contrast <- cname
    long_rows[[cname]] <- sub
  }
  
  if (length(long_rows) == 0) {
    return(list(lfc_matrix = NULL, padj_matrix = NULL, sources = sources))
  }
  
  long_df <- do.call(rbind, long_rows)
  
  long_df <- long_df[!is.na(long_df$gene) & long_df$gene != "", ]
  long_df <- long_df[!duplicated(long_df[, c("gene", "contrast")]), ]
  
  all_genes <- unique(long_df$gene)
  
  lfc_matrix <- matrix(NA_real_, nrow = length(all_genes), ncol = length(contrasts),
                       dimnames = list(all_genes, contrasts))
  padj_matrix <- matrix(NA_real_, nrow = length(all_genes), ncol = length(contrasts),
                        dimnames = list(all_genes, contrasts))
  
  for (i in seq_len(nrow(long_df))) {
    g <- long_df$gene[i]
    c_idx <- long_df$contrast[i]
    lfc_matrix[g, c_idx] <- long_df$log2FoldChange[i]
    padj_matrix[g, c_idx] <- long_df$padj[i]
  }
  
  list(lfc_matrix = lfc_matrix, padj_matrix = padj_matrix, sources = sources)
}


# =============================================================================
# GENE SELECTION
# =============================================================================

#' Parse Custom Gene List
#'
#' Splits a free-form text input on commas, semicolons, whitespace, and
#' newlines. Optionally appends genes from an uploaded plain text or CSV file.
#'
#' @param text Character string from a textAreaInput, may be NULL or empty.
#' @param file_path Optional path to an uploaded .txt or .csv file. The first
#'   column (or all rows) is treated as the gene list.
#'
#' @return Character vector of unique, trimmed, non-empty gene names.
#'
#' @export
parse_heatmap_gene_list <- function(text = NULL, file_path = NULL) {
  out <- character(0)
  
  if (!is.null(text) && nchar(trimws(text)) > 0) {
    parts <- unlist(strsplit(text, "[,;\\s\n\r\t]+", perl = TRUE))
    out <- c(out, trimws(parts))
  }
  
  if (!is.null(file_path) && file.exists(file_path)) {
    ext <- tools::file_ext(file_path)
    if (tolower(ext) == "csv") {
      df <- tryCatch(
        utils::read.csv(file_path, header = TRUE, stringsAsFactors = FALSE),
        error = function(e) NULL
      )
      if (!is.null(df) && ncol(df) >= 1) {
        out <- c(out, trimws(as.character(df[[1]])))
      }
    } else {
      lines <- tryCatch(
        readLines(file_path, warn = FALSE),
        error = function(e) character(0)
      )
      parts <- unlist(strsplit(lines, "[,;\\s\n\r\t]+", perl = TRUE))
      out <- c(out, trimws(parts))
    }
  }
  
  out <- out[nchar(out) > 0]
  unique(out)
}


#' Select Genes for the Heatmap
#'
#' Implements the four supported gene-selection methods. The exact data needed
#' depends on the heatmap mode and the method:
#'   - Expression mode receives the per-contrast DESeq2 results data frame.
#'   - LFC mode receives lfc_matrix and padj_matrix from
#'     get_heatmap_lfc_matrices().
#'
#' @param method One of "top_padj", "top_lfc", "threshold", "custom".
#' @param mode One of "expression", "lfc".
#' @param params Named list with possible keys: top_n, padj_cutoff, lfc_cutoff,
#'   threshold_quantifier ("any" or "all"), custom_genes (character vector).
#' @param results_df Per-contrast results data frame (expression mode only).
#' @param lfc_matrix Numeric matrix (genes x contrasts) for LFC mode.
#' @param padj_matrix Numeric matrix (genes x contrasts) for LFC mode.
#'
#' @return Character vector of gene names.
#'
#' @export
select_heatmap_genes <- function(method, mode, params,
                                 results_df = NULL,
                                 lfc_matrix = NULL,
                                 padj_matrix = NULL) {
  
  top_n <- if (!is.null(params$top_n) && !is.na(params$top_n)) params$top_n else 50
  padj_cutoff <- if (!is.null(params$padj_cutoff) && !is.na(params$padj_cutoff)) params$padj_cutoff else 0.05
  lfc_cutoff <- if (!is.null(params$lfc_cutoff) && !is.na(params$lfc_cutoff)) params$lfc_cutoff else 1
  quantifier <- if (!is.null(params$threshold_quantifier)) params$threshold_quantifier else "any"
  
  if (mode == "expression") {
    if (is.null(results_df) || nrow(results_df) == 0) return(character(0))
    df <- results_df
    
    if (method == "top_padj") {
      df <- df[!is.na(df$padj), , drop = FALSE]
      if (nrow(df) == 0) return(character(0))
      df <- df[order(df$padj, decreasing = FALSE), , drop = FALSE]
      n <- min(top_n, nrow(df))
      return(as.character(df$gene[seq_len(n)]))
    }
    
    if (method == "top_lfc") {
      df <- df[!is.na(df$log2FoldChange), , drop = FALSE]
      if (nrow(df) == 0) return(character(0))
      df <- df[order(abs(df$log2FoldChange), decreasing = TRUE), , drop = FALSE]
      n <- min(top_n, nrow(df))
      return(as.character(df$gene[seq_len(n)]))
    }
    
    if (method == "threshold") {
      mask <- !is.na(df$padj) & !is.na(df$log2FoldChange) &
              df$padj < padj_cutoff & abs(df$log2FoldChange) > lfc_cutoff
      return(as.character(df$gene[mask]))
    }
    
    if (method == "custom") {
      requested <- params$custom_genes
      if (is.null(requested) || length(requested) == 0) return(character(0))
      available <- as.character(df$gene)
      return(requested[requested %in% available])
    }
    
    return(character(0))
  }
  
  # LFC mode
  if (is.null(lfc_matrix) || nrow(lfc_matrix) == 0) return(character(0))
  
  if (method == "top_padj") {
    if (is.null(padj_matrix)) return(rownames(lfc_matrix)[seq_len(min(top_n, nrow(lfc_matrix)))])
    min_padj <- apply(padj_matrix, 1, function(v) suppressWarnings(min(v, na.rm = TRUE)))
    min_padj[!is.finite(min_padj)] <- NA_real_
    ord <- order(min_padj, decreasing = FALSE, na.last = TRUE)
    valid_n <- sum(!is.na(min_padj))
    n <- min(top_n, valid_n)
    if (n == 0) return(character(0))
    return(rownames(lfc_matrix)[ord[seq_len(n)]])
  }
  
  if (method == "top_lfc") {
    max_abs_lfc <- apply(lfc_matrix, 1, function(v) suppressWarnings(max(abs(v), na.rm = TRUE)))
    max_abs_lfc[!is.finite(max_abs_lfc)] <- NA_real_
    ord <- order(max_abs_lfc, decreasing = TRUE, na.last = TRUE)
    valid_n <- sum(!is.na(max_abs_lfc))
    n <- min(top_n, valid_n)
    if (n == 0) return(character(0))
    return(rownames(lfc_matrix)[ord[seq_len(n)]])
  }
  
  if (method == "threshold") {
    if (is.null(padj_matrix)) {
      mask_lfc <- abs(lfc_matrix) > lfc_cutoff
      mask_lfc[is.na(mask_lfc)] <- FALSE
      keep <- if (quantifier == "all") apply(mask_lfc, 1, all) else apply(mask_lfc, 1, any)
    } else {
      mask <- !is.na(padj_matrix) & !is.na(lfc_matrix) &
              padj_matrix < padj_cutoff & abs(lfc_matrix) > lfc_cutoff
      keep <- if (quantifier == "all") apply(mask, 1, all) else apply(mask, 1, any)
    }
    return(rownames(lfc_matrix)[keep])
  }
  
  if (method == "custom") {
    requested <- params$custom_genes
    if (is.null(requested) || length(requested) == 0) return(character(0))
    return(requested[requested %in% rownames(lfc_matrix)])
  }
  
  character(0)
}


# =============================================================================
# ANNOTATIONS
# =============================================================================

#' Build Column Annotation Data Frame for Expression Heatmap
#'
#' Pulls colData from the dds and selects the requested factor columns. The
#' returned data frame has rownames matching colnames(dds) and is ready for
#' pheatmap's annotation_col argument.
#'
#' @param dds A DESeqDataSet object.
#' @param factor_cols Character vector of column names to include from
#'   colData(dds). Columns not present are silently skipped.
#'
#' @return data.frame or NULL when factor_cols is empty.
#'
#' @export
build_heatmap_annotations <- function(dds, factor_cols) {
  if (is.null(factor_cols) || length(factor_cols) == 0) return(NULL)
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) return(NULL)
  
  cd <- as.data.frame(SummarizedExperiment::colData(dds), stringsAsFactors = FALSE)
  factor_cols <- intersect(factor_cols, colnames(cd))
  if (length(factor_cols) == 0) return(NULL)
  
  out <- cd[, factor_cols, drop = FALSE]
  
  for (j in seq_along(out)) {
    if (is.character(out[[j]])) {
      out[[j]] <- factor(out[[j]])
    } else if (is.logical(out[[j]])) {
      out[[j]] <- factor(out[[j]])
    }
  }
  
  rownames(out) <- rownames(cd)
  out
}


#' Identify Suitable Annotation Columns
#'
#' Returns colData column names whose unique-value count is between 2 and
#' n_samples - 1 (inclusive). This filters out single-value columns (no
#' visual signal) and per-sample identifiers (legend explosion).
#'
#' @param dds A DESeqDataSet object.
#'
#' @return Character vector of eligible column names.
#'
#' @export
eligible_annotation_columns <- function(dds) {
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) return(character(0))
  cd <- as.data.frame(SummarizedExperiment::colData(dds), stringsAsFactors = FALSE)
  n_samples <- nrow(cd)
  out <- character(0)
  for (col in colnames(cd)) {
    vals <- cd[[col]]
    if (is.numeric(vals)) {
      n_unique <- length(unique(vals[!is.na(vals)]))
    } else {
      vals <- as.character(vals)
      n_unique <- length(unique(vals[!is.na(vals) & vals != ""]))
    }
    if (n_unique >= 2 && n_unique <= max(2, n_samples - 1)) {
      out <- c(out, col)
    }
  }
  out
}


# =============================================================================
# COLOR PALETTES
# =============================================================================

#' Build a Color Vector for pheatmap
#'
#' @param palette Palette identifier; one of "RdBu", "RdYlBu", "Spectral",
#'   "Viridis", "Plasma".
#' @param n Number of colors to return (default 100).
#' @param reverse Logical, reverse the palette order.
#'
#' @return Character vector of hex color codes.
#'
#' @export
build_heatmap_palette <- function(palette = "RdBu", n = 100, reverse = FALSE) {
  cols <- switch(
    palette,
    "RdBu"     = rev(grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "RdBu"))(n)),
    "RdYlBu"   = rev(grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlBu"))(n)),
    "Spectral" = rev(grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(n)),
    "Viridis"  = grDevices::hcl.colors(n, "Viridis"),
    "Plasma"   = grDevices::hcl.colors(n, "Plasma"),
    rev(grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "RdBu"))(n))
  )
  if (isTRUE(reverse)) cols <- rev(cols)
  cols
}


# =============================================================================
# PLOT GENERATION
# =============================================================================

# Internal helper: drop rows that pheatmap cannot handle gracefully.
.drop_problematic_rows <- function(mat, scale = "none") {
  keep <- rep(TRUE, nrow(mat))
  
  all_na <- apply(mat, 1, function(v) all(is.na(v)))
  keep <- keep & !all_na
  
  if (scale == "row") {
    sds <- apply(mat, 1, function(v) stats::sd(v, na.rm = TRUE))
    keep <- keep & is.finite(sds) & sds > 0
  }
  
  mat[keep, , drop = FALSE]
}


#' Generate Expression Heatmap (genes x samples)
#'
#' Builds a pheatmap object from a transformed expression matrix. The function
#' is intentionally pure: caching, tryCatch error reporting, and rendering
#' belong to the calling Shiny module.
#'
#' @param expr_matrix Numeric matrix from compute_transformed_matrix().
#' @param gene_set Character vector of genes to display (will be intersected
#'   with rownames(expr_matrix)).
#' @param params Named list of styling parameters; see code for full list.
#' @param annotation_df Optional data frame for column annotations (or NULL).
#'
#' @return List with components:
#'   - plot: pheatmap object (silent = TRUE).
#'   - matrix: numeric matrix of values actually used.
#'   - n_genes: number of rows after filtering.
#'   - warnings: character vector of human-readable warnings.
#'
#' @export
generate_heatmap_expression <- function(expr_matrix, gene_set, params, annotation_df = NULL) {
  if (!requireNamespace("pheatmap", quietly = TRUE)) {
    stop("pheatmap package is required.")
  }
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop("RColorBrewer package is required.")
  }
  
  warnings_out <- character(0)
  
  defaults <- list(
    scale = "row",
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    distance = "euclidean",
    linkage = "complete",
    palette = "RdBu",
    palette_reverse = FALSE,
    zlim_min = NA_real_,
    zlim_max = NA_real_,
    show_rownames = TRUE,
    show_colnames = TRUE,
    fontsize_row = 8,
    fontsize_col = 10,
    cellwidth = NA_real_,
    cellheight = NA_real_,
    treeheight_row = 30,
    treeheight_col = 30,
    show_legend = TRUE,
    show_annotation_legend = TRUE,
    border_color = NA,
    na_color = "#DDDDDD",
    title = NULL
  )
  params <- modifyList(defaults, params %||% list())
  
  available_genes <- intersect(gene_set, rownames(expr_matrix))
  if (length(available_genes) == 0) {
    return(list(plot = NULL, matrix = NULL, n_genes = 0,
                warnings = "No selected genes are present in the expression matrix."))
  }
  
  mat <- expr_matrix[available_genes, , drop = FALSE]
  
  before_n <- nrow(mat)
  mat <- .drop_problematic_rows(mat, scale = params$scale)
  if (nrow(mat) < before_n) {
    warnings_out <- c(warnings_out,
      paste0("Dropped ", before_n - nrow(mat),
             " gene(s) that were all-NA or had zero variance under '",
             params$scale, "' scaling."))
  }
  
  if (nrow(mat) < 2) {
    if (params$cluster_rows) {
      warnings_out <- c(warnings_out, "Row clustering disabled: fewer than 2 genes available.")
      params$cluster_rows <- FALSE
    }
  }
  if (ncol(mat) < 2 && params$cluster_cols) {
    warnings_out <- c(warnings_out, "Column clustering disabled: fewer than 2 samples available.")
    params$cluster_cols <- FALSE
  }
  
  if (nrow(mat) == 0 || ncol(mat) == 0) {
    return(list(plot = NULL, matrix = mat, n_genes = nrow(mat),
                warnings = c(warnings_out,
                  "Resulting matrix is empty after filtering.")))
  }
  
  n_colors <- 100
  color_vec <- build_heatmap_palette(params$palette, n = n_colors, reverse = params$palette_reverse)
  
  breaks_arg <- NA
  if (!is.na(params$zlim_min) && !is.na(params$zlim_max) && params$zlim_min < params$zlim_max) {
    breaks_arg <- seq(params$zlim_min, params$zlim_max, length.out = n_colors + 1)
  }
  
  pheatmap_args <- list(
    mat                 = mat,
    scale               = params$scale,
    cluster_rows        = isTRUE(params$cluster_rows),
    cluster_cols        = isTRUE(params$cluster_cols),
    clustering_distance_rows = params$distance,
    clustering_distance_cols = params$distance,
    clustering_method   = params$linkage,
    color               = color_vec,
    show_rownames       = isTRUE(params$show_rownames),
    show_colnames       = isTRUE(params$show_colnames),
    fontsize_row        = params$fontsize_row,
    fontsize_col        = params$fontsize_col,
    treeheight_row      = params$treeheight_row,
    treeheight_col      = params$treeheight_col,
    legend              = isTRUE(params$show_legend),
    border_color        = params$border_color,
    na_col              = params$na_color,
    silent              = TRUE
  )
  
  if (!identical(breaks_arg, NA)) pheatmap_args$breaks <- breaks_arg
  if (!is.null(annotation_df) && nrow(annotation_df) > 0) {
    pheatmap_args$annotation_col <- annotation_df
    pheatmap_args$annotation_legend <- isTRUE(params$show_annotation_legend)
  }
  if (!is.na(params$cellwidth)) pheatmap_args$cellwidth <- params$cellwidth
  if (!is.na(params$cellheight)) pheatmap_args$cellheight <- params$cellheight
  if (!is.null(params$title) && nchar(trimws(as.character(params$title))) > 0) {
    pheatmap_args$main <- params$title
  }
  
  plot_obj <- do.call(pheatmap::pheatmap, pheatmap_args)
  
  list(plot = plot_obj, matrix = mat, n_genes = nrow(mat), warnings = warnings_out)
}


#' Generate Cross-Contrast LFC Heatmap (genes x contrasts)
#'
#' Builds a pheatmap object from a gene-by-contrast log2 fold change matrix.
#' The color scale is forced symmetric around zero by default; user-supplied
#' zlim_min/zlim_max override.
#'
#' @param lfc_matrix Numeric matrix (genes x contrasts) from
#'   get_heatmap_lfc_matrices().
#' @param gene_set Character vector of genes to display.
#' @param params Named list of styling parameters; see code for full list.
#'
#' @return List(plot, matrix, n_genes, warnings).
#'
#' @export
generate_heatmap_lfc <- function(lfc_matrix, gene_set, params) {
  if (!requireNamespace("pheatmap", quietly = TRUE)) {
    stop("pheatmap package is required.")
  }
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop("RColorBrewer package is required.")
  }
  
  warnings_out <- character(0)
  
  defaults <- list(
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    distance = "euclidean",
    linkage = "complete",
    palette = "RdBu",
    palette_reverse = FALSE,
    zlim_min = NA_real_,
    zlim_max = NA_real_,
    show_rownames = TRUE,
    show_colnames = TRUE,
    fontsize_row = 8,
    fontsize_col = 10,
    cellwidth = NA_real_,
    cellheight = NA_real_,
    treeheight_row = 30,
    treeheight_col = 30,
    show_legend = TRUE,
    border_color = NA,
    na_color = "#DDDDDD",
    title = NULL
  )
  params <- modifyList(defaults, params %||% list())
  
  available_genes <- intersect(gene_set, rownames(lfc_matrix))
  if (length(available_genes) == 0) {
    return(list(plot = NULL, matrix = NULL, n_genes = 0,
                warnings = "No selected genes are present in the LFC matrix."))
  }
  
  mat <- lfc_matrix[available_genes, , drop = FALSE]
  
  all_na <- apply(mat, 1, function(v) all(is.na(v)))
  if (any(all_na)) {
    warnings_out <- c(warnings_out,
      paste0("Dropped ", sum(all_na), " gene(s) with NA values across all selected contrasts."))
    mat <- mat[!all_na, , drop = FALSE]
  }
  
  if (nrow(mat) < 2 && isTRUE(params$cluster_rows)) {
    warnings_out <- c(warnings_out, "Row clustering disabled: fewer than 2 genes available.")
    params$cluster_rows <- FALSE
  }
  if (ncol(mat) < 2 && isTRUE(params$cluster_cols)) {
    warnings_out <- c(warnings_out, "Column clustering disabled: fewer than 2 contrasts selected.")
    params$cluster_cols <- FALSE
  }
  
  if (nrow(mat) == 0 || ncol(mat) == 0) {
    return(list(plot = NULL, matrix = mat, n_genes = nrow(mat),
                warnings = c(warnings_out, "Resulting matrix is empty after filtering.")))
  }
  
  if (any(apply(mat, 1, function(v) {
    nv <- v[!is.na(v)]
    length(nv) >= 2 && stats::sd(nv) == 0
  }))) {
    if (isTRUE(params$cluster_rows)) {
      warnings_out <- c(warnings_out,
        "Some rows are constant; row clustering may be unstable for euclidean distance.")
    }
  }
  
  n_colors <- 100
  if (!is.na(params$zlim_min) && !is.na(params$zlim_max) && params$zlim_min < params$zlim_max) {
    breaks_arg <- seq(params$zlim_min, params$zlim_max, length.out = n_colors + 1)
  } else {
    max_abs <- suppressWarnings(max(abs(mat), na.rm = TRUE))
    if (!is.finite(max_abs) || max_abs == 0) max_abs <- 1
    breaks_arg <- seq(-max_abs, max_abs, length.out = n_colors + 1)
  }
  
  color_vec <- build_heatmap_palette(params$palette, n = n_colors, reverse = params$palette_reverse)
  
  pheatmap_args <- list(
    mat                  = mat,
    scale                = "none",
    cluster_rows         = isTRUE(params$cluster_rows),
    cluster_cols         = isTRUE(params$cluster_cols),
    clustering_distance_rows = params$distance,
    clustering_distance_cols = params$distance,
    clustering_method    = params$linkage,
    color                = color_vec,
    breaks               = breaks_arg,
    show_rownames        = isTRUE(params$show_rownames),
    show_colnames        = isTRUE(params$show_colnames),
    fontsize_row         = params$fontsize_row,
    fontsize_col         = params$fontsize_col,
    treeheight_row       = params$treeheight_row,
    treeheight_col       = params$treeheight_col,
    legend               = isTRUE(params$show_legend),
    border_color         = params$border_color,
    na_col               = params$na_color,
    silent               = TRUE
  )
  
  if (!is.na(params$cellwidth)) pheatmap_args$cellwidth <- params$cellwidth
  if (!is.na(params$cellheight)) pheatmap_args$cellheight <- params$cellheight
  if (!is.null(params$title) && nchar(trimws(as.character(params$title))) > 0) {
    pheatmap_args$main <- params$title
  }
  
  plot_obj <- do.call(pheatmap::pheatmap, pheatmap_args)
  
  list(plot = plot_obj, matrix = mat, n_genes = nrow(mat), warnings = warnings_out)
}


# =============================================================================
# CACHE KEYS
# =============================================================================

#' Generate Cache Key for Heatmap Plot
#'
#' Hashes the mode, the (sorted) parameter list, and the selected gene set to
#' produce a stable identifier suitable for the values$heatmap_cache list.
#'
#' @param mode "expression" or "lfc".
#' @param params Named list of styling/data parameters.
#' @param gene_set Character vector of selected gene names.
#'
#' @return MD5 hex string.
#'
#' @export
generate_cache_key_heatmap <- function(mode, params, gene_set) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("digest package is required.")
  }
  cache_input <- list(
    mode = mode,
    params = params[order(names(params))],
    gene_set = sort(unique(as.character(gene_set)))
  )
  digest::digest(cache_input, algo = "md5")
}


# Internal: %||% operator (NULL-safe default)
`%||%` <- function(a, b) if (is.null(a)) b else a
