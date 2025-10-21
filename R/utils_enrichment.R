# Enrichment Analysis Utilities
# Author: Eren Ada, PhD
# Date: 10/17/2025
#
# This file contains utility functions for GO enrichment analysis

#' Map Gene IDs using bitr with error handling
#' 
#' @param genes Character vector of gene IDs to convert
#' @param from_type Source ID type (e.g., "SYMBOL", "ENSEMBL")
#' @param to_type Target ID type (usually "ENTREZID")
#' @param orgdb Organism annotation database (org.Mm.eg.db or org.Hs.eg.db)
#' @return Data frame with mapped IDs or NULL if error
map_gene_ids <- function(genes, from_type, to_type, orgdb) {
  if (length(genes) == 0) {
    warning("Empty gene list provided to map_gene_ids")
    return(NULL)
  }
  
  tryCatch({
    # Remove any NA or empty values
    genes <- genes[!is.na(genes) & genes != ""]
    
    if (length(genes) == 0) {
      warning("No valid genes after filtering NAs")
      return(NULL)
    }
    
    # Perform ID conversion using bitr
    mapped <- clusterProfiler::bitr(
      geneID = genes,
      fromType = from_type,
      toType = to_type,
      OrgDb = orgdb
    )
    
    return(mapped)
    
  }, error = function(e) {
    warning(paste("Error in ID mapping:", e$message))
    return(NULL)
  })
}

#' Compute mapping statistics
#' 
#' @param original Character vector of original gene IDs
#' @param mapped Data frame returned from map_gene_ids
#' @return List with mapping statistics
compute_mapping_stats <- function(original, mapped) {
  n_original <- length(original)
  n_mapped <- if (is.null(mapped)) 0 else nrow(mapped)
  mapping_rate <- if (n_original == 0) 0 else n_mapped / n_original
  
  # Find unmapped genes
  if (!is.null(mapped) && nrow(mapped) > 0) {
    # Get the first column name (usually SYMBOL or ENSEMBL)
    from_col <- colnames(mapped)[1]
    unmapped <- setdiff(original, mapped[[from_col]])
  } else {
    unmapped <- original
  }
  
  return(list(
    n_original = n_original,
    n_mapped = n_mapped,
    mapping_rate = mapping_rate,
    mapping_rate_pct = round(mapping_rate * 100, 1),
    n_unmapped = length(unmapped),
    unmapped_genes = unmapped
  ))
}

#' Safe wrapper for simplify with error handling
#' 
#' @param ego enrichResult object from enrichGO
#' @param cutoff Similarity cutoff for term reduction (0-1)
#' @return Simplified enrichResult or original if simplification fails
safe_simplify <- function(ego, cutoff = 0.7) {
  if (is.null(ego) || nrow(ego) == 0) {
    return(ego)
  }
  
  tryCatch({
    ego_simplified <- clusterProfiler::simplify(
      ego, 
      cutoff = cutoff,
      by = "p.adjust",
      select_fun = min
    )
    return(ego_simplified)
  }, error = function(e) {
    warning(paste("Could not simplify GO terms:", e$message, "- Using original results"))
    return(ego)
  })
}

#' Compute mean log2 fold change for gene sets
#' 
#' @param gene_ids Character vector of gene IDs (can be "/" separated)
#' @param fold_changes Named vector of log2 fold changes
#' @return Numeric mean log2FC
compute_mean_log2fc <- function(gene_ids, fold_changes) {
  if (length(gene_ids) == 0) {
    return(NA)
  }
  
  # Split gene IDs if they're "/" separated (from GO results)
  genes <- unlist(strsplit(gene_ids, "/"))
  
  # Get fold changes for these genes
  fc_values <- fold_changes[genes]
  
  # Compute mean, removing NAs
  mean_fc <- mean(fc_values, na.rm = TRUE)
  
  return(mean_fc)
}

#' Generate cache key for enrichment results
#' 
#' Uses digest package to create a hash of parameters for caching
#' 
#' @param contrast_name Name of the contrast
#' @param params List of analysis parameters
#' @return Character string cache key
generate_cache_key <- function(contrast_name, params) {
  # Create a list of all relevant parameters
  key_components <- list(
    contrast = contrast_name,
    organism = params$organism,
    id_type = params$id_type,
    ontologies = sort(params$ontologies),  # Sort to ensure consistent ordering
    direction = params$direction,
    alpha = params$alpha,
    lfc_threshold = params$lfc_threshold,
    pvalue_cutoff = params$pvalue_cutoff,
    qvalue_cutoff = params$qvalue_cutoff,
    min_gs_size = params$min_gs_size,
    max_gs_size = params$max_gs_size,
    simplify_cutoff = params$simplify_cutoff
  )
  
  # Generate hash
  cache_key <- digest::digest(key_components, algo = "md5")
  
  return(cache_key)
}

#' Augment enrichment results with additional metrics
#' 
#' Adds mean_log2FC and enrichment_score columns to enrichment results
#' 
#' @param ego enrichResult object
#' @param fold_changes Named vector of log2 fold changes (gene names as names)
#' @return enrichResult object with augmented results
augment_enrichment_results <- function(ego, fold_changes) {
  if (is.null(ego) || nrow(ego) == 0) {
    return(ego)
  }
  
  # Add mean log2FC for genes in each term
  ego@result$mean_log2FC <- sapply(ego@result$geneID, function(genes) {
    compute_mean_log2fc(genes, fold_changes)
  })
  
  # Add enrichment score (fold enrichment)
  # enrichment_score = (k/n) / (M/N)
  # where k=genes in term and list, n=genes in list, M=genes in term, N=total genes
  # Parse GeneRatio (k/n)
  gene_ratio_parts <- strsplit(as.character(ego@result$GeneRatio), "/")
  k <- as.numeric(sapply(gene_ratio_parts, `[`, 1))  # Count in your list
  n <- as.numeric(sapply(gene_ratio_parts, `[`, 2))  # Total in your list
  
  # Parse BgRatio (M/N)
  bg_ratio_parts <- strsplit(as.character(ego@result$BgRatio), "/")
  M <- as.numeric(sapply(bg_ratio_parts, `[`, 1))  # Total in GO term
  N <- as.numeric(sapply(bg_ratio_parts, `[`, 2))  # Total universe
  
  # Calculate fold enrichment: (k/n) / (M/N) = (k*N) / (n*M)
  ego@result$enrichment_score <- (k / n) / (M / N)
  
  return(ego)
}

