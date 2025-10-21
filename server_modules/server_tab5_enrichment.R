# Server logic for Tab 5: Enrichment Analysis
# Author: Eren Ada, PhD
#
# This file contains all server-side logic for GO enrichment analysis

# =============================================================================
# TAB 5: ENRICHMENT ANALYSIS
# =============================================================================

# =============================================================================
# PRE-CONDITION CHECKS & AVAILABILITY
# =============================================================================

# Control enrichment tab availability for conditional panels
# Now checks for EITHER computed or uploaded results
output$enrichment_available <- reactive({
  # Check if DEG results exist (computed OR uploaded)
  has_computed <- !is.null(values$deseq_results) && length(values$deseq_results) > 0
  has_uploaded <- !is.null(values$use_uploaded_results) && 
                  values$use_uploaded_results && 
                  !is.null(values$uploaded_deg_results) && 
                  length(values$uploaded_deg_results) > 0
  
  return(has_computed || has_uploaded)
})
outputOptions(output, "enrichment_available", suspendWhenHidden = FALSE)

# =============================================================================
# CSV UPLOAD FOR PRE-COMPUTED DEG RESULTS
# =============================================================================

# Handle DEG results CSV upload (multiple files supported)
observeEvent(input$upload_deg_results, {
  req(input$upload_deg_results)
  
  tryCatch({
    
    message("\n=== UPLOADING DEG RESULTS CSV(s) ===")
    
    # Initialize uploaded results list if needed
    if (is.null(values$uploaded_deg_results)) {
      values$uploaded_deg_results <- list()
    }
    
    # Process each uploaded file
    n_files <- nrow(input$upload_deg_results)
    uploaded_contrasts <- character(0)
    errors <- character(0)
    
    for (i in seq_len(n_files)) {
      file_path <- input$upload_deg_results$datapath[i]
      file_name <- input$upload_deg_results$name[i]
      
      message(paste("Processing file", i, "of", n_files, ":", file_name))
      
      file_result <- tryCatch({
        
        # Read CSV
        uploaded_data <- read.csv(file_path, stringsAsFactors = FALSE)
        message(paste("  Read", nrow(uploaded_data), "rows"))
        
        # Validate required columns
        required_cols <- c("gene", "log2FoldChange", "padj", "contrast")
        missing_cols <- setdiff(required_cols, colnames(uploaded_data))
        
        if (length(missing_cols) > 0) {
          stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
        }
        
        # Validate data types
        if (!is.numeric(uploaded_data$log2FoldChange)) {
          stop("log2FoldChange must be numeric")
        }
        
        if (!is.numeric(uploaded_data$padj)) {
          stop("padj must be numeric")
        }
        
        # Auto-generate direction column if missing
        if (!"direction" %in% colnames(uploaded_data)) {
          uploaded_data$direction <- ifelse(
            uploaded_data$log2FoldChange > 0, "Up",
            ifelse(uploaded_data$log2FoldChange < 0, "Down", "NS")
          )
          message("  Auto-generated direction column from log2FoldChange")
        } else {
          # Fill missing direction values based on log2FoldChange
          missing_dir <- is.na(uploaded_data$direction) | uploaded_data$direction == ""
          if (any(missing_dir)) {
            uploaded_data$direction[missing_dir] <- ifelse(
              uploaded_data$log2FoldChange[missing_dir] > 0, "Up",
              ifelse(uploaded_data$log2FoldChange[missing_dir] < 0, "Down", "NS")
            )
            message(paste("  Filled", sum(missing_dir), "missing direction values"))
          }
        }
        
        # Get unique contrasts in this file
        contrasts_in_file <- unique(uploaded_data$contrast)
        message(paste("  Found", length(contrasts_in_file), "contrast(s):",
                     paste(contrasts_in_file, collapse = ", ")))
        
        # Store each contrast
        for (contrast_name in contrasts_in_file) {
          contrast_data <- uploaded_data[uploaded_data$contrast == contrast_name, ]
          
          # Set gene as rownames (matches DESeq structure)
          results_df <- contrast_data
          rownames(results_df) <- results_df$gene
          
          # Store in uploaded results
          values$uploaded_deg_results[[contrast_name]] <- list(
            results = results_df,
            source = "uploaded",
            upload_timestamp = Sys.time(),
            filename = file_name
          )
          
          uploaded_contrasts <- c(uploaded_contrasts, contrast_name)
          message(paste("    Stored", nrow(results_df), "genes for:", contrast_name))
        }
        
        return(list(success = TRUE, contrasts = contrasts_in_file))
        
      }, error = function(e) {
        errors <<- c(errors, paste0(file_name, ": ", e$message))
        return(list(success = FALSE, error = e$message))
      })
    }
    
    # Set flag that uploaded results are active
    if (length(uploaded_contrasts) > 0) {
      
      message(paste("Setting use_uploaded_results = TRUE"))
      message(paste("Number of contrasts in values$uploaded_deg_results:", 
                   length(values$uploaded_deg_results)))
      message(paste("Contrast names:", paste(names(values$uploaded_deg_results), collapse = ", ")))
      
      values$use_uploaded_results <- TRUE
      
      message(paste("Flag set. values$use_uploaded_results =", values$use_uploaded_results))
      
      # Show success notification
      success_msg <- paste0(
        "Successfully uploaded ", length(unique(uploaded_contrasts)),
        " contrast(s) from ", n_files, " file(s)"
      )
      
      if (length(errors) > 0) {
        success_msg <- paste0(success_msg, " (", length(errors), " file(s) had errors)")
      }
      
      showNotification(
        success_msg,
        type = if (length(errors) == 0) "message" else "warning",
        duration = 8
      )
      
      # Show error details if any
      if (length(errors) > 0) {
        showNotification(
          HTML(paste("Errors:<br>", paste(errors, collapse = "<br>"))),
          type = "error",
          duration = 10
        )
      }
      
      message(paste("Upload complete:", length(unique(uploaded_contrasts)), "contrasts loaded"))
    } else {
      showNotification(
        "No valid contrasts could be loaded from uploaded files",
        type = "error",
        duration = 10
      )
    }
    
  }, error = function(e) {
    message(paste("Error in upload handler:", e$message))
    showNotification(
      paste("Error uploading DEG results:", e$message),
      type = "error",
      duration = 10
    )
  })
})

# Clear uploaded results
observeEvent(input$clear_uploaded_results, {
  values$uploaded_deg_results <- NULL
  values$use_uploaded_results <- FALSE
  
  showNotification(
    "Uploaded results cleared. Using computed results if available.",
    type = "message",
    duration = 5
  )
  
  message("Cleared uploaded DEG results")
})

# Display upload status
output$upload_status <- renderUI({
  if (!is.null(values$use_uploaded_results) && values$use_uploaded_results && 
      !is.null(values$uploaded_deg_results)) {
    n_contrasts <- length(values$uploaded_deg_results)
    contrast_names <- names(values$uploaded_deg_results)
    
    div(
      style = "background-color: #d4edda; padding: 12px; border-radius: 4px; border: 1px solid #c3e6cb;",
      icon("check-circle", style = "color: #28a745;"),
      strong(paste(" Upload Complete:", n_contrasts, "contrast(s)"), style = "color: #155724; font-size: 13px;"),
      br(),
      span(paste("Loaded:", paste(contrast_names, collapse = ", ")), 
           style = "color: #155724; font-size: 11px; font-style: italic;"),
      br(), br(),
      div(
        style = "background-color: #fff; padding: 8px; border-radius: 3px; border-left: 3px solid #28a745;",
        icon("arrow-right", style = "color: #28a745;"),
        strong(" Next:", style = "color: #155724;"),
        span(" Configure settings below and click 'Run Selected Contrasts'", 
             style = "color: #155724; font-size: 11px;")
      )
    )
  } else if (!is.null(values$deseq_results) && length(values$deseq_results) > 0) {
    div(
      style = "background-color: #d1ecf1; padding: 10px; border-radius: 4px; border: 1px solid #bee5eb;",
      icon("database", style = "color: #0c5460;"),
      span(" Using computed DESeq2 results", style = "color: #0c5460; font-size: 11px;")
    )
  } else {
    div(
      style = "background-color: #f8d7da; padding: 10px; border-radius: 4px; border: 1px solid #f5c6cb;",
      icon("exclamation-triangle", style = "color: #721c24;"),
      span(" No results available", style = "color: #721c24; font-size: 11px;")
    )
  }
})

# =============================================================================
# CONTRAST SELECTOR SETUP
# =============================================================================

# Populate contrast selector from uploaded or computed results
# This observer monitors both uploaded and computed results
observe({
  
  message("\n--- Contrast Selector Observer Triggered ---")
  
  # Explicitly depend on reactive values to trigger updates
  uploaded_results <- values$uploaded_deg_results
  use_uploaded <- values$use_uploaded_results
  computed_results <- values$deseq_results
  
  message(paste("  use_uploaded:", ifelse(is.null(use_uploaded), "NULL", use_uploaded)))
  message(paste("  uploaded_results is NULL:", is.null(uploaded_results)))
  if (!is.null(uploaded_results)) {
    message(paste("  uploaded_results length:", length(uploaded_results)))
    message(paste("  uploaded_results names:", paste(names(uploaded_results), collapse = ", ")))
  }
  message(paste("  computed_results is NULL:", is.null(computed_results)))
  if (!is.null(computed_results)) {
    message(paste("  computed_results length:", length(computed_results)))
  }
  
  # Get available contrasts from either uploaded or computed results
  available_contrasts <- character(0)
  
  # Priority 1: Uploaded results (check if data exists, flag is for UI display only)
  if (!is.null(uploaded_results) && length(uploaded_results) > 0) {
    available_contrasts <- names(uploaded_results)
    message(paste("  -> Using uploaded results for contrast selector:", 
                 length(available_contrasts), "contrasts"))
  }
  
  # Priority 2: Computed DESeq results
  if (length(available_contrasts) == 0 && !is.null(computed_results) && length(computed_results) > 0) {
    available_contrasts <- names(computed_results)
    message(paste("  -> Using computed DESeq results for contrast selector:", 
                 length(available_contrasts), "contrasts"))
  }
  
  # Update selector if contrasts available
  if (length(available_contrasts) > 0) {
    updateSelectizeInput(
      session,
      "enrich_contrast",
      choices = available_contrasts,
      selected = NULL  # Don't auto-select, let user choose
    )
    
    message(paste("Enrichment contrast selector updated with", length(available_contrasts), "contrasts"))
  } else {
    # Clear selector if no results
    updateSelectizeInput(
      session,
      "enrich_contrast",
      choices = character(0),
      selected = NULL
    )
    message("No contrasts available - cleared selector")
  }
})

# =============================================================================
# DEFAULT PARAMETERS FROM CONTRAST SETTINGS
# =============================================================================

# Update advanced parameters when contrast changes
observeEvent(input$enrich_contrast, {
  req(input$enrich_contrast)
  req(values$deseq_results)
  
  # Use first selected contrast for parameter defaults (if multiple selected)
  first_contrast <- input$enrich_contrast[1]
  
  # Check if this contrast exists in deseq_results
  if (first_contrast %in% names(values$deseq_results)) {
    
    # Get analysis parameters from the contrast
    contrast_data <- values$deseq_results[[first_contrast]]
    
    # Check if analysis_params exist
    if (!is.null(contrast_data$analysis_params)) {
      params <- contrast_data$analysis_params
      
      # Update padj cutoff if available
      if (!is.null(params$alpha)) {
        updateNumericInput(
          session,
          "enrich_padj_cutoff",
          value = params$alpha
        )
      }
      
      # Update LFC threshold if available
      if (!is.null(params$lfc_threshold)) {
        updateNumericInput(
          session,
          "enrich_lfc_cutoff",
          value = params$lfc_threshold
        )
      }
      
      message(paste("Updated enrichment parameters from contrast:", first_contrast))
    }
  }
})

# =============================================================================
# CLEAR CACHE HANDLER
# =============================================================================

# Clear enrichment cache
observeEvent(input$clear_enrichment_cache, {
  # Reset enrichment results
  values$enrichment_results <- list()
  
  showNotification(
    "Enrichment cache cleared successfully",
    type = "message",
    duration = 3
  )
  
  message("Enrichment cache cleared by user")
})

# =============================================================================
# GENE LIST PREPARATION
# =============================================================================

# Function to build gene lists for enrichment analysis
build_gene_lists <- function(contrast_name, direction, alpha, lfc_threshold) {
  
  tryCatch({
    
    message(paste("\n=== Building Gene Lists for:", contrast_name, "==="))
    
    # Get results from uploaded or computed source
    deg_data <- NULL
    source_type <- NULL
    
    # Priority 1: Check uploaded results (check data existence directly)
    if (!is.null(values$uploaded_deg_results) && 
        !is.null(values$uploaded_deg_results[[contrast_name]])) {
      
      deg_data <- values$uploaded_deg_results[[contrast_name]]$results
      source_type <- "uploaded"
      message("Using uploaded DEG results")
      
    } 
    # Priority 2: Check computed results
    else if (!is.null(values$deseq_results) && 
             !is.null(values$deseq_results[[contrast_name]])) {
      
      deg_data <- values$deseq_results[[contrast_name]]$results
      source_type <- "computed"
      message("Using computed DESeq2 results")
      
    } else {
      message(paste("Error: No results found for contrast:", contrast_name))
      message(paste("  Checked uploaded_deg_results:", !is.null(values$uploaded_deg_results)))
      message(paste("  Checked deseq_results:", !is.null(values$deseq_results)))
      return(NULL)
    }
    
    # Validate data
    if (is.null(deg_data) || nrow(deg_data) == 0) {
      message("Error: No DEG data found for contrast")
      return(NULL)
    }
    
    message(paste("Building gene lists for contrast:", contrast_name))
    message(paste("Total genes in DEG results:", nrow(deg_data)))
    
    # Build universe: all genes with non-NA padj
    universe <- deg_data[!is.na(deg_data$padj), ]
    message(paste("Universe size (non-NA padj):", nrow(universe)))
    
    # Filter significant genes: padj < alpha
    sig_genes <- universe[universe$padj < alpha, ]
    message(paste("Significant genes (padj <", alpha, "):", nrow(sig_genes)))
    
    # Apply LFC magnitude filter if threshold > 0
    if (lfc_threshold > 0) {
      sig_genes <- sig_genes[abs(sig_genes$log2FoldChange) > lfc_threshold, ]
      message(paste("After |LFC| >", lfc_threshold, "filter:", nrow(sig_genes)))
    }
    
    # Split by direction based on user selection
    if (direction == "Up") {
      # Up-regulated genes
      if (lfc_threshold > 0) {
        genes <- sig_genes[sig_genes$log2FoldChange > lfc_threshold, ]
      } else {
        genes <- sig_genes[sig_genes$log2FoldChange > 0, ]
      }
      message(paste("Up-regulated genes:", nrow(genes)))
      
    } else if (direction == "Down") {
      # Down-regulated genes
      if (lfc_threshold > 0) {
        genes <- sig_genes[sig_genes$log2FoldChange < -lfc_threshold, ]
      } else {
        genes <- sig_genes[sig_genes$log2FoldChange < 0, ]
      }
      message(paste("Down-regulated genes:", nrow(genes)))
      
    } else if (direction == "All") {
      # All significant genes (no direction split)
      genes <- sig_genes
      message(paste("All significant genes:", nrow(genes)))
      
    } else {
      message("Error: Invalid direction parameter")
      return(NULL)
    }
    
    # Check if we have genes
    if (nrow(genes) == 0) {
      message("Warning: No genes found after filtering")
      return(list(
        genes = genes,
        universe = universe,
        n_genes = 0,
        n_universe = nrow(universe),
        fold_changes = numeric(0)
      ))
    }
    
    # Extract gene identifiers (from row names or 'gene' column)
    if ("gene" %in% colnames(genes)) {
      gene_ids <- genes$gene
      universe_ids <- universe$gene
    } else {
      gene_ids <- rownames(genes)
      universe_ids <- rownames(universe)
    }
    
    # Store fold changes for later use (for mean_log2FC calculation)
    if ("gene" %in% colnames(genes)) {
      fold_changes <- setNames(genes$log2FoldChange, genes$gene)
    } else {
      fold_changes <- setNames(genes$log2FoldChange, rownames(genes))
    }
    
    message(paste("Gene identifiers extracted:", length(gene_ids)))
    
    # Return list with all necessary components
    return(list(
      genes = genes,                    # Full dataframe of selected genes
      universe = universe,              # Full dataframe of all testable genes
      gene_ids = gene_ids,             # Vector of gene IDs for enrichment
      universe_ids = universe_ids,     # Vector of universe gene IDs
      fold_changes = fold_changes,     # Named vector of log2FC
      n_genes = length(gene_ids),
      n_universe = length(universe_ids),
      direction = direction,
      alpha = alpha,
      lfc_threshold = lfc_threshold
    ))
    
  }, error = function(e) {
    message(paste("Error in build_gene_lists:", e$message))
    return(NULL)
  })
}

# =============================================================================
# ID MAPPING
# =============================================================================

# Function to map gene IDs to ENTREZ IDs with statistics
perform_id_mapping <- function(gene_lists, id_type, orgdb) {
  
  # Validate inputs
  if (is.null(gene_lists) || is.null(orgdb)) {
    message("Error: Invalid inputs for ID mapping")
    return(NULL)
  }
  
  if (gene_lists$n_genes == 0) {
    message("Warning: No genes to map (n_genes = 0)")
    return(list(
      mapped_genes = NULL,
      mapped_universe = NULL,
      gene_stats = list(n_original = 0, n_mapped = 0, mapping_rate = 0),
      universe_stats = list(n_original = 0, n_mapped = 0, mapping_rate = 0),
      warnings = "No genes in input list"
    ))
  }
  
  tryCatch({
    message("=== Starting ID Mapping ===")
    
    # Determine ID type (handle auto-detect)
    from_type <- id_type
    if (id_type == "auto") {
      # Simple heuristic: if IDs start with "ENS", assume ENSEMBL, else SYMBOL
      sample_ids <- head(gene_lists$gene_ids, 10)
      if (any(grepl("^ENS", sample_ids, ignore.case = TRUE))) {
        from_type <- "ENSEMBL"
        message("Auto-detected ID type: ENSEMBL")
      } else {
        from_type <- "SYMBOL"
        message("Auto-detected ID type: SYMBOL")
      }
    }
    
    message(paste("Mapping from", from_type, "to ENTREZID"))
    message(paste("Using organism database:", class(orgdb)[1]))
    
    # Map gene IDs using utility function from Phase 1
    message(paste("Mapping", gene_lists$n_genes, "gene IDs..."))
    mapped_genes <- map_gene_ids(
      genes = gene_lists$gene_ids,
      from_type = from_type,
      to_type = "ENTREZID",
      orgdb = orgdb
    )
    
    # Map universe IDs
    message(paste("Mapping", gene_lists$n_universe, "universe IDs..."))
    mapped_universe <- map_gene_ids(
      genes = gene_lists$universe_ids,
      from_type = from_type,
      to_type = "ENTREZID",
      orgdb = orgdb
    )
    
    # Check for mapping failures
    if (is.null(mapped_genes) || is.null(mapped_universe)) {
      message("Error: ID mapping failed")
      return(list(
        mapped_genes = NULL,
        mapped_universe = NULL,
        gene_stats = list(n_original = gene_lists$n_genes, n_mapped = 0, mapping_rate = 0),
        universe_stats = list(n_original = gene_lists$n_universe, n_mapped = 0, mapping_rate = 0),
        warnings = "ID mapping failed - check organism and ID type settings"
      ))
    }
    
    # Compute mapping statistics using utility function from Phase 1
    gene_stats <- compute_mapping_stats(gene_lists$gene_ids, mapped_genes)
    universe_stats <- compute_mapping_stats(gene_lists$universe_ids, mapped_universe)
    
    message(paste("Gene mapping:", gene_stats$n_mapped, "/", gene_stats$n_original,
                  "(", gene_stats$mapping_rate_pct, "%)"))
    message(paste("Universe mapping:", universe_stats$n_mapped, "/", universe_stats$n_original,
                  "(", universe_stats$mapping_rate_pct, "%)"))
    
    # Check for warnings
    warnings <- character(0)
    
    # Warning 1: Too few genes mapped (< 10)
    if (gene_stats$n_mapped < 10) {
      warning_msg <- paste0("Too few genes mapped (", gene_stats$n_mapped, " < 10). ",
                           "Enrichment analysis requires at least 10 genes.")
      warnings <- c(warnings, warning_msg)
      message(paste("WARNING:", warning_msg))
    }
    
    # Warning 2: Low mapping rate (< 60%)
    if (gene_stats$mapping_rate < 0.60) {
      warning_msg <- paste0("Low gene mapping rate (", gene_stats$mapping_rate_pct, "% < 60%). ",
                           "Check organism and ID type settings.")
      warnings <- c(warnings, warning_msg)
      message(paste("WARNING:", warning_msg))
    }
    
    if (universe_stats$mapping_rate < 0.60) {
      warning_msg <- paste0("Low universe mapping rate (", universe_stats$mapping_rate_pct, "% < 60%). ",
                           "This may affect background correction.")
      warnings <- c(warnings, warning_msg)
      message(paste("WARNING:", warning_msg))
    }
    
    message("=== ID Mapping Complete ===")
    
    # Return mapped data with statistics
    return(list(
      mapped_genes = mapped_genes,
      mapped_universe = mapped_universe,
      gene_stats = gene_stats,
      universe_stats = universe_stats,
      from_type = from_type,
      warnings = if(length(warnings) > 0) warnings else NULL,
      success = gene_stats$n_mapped >= 10  # Flag for proceeding with enrichment
    ))
    
  }, error = function(e) {
    message(paste("Error in perform_id_mapping:", e$message))
    return(list(
      mapped_genes = NULL,
      mapped_universe = NULL,
      gene_stats = list(n_original = gene_lists$n_genes, n_mapped = 0, mapping_rate = 0),
      universe_stats = list(n_original = gene_lists$n_universe, n_mapped = 0, mapping_rate = 0),
      warnings = paste("Mapping error:", e$message),
      success = FALSE
    ))
  })
}

# =============================================================================
# ENRICHMENT EXECUTION
# =============================================================================

# =============================================================================
# SINGLE CONTRAST ENRICHMENT FUNCTION
# =============================================================================

# Function to run enrichment for a single contrast
# This is called by both single-contrast and batch modes
# Returns: list(success, contrast, results, stats, warnings, error)
run_enrichment_for_single_contrast <- function(
  contrast_name, 
  params, 
  progress_callback = NULL
) {
  
  message(paste("\n--- Processing contrast:", contrast_name, "---"))
  
  tryCatch({
    
    # === STEP 1: Build gene lists ===
    if (!is.null(progress_callback)) {
      progress_callback(detail = "Building gene lists")
    }
    
    gene_lists <- build_gene_lists(
      contrast_name = contrast_name,
      direction = params$direction,
      alpha = params$alpha,
      lfc_threshold = params$lfc_threshold
    )
    
    # Validate gene lists
    if (is.null(gene_lists)) {
      return(list(
        success = FALSE,
        contrast = contrast_name,
        error = "Failed to build gene lists",
        n_genes = 0
      ))
    }
    
    if (gene_lists$n_genes == 0) {
      return(list(
        success = FALSE,
        contrast = contrast_name,
        error = paste0("No genes found (padj < ", params$alpha, 
                      ", |LFC| > ", params$lfc_threshold, ")"),
        n_genes = 0
      ))
    }
    
    message(paste("  Gene lists built:", gene_lists$n_genes, "genes"))
    
    # === STEP 2: Map IDs ===
    if (!is.null(progress_callback)) {
      progress_callback(detail = paste("Mapping", gene_lists$n_genes, "gene IDs"))
    }
    
    mapping_result <- perform_id_mapping(
      gene_lists = gene_lists,
      id_type = params$id_type,
      orgdb = params$orgdb
    )
    
    # Validate mapping
    if (is.null(mapping_result) || !mapping_result$success) {
      warning_msg <- if (!is.null(mapping_result$warnings)) {
        paste(mapping_result$warnings, collapse = "; ")
      } else {
        "ID mapping failed"
      }
      
      return(list(
        success = FALSE,
        contrast = contrast_name,
        error = warning_msg,
        n_genes = gene_lists$n_genes,
        mapping_stats = if (!is.null(mapping_result)) mapping_result$gene_stats else NULL
      ))
    }
    
    message(paste("  ID mapping complete:", mapping_result$gene_stats$n_mapped, "genes mapped"))
    
    # === STEP 3: Run enrichGO for each ontology ===
    if (!is.null(progress_callback)) {
      progress_callback(detail = "Running GO enrichment")
    }
    
    enrichment_results <- list()
    n_total_terms <- 0
    
    for (ont in params$ontologies) {
      message(paste("    Running enrichGO for", ont))
      
      ego <- tryCatch({
        clusterProfiler::enrichGO(
          gene = mapping_result$mapped_genes$ENTREZID,
          universe = mapping_result$mapped_universe$ENTREZID,
          OrgDb = params$orgdb,
          ont = ont,
          pAdjustMethod = "BH",
          pvalueCutoff = params$pvalue_cutoff,
          qvalueCutoff = params$qvalue_cutoff,
          minGSSize = params$min_gs_size,
          maxGSSize = params$max_gs_size,
          readable = TRUE
        )
      }, error = function(e) {
        message(paste("      Error in enrichGO:", e$message))
        return(NULL)
      })
      
      # Process results
      if (!is.null(ego) && nrow(ego) > 0) {
        # Apply simplify
        if (params$simplify_cutoff > 0 && params$simplify_cutoff < 1) {
          ego <- safe_simplify(ego, cutoff = params$simplify_cutoff)
        }
        
        # Augment with additional metrics
        ego <- augment_enrichment_results(ego, gene_lists$fold_changes)
        
        enrichment_results[[ont]] <- list(
          ego = ego,
          metadata = list(
            ontology = ont,
            n_genes = gene_lists$n_genes,
            n_mapped = mapping_result$gene_stats$n_mapped,
            mapping_rate = mapping_result$gene_stats$mapping_rate_pct,
            n_universe = gene_lists$n_universe,
            n_universe_mapped = mapping_result$universe_stats$n_mapped,
            n_enriched_terms = nrow(ego),
            timestamp = Sys.time()
          )
        )
        
        n_total_terms <- n_total_terms + nrow(ego)
        message(paste("      Found", nrow(ego), "enriched terms"))
      } else {
        message(paste("      No enriched terms found for", ont))
      }
    }
    
    # === STEP 4: Return success ===
    return(list(
      success = TRUE,
      contrast = contrast_name,
      results = enrichment_results,
      gene_lists = list(
        n_genes = gene_lists$n_genes,
        n_universe = gene_lists$n_universe,
        direction = gene_lists$direction
      ),
      mapping = list(
        gene_stats = mapping_result$gene_stats,
        universe_stats = mapping_result$universe_stats,
        from_type = mapping_result$from_type
      ),
      warnings = mapping_result$warnings,
      n_genes = gene_lists$n_genes,
      n_terms = n_total_terms,
      timestamp = Sys.time()
    ))
    
  }, error = function(e) {
    message(paste("  ERROR in run_enrichment_for_single_contrast:", e$message))
    return(list(
      success = FALSE,
      contrast = contrast_name,
      error = e$message
    ))
  })
}

# =============================================================================
# ENRICHMENT EXECUTION - MULTI-CONTRAST SUPPORT
# =============================================================================

# Main enrichment analysis handler (supports single or multiple contrasts)
observeEvent(input$run_enrichment, {
  
  # Validate required inputs
  req(input$enrich_contrast)
  req(input$enrich_ontology)
  req(input$enrich_direction)
  req(values$organism)
  req(values$id_type)
  req(values$orgdb)
  
  # Get selected contrasts (can be one or multiple)
  selected_contrasts <- input$enrich_contrast
  n_contrasts <- length(selected_contrasts)
  
  message(paste("\n=== ENRICHMENT ANALYSIS FOR", n_contrasts, "CONTRAST(S) ==="))
  message(paste("Contrasts:", paste(selected_contrasts, collapse = ", ")))
  
  # Collect parameters (same for all contrasts in batch)
  params <- list(
    organism = values$organism,
    id_type = values$id_type,
    orgdb = values$orgdb,  # Include orgdb in params for function scope
    ontologies = input$enrich_ontology,
    direction = input$enrich_direction,
    alpha = input$enrich_padj_cutoff,
    lfc_threshold = input$enrich_lfc_cutoff,
    pvalue_cutoff = input$enrich_pvalue_cutoff,
    qvalue_cutoff = input$enrich_qvalue_cutoff,
    min_gs_size = input$enrich_min_gs_size,
    max_gs_size = input$enrich_max_gs_size,
    simplify_cutoff = input$enrich_simplify_cutoff
  )
  
  # Disable buttons during execution
  shinyjs::disable("run_enrichment")
  shinyjs::disable("run_all_contrasts")
  
  # Track batch results
  batch_results <- list()
  n_success <- 0
  n_cached <- 0
  n_failed <- 0
  failed_contrasts <- character(0)
  
  tryCatch({
    
    # === BATCH PROGRESS WRAPPER ===
    withProgress(message = paste("Processing", n_contrasts, "contrast(s)..."), value = 0, {
      
      for (i in seq_along(selected_contrasts)) {
        contrast_name <- selected_contrasts[i]
        
        # Update overall progress
        incProgress(
          amount = 0,  # We'll increment manually after processing
          message = paste("Contrast", i, "of", n_contrasts),
          detail = contrast_name
        )
        
        message(paste("\n>>> Processing contrast", i, "of", n_contrasts, ":", contrast_name))
        
        # === CHECK CACHE FIRST ===
        cache_key <- generate_cache_key(contrast_name, params)
        
        if (!is.null(values$enrichment_results[[contrast_name]][[cache_key]])) {
          message("  Using cached results for:", contrast_name)
          batch_results[[contrast_name]] <- list(
            success = TRUE,
            cached = TRUE,
            contrast = contrast_name
          )
          n_cached <- n_cached + 1
          
          # Increment progress for cached result
          incProgress(1 / n_contrasts)
          next
        }
        
        # === RUN ENRICHMENT FOR THIS CONTRAST ===
        result <- run_enrichment_for_single_contrast(
          contrast_name = contrast_name,
          params = params,
          progress_callback = function(detail = NULL) {
            # Update sub-progress detail without changing overall progress bar
            if (!is.null(detail)) {
              incProgress(0, detail = detail)
            }
          }
        )
        
        batch_results[[contrast_name]] <- result
        
        # === PROCESS RESULT ===
        if (result$success) {
          # Cache the results
          if (is.null(values$enrichment_results[[contrast_name]])) {
            values$enrichment_results[[contrast_name]] <- list()
          }
          
          values$enrichment_results[[contrast_name]][[cache_key]] <- list(
            results = result$results,
            params = params,
            gene_lists = result$gene_lists,
            mapping = result$mapping,
            timestamp = result$timestamp
          )
          
          n_success <- n_success + 1
          message(paste("  SUCCESS:", contrast_name, "-", result$n_terms, "terms found"))
          
        } else {
          # Failed
          n_failed <- n_failed + 1
          failed_contrasts <- c(failed_contrasts, contrast_name)
          message(paste("  FAILED:", contrast_name, "-", result$error))
          
          # Show individual failure notification (but don't interrupt batch)
          showNotification(
            paste0(contrast_name, ": ", result$error),
            type = "warning",
            duration = 5
          )
        }
        
        # Increment progress after processing this contrast
        incProgress(1 / n_contrasts)
      }
      
    })  # End of withProgress
    
    # === FINAL BATCH SUMMARY ===
    message("\n=== BATCH ENRICHMENT COMPLETE ===")
    message(paste("Total:", n_contrasts))
    message(paste("Success:", n_success))
    message(paste("Cached:", n_cached))
    message(paste("Failed:", n_failed))
    if (n_failed > 0) {
      message(paste("Failed contrasts:", paste(failed_contrasts, collapse = ", ")))
    }
    
    # Show summary notification
    if (n_contrasts == 1) {
      # Single contrast - simple message
      if (n_success + n_cached > 0) {
        result_info <- batch_results[[selected_contrasts[1]]]
        n_terms <- if (!is.null(result_info$n_terms)) result_info$n_terms else 0
        showNotification(
          paste0("Enrichment complete. Found ", n_terms, " enriched term(s)."),
          type = "message",
          duration = 5
        )
      }
    } else {
      # Multiple contrasts - batch summary
      summary_msg <- paste0(
        "Batch enrichment complete:\n",
        n_success, " processed, ",
        n_cached, " cached",
        if (n_failed > 0) paste0(", ", n_failed, " failed") else ""
      )
      
      showNotification(
        summary_msg,
        type = if (n_failed == 0) "message" else "warning",
        duration = 8
      )
    }
    
  }, error = function(e) {
    message(paste("ERROR in batch enrichment:", e$message))
    showNotification(
      paste("Error during batch enrichment:", e$message),
      type = "error",
      duration = 10
    )
    
  }, finally = {
    # Re-enable buttons
    shinyjs::enable("run_enrichment")
    shinyjs::enable("run_all_contrasts")
  })
})

# =============================================================================
# RUN ALL CONTRASTS HANDLER
# =============================================================================

# Run enrichment on all available contrasts
observeEvent(input$run_all_contrasts, {
  
  # Get all available contrasts from BOTH uploaded and computed sources
  uploaded_contrasts <- if (!is.null(values$uploaded_deg_results)) {
    names(values$uploaded_deg_results)
  } else {
    character(0)
  }
  
  computed_contrasts <- if (!is.null(values$deseq_results)) {
    names(values$deseq_results)
  } else {
    character(0)
  }
  
  # Combine and deduplicate (uploaded takes precedence)
  all_contrasts <- unique(c(uploaded_contrasts, computed_contrasts))
  
  if (length(all_contrasts) == 0) {
    showNotification(
      "No contrasts available. Please run DEG analysis or upload DEG results first.",
      type = "warning",
      duration = 5
    )
    return()
  }
  
  message(paste("Run All Contrasts: Found", length(all_contrasts), "contrasts"))
  message(paste("  Uploaded:", length(uploaded_contrasts)))
  message(paste("  Computed:", length(computed_contrasts)))
  
  # Confirm with user if many contrasts
  if (length(all_contrasts) > 5) {
    showModal(modalDialog(
      title = "Confirm Batch Analysis",
      size = "m",
      
      p(paste0("You are about to run enrichment analysis on ", 
               strong(length(all_contrasts)), " contrasts."),
        style = "font-size: 14px;"),
      
      p("This may take several minutes depending on the number of significant genes.",
        style = "font-size: 13px; color: #666;"),
      
      br(),
      
      div(
        style = "background-color: #f8f9fa; padding: 10px; border-radius: 4px; max-height: 200px; overflow-y: auto;",
        strong("Contrasts to analyze:"),
        tags$ul(
          style = "margin-top: 5px; margin-bottom: 0;",
          lapply(all_contrasts, function(c) tags$li(c))
        )
      ),
      
      br(),
      
      p(strong("Current settings:"),
        style = "font-size: 13px; margin-bottom: 5px;"),
      
      tags$ul(
        style = "font-size: 12px; color: #555; margin-top: 0;",
        tags$li(paste("Ontologies:", paste(input$enrich_ontology, collapse = ", "))),
        tags$li(paste("Direction:", input$enrich_direction)),
        tags$li(paste("padj <", input$enrich_padj_cutoff)),
        tags$li(paste("|log2FC| >", input$enrich_lfc_cutoff))
      ),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          "confirm_run_all", 
          "Proceed with Batch Analysis", 
          class = "btn-primary",
          icon = icon("check")
        )
      )
    ))
    
  } else {
    # Few contrasts - auto-proceed without confirmation
    message("Auto-proceeding with", length(all_contrasts), "contrasts (no confirmation needed)")
    updateSelectizeInput(session, "enrich_contrast", selected = all_contrasts)
    
    # Small delay to let UI update, then trigger run
    shinyjs::delay(100, shinyjs::click("run_enrichment"))
  }
})

# Handle confirmation from modal
observeEvent(input$confirm_run_all, {
  
  # Re-fetch contrasts (same logic as above)
  uploaded_contrasts <- if (!is.null(values$uploaded_deg_results)) {
    names(values$uploaded_deg_results)
  } else {
    character(0)
  }
  
  computed_contrasts <- if (!is.null(values$deseq_results)) {
    names(values$deseq_results)
  } else {
    character(0)
  }
  
  all_contrasts <- unique(c(uploaded_contrasts, computed_contrasts))
  
  message("User confirmed batch analysis for", length(all_contrasts), "contrasts")
  
  # Close modal
  removeModal()
  
  # Select all contrasts
  updateSelectizeInput(session, "enrich_contrast", selected = all_contrasts)
  
  # Trigger run enrichment
  # Small delay to ensure UI updates
  shinyjs::delay(100, shinyjs::click("run_enrichment"))
})

# =============================================================================
# RESULTS RENDERING
# =============================================================================

# Main results panel renderer (multi-contrast aware)
output$enrichment_results_panel <- renderUI({
  
  # Get all contrasts with cached results
  contrasts_with_results <- names(values$enrichment_results)
  contrasts_with_results <- contrasts_with_results[
    sapply(contrasts_with_results, function(c) {
      length(values$enrichment_results[[c]]) > 0
    })
  ]
  
  if (length(contrasts_with_results) == 0) {
    # No results yet - show ready state
    return(div(
      style = "text-align: center; margin-top: 50px; margin-bottom: 50px;",
      icon("flask", class = "fa-2x", style = "color: #9B59B6;"),
      h4("Ready to Run Enrichment Analysis", style = "color: #2C3E50; margin-top: 20px;"),
      p("Select contrast(s) and configure settings above, then click 'Run Selected Contrasts' or 'Run All Contrasts'.",
        style = "color: #7F8C8D; font-size: 14px;")
    ))
  }
  
  # Determine display mode
  if (length(contrasts_with_results) == 1) {
    # Single contrast - direct display
    return(uiOutput("enrichment_single_contrast_display"))
    
  } else {
    # Multiple contrasts - show selector + display
    return(tagList(
      
      # Contrast selector panel
      wellPanel(
        style = "background-color: #e8f4f8; margin-bottom: 20px; border-left: 4px solid #3498DB;",
        fluidRow(
          column(4,
            h5("View Results for Contrast:", style = "margin-top: 8px; color: #2C3E50;")
          ),
          column(8,
            selectInput(
              "view_enrichment_contrast",
              NULL,
              choices = contrasts_with_results,
              selected = if (!is.null(input$view_enrichment_contrast) && 
                            input$view_enrichment_contrast %in% contrasts_with_results) {
                input$view_enrichment_contrast
              } else {
                contrasts_with_results[1]
              },
              width = "100%"
            )
          )
        ),
        p(
          icon("info-circle"),
          paste(" ", length(contrasts_with_results), "contrast(s) analyzed. Select to view detailed results."),
          style = "color: #555; font-size: 12px; margin-bottom: 0; margin-top: 10px;"
        )
      ),
      
      # Single contrast display (will render based on selector)
      uiOutput("enrichment_single_contrast_display")
    ))
  }
})

# Single contrast display (used by both single and multi-contrast modes)
output$enrichment_single_contrast_display <- renderUI({
  
  # Determine which contrast to display
  contrasts_with_results <- names(values$enrichment_results)
  contrasts_with_results <- contrasts_with_results[
    sapply(contrasts_with_results, function(c) {
      length(values$enrichment_results[[c]]) > 0
    })
  ]
  
  if (length(contrasts_with_results) == 0) {
    return(NULL)  # Parent will show empty state
  }
  
  # Select contrast to display
  display_contrast <- if (length(contrasts_with_results) > 1) {
    # Multi-contrast mode: use selector
    req(input$view_enrichment_contrast)
    input$view_enrichment_contrast
  } else {
    # Single contrast mode: use the only one available
    contrasts_with_results[1]
  }
  
  message(paste("Displaying enrichment results for:", display_contrast))
  
  # Get current parameters to build cache key
  params <- list(
    contrast = display_contrast,
    organism = values$organism,
    id_type = values$id_type,
    ontologies = input$enrich_ontology,
    direction = input$enrich_direction,
    alpha = input$enrich_padj_cutoff,
    lfc_threshold = input$enrich_lfc_cutoff,
    pvalue_cutoff = input$enrich_pvalue_cutoff,
    qvalue_cutoff = input$enrich_qvalue_cutoff,
    min_gs_size = input$enrich_min_gs_size,
    max_gs_size = input$enrich_max_gs_size,
    simplify_cutoff = input$enrich_simplify_cutoff
  )
  
  cache_key <- generate_cache_key(display_contrast, params)
  
  # Check if results exist for this contrast + parameters
  if (is.null(values$enrichment_results[[display_contrast]][[cache_key]])) {
    return(div(
      style = "text-align: center; margin-top: 50px;",
      icon("info-circle", class = "fa-2x", style = "color: #3498DB;"),
      h4(paste("No Results for", display_contrast, "with Current Parameters"), 
         style = "color: #2C3E50; margin-top: 20px;"),
      p("Run enrichment analysis with current parameters to view results.",
        style = "color: #7F8C8D; font-size: 14px;")
    ))
  }
  
  # Get cached results
  cached_data <- values$enrichment_results[[display_contrast]][[cache_key]]
  results <- cached_data$results
  
  if (length(results) == 0) {
    return(div(
      style = "text-align: center; margin-top: 50px;",
      icon("info-circle", class = "fa-2x", style = "color: #FFA500;"),
      h4("No Enriched Terms Found", style = "color: #2C3E50; margin-top: 20px;"),
      p(paste("No significantly enriched GO terms were found for", display_contrast, "with current parameters."),
        style = "color: #7F8C8D; font-size: 14px;"),
      p("Try adjusting the parameters (lower p-value cutoff, change gene set, etc.)",
        style = "color: #7F8C8D; font-size: 12px; font-style: italic;")
    ))
  }
  
  # Build UI for results
  tagList(
    # Summary Panel
    fluidRow(
      column(12,
        wellPanel(
          style = "background-color: #e8f5e9; border-left: 4px solid #4CAF50;",
          h4("Analysis Summary", style = "color: #2E7D32; margin-top: 0;"),
          fluidRow(
            column(3,
              div(
                p(strong("Gene Set:"), style = "margin-bottom: 5px; color: #666;"),
                p(paste(cached_data$gene_lists$direction, "regulated"), 
                  style = "font-size: 18px; color: #2E7D32; margin: 0;"),
                p(paste(cached_data$gene_lists$n_genes, "genes"), 
                  style = "font-size: 14px; color: #666; margin-top: 5px;")
              )
            ),
            column(3,
              div(
                p(strong("ID Mapping:"), style = "margin-bottom: 5px; color: #666;"),
                p(paste0(cached_data$mapping$gene_stats$mapping_rate_pct, "%"), 
                  style = "font-size: 18px; color: #2E7D32; margin: 0;"),
                p(paste(cached_data$mapping$gene_stats$n_mapped, "mapped"), 
                  style = "font-size: 14px; color: #666; margin-top: 5px;")
              )
            ),
            column(3,
              div(
                p(strong("Ontologies:"), style = "margin-bottom: 5px; color: #666;"),
                p(length(results), 
                  style = "font-size: 18px; color: #2E7D32; margin: 0;"),
                p(paste(names(results), collapse = ", "), 
                  style = "font-size: 14px; color: #666; margin-top: 5px;")
              )
            ),
            column(3,
              div(
                p(strong("Total Terms:"), style = "margin-bottom: 5px; color: #666;"),
                p(sum(sapply(results, function(x) nrow(x$ego))), 
                  style = "font-size: 18px; color: #2E7D32; margin: 0;"),
                p("enriched", 
                  style = "font-size: 14px; color: #666; margin-top: 5px;")
              )
            )
          )
        )
      )
    ),
    
    # Results by Ontology
    fluidRow(
      column(12,
        h4("Enriched GO Terms", style = "color: #2C3E50; margin-top: 20px; margin-bottom: 15px;"),
        do.call(tabsetPanel, c(
          id = "enrichment_ontology_tabs",
          lapply(names(results), function(ont) {
            create_ontology_tab(ont, results[[ont]], params$contrast, cache_key)
          })
        ))
      )
    )
  )
})

# Helper function to create ontology tab
create_ontology_tab <- function(ont, result_data, contrast_name, cache_key) {
  
  ont_names <- c("BP" = "Biological Process", "MF" = "Molecular Function", "CC" = "Cellular Component")
  ont_full_name <- ont_names[ont]
  
  ego <- result_data$ego
  metadata <- result_data$metadata
  
  tabPanel(
    title = ont_full_name,
    value = paste0("ont_", ont),
    
    br(),
    
    # Ontology-specific summary
    fluidRow(
      column(12,
        div(
          style = "background-color: #f5f5f5; padding: 10px; border-radius: 4px; margin-bottom: 15px;",
          fluidRow(
            column(4,
              p(strong("Enriched Terms:"), paste(metadata$n_enriched_terms), style = "margin: 0;")
            ),
            column(4,
              p(strong("Analysis Time:"), format(metadata$timestamp, "%Y-%m-%d %H:%M"), style = "margin: 0;")
            ),
            column(4,
              div(
                style = "text-align: right;",
                downloadButton(
                  paste0("download_csv_", ont, "_", gsub("[^A-Za-z0-9]", "_", contrast_name)),
                  "Export Table (CSV)",
                  class = "btn-sm btn-outline-primary",
                  style = "margin-right: 5px;"
                ),
                downloadButton(
                  paste0("download_xlsx_", ont, "_", gsub("[^A-Za-z0-9]", "_", contrast_name)),
                  "Export Table (Excel)",
                  class = "btn-sm btn-outline-success",
                  style = "margin-right: 5px;"
                ),
                downloadButton(
                  paste0("download_genes_", ont, "_", gsub("[^A-Za-z0-9]", "_", contrast_name)),
                  "Download Gene Lists",
                  icon = icon("list"),
                  class = "btn-sm btn-outline-info"
                )
              )
            )
          )
        )
      )
    ),
    
    # Data table
    fluidRow(
      column(12,
        DT::dataTableOutput(paste0("enrich_table_", ont, "_", gsub("[^A-Za-z0-9]", "_", contrast_name)))
      )
    ),
    
    br(),
    
    # Plots section
    fluidRow(
      column(6,
        h5("Top Terms by Significance", style = "color: #2C3E50;"),
        plotOutput(paste0("enrich_barplot_", ont, "_", gsub("[^A-Za-z0-9]", "_", contrast_name)), height = "400px")
      ),
      column(6,
        h5("Enrichment Overview", style = "color: #2C3E50;"),
        plotOutput(paste0("enrich_dotplot_", ont, "_", gsub("[^A-Za-z0-9]", "_", contrast_name)), height = "400px")
      )
    )
  )
}

# =============================================================================
# DYNAMIC RENDERING: TABLES, PLOTS, AND EXPORTS
# =============================================================================

# Observe and create outputs dynamically when results change
observe({
  
  # Determine which contrast to display
  # This mirrors the logic in output$enrichment_single_contrast_display
  contrasts_with_results <- names(values$enrichment_results)
  contrasts_with_results <- contrasts_with_results[
    sapply(contrasts_with_results, function(c) {
      length(values$enrichment_results[[c]]) > 0
    })
  ]
  
  if (length(contrasts_with_results) == 0) {
    return()
  }
  
  # Select contrast to display
  display_contrast <- if (length(contrasts_with_results) > 1) {
    # Multi-contrast mode: use selector
    req(input$view_enrichment_contrast)
    input$view_enrichment_contrast
  } else {
    # Single contrast mode: use the only one available
    contrasts_with_results[1]
  }
  
  # Build cache key
  params <- list(
    contrast = display_contrast,
    organism = values$organism,
    id_type = values$id_type,
    ontologies = input$enrich_ontology,
    direction = input$enrich_direction,
    alpha = input$enrich_padj_cutoff,
    lfc_threshold = input$enrich_lfc_cutoff,
    pvalue_cutoff = input$enrich_pvalue_cutoff,
    qvalue_cutoff = input$enrich_qvalue_cutoff,
    min_gs_size = input$enrich_min_gs_size,
    max_gs_size = input$enrich_max_gs_size,
    simplify_cutoff = input$enrich_simplify_cutoff
  )
  
  cache_key <- generate_cache_key(params$contrast, params)
  
  # Check if results exist
  if (is.null(values$enrichment_results[[params$contrast]][[cache_key]])) {
    return()
  }
  
  cached_data <- values$enrichment_results[[params$contrast]][[cache_key]]
  results <- cached_data$results
  
  if (length(results) == 0) {
    return()
  }
  
  # Create outputs for each ontology
  lapply(names(results), function(ont) {
    
    ego <- results[[ont]]$ego
    safe_contrast_name <- gsub("[^A-Za-z0-9]", "_", params$contrast)
    
    # Data Table
    table_id <- paste0("enrich_table_", ont, "_", safe_contrast_name)
    output[[table_id]] <- DT::renderDataTable({
      
      df <- as.data.frame(ego)
      
      # Add row index for download buttons
      df$row_id <- seq_len(nrow(df))
      
      # Add download button column
      df$download_genes <- sprintf(
        '<button class="btn btn-xs btn-info download-genes-btn" data-row="%d" data-ont="%s" data-contrast="%s">
          <i class="fa fa-download"></i> Genes
        </button>',
        df$row_id, ont, safe_contrast_name
      )
      
      # Select and order columns (put download button first)
      cols_to_show <- c("download_genes", "Description", "GeneRatio", "BgRatio", "pvalue", "p.adjust", 
                       "qvalue", "Count", "mean_log2FC", "enrichment_score")
      cols_to_show <- cols_to_show[cols_to_show %in% colnames(df)]
      df_display <- df[, cols_to_show]
      
      DT::datatable(
        df_display,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          lengthMenu = c(10, 25, 50, 100),
          searchHighlight = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          order = list(list(5, 'asc')),  # Sort by p.adjust ascending (column shifted)
          columnDefs = list(
            list(targets = 0, orderable = FALSE, width = '80px')  # Download button column
          )
        ),
        escape = FALSE,  # Allow HTML in download_genes column
        rownames = FALSE,
        class = 'cell-border stripe hover'
      ) %>%
        DT::formatSignif(columns = c('pvalue', 'p.adjust', 'qvalue'), digits = 3) %>%
        DT::formatRound(columns = c('mean_log2FC', 'enrichment_score'), digits = 2)
    })
    
    # Store full data for download access
    gene_data_id <- paste0("gene_data_", ont, "_", safe_contrast_name)
    values[[gene_data_id]] <- as.data.frame(ego)
    
    # Bar Plot
    barplot_id <- paste0("enrich_barplot_", ont, "_", safe_contrast_name)
    output[[barplot_id]] <- renderPlot({
      
      df <- as.data.frame(ego) %>%
        arrange(p.adjust) %>%
        head(20) %>%
        mutate(
          neg_log10_padj = -log10(p.adjust),
          Description = factor(Description, levels = Description)
        )
      
      ggplot(df, aes(x = reorder(Description, neg_log10_padj), y = neg_log10_padj)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        geom_text(aes(label = Count), hjust = -0.2, size = 3) +
        coord_flip() +
        labs(
          title = paste("Top 20", ont, "Terms"),
          x = "GO Term",
          y = "-log10(adjusted p-value)"
        ) +
        theme_minimal() +
        theme(
          axis.text.y = element_text(size = 9),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.grid.major.y = element_blank()
        )
    })
    
    # Dot Plot
    dotplot_id <- paste0("enrich_dotplot_", ont, "_", safe_contrast_name)
    output[[dotplot_id]] <- renderPlot({
      
      tryCatch({
        enrichplot::dotplot(ego, showCategory = 20) +
          labs(title = paste(ont, "Enrichment")) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      }, error = function(e) {
        # Fallback simple plot if dotplot fails
        df <- as.data.frame(ego) %>%
          arrange(p.adjust) %>%
          head(20)
        
        ggplot(df, aes(x = Count, y = reorder(Description, Count))) +
          geom_point(aes(size = Count, color = p.adjust)) +
          scale_color_gradient(low = "red", high = "blue") +
          labs(title = paste(ont, "Enrichment"), x = "Gene Count", y = "GO Term") +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.y = element_text(size = 9)
          )
      })
    })
    
    # CSV Export
    csv_id <- paste0("download_csv_", ont, "_", safe_contrast_name)
    output[[csv_id]] <- downloadHandler(
      filename = function() {
        paste0(params$contrast, "_", ont, "_", params$direction, "_enrichment.csv")
      },
      content = function(file) {
        tryCatch({
          df <- as.data.frame(ego)
          
          # Also save to fixed subfolder
          output_dir <- file.path(
            "results", "enrichment", params$contrast, 
            params$organism, params$id_type, ont, params$direction
          )
          dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
          
          fixed_file <- file.path(
            output_dir,
            paste0(params$contrast, "_", ont, "_", params$direction, "_enrichment.csv")
          )
          
          # Write to both browser download and fixed location
          write.csv(df, file, row.names = FALSE)
          write.csv(df, fixed_file, row.names = FALSE)
          
          message(paste("CSV exported to:", fixed_file))
          
          # Show notification about fixed location
          showNotification(
            paste0("Results also saved to: ", fixed_file),
            type = "message",
            duration = 5
          )
          
        }, error = function(e) {
          message(paste("Error exporting CSV:", e$message))
          showNotification(
            paste("Error exporting CSV:", e$message),
            type = "error",
            duration = 10
          )
        })
      }
    )
    
    # Excel Export
    xlsx_id <- paste0("download_xlsx_", ont, "_", safe_contrast_name)
    output[[xlsx_id]] <- downloadHandler(
      filename = function() {
        paste0(params$contrast, "_", ont, "_", params$direction, "_enrichment.xlsx")
      },
      content = function(file) {
        tryCatch({
          df <- as.data.frame(ego)
          
          # Also save to fixed subfolder
          output_dir <- file.path(
            "results", "enrichment", params$contrast, 
            params$organism, params$id_type, ont, params$direction
          )
          dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
          
          fixed_file <- file.path(
            output_dir,
            paste0(params$contrast, "_", ont, "_", params$direction, "_enrichment.xlsx")
          )
          
          # Create workbook with formatting
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, sheetName = "Enrichment Results")
          
          # Write data with header style
          openxlsx::writeData(wb, sheet = 1, x = df, startRow = 1, startCol = 1)
          
          # Style header row
          header_style <- openxlsx::createStyle(
            fontSize = 12,
            fontColour = "#FFFFFF",
            fgFill = "#4472C4",
            halign = "center",
            textDecoration = "bold",
            border = "TopBottomLeftRight"
          )
          openxlsx::addStyle(wb, sheet = 1, style = header_style, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
          
          # Auto-size columns
          openxlsx::setColWidths(wb, sheet = 1, cols = 1:ncol(df), widths = "auto")
          
          # Freeze header row
          openxlsx::freezePane(wb, sheet = 1, firstRow = TRUE)
          
          # Write to both locations
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
          openxlsx::saveWorkbook(wb, fixed_file, overwrite = TRUE)
          
          message(paste("Excel exported to:", fixed_file))
          
          # Show notification about fixed location
          showNotification(
            paste0("Results also saved to: ", fixed_file),
            type = "message",
            duration = 5
          )
          
        }, error = function(e) {
          message(paste("Error exporting Excel:", e$message))
          showNotification(
            paste("Error exporting Excel:", e$message),
            type = "error",
            duration = 10
          )
        })
      }
    )
    
    # Gene Lists Export
    genes_id <- paste0("download_genes_", ont, "_", safe_contrast_name)
    output[[genes_id]] <- downloadHandler(
      filename = function() {
        paste0(params$contrast, "_", ont, "_", params$direction, "_gene_lists.csv")
      },
      content = function(file) {
        tryCatch({
          df <- as.data.frame(ego)
          
          # Create a dataframe with term and genes
          gene_lists <- data.frame(
            GO_ID = df$ID,
            GO_Term = df$Description,
            Gene_Count = df$Count,
            P_Adjust = df$p.adjust,
            Genes = df$geneID,
            stringsAsFactors = FALSE
          )
          
          # Also save to fixed subfolder
          output_dir <- file.path(
            "results", "enrichment", params$contrast, 
            params$organism, params$id_type, ont, params$direction
          )
          dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
          
          fixed_file <- file.path(
            output_dir,
            paste0(params$contrast, "_", ont, "_", params$direction, "_gene_lists.csv")
          )
          
          # Write to both locations
          write.csv(gene_lists, file, row.names = FALSE)
          write.csv(gene_lists, fixed_file, row.names = FALSE)
          
          message(paste("Gene lists exported to:", fixed_file))
          
          # Show notification
          showNotification(
            paste0("Gene lists exported: ", nrow(gene_lists), " terms"),
            type = "message",
            duration = 5
          )
          
        }, error = function(e) {
          message(paste("Error exporting gene lists:", e$message))
          showNotification(
            paste("Error exporting gene lists:", e$message),
            type = "error",
            duration = 10
          )
        })
      }
    )
  })
})

# =============================================================================
# INDIVIDUAL GENE LIST DOWNLOAD (Per GO Term)
# =============================================================================

# Handle download button clicks for individual GO terms
observeEvent(input$download_gene_list, {
  req(input$download_gene_list)
  
  tryCatch({
    
    # Extract info from JavaScript
    row_num <- input$download_gene_list$row
    ont <- input$download_gene_list$ont
    contrast <- input$download_gene_list$contrast
    
    message(paste("Download request: Row", row_num, "Ontology", ont, "Contrast", contrast))
    
    # Get stored data
    gene_data_id <- paste0("gene_data_", ont, "_", contrast)
    
    if (is.null(values[[gene_data_id]])) {
      showNotification(
        "Error: Gene data not found. Please re-run enrichment analysis.",
        type = "error",
        duration = 5
      )
      return()
    }
    
    ego_df <- values[[gene_data_id]]
    message(paste("  Found ego_df with", nrow(ego_df), "rows"))
    
    # Check if row exists
    if (row_num > nrow(ego_df)) {
      showNotification(
        paste0("Error: Invalid row number (", row_num, " > ", nrow(ego_df), ")"),
        type = "error",
        duration = 5
      )
      return()
    }
    
    # Get the selected term
    term_data <- ego_df[row_num, ]
    go_id <- term_data$ID
    go_term <- term_data$Description
    gene_string <- term_data$geneID
    
    message(paste("  GO Term:", go_term))
    message(paste("  GO ID:", go_id))
    message(paste("  Genes:", substr(gene_string, 1, 100)))
    
    # Split genes by "/"
    genes <- strsplit(gene_string, "/")[[1]]
    message(paste("  Split into", length(genes), "genes"))
    
    # Get DEG results for this contrast to add statistics
    # Check both uploaded and computed results
    deg_results <- NULL
    
    # Priority 1: Check uploaded results
    if (!is.null(values$uploaded_deg_results) && 
        !is.null(values$uploaded_deg_results[[contrast]])) {
      deg_results <- values$uploaded_deg_results[[contrast]]$results
      message(paste("  Found uploaded DEG results with", nrow(deg_results), "genes"))
    }
    # Priority 2: Check computed results
    else if (!is.null(values$deseq_results) && 
             !is.null(values$deseq_results[[contrast]])) {
      deg_results <- values$deseq_results[[contrast]]$results
      message(paste("  Found computed DEG results with", nrow(deg_results), "genes"))
    }
    
    # Create base dataframe with gene info
    gene_df <- data.frame(
      Gene = genes,
      GO_ID = go_id,
      GO_Term = go_term,
      stringsAsFactors = FALSE
    )
    
    # Add DEG statistics if available
    if (!is.null(deg_results)) {
      
      # Match genes to DEG results
      # Try matching by 'gene' column first, then by rownames
      if ("gene" %in% colnames(deg_results)) {
        match_idx <- match(genes, deg_results$gene)
      } else {
        match_idx <- match(genes, rownames(deg_results))
      }
      
      # Add log2FoldChange
      gene_df$log2FoldChange <- ifelse(
        !is.na(match_idx),
        deg_results$log2FoldChange[match_idx],
        NA
      )
      
      # Add padj
      if ("padj" %in% colnames(deg_results)) {
        gene_df$padj <- ifelse(
          !is.na(match_idx),
          deg_results$padj[match_idx],
          NA
        )
      }
      
      # Add baseMean
      if ("baseMean" %in% colnames(deg_results)) {
        gene_df$baseMean <- ifelse(
          !is.na(match_idx),
          deg_results$baseMean[match_idx],
          NA
        )
      }
      
      # Add pvalue
      if ("pvalue" %in% colnames(deg_results)) {
        gene_df$pvalue <- ifelse(
          !is.na(match_idx),
          deg_results$pvalue[match_idx],
          NA
        )
      }
      
      # Add lfcSE
      if ("lfcSE" %in% colnames(deg_results)) {
        gene_df$lfcSE <- ifelse(
          !is.na(match_idx),
          deg_results$lfcSE[match_idx],
          NA
        )
      }
      
      # Add direction (Up/Down based on log2FC)
      gene_df$Direction <- ifelse(
        !is.na(gene_df$log2FoldChange),
        ifelse(gene_df$log2FoldChange > 0, "Up", "Down"),
        NA
      )
      
      message(paste("  Added DEG statistics for", sum(!is.na(match_idx)), "genes"))
      
    } else {
      message("  Warning: No DEG results available to add statistics")
    }
    
    # Reorder columns for better readability
    # Primary info first, then DEG stats
    desired_order <- c(
      "Gene", "Direction", "log2FoldChange", "padj", "baseMean",
      "pvalue", "lfcSE", "GO_ID", "GO_Term"
    )
    
    # Only include columns that exist
    final_cols <- desired_order[desired_order %in% colnames(gene_df)]
    gene_df <- gene_df[, final_cols, drop = FALSE]
    
    # Sort genes by biological relevance
    # Priority 1: If log2FoldChange exists, sort by absolute value (descending)
    if ("log2FoldChange" %in% colnames(gene_df)) {
      gene_df <- gene_df[order(-abs(gene_df$log2FoldChange), na.last = TRUE), ]
      message(paste("  Sorted genes by |log2FoldChange| (descending)"))
    }
    # Priority 2: If padj exists but not log2FC, sort by padj (ascending)
    else if ("padj" %in% colnames(gene_df)) {
      gene_df <- gene_df[order(gene_df$padj, na.last = TRUE), ]
      message(paste("  Sorted genes by padj (ascending)"))
    }
    # Priority 3: Alphabetical by gene name
    else {
      gene_df <- gene_df[order(gene_df$Gene), ]
      message(paste("  Sorted genes alphabetically"))
    }
    
    # Create filename
    safe_term <- gsub("[^A-Za-z0-9]", "_", go_term)
    safe_term <- substr(safe_term, 1, 50)  # Limit length
    filename <- paste0(safe_term, "_", go_id, "_genes.csv")
    
    message(paste("  Filename:", filename))
    
    # Also save to results folder
    output_dir <- file.path(
      "results", "enrichment", "gene_lists",
      gsub("[^A-Za-z0-9]", "_", contrast), ont
    )
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    fixed_file <- file.path(output_dir, filename)
    write.csv(gene_df, fixed_file, row.names = FALSE)
    
    message(paste("  Gene list saved to fixed location:", fixed_file))
    
    # Store the data and filename for download
    values$gene_download_data <- list(
      data = gene_df,
      filename = filename,
      ready = TRUE
    )
    
    message("  Stored gene_download_data in values")
    
    # Show notification
    showNotification(
      paste0("Prepared ", nrow(gene_df), " genes from: ", 
             substr(go_term, 1, 40), if(nchar(go_term) > 40) "..." else ""),
      type = "message",
      duration = 3
    )
    
    # Create a direct download link using JavaScript and data URL
    message("  Creating direct download...")
    
    # Convert dataframe to CSV string
    csv_content <- paste(
      paste(colnames(gene_df), collapse = ","),
      paste(apply(gene_df, 1, function(row) paste(row, collapse = ",")), collapse = "\n"),
      sep = "\n"
    )
    
    # Escape special characters for JavaScript
    csv_content_escaped <- gsub("\\\\", "\\\\\\\\", csv_content)
    csv_content_escaped <- gsub("'", "\\\\'", csv_content_escaped)
    csv_content_escaped <- gsub("\n", "\\\\n", csv_content_escaped)
    
    # JavaScript to create and trigger download
    js_code <- sprintf("
      var csv = '%s';
      var blob = new Blob([csv], { type: 'text/csv;charset=utf-8;' });
      var link = document.createElement('a');
      var url = URL.createObjectURL(blob);
      link.setAttribute('href', url);
      link.setAttribute('download', '%s');
      link.style.visibility = 'hidden';
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
    ", csv_content_escaped, filename)
    
    # Execute the download
    shinyjs::runjs(js_code)
    message("  Download triggered via JavaScript")
    
  }, error = function(e) {
    message(paste("ERROR in download_gene_list handler:", e$message))
    showNotification(
      paste("Error preparing gene download:", e$message),
      type = "error",
      duration = 10
    )
  })
})

