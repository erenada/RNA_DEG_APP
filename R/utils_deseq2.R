# DESeq2 Analysis Utilities
# Author: Eren Ada, PhD

# This file will contain DESeq2 wrapper functions for the analysis pipeline
# To be implemented in Sprint 2

#' Create DESeqDataSet from count matrix and metadata
#' 
#' @param count_matrix Matrix of count data
#' @param metadata Data frame with sample information
#' @param design Design formula for the analysis
#' @return DESeqDataSet object
create_deseq_dataset <- function(count_matrix, metadata, design) {
  # To be implemented in Sprint 2
  stop("DESeq2 functions not yet implemented - coming in Sprint 2!")
}

#' Run DESeq2 analysis
#' 
#' @param dds DESeqDataSet object
#' @param test Test type ("Wald" or "LRT")
#' @param fitType Fit type for dispersion estimation
#' @return DESeqDataSet object with results
run_deseq_analysis <- function(dds, test = "Wald", fitType = "parametric") {
  # To be implemented in Sprint 2
  stop("DESeq2 functions not yet implemented - coming in Sprint 2!")
}

#' Extract results from DESeq2 analysis
#' 
#' @param dds DESeqDataSet object
#' @param contrast Contrast specification
#' @param alpha Significance level
#' @param lfcThreshold Log fold change threshold
#' @return Results table
extract_results <- function(dds, contrast, alpha = 0.05, lfcThreshold = 0) {
  # To be implemented in Sprint 2
  stop("DESeq2 functions not yet implemented - coming in Sprint 2!")
}

#' Apply LFC shrinkage with robust coefficient name handling (apeglm only)
#' 
#' This function handles apeglm coefficient naming for both simple and multi-factor designs:
#' 1. Prioritizes exact matches with factor_name to avoid covariate confusion
#' 2. Validates that the found coefficient belongs to the contrast factor
#' 3. Provides detailed debugging information
#' 4. Gracefully handles errors
#' 
#' For multi-factor designs (e.g., ~ batch + time + group), this ensures shrinkage
#' is applied to the correct coefficient (group, not batch or time).
#' 
#' Note: Uses filtering approach - no lfcThreshold for hypothesis testing
#' 
#' @param dds DESeqDataSet object
#' @param contrast Contrast specification (factor, numerator, denominator)
#' @param shrinkage_type Type of shrinkage ("apeglm", "ashr", "normal")
#' @return List with shrunken and unshrunken results plus metadata
apply_lfc_shrinkage <- function(dds, contrast, shrinkage_type = "apeglm") {
  
  # Get unshrunken results first (no threshold - filtering approach)
  res_unshrunken <- results(dds, contrast = contrast)
  
  # Get available coefficient names for debugging
  coef_names <- resultsNames(dds)
  message("Available coefficients: ", paste(coef_names, collapse = ", "))
  
  # Policy: Only allow apeglm; skip shrinkage for other types
  if (!identical(shrinkage_type, "apeglm")) {
    message("Shrinkage type '", shrinkage_type, "' is disabled. Proceeding without shrinkage.")
    return(list(
      shrunk = res_unshrunken,
      unshrunken = res_unshrunken,
      shrinkage_applied = FALSE,
      shrinkage_method = "none",
      note = paste0("Non-apeglm shrinkage disabled (requested '", shrinkage_type, "')"),
      available_coefficients = coef_names
    ))
  }
  
  # For apeglm, we need to find the correct coefficient
  if (shrinkage_type == "apeglm") {
    
    # Extract contrast components
    factor_name <- contrast[1]
    numerator <- contrast[2]
    denominator <- contrast[3]
    
    message("Searching for apeglm coefficient for contrast: ", paste(contrast, collapse = " "))
    message("Factor of interest: ", factor_name)
    
    # Build sanitized name variants (DESeq2 often uses make.names-like coercion)
    num_us <- gsub("[^A-Za-z0-9]+", "_", numerator)
    den_us <- gsub("[^A-Za-z0-9]+", "_", denominator)
    num_dot <- gsub("[^A-Za-z0-9]+", ".", numerator)
    den_dot <- gsub("[^A-Za-z0-9]+", ".", denominator)
    num_make <- make.names(numerator)
    den_make <- make.names(denominator)
    factor_make <- make.names(factor_name)

    # STRATEGY 1: Exact patterns with factor_name prefix (most reliable for multi-factor designs)
    # These patterns ensure we match the contrast factor, not covariates
    exact_patterns <- c(
      # Standard DESeq2 naming: factor_numerator_vs_denominator
      paste0(factor_name, "_", numerator, "_vs_", denominator),
      paste0(factor_make, "_", num_make, "_vs_", den_make),
      paste0(factor_name, "_", num_us, "_vs_", den_us),
      paste0(factor_name, "_", num_dot, "_vs_", den_dot),
      # Simpler factor_level naming (reference level omitted)
      paste0(factor_name, "_", numerator),
      paste0(factor_make, "_", num_make),
      paste0(factor_name, "_", num_us),
      paste0(factor_name, "_", num_dot),
      # Dotted variants
      paste0(factor_name, ".", numerator),
      paste0(factor_make, ".", num_make),
      paste0(factor_name, ".", num_dot),
      # .vs. variants
      paste0(factor_name, ".", numerator, ".vs.", denominator),
      paste0(factor_make, ".", num_make, ".vs.", den_make)
    )
    
    # Find matching coefficient using exact patterns
    coef_found <- NULL
    pattern_used <- NULL
    
    message("Trying exact patterns with factor_name prefix...")
    for (pattern in exact_patterns) {
      # Try exact match first
      if (pattern %in% coef_names) {
        coef_found <- pattern
        pattern_used <- paste0("exact: ", pattern)
        message("Found exact coefficient match: ", coef_found)
        break
      }
      
      # Try grep if no exact match
      matching_coefs <- grep(paste0("^", pattern, "$"), coef_names, value = TRUE)
      if (length(matching_coefs) == 1) {
        coef_found <- matching_coefs[1]
        pattern_used <- paste0("pattern: ", pattern)
        message("Found coefficient using pattern '", pattern, "': ", coef_found)
        break
      }
    }
    
    # STRATEGY 2: Filter coefficients by factor_name, then search within those
    # This prevents accidentally matching covariate coefficients
    if (is.null(coef_found)) {
      message("Exact pattern matching failed. Filtering by factor_name...")
      
      # Get coefficients that start with factor_name (or factor_make)
      factor_coefs <- coef_names[grepl(paste0("^", factor_name), coef_names) | 
                                  grepl(paste0("^", factor_make), coef_names)]
      # Exclude intercept
      factor_coefs <- factor_coefs[!grepl("Intercept", factor_coefs, ignore.case = TRUE)]
      
      message("Coefficients matching factor '", factor_name, "': ", 
              if(length(factor_coefs) > 0) paste(factor_coefs, collapse = ", ") else "none")
      
      if (length(factor_coefs) == 1) {
        # Only one coefficient for this factor - use it
        coef_found <- factor_coefs[1]
        pattern_used <- paste0("factor_filter(", factor_name, ")")
        message("Single coefficient found for factor: ", coef_found)
      } else if (length(factor_coefs) > 1) {
        # Multiple coefficients for this factor - search for numerator
        numerator_matches <- grep(numerator, factor_coefs, value = TRUE)
        
        if (length(numerator_matches) == 1) {
          coef_found <- numerator_matches[1]
          pattern_used <- paste0("factor_filter(", factor_name, ") + grep(", numerator, ")")
          message("Found coefficient using factor filter + numerator: ", coef_found)
        } else if (length(numerator_matches) > 1) {
          # Multiple matches - try to narrow down with denominator
          denominator_matches <- grep(denominator, numerator_matches, value = TRUE)
          if (length(denominator_matches) == 1) {
            coef_found <- denominator_matches[1]
            pattern_used <- paste0("factor_filter(", factor_name, ") + grep(", numerator, ",", denominator, ")")
            message("Found coefficient using factor filter + numerator + denominator: ", coef_found)
          } else {
            # Still ambiguous - take first match but warn
            coef_found <- numerator_matches[1]
            pattern_used <- paste0("factor_filter(", factor_name, ") + first_match(", numerator, ")")
            message("WARNING: Multiple matches for numerator within factor. Using first: ", coef_found)
          }
        }
      }
    }
    
    # STRATEGY 3: Fallback - search all coefficients (risky with covariates, use with caution)
    if (is.null(coef_found)) {
      message("WARNING: Factor-specific search failed. Trying broad search (may match covariates)...")
      numerator_matches <- grep(numerator, coef_names, value = TRUE)
      # Exclude intercept
      numerator_matches <- numerator_matches[!grepl("Intercept", numerator_matches, ignore.case = TRUE)]
      
      if (length(numerator_matches) == 1) {
        coef_found <- numerator_matches[1]
        pattern_used <- paste0("fallback_grep(", numerator, ")")
        message("Found coefficient using broad numerator search: ", coef_found)
      } else if (length(numerator_matches) > 1) {
        # Try to filter by factor_name within matches
        factor_filtered <- numerator_matches[grepl(factor_name, numerator_matches) | 
                                              grepl(factor_make, numerator_matches)]
        if (length(factor_filtered) == 1) {
          coef_found <- factor_filtered[1]
          pattern_used <- paste0("fallback_grep(", numerator, ") + factor_filter")
          message("Found coefficient using broad search + factor filter: ", coef_found)
        } else if (length(factor_filtered) > 1) {
          coef_found <- factor_filtered[1]
          pattern_used <- paste0("fallback_first_match")
          message("WARNING: Multiple ambiguous matches. Using first: ", coef_found)
        } else {
          # No factor match - this is risky, might be a covariate
          message("ERROR: Found numerator matches but none contain factor_name. Possible covariate confusion.")
          message("Matches found: ", paste(numerator_matches, collapse = ", "))
          message("Skipping shrinkage to avoid applying to wrong coefficient.")
          return(list(
            shrunk = res_unshrunken,
            unshrunken = res_unshrunken,
            shrinkage_applied = FALSE,
            shrinkage_method = "none",
            note = "Coefficient search ambiguous - possible covariate confusion",
            available_coefficients = coef_names
          ))
        }
      }
    }
    
    # VALIDATION: Ensure found coefficient belongs to the contrast factor
    if (!is.null(coef_found)) {
      # Check if coefficient name contains the factor_name
      if (!grepl(factor_name, coef_found) && !grepl(factor_make, coef_found)) {
        message("ERROR: Found coefficient '", coef_found, "' does not contain factor_name '", factor_name, "'")
        message("This may be a covariate coefficient. Skipping shrinkage.")
        return(list(
          shrunk = res_unshrunken,
          unshrunken = res_unshrunken,
          shrinkage_applied = FALSE,
          shrinkage_method = "none",
          note = paste0("Found coefficient '", coef_found, "' does not match factor '", factor_name, "'"),
          available_coefficients = coef_names
        ))
      }
    }
    
    # Apply shrinkage if coefficient found
    if (!is.null(coef_found) && coef_found %in% coef_names) {
      tryCatch({
        # Apply apeglm shrinkage to get better LFC estimates
        res_shrunk <- lfcShrink(dds, 
                               coef = coef_found, 
                               type = "apeglm")
        
        message("Successfully applied apeglm shrinkage using coefficient: ", coef_found)
        return(list(
          shrunk = res_shrunk,
          unshrunken = res_unshrunken,
          shrinkage_applied = TRUE,
          shrinkage_method = "apeglm",
          coefficient_used = coef_found,
          pattern_used = pattern_used
        ))
      }, error = function(e) {
        message("Warning: Failed to apply apeglm shrinkage with coefficient '", coef_found, "': ", e$message)
        message("Proceeding without shrinkage.")
        return(list(
          shrunk = res_unshrunken,
          unshrunken = res_unshrunken,
          shrinkage_applied = FALSE,
          shrinkage_method = "none",
          note = paste0("apeglm failed for coef '", coef_found, "'"),
          available_coefficients = coef_names
        ))
      })
    } else {
      message("No suitable coefficient found for apeglm shrinkage. Proceeding without shrinkage.")
      return(list(
        shrunk = res_unshrunken,
        unshrunken = res_unshrunken,
        shrinkage_applied = FALSE,
        shrinkage_method = "none",
        note = "apeglm coefficient not found",
        available_coefficients = coef_names
      ))
    }
  }
  
  # Final safeguard (shouldn't reach here)
  message("No shrinkage applied.")
  return(list(
    shrunk = res_unshrunken,
    unshrunken = res_unshrunken,
    shrinkage_applied = FALSE,
    shrinkage_method = "none",
    note = "No shrinkage applied",
    available_coefficients = coef_names
  ))
} 