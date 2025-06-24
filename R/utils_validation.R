# Data Validation Utilities for DEG Analysis
# Author: Eren Ada, PhD

#' Validate input data for DESeq2 analysis
#' 
#' @param count_matrix Data frame with genes as rows and samples as columns
#' @param metadata Data frame with samples as rows and variables as columns
#' @return List with validation results
validate_input_data <- function(count_matrix, metadata) {
  
  errors <- c()
  warnings <- c()
  
  # Check if data exists
  if (is.null(count_matrix) || is.null(metadata)) {
    return(list(
      valid = FALSE,
      errors = "Both count matrix and metadata are required",
      warnings = NULL,
      n_matched = 0
    ))
  }
  
  # Validate count matrix
  count_validation <- validate_count_matrix(count_matrix)
  if (!count_validation$valid) {
    errors <- c(errors, count_validation$errors)
  }
  warnings <- c(warnings, count_validation$warnings)
  
  # Validate metadata
  meta_validation <- validate_metadata(metadata)
  if (!meta_validation$valid) {
    errors <- c(errors, meta_validation$errors)
  }
  warnings <- c(warnings, meta_validation$warnings)
  
  # Check sample matching
  sample_matching <- validate_sample_matching(count_matrix, metadata)
  if (!sample_matching$valid) {
    errors <- c(errors, sample_matching$errors)
  }
  warnings <- c(warnings, sample_matching$warnings)
  
  # Return validation result
  return(list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    n_matched = sample_matching$n_matched
  ))
}

#' Validate count matrix
#' 
#' @param count_matrix Data frame with count data
#' @return List with validation results
validate_count_matrix <- function(count_matrix) {
  
  errors <- c()
  warnings <- c()
  
  # Check dimensions
  if (nrow(count_matrix) == 0) {
    errors <- c(errors, "Count matrix has no rows (genes)")
  }
  
  if (ncol(count_matrix) == 0) {
    errors <- c(errors, "Count matrix has no columns (samples)")
  }
  
  if (ncol(count_matrix) < 2) {
    errors <- c(errors, "Count matrix must have at least 2 samples")
  }
  
  # Check for numeric data
  numeric_cols <- sapply(count_matrix, is.numeric)
  if (!all(numeric_cols)) {
    non_numeric <- names(count_matrix)[!numeric_cols]
    errors <- c(errors, paste("Non-numeric columns found:", paste(non_numeric, collapse = ", ")))
  }
  
  # Check for negative values
  if (any(sapply(count_matrix[numeric_cols], function(x) any(x < 0, na.rm = TRUE)))) {
    errors <- c(errors, "Count matrix contains negative values")
  }
  
  # Check for non-integer values (warning only)
  if (any(sapply(count_matrix[numeric_cols], function(x) any(x != as.integer(x), na.rm = TRUE)))) {
    warnings <- c(warnings, "Count matrix contains non-integer values (this may be expected for some preprocessing methods)")
  }
  
  # Check for missing values
  if (any(is.na(count_matrix))) {
    warnings <- c(warnings, "Count matrix contains missing values (NA)")
  }
  
  # Check for very low counts
  if (all(numeric_cols)) {
    max_counts <- apply(count_matrix, 1, max, na.rm = TRUE)
    low_count_genes <- sum(max_counts < 10, na.rm = TRUE)
    if (low_count_genes > nrow(count_matrix) * 0.5) {
      warnings <- c(warnings, paste("More than 50% of genes have maximum counts < 10. Consider pre-filtering."))
    }
  }
  
  return(list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings
  ))
}

#' Validate metadata
#' 
#' @param metadata Data frame with sample information
#' @return List with validation results
validate_metadata <- function(metadata) {
  
  errors <- c()
  warnings <- c()
  
  # Check dimensions
  if (nrow(metadata) == 0) {
    errors <- c(errors, "Metadata has no rows (samples)")
  }
  
  if (ncol(metadata) == 0) {
    errors <- c(errors, "Metadata has no columns (variables)")
  }
  
  # Check for empty sample names
  if (is.null(rownames(metadata)) || any(rownames(metadata) == "")) {
    errors <- c(errors, "Metadata must have sample names as row names")
  }
  
  # Check for duplicate sample names
  if (any(duplicated(rownames(metadata)))) {
    errors <- c(errors, "Metadata contains duplicate sample names")
  }
  
  # Check for missing values in important columns
  if (any(is.na(metadata))) {
    warnings <- c(warnings, "Metadata contains missing values (NA)")
  }
  
  # Identify factor columns and warn about single-level factors
  factor_cols <- sapply(metadata, function(x) is.factor(x) || is.character(x))
  if (any(factor_cols)) {
    for (col in names(metadata)[factor_cols]) {
      unique_vals <- length(unique(metadata[[col]]))
      if (unique_vals == 1) {
        warnings <- c(warnings, paste("Column", col, "has only one unique value"))
      } else if (unique_vals == nrow(metadata)) {
        warnings <- c(warnings, paste("Column", col, "has all unique values (may not be suitable for grouping)"))
      }
    }
  }
  
  return(list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings
  ))
}

#' Validate sample matching between count matrix and metadata
#' 
#' @param count_matrix Data frame with count data
#' @param metadata Data frame with sample information
#' @return List with validation results
validate_sample_matching <- function(count_matrix, metadata) {
  
  errors <- c()
  warnings <- c()
  
  count_samples <- colnames(count_matrix)
  meta_samples <- rownames(metadata)
  
  # Check for missing sample names
  if (is.null(count_samples) || any(count_samples == "")) {
    errors <- c(errors, "Count matrix must have sample names as column names")
  }
  
  if (is.null(meta_samples) || any(meta_samples == "")) {
    errors <- c(errors, "Metadata must have sample names as row names")
  }
  
  if (length(errors) > 0) {
    return(list(
      valid = FALSE,
      errors = errors,
      warnings = warnings,
      n_matched = 0
    ))
  }
  
  # Find matching samples
  matched_samples <- intersect(count_samples, meta_samples)
  n_matched <- length(matched_samples)
  
  # Check for sufficient overlap
  if (n_matched == 0) {
    errors <- c(errors, "No matching samples found between count matrix and metadata")
  } else if (n_matched < 2) {
    errors <- c(errors, "At least 2 matching samples are required for analysis")
  }
  
  # Warn about non-matching samples
  count_only <- setdiff(count_samples, meta_samples)
  meta_only <- setdiff(meta_samples, count_samples)
  
  if (length(count_only) > 0) {
    warnings <- c(warnings, paste("Samples in count matrix but not in metadata:", 
                                  paste(head(count_only, 3), collapse = ", "),
                                  if(length(count_only) > 3) "..." else ""))
  }
  
  if (length(meta_only) > 0) {
    warnings <- c(warnings, paste("Samples in metadata but not in count matrix:", 
                                  paste(head(meta_only, 3), collapse = ", "),
                                  if(length(meta_only) > 3) "..." else ""))
  }
  
  return(list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    n_matched = n_matched,
    matched_samples = matched_samples
  ))
} 