# Server logic for Tab 2: Experimental Design
# Author: Eren Ada, PhD
#
# This file contains all server-side logic for experimental design and contrast creation

# =============================================================================
# SANITIZATION HELPERS
# =============================================================================

# Name sanitization helpers
sanitize_identifier <- function(x, prefix = "g_") {
  if (is.null(x)) return("")
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9]", "_", x)              # replace non-alnum with _
  x <- gsub("_+", "_", x)                         # collapse multiple _
  x <- gsub("^_+|_+$", "", x)                      # trim leading/trailing _
  if (!grepl("^[A-Za-z]", x)) x <- paste0(prefix, x) # ensure starts with letter
  if (x == "") x <- prefix
  x
}

# Validation-only checker
is_valid_identifier <- function(x) {
  if (is.null(x) || !nzchar(x)) return(FALSE)
  grepl("^[A-Za-z][A-Za-z0-9_]*$", x)
}

# Validation (no auto-correct) for input names
observeEvent(input$group1_name, {
  req(!is.null(input$group1_name))
  if (!is_valid_identifier(input$group1_name)) {
    showNotification("Group 1 name invalid. Use letters, digits, underscore; start with a letter.", type = "error", duration = 5)
  }
}, ignoreInit = TRUE)

observeEvent(input$group2_name, {
  req(!is.null(input$group2_name))
  if (!is_valid_identifier(input$group2_name)) {
    showNotification("Group 2 name invalid. Use letters, digits, underscore; start with a letter.", type = "error", duration = 5)
  }
}, ignoreInit = TRUE)

observeEvent(input$contrast_factor_name, {
  req(!is.null(input$contrast_factor_name))
  if (!is_valid_identifier(input$contrast_factor_name)) {
    showNotification("Factor name invalid. Use letters, digits, underscore; start with a letter.", type = "error", duration = 5)
  }
}, ignoreInit = TRUE)

# =============================================================================
# TAB 2: EXPERIMENTAL DESIGN
# =============================================================================

# Update available samples from validated data
observe({
  if (!is.null(values$validation_status) && values$validation_status$valid) {
    # Get matched samples
    count_samples <- colnames(values$count_data)
    meta_samples <- rownames(values$meta_data)
    matched_samples <- intersect(count_samples, meta_samples)
    
    values$available_samples <- matched_samples
    values$group1_samples <- character(0)
    values$group2_samples <- character(0)
  }
})

# Create metadata filter dropdowns
output$metadata_filters <- renderUI({
  req(values$meta_data)
  
  meta_cols <- colnames(values$meta_data)
  
  filter_controls <- lapply(meta_cols, function(col) {
    # Get unique values for this column from the full metadata
    unique_vals <- unique(values$meta_data[[col]][values$meta_data[[col]] != ""]) 
    unique_vals <- unique_vals[!is.na(unique_vals)]
    
    choices <- c("All" = "all", setNames(unique_vals, unique_vals))
    
    div(
      style = "margin-bottom: 8px;",
      selectInput(
        paste0("filter_", col),
        paste0(col, ":"),
        choices = choices,
        selected = "all",
        width = "100%"
      )
    )
  })
  
  do.call(div, filter_controls)
})

# Function to update filtered samples
update_filtered_samples <- function() {
  req(values$meta_data)
  
  # If no available samples, set filtered samples to empty
  if (length(values$available_samples) == 0) {
    values$filtered_samples <- character(0)
    values$selected_filters <- list()
    return()
  }
  
  # Get all filter values
  meta_cols <- colnames(values$meta_data)
  current_filters <- list()
  
  for (col in meta_cols) {
    filter_input <- input[[paste0("filter_", col)]]
    if (!is.null(filter_input) && filter_input != "all") {
      current_filters[[col]] <- filter_input
    }
  }
  
  # Filter samples based on selected criteria
  if (length(current_filters) == 0) {
    values$filtered_samples <- values$available_samples
  } else {
    filtered_samples <- values$available_samples
    
    for (col in names(current_filters)) {
      filter_val <- current_filters[[col]]
      matching_samples <- rownames(values$meta_data)[values$meta_data[[col]] == filter_val]
      filtered_samples <- intersect(filtered_samples, matching_samples)
    }
    
    values$filtered_samples <- filtered_samples
  }
  
  values$selected_filters <- current_filters
}

# Update filtered samples when filters change
observe({
  # Only react to filter changes if metadata is available
  req(values$meta_data)
  
  # React to filter changes
  meta_cols <- colnames(values$meta_data)
  for (col in meta_cols) {
    input[[paste0("filter_", col)]]
  }
  
  update_filtered_samples()
})

# Show filtered sample information
output$filtered_sample_info <- renderUI({
  req(values$available_samples)
  
  total_available <- length(values$available_samples)
  total_filtered <- length(values$filtered_samples)
  
  # Handle case where no samples are available
  if (total_available == 0) {
    return(div(
      style = "background-color: #f8f9fa; padding: 8px; border-radius: 3px; margin-bottom: 10px;",
      p("No samples available to filter.", style = "color: #7F8C8D; margin: 0;")
    ))
  }
  
  filter_text <- if (length(values$selected_filters) > 0) {
    filter_desc <- paste(
      paste(names(values$selected_filters), values$selected_filters, sep = "="),
      collapse = ", "
    )
    paste0("Filtered by: ", filter_desc)
  } else {
    "No filters applied"
  }
  
  div(
    style = "background-color: #f8f9fa; padding: 8px; border-radius: 3px; margin-bottom: 10px;",
    p(strong(paste0("Showing ", total_filtered, " of ", total_available, " samples")), style = "margin: 0;"),
    p(filter_text, style = "margin: 5px 0 0 0; font-size: 12px; color: #666;")
  )
})

# Clear all filters
observeEvent(input$show_all_samples, {
  meta_cols <- colnames(values$meta_data)
  for (col in meta_cols) {
    updateSelectInput(session, paste0("filter_", col), selected = "all")
  }
})

# Available samples list (simplified)
output$available_samples_list <- renderUI({
  samples_to_show <- values$filtered_samples
  
  if (length(samples_to_show) == 0) {
    return(p("No samples match the current filters.", style = "color: #7F8C8D; text-align: center; margin-top: 30px;"))
  }
  
  # Create simple draggable sample items
  sample_items <- lapply(samples_to_show, function(sample) {
    # Create tooltip with metadata
    sample_tooltip <- sample
    if (!is.null(values$meta_data) && sample %in% rownames(values$meta_data)) {
      sample_meta <- values$meta_data[sample, , drop = FALSE]
      meta_info <- paste(paste(colnames(sample_meta), sample_meta[1, ], sep = ": "), collapse = ", ")
      sample_tooltip <- paste0(sample, " - ", meta_info)
    }
    
    div(
      class = "sample-item",
      `data-sample-id` = sample,
      title = sample_tooltip,
      style = "text-align: center; padding: 8px 12px; margin: 3px; display: inline-block;",
      strong(sample)
    )
  })
  
  do.call(div, sample_items)
})

# Group 1 samples list
output$group1_samples_list <- renderUI({
  if (length(values$group1_samples) == 0) {
    return(p("Drop samples here", style = "color: #7F8C8D; text-align: center; margin-top: 40px;"))
  }
  
  sample_items <- lapply(values$group1_samples, function(sample) {
    # Get sample metadata for display
    sample_tooltip <- sample
    
    if (!is.null(values$meta_data) && sample %in% rownames(values$meta_data)) {
      # Create detailed tooltip
      sample_meta <- values$meta_data[sample, , drop = FALSE]
      meta_info <- paste(paste(colnames(sample_meta), sample_meta[1, ], sep = ": "), collapse = ", ")
      sample_tooltip <- paste0(sample, " - ", meta_info)
    }
    
    div(
      class = "sample-item",
      `data-sample-id` = sample,
      title = sample_tooltip,
      style = "text-align: center; line-height: 1.2; padding: 8px;",
      div(strong(sample), style = "font-size: 12px;"),
      if (!is.null(values$meta_data) && sample %in% rownames(values$meta_data)) {
        div(
          paste(values$meta_data[sample, ], collapse = " | "),
          style = "font-size: 10px; color: #666; margin-top: 2px;"
        )
      }
    )
  })
  
  do.call(div, sample_items)
})

# Group 2 samples list  
output$group2_samples_list <- renderUI({
  if (length(values$group2_samples) == 0) {
    return(p("Drop samples here", style = "color: #7F8C8D; text-align: center; margin-top: 40px;"))
  }
  
  sample_items <- lapply(values$group2_samples, function(sample) {
    # Get sample metadata for display
    sample_tooltip <- sample
    
    if (!is.null(values$meta_data) && sample %in% rownames(values$meta_data)) {
      # Create detailed tooltip
      sample_meta <- values$meta_data[sample, , drop = FALSE]
      meta_info <- paste(paste(colnames(sample_meta), sample_meta[1, ], sep = ": "), collapse = ", ")
      sample_tooltip <- paste0(sample, " - ", meta_info)
    }
    
    div(
      class = "sample-item",
      `data-sample-id` = sample,
      title = sample_tooltip,
      style = "text-align: center; line-height: 1.2; padding: 8px;",
      div(strong(sample), style = "font-size: 12px;"),
      if (!is.null(values$meta_data) && sample %in% rownames(values$meta_data)) {
        div(
          paste(values$meta_data[sample, ], collapse = " | "),
          style = "font-size: 10px; color: #666; margin-top: 2px;"
        )
      }
    )
  })
  
  do.call(div, sample_items)
})

# Handle drag and drop movements
observeEvent(input$sample_moved, {
  req(input$sample_moved)
  
  sample_id <- input$sample_moved$sample
  target_zone <- input$sample_moved$target
  
  # Remove sample from all lists first
  values$available_samples <- setdiff(values$available_samples, sample_id)
  values$group1_samples <- setdiff(values$group1_samples, sample_id)
  values$group2_samples <- setdiff(values$group2_samples, sample_id)
  
  # Add to target zone
  if (target_zone == "available_samples") {
    values$available_samples <- c(values$available_samples, sample_id)
  } else if (target_zone == "group1_samples") {
    values$group1_samples <- c(values$group1_samples, sample_id)
  } else if (target_zone == "group2_samples") {
    values$group2_samples <- c(values$group2_samples, sample_id)
  }
  
  # Update filtered samples manually
  update_filtered_samples()
})

# =============================================================================
# COVARIATE SELECTION AND VALIDATION
# =============================================================================

# Update covariate choices based on metadata
observe({
  req(values$meta_data)
  req(length(values$group1_samples) > 0 || length(values$group2_samples) > 0)
  
  # Get factor name
  factor_name <- if(is.null(input$contrast_factor_name) || input$contrast_factor_name == "") {
    "contrast_group"
  } else {
    sanitize_identifier(input$contrast_factor_name, prefix = "f_")
  }
  
  # Get all metadata columns
  meta_cols <- colnames(values$meta_data)
  
  # Exclude: factor_name, sample ID columns, and all-unique columns
  exclude_patterns <- c("sample_id", "SampleID", "sample", "Sample", "SAMPLE")
  exclude_cols <- meta_cols[tolower(meta_cols) %in% tolower(exclude_patterns)]
  
  # Detect all-unique columns (likely IDs)
  contrast_samples <- c(values$group1_samples, values$group2_samples)
  if (length(contrast_samples) > 0) {
    unique_cols <- sapply(meta_cols, function(col) {
      vals <- values$meta_data[contrast_samples, col]
      length(unique(vals)) == length(vals)
    })
    exclude_cols <- c(exclude_cols, names(unique_cols)[unique_cols])
  }
  
  # Final choices
  covariate_choices <- setdiff(meta_cols, c(factor_name, exclude_cols))
  
  updateSelectizeInput(session, "covariates", choices = covariate_choices, selected = input$covariates)
})

# Dynamic reference level controls for categorical covariates
output$covariate_reference_controls <- renderUI({
  req(values$meta_data)
  req(input$covariates)
  
  if (length(input$covariates) == 0) {
    return(p("No covariates selected.", style = "color: #999; font-size: 12px;"))
  }
  
  contrast_samples <- c(values$group1_samples, values$group2_samples)
  if (length(contrast_samples) == 0) {
    return(p("Assign samples to groups first.", style = "color: #999; font-size: 12px;"))
  }
  
  md_subset <- values$meta_data[contrast_samples, , drop = FALSE]
  
  ref_controls <- lapply(input$covariates, function(cv) {
    # Check if numeric
    if (is.numeric(md_subset[[cv]])) {
      return(NULL)  # No reference level for numeric
    }
    
    # Get unique levels
    levels_available <- sort(unique(as.character(md_subset[[cv]])))
    
    if (length(levels_available) <= 1) {
      return(div(
        style = "margin-bottom: 10px; padding: 8px; background-color: #fff3cd; border-radius: 4px;",
        strong(cv, style = "color: #856404;"),
        br(),
        span("Single level detected. Remove this covariate.", style = "color: #856404; font-size: 11px;")
      ))
    }
    
    div(
      style = "margin-bottom: 10px;",
      selectInput(
        paste0("ref_", cv),
        paste0("Reference level for ", cv, ":"),
        choices = levels_available,
        selected = levels_available[1]
      ),
      div(
        style = "font-size: 11px; color: #666; margin-top: -10px;",
        "First level is baseline.",
        tags$span(
          icon("info-circle"),
          style = "color: #3498DB; cursor: pointer; margin-left: 6px;",
          tabindex = "0",
          `data-toggle` = "popover",
          `data-placement` = "right",
          title = "Reference level",
          `data-content` = "Set the reference level for this covariate. The first level is used as baseline in the model."
        )
      )
    )
  })
  
  do.call(div, Filter(Negate(is.null), ref_controls))
})

# Design formula preview
output$design_formula_preview <- renderUI({
  req(length(values$group1_samples) > 0, length(values$group2_samples) > 0)
  
  covs <- input$covariates
  if (is.null(covs)) covs <- character(0)
  
  factor_name <- if(is.null(input$contrast_factor_name) || input$contrast_factor_name == "") {
    "contrast_group"
  } else {
    sanitize_identifier(input$contrast_factor_name, prefix = "f_")
  }
  
  # Construct formula: covariates + factor_of_interest
  rhs_terms <- c(covs, factor_name)
  rhs_terms <- unique(rhs_terms[rhs_terms != ""])
  formula_str <- paste("~", paste(rhs_terms, collapse = " + "))
  
  div(
    strong("Design Formula: "),
    code(formula_str, style="background: #f5f5f5; padding: 4px; border-radius: 3px;"),
    if (length(covs) > 0) {
      div(
        style = "margin-top: 8px; font-size: 11px; color: #666;",
        icon("info-circle", style = "color: #3498DB;"),
        span(" Covariates will be adjusted for in the analysis. The factor of interest is placed last.", style = "margin-left: 4px;")
      )
    }
  )
})

# Design warnings (validation feedback)
output$design_warnings <- renderUI({
  req(length(values$group1_samples) > 0, length(values$group2_samples) > 0)
  
  covs <- input$covariates
  if (is.null(covs) || length(covs) == 0) {
    return(NULL)
  }
  
  contrast_samples <- c(values$group1_samples, values$group2_samples)
  md_subset <- values$meta_data[contrast_samples, , drop = FALSE]
  
  warnings <- character(0)
  
  # Check each covariate
  for (cv in covs) {
    # Check if column exists
    if (!cv %in% colnames(md_subset)) {
      warnings <- c(warnings, paste0("Covariate '", cv, "' not found in metadata."))
      next
    }
    
    # Check for single level (categorical)
    if (!is.numeric(md_subset[[cv]])) {
      n_levels <- length(unique(md_subset[[cv]][!is.na(md_subset[[cv]])]))
      if (n_levels <= 1) {
        warnings <- c(warnings, paste0("Warning: '", cv, "' has ≤1 level among selected samples."))
      }
    } else {
      # Check variance for numeric
      if (var(md_subset[[cv]], na.rm = TRUE) < 1e-6) {
        warnings <- c(warnings, paste0("Warning: '", cv, "' has near-zero variance."))
      }
    }
  }
  
  if (length(warnings) > 0) {
    div(
      style = "margin-top: 10px; padding: 10px; background-color: #fff3cd; border-left: 4px solid #ffc107; border-radius: 4px;",
      strong("Validation Warnings:", style = "color: #856404;"),
      tags$ul(
        style = "margin: 5px 0 0 0; padding-left: 20px; color: #856404; font-size: 12px;",
        lapply(warnings, function(w) tags$li(w))
      )
    )
  }
})

# Reset covariates button
observeEvent(input$reset_covariates, {
  updateSelectizeInput(session, "covariates", selected = character(0))
  showNotification("Covariates cleared. Design reset to simple single-factor.", type = "message", duration = 3)
})

# Contrast preview
output$contrast_preview <- renderUI({
  group1_name <- if(is.null(input$group1_name) || input$group1_name == "") "Group1" else sanitize_identifier(input$group1_name, prefix = "g1_")
  group2_name <- if(is.null(input$group2_name) || input$group2_name == "") "Group2" else sanitize_identifier(input$group2_name, prefix = "g2_")
  
  if (length(values$group1_samples) == 0 && length(values$group2_samples) == 0) {
    return(p("No samples assigned to groups yet.", style = "color: #7F8C8D;"))
  }
  
  # Show design preview if both groups have samples
  if (length(values$group1_samples) > 0 && length(values$group2_samples) > 0) {
    # Get the factor name for preview
    preview_factor_name <- if(is.null(input$contrast_factor_name) || input$contrast_factor_name == "") {
      "contrast_group"
    } else {
      sanitize_identifier(input$contrast_factor_name, prefix = "f_")
    }
    
    # Get covariates for formula preview
    covs <- input$covariates
    if (is.null(covs)) covs <- character(0)
    
    # Construct full formula
    rhs_terms <- c(covs, preview_factor_name)
    rhs_terms <- unique(rhs_terms[rhs_terms != ""])
    formula_str <- paste("~", paste(rhs_terms, collapse = " + "))
    
    div(
      strong(paste0(group1_name, " (", length(values$group1_samples), " samples) vs ", 
                   group2_name, " (", length(values$group2_samples), " samples)")),
      br(),
      p("Ready to create contrast!", style = "color: #4caf50;"),
      br(),
      div(
        style = "background-color: #e8f5e8; padding: 8px; border-radius: 4px; font-size: 12px;",
        strong("Design Preview:", style = "color: #2e7d32;"),
        br(),
        span("Approach: ", style = "font-weight: bold;"),
        span(if (length(covs) > 0) "User-defined with covariates" else "User-defined groups"),
        br(),
        span("Formula: ", style = "font-weight: bold;"),
        code(formula_str, style = "background: #fff; padding: 2px;"),
        br(),
        span("Contrast: ", style = "font-weight: bold;"),
        span(paste0(group1_name, " vs ", group2_name)),
        if (length(covs) > 0) {
          tagList(
            br(),
            span("Covariates: ", style = "font-weight: bold;"),
            span(paste(covs, collapse = ", "), style = "color: #1976d2;")
          )
        }
      )
    )
  } else {
    div(
      strong(paste0(group1_name, " (", length(values$group1_samples), " samples) vs ", 
                   group2_name, " (", length(values$group2_samples), " samples)")),
      br(),
      p("Assign samples to both groups to create contrast.", style = "color: #ff9800;")
    )
  }
})

# Enable/disable create contrast button
observe({
  valid_names <- is_valid_identifier(input$group1_name) && is_valid_identifier(input$group2_name) &&
                 (is.null(input$contrast_factor_name) || input$contrast_factor_name == "" || is_valid_identifier(input$contrast_factor_name))
  can_create <- length(values$group1_samples) > 0 && length(values$group2_samples) > 0 &&
               valid_names
  
  if (can_create) {
    shinyjs::enable("create_contrast")
  } else {
    shinyjs::disable("create_contrast")
  }
})

# Helper function to detect rank deficiency cause
detect_rank_issue <- function(md, covs, factor_name) {
  # Check for perfect confounding between any covariate and factor_of_interest
  for (cv in covs) {
    if (is.factor(md[[cv]]) && is.factor(md[[factor_name]])) {
      ct <- table(md[[cv]], md[[factor_name]])
      # If contingency table has only one non-zero cell per row/col, confounded
      if (all(rowSums(ct > 0) == 1) || all(colSums(ct > 0) == 1)) {
        return(paste0("Covariate '", cv, "' is perfectly confounded with the contrast factor. Balance samples across levels or remove this covariate."))
      }
    }
  }
  
  # Generic message if no specific cause detected
  return("Design is not full rank. Check for confounding, empty levels, or linear dependencies among covariates.")
}

# User-defined grouping approach with optional covariates
create_contrast_design <- function(group1_samples, group2_samples, group1_name, group2_name, factor_name = "contrast_group", covariates = NULL) {
  req(values$meta_data)
  
  # Validate and clean factor name
  factor_name <- sanitize_identifier(factor_name, prefix = "f_")
  
  # Get covariates from input if not provided
  if (is.null(covariates)) {
    covariates <- input$covariates
  }
  if (is.null(covariates)) {
    covariates <- character(0)
  }
  
  # Gather contrast samples (exclude "Unused")
  contrast_samples <- c(group1_samples, group2_samples)
  md <- values$meta_data[contrast_samples, , drop = FALSE]
  
  # Validate covariate column names (must be valid R identifiers)
  if (length(covariates) > 0) {
    invalid_names <- covariates[!grepl("^[A-Za-z][A-Za-z0-9_]*$", covariates)]
    if (length(invalid_names) > 0) {
      stop(paste("Invalid covariate names:", paste(invalid_names, collapse=", ")))
    }
    
    # Validate covariate has >1 level among contrast samples (categorical only)
    for (cv in covariates) {
      if (!is.numeric(md[[cv]])) {
        n_levels <- length(unique(md[[cv]][!is.na(md[[cv]])]))
        if (n_levels <= 1) {
          stop(paste0("Covariate '", cv, "' has ≤1 level among selected samples. Remove it or add more samples with different levels."))
        }
      }
    }
  }
  
  # Create enhanced metadata with contrast grouping
  enhanced_metadata <- values$meta_data
  
  # Add custom factor based on user selections
  contrast_factor <- character(nrow(enhanced_metadata))
  contrast_factor[rownames(enhanced_metadata) %in% group1_samples] <- sanitize_identifier(group1_name, prefix = "g1_")
  contrast_factor[rownames(enhanced_metadata) %in% group2_samples] <- sanitize_identifier(group2_name, prefix = "g2_")
  contrast_factor[!rownames(enhanced_metadata) %in% c(group1_samples, group2_samples)] <- "Unused"
  
  enhanced_metadata[[factor_name]] <- factor(contrast_factor, levels = c(group2_name, group1_name, "Unused"))
  
  # Coerce covariate types and set references (only for contrast samples)
  md_enhanced <- enhanced_metadata[contrast_samples, , drop = FALSE]
  
  # IMPORTANT: Drop empty levels for the contrast subset to avoid rank deficiency
  # The factor of interest is created with an "Unused" level for non-contrast samples.
  # After subsetting to contrast_samples, that level has zero rows but remains in the levels,
  # which introduces an all-zero column in model.matrix(). We drop it here.
  if (is.factor(md_enhanced[[factor_name]])) {
    md_enhanced[[factor_name]] <- droplevels(md_enhanced[[factor_name]])
  }
  
  # Also drop empty levels for any categorical covariates within the contrast subset
  if (length(covariates) > 0) {
    for (cv in covariates) {
      if (!is.numeric(md_enhanced[[cv]])) {
        md_enhanced[[cv]] <- droplevels(factor(md_enhanced[[cv]]))
      }
    }
  }
  
  if (length(covariates) > 0) {
    for (cv in covariates) {
      if (!is.numeric(md_enhanced[[cv]])) {
        # Categorical: set reference level
        ref <- input[[paste0("ref_", cv)]]
        if (is.null(ref)) {
          ref <- sort(unique(as.character(md_enhanced[[cv]])))[1]
        }
        lvl <- unique(as.character(md_enhanced[[cv]]))
        lvl <- c(ref, setdiff(lvl, ref))
        md_enhanced[[cv]] <- factor(md_enhanced[[cv]], levels = lvl)
        enhanced_metadata[[cv]] <- factor(enhanced_metadata[[cv]], levels = lvl)
      } else {
        # Numeric: coerce
        md_enhanced[[cv]] <- as.numeric(md_enhanced[[cv]])
        enhanced_metadata[[cv]] <- as.numeric(enhanced_metadata[[cv]])
        
        # Check variance
        if (var(md_enhanced[[cv]], na.rm=TRUE) < 1e-6) {
          showNotification(paste0("Warning: Covariate '", cv, "' has near-zero variance."), type = "warning", duration = 6)
        }
      }
    }
  }
  
  # Construct design formula (additive only; enforce order: covariates + factor_of_interest)
  rhs_terms <- c(covariates, factor_name)
  rhs_terms <- unique(rhs_terms[rhs_terms != ""])
  design_formula <- as.formula(paste("~", paste(rhs_terms, collapse = " + ")))
  
  # Validate full rank (on contrast samples only; "Unused" excluded)
  X <- try(model.matrix(design_formula, md_enhanced), silent = TRUE)
  if (inherits(X, "try-error")) {
    stop("Error creating model matrix. Check covariate types and levels. Ensure all categorical covariates have valid factor levels.")
  }
  
  rank_deficient <- qr(X)$rank < ncol(X)
  if (rank_deficient) {
    # Detect specific cause
    error_msg <- detect_rank_issue(md_enhanced, covariates, factor_name)
    stop(error_msg)
  }
  
  # Additional guardrails: low replication per level
  for (cv in c(covariates, factor_name)) {
    if (is.factor(md_enhanced[[cv]])) {
      level_counts <- table(md_enhanced[[cv]])
      low_rep_levels <- names(level_counts[level_counts < 2])
      if (length(low_rep_levels) > 0) {
        showNotification(paste0("Warning: Covariate '", cv, "' has levels with <2 samples: ", paste(low_rep_levels, collapse=", ")), type = "warning", duration = 8)
      }
    }
  }
  
  # Design strategy with covariates
  design_info <- list(
    approach = if (length(covariates) > 0) "user_defined_additive" else "user_defined",
    design_formula = format(design_formula),
    covariates = covariates,
    contrast_specification = c(factor_name, group1_name, group2_name),
    primary_factor = factor_name,
    factor_name = factor_name,
    group1_name = group1_name,
    group2_name = group2_name,
    total_samples = length(contrast_samples),
    unused_samples = nrow(values$meta_data) - length(contrast_samples)
  )
  
  return(list(
    design_strategy = design_info,
    enhanced_metadata = enhanced_metadata
  ))
}

# Create contrast with proper experimental design
observeEvent(input$create_contrast, {
  req(input$group1_name, input$group2_name)
  req(length(values$group1_samples) > 0, length(values$group2_samples) > 0)
  
  # Require valid identifiers (no auto-correct)
  if (!is_valid_identifier(input$group1_name) || !is_valid_identifier(input$group2_name) ||
      (!is.null(input$contrast_factor_name) && nzchar(input$contrast_factor_name) && !is_valid_identifier(input$contrast_factor_name))) {
    showNotification("Please fix invalid names (letters, digits, underscore; start with a letter).", type = "error", duration = 6)
    return()
  }
  
  # Use sanitized names for safe downstream (in case of subtle unicode variants)
  g1n <- sanitize_identifier(input$group1_name, prefix = "g1_")
  g2n <- sanitize_identifier(input$group2_name, prefix = "g2_")
  contrast_name <- paste0(g1n, "_vs_", g2n)
  
  # Check for duplicate names
  if (contrast_name %in% names(values$created_contrasts)) {
    showNotification("Contrast name already exists. Please use different group names.", type = "warning")
    return()
  }
  
  # Create user-defined contrast design with covariates
  factor_name <- if(is.null(input$contrast_factor_name) || input$contrast_factor_name == "") {
    "contrast_group"
  } else {
    sanitize_identifier(input$contrast_factor_name, prefix = "f_")
  }
  
  # Wrap in tryCatch to handle validation errors
  contrast_design <- tryCatch({
    create_contrast_design(
      values$group1_samples, 
      values$group2_samples,
      g1n,
      g2n,
      factor_name,
      covariates = input$covariates
    )
  }, error = function(e) {
    showNotification(paste("Error creating contrast:", e$message), type = "error", duration = 10)
    return(NULL)
  })
  
  # If validation failed, return early
  if (is.null(contrast_design)) {
    return()
  }
  
  # Create contrast object
  values$created_contrasts[[contrast_name]] <- list(
    name = contrast_name,
    group1_name = g1n,
    group2_name = g2n,
    group1_samples = values$group1_samples,
    group2_samples = values$group2_samples,
    design_analysis = contrast_design,
    enhanced_metadata = contrast_design$enhanced_metadata,
    creation_timestamp = Sys.time()
  )
  
  # Set values$metadata to the uploaded meta_data for button enabling logic
  values$metadata <- values$meta_data
  
  # Success message
  success_msg <- if (length(contrast_design$design_strategy$covariates) > 0) {
    paste0("Contrast created with ", length(contrast_design$design_strategy$covariates), " covariate(s)!")
  } else {
    "Contrast created successfully!"
  }
  showNotification(success_msg, type = "message")
  
  # Clear groups and reset covariates
  values$available_samples <- c(values$available_samples, values$group1_samples, values$group2_samples)
  values$group1_samples <- character(0)
  values$group2_samples <- character(0)
  updateSelectizeInput(session, "covariates", selected = character(0))
  
  # Reset group names (do not auto-correct; leave defaults)
  updateTextInput(session, "group1_name", value = "Treatment")
  updateTextInput(session, "group2_name", value = "Control")
})

# Clear groups
observeEvent(input$clear_groups, {
  values$available_samples <- c(values$available_samples, values$group1_samples, values$group2_samples)
  values$group1_samples <- character(0)
  values$group2_samples <- character(0)
})

# Display created contrasts
output$created_contrasts_list <- renderUI({
  if (length(values$created_contrasts) == 0) {
    return(p("No contrasts created yet.", style = "color: #7F8C8D;"))
  }
  
  contrast_items <- lapply(names(values$created_contrasts), function(name) {
    contrast <- values$created_contrasts[[name]]
    
    div(
      class = "well well-sm",
      style = "padding: 15px; margin-bottom: 15px; background-color: #f8f9fa; border-left: 4px solid #2196f3;",
      fluidRow(
        column(9,
          h5(contrast$name, style = "margin-top: 0; color: #2C3E50;"),
          p(
            strong(contrast$group1_name), 
            paste0(" (", length(contrast$group1_samples), " samples): ",
                   paste(head(contrast$group1_samples, 3), collapse = ", "),
                   if(length(contrast$group1_samples) > 3) "..." else "")
          ),
          p(
            strong(contrast$group2_name), 
            paste0(" (", length(contrast$group2_samples), " samples): ",
                   paste(head(contrast$group2_samples, 3), collapse = ", "),
                   if(length(contrast$group2_samples) > 3) "..." else "")
          ),
          br(),
          div(
            style = "background-color: #f1f3f4; padding: 10px; border-radius: 4px; font-family: monospace; font-size: 12px;",
            
            # Basic info (always visible)
            div(
              strong("Design Formula:", style = "color: #1976d2;"),
              br(),
              code(paste0("design = ", contrast$design_analysis$design_strategy$design_formula))
            ),
            # Show covariates if present
            if (!is.null(contrast$design_analysis$design_strategy$covariates) && 
                length(contrast$design_analysis$design_strategy$covariates) > 0) {
              tagList(
                br(),
                div(
                  strong("Covariates:", style = "color: #1976d2;"),
                  br(),
                  span(paste(contrast$design_analysis$design_strategy$covariates, collapse = ", "), 
                       style = "color: #555; font-size: 11px;"),
                  br(),
                  span("(Adjusted for in the analysis)", style = "color: #999; font-size: 10px; font-style: italic;")
                )
              )
            },
            br(),
            div(
              strong("Results Command:", style = "color: #1976d2;"),
              br(),
              code(paste0("results(dds, contrast = c('", 
                         paste(contrast$design_analysis$design_strategy$contrast_specification, collapse = "', '"), 
                         "'))"))
            ),
            br(),
            
            # Toggle button for details
            tags$a(
              href = paste0("#details_", gsub("[^A-Za-z0-9]", "_", name)),
              `data-toggle` = "collapse",
              style = "color: #1976d2; font-size: 11px; text-decoration: none; cursor: pointer;",
              icon("chevron-right", style = "font-size: 10px;"),
              span(" Show Details", style = "margin-left: 5px;")
            ),
            
            # Collapsible details section
            div(
              id = paste0("details_", gsub("[^A-Za-z0-9]", "_", name)),
              class = "collapse",
              style = "margin-top: 10px;",
              
              # Design approach
              div(
                strong("Approach: ", style = "color: #d32f2f;"),
                span(
                  if (contrast$design_analysis$design_strategy$approach == "user_defined_additive") {
                    "User-defined with additive covariates"
                  } else {
                    "User-defined contrast groups"
                  },
                  style = "color: #388e3c;"
                )
              ),
              br(),
              
              # Contrast explanation
              div(
                style = "background-color: #fff3cd; padding: 6px; border-radius: 3px; font-size: 11px;",
                strong("Contrast Explanation:", style = "color: #856404;"),
                br(),
                span("• Factor: ", style = "color: #856404;"),
                code(paste0("'", contrast$design_analysis$design_strategy$factor_name, "'"), style = "font-size: 10px;"),
                span(" (the experimental factor)", style = "color: #856404;"),
                br(),
                span("• Numerator: ", style = "color: #856404;"),
                code(paste0("'", contrast$group1_name, "'"), style = "font-size: 10px;"),
                span(" (condition of interest)", style = "color: #856404;"),
                br(),
                span("• Denominator: ", style = "color: #856404;"),
                code(paste0("'", contrast$group2_name, "'"), style = "font-size: 10px;"),
                span(" (reference/baseline)", style = "color: #856404;")
              ),
              br(),
              
              # Log2FC explanation
              div(
                span("Log2FC: ", style = "color: #666; font-size: 11px;"),
                span(paste0(contrast$group1_name, " / ", contrast$group2_name), style = "color: #666; font-size: 11px;"),
                span(" (positive = upregulated in ", style = "color: #666; font-size: 10px;"),
                span(contrast$group1_name, style = "color: #666; font-size: 10px; font-weight: bold;"),
                span(")", style = "color: #666; font-size: 10px;")
              ),
              br(),
              
              # Sample information with metadata details
              div(
                strong("Sample Assignment:", style = "color: #d32f2f;"),
                br(),
                span(paste0("Total samples in contrast: ", contrast$design_analysis$design_strategy$total_samples), 
                     style = "color: #1976d2; font-size: 11px;"),
                br(),
                span(paste0("Unused samples: ", contrast$design_analysis$design_strategy$unused_samples), 
                     style = "color: #ff9800; font-size: 11px;"),
                br(), br(),
                div(
                  style = "background-color: #e3f2fd; padding: 6px; border-radius: 3px; font-size: 11px;",
                  strong("Metadata Factor Created:", style = "color: #1565c0;"),
                  br(),
                  span("Column: ", style = "color: #1565c0;"),
                  code(contrast$design_analysis$design_strategy$factor_name, style = "font-size: 10px;"),
                  br(),
                  span("Values:", style = "color: #1565c0;"),
                  br(),
                  span(paste0("  • '", contrast$group1_name, "' for samples: ", 
                             paste(head(contrast$group1_samples, 3), collapse = ", "),
                             if(length(contrast$group1_samples) > 3) "..." else ""), 
                       style = "color: #1565c0; font-size: 10px;"),
                  br(),
                  span(paste0("  • '", contrast$group2_name, "' for samples: ", 
                             paste(head(contrast$group2_samples, 3), collapse = ", "),
                             if(length(contrast$group2_samples) > 3) "..." else ""), 
                       style = "color: #1565c0; font-size: 10px;"),
                  br(),
                  span(paste0("  • 'Unused' for remaining ", contrast$design_analysis$design_strategy$unused_samples, " samples"), 
                       style = "color: #1565c0; font-size: 10px;")
                )
              )
            )
          )
        ),
        column(3,
          div(
            style = "text-align: right;",
            actionButton(
              paste0("remove_contrast_", name),
              "Remove",
              icon = icon("trash"),
              class = "btn-outline-danger btn-sm"
            ),
            br(),
            # Per-contrast shrinkage indicator
            if (!is.null(values$deseq_results[[name]])) {
              si <- values$deseq_results[[name]]$shrinkage_info
              div(
                style = "background-color: #eef7ff; padding: 8px; border-radius: 4px; margin-top: 6px;",
                strong("Shrinkage: "),
                span(if (isTRUE(si$applied)) paste0("Applied (", si$method, ")") else "Not applied"),
                if (!isTRUE(si$applied) && !is.null(si$note)) span(paste0(" — ", si$note), style = "color:#666; font-size:11px;"),
                if (isTRUE(si$applied) && !is.null(si$coefficient)) div(paste("Coefficient:", si$coefficient), style = "font-size:11px; color:#555;")
              )
            }
          )
        )
      )
    )
  })
  
  # Add JavaScript to toggle chevron icon
  tagList(
    do.call(div, contrast_items),
    tags$script(HTML("\n      $(document).ready(function() {\n        $('[data-toggle=\"collapse\"]').on('click', function() {\n          var icon = $(this).find('i');\n          var text = $(this).find('span');\n          if ($(this).attr('aria-expanded') === 'true') {\n            icon.removeClass('fa-chevron-down').addClass('fa-chevron-right');\n            text.text(' Show Details');\n          } else {\n            icon.removeClass('fa-chevron-right').addClass('fa-chevron-down');\n            text.text(' Hide Details');\n          }\n        });\n        \n        $('.collapse').on('shown.bs.collapse', function() {\n          var toggle = $('[href=\"#' + $(this).attr('id') + '\"]');\n          toggle.find('i').removeClass('fa-chevron-right').addClass('fa-chevron-down');\n          toggle.find('span').text(' Hide Details');\n        });\n        \n        $('.collapse').on('hidden.bs.collapse', function() {\n          var toggle = $('[href=\"#' + $(this).attr('id') + '\"]');\n          toggle.find('i').removeClass('fa-chevron-down').addClass('fa-chevron-right');\n          toggle.find('span').text(' Show Details');\n        });\n      });\n    "))
  )
})

# Dynamic remove contrast observers
observe({
  req(values$created_contrasts)
  
  lapply(names(values$created_contrasts), function(name) {
    button_id <- paste0("remove_contrast_", name)
    
    observeEvent(input[[button_id]], {
      values$created_contrasts[[name]] <- NULL
      showNotification(paste("Removed contrast:", name), type = "message")
    })
  })
})

# Back to input button
observeEvent(input$back_to_input, {
  updateTabsetPanel(session, "main_tabs", selected = "tab_input")
})

# Design next button (conditional on having valid contrasts)
output$design_next_button <- renderUI({
  has_contrasts <- length(values$created_contrasts) > 0
  
  if (has_contrasts) {
    actionButton(
      "go_to_config",
      "Next: Analysis Configuration",
      icon = icon("arrow-right"),
      class = "btn-primary btn-lg"
    )
  } else {
    div(
      style = "color: #7F8C8D;",
      "At least one contrast required."
    )
  }
})

# Navigation to configuration tab
observeEvent(input$go_to_config, {
  updateTabsetPanel(session, "main_tabs", selected = "tab_config")
})

# =============================================================================
# CONTRAST EXPORT/IMPORT
# =============================================================================

# Export contrasts to CSV
output$export_contrasts <- downloadHandler(
  filename = function() {
    paste0("contrasts_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  },
  content = function(file) {
    tryCatch({
      # Check if contrasts exist
      if (is.null(values$created_contrasts) || length(values$created_contrasts) == 0) {
        showNotification(
          "No contrasts to export. Please create contrasts first.",
          type = "warning",
          duration = 5
        )
        return()
      }
      
      # Build dataframe with contrast information
      contrast_list <- lapply(names(values$created_contrasts), function(contrast_name) {
        contrast_info <- values$created_contrasts[[contrast_name]]
        
        # Debug: print contrast structure
        message(paste("Exporting contrast:", contrast_name))
        message(paste("  Keys in contrast:", paste(names(contrast_info), collapse = ", ")))
        
        # Handle both key formats (group1_samples and group1)
        g1_samples <- if (!is.null(contrast_info$group1_samples)) {
          contrast_info$group1_samples
        } else {
          contrast_info$group1
        }
        
        g2_samples <- if (!is.null(contrast_info$group2_samples)) {
          contrast_info$group2_samples
        } else {
          contrast_info$group2
        }
        
        message(paste("  Group1 samples:", paste(g1_samples, collapse = ", ")))
        message(paste("  Group2 samples:", paste(g2_samples, collapse = ", ")))
        
        # Get design info
        design_formula <- contrast_info$design_analysis$design_strategy$design_formula
        covariates <- contrast_info$design_analysis$design_strategy$covariates
        if (is.null(covariates)) covariates <- character(0)
        
        data.frame(
          contrast_name = contrast_name,
          group1_samples = paste(g1_samples, collapse = ","),
          group2_samples = paste(g2_samples, collapse = ","),
          design_formula = design_formula,
          covariates = if (length(covariates) > 0) paste(covariates, collapse = ",") else "",
          stringsAsFactors = FALSE
        )
      })
      
      contrast_df <- do.call(rbind, contrast_list)
      
      # Write to CSV
      write.csv(contrast_df, file, row.names = FALSE)
      
      message(paste("Exported", nrow(contrast_df), "contrasts to:", file))
      
      showNotification(
        paste0("Successfully exported ", nrow(contrast_df), " contrast(s)"),
        type = "message",
        duration = 3
      )
      
    }, error = function(e) {
      message(paste("Error exporting contrasts:", e$message))
      showNotification(
        paste("Error exporting contrasts:", e$message),
        type = "error",
        duration = 10
      )
    })
  }
)

# Import contrasts from CSV
observeEvent(input$import_contrasts_file, {
  req(input$import_contrasts_file)
  
  tryCatch({
    # Read the CSV file
    contrast_data <- read.csv(input$import_contrasts_file$datapath, stringsAsFactors = FALSE)
    
    message("Importing contrasts from file...")
    
    # Validate required columns
    required_cols <- c("contrast_name", "group1_samples", "group2_samples")
    if (!all(required_cols %in% colnames(contrast_data))) {
      showNotification(
        paste("Invalid CSV format. Required columns:", paste(required_cols, collapse = ", ")),
        type = "error",
        duration = 10
      )
      return()
    }
    
    # Get available samples from metadata
    available_samples <- rownames(values$meta_data)
    
    # Parse and validate contrasts
    imported_contrasts <- list()
    invalid_contrasts <- character(0)
    
    for (i in seq_len(nrow(contrast_data))) {
      contrast_name <- contrast_data$contrast_name[i]
      group1_samples <- strsplit(contrast_data$group1_samples[i], ",")[[1]]
      group2_samples <- strsplit(contrast_data$group2_samples[i], ",")[[1]]
      
      # Trim whitespace
      group1_samples <- trimws(group1_samples)
      group2_samples <- trimws(group2_samples)
      
      # Validate samples exist in metadata
      invalid_samples <- c(
        group1_samples[!group1_samples %in% available_samples],
        group2_samples[!group2_samples %in% available_samples]
      )
      
      if (length(invalid_samples) > 0) {
        invalid_contrasts <- c(invalid_contrasts, 
          paste0(contrast_name, " (invalid samples: ", paste(invalid_samples, collapse = ", "), ")")
        )
        next
      }
      
      # Check for sample overlap
      if (any(group1_samples %in% group2_samples)) {
        invalid_contrasts <- c(invalid_contrasts, 
          paste0(contrast_name, " (samples appear in both groups)")
        )
        next
      }
      
      # Extract group names from contrast name (e.g., "Treatment_vs_Control" -> "Treatment", "Control")
      # Try to split by common separators
      group_names <- strsplit(contrast_name, "_vs_|_VS_|-vs-")[[1]]
      if (length(group_names) == 2) {
        group1_name <- group_names[1]
        group2_name <- group_names[2]
      } else {
        group1_name <- "Group1"
        group2_name <- "Group2"
      }
      
      # Get design formula and covariates from CSV (if present)
      design_formula_str <- if ("design_formula" %in% names(contrast_data) && !is.na(contrast_data$design_formula[i]) && nzchar(contrast_data$design_formula[i])) {
        contrast_data$design_formula[i]
      } else {
        NULL
      }
      
      covariates_str <- if ("covariates" %in% names(contrast_data) && !is.na(contrast_data$covariates[i]) && nzchar(contrast_data$covariates[i])) {
        strsplit(contrast_data$covariates[i], ",")[[1]]
      } else {
        character(0)
      }
      
      # Determine factor name from formula or use default
      if (!is.null(design_formula_str)) {
        # Extract factor name from formula (last term)
        formula_terms <- strsplit(gsub("~\\s*", "", design_formula_str), "\\s*\\+\\s*")[[1]]
        factor_name <- trimws(formula_terms[length(formula_terms)])
      } else {
        factor_name <- "condition"
      }
      
      # Create enhanced metadata for this contrast
      enhanced_metadata <- values$meta_data
      contrast_factor <- character(nrow(enhanced_metadata))
      contrast_factor[rownames(enhanced_metadata) %in% group1_samples] <- group1_name
      contrast_factor[rownames(enhanced_metadata) %in% group2_samples] <- group2_name
      contrast_factor[!rownames(enhanced_metadata) %in% c(group1_samples, group2_samples)] <- "Unused"
      enhanced_metadata[[factor_name]] <- factor(contrast_factor, levels = c(group2_name, group1_name, "Unused"))
      
      # Reconstruct design formula if missing
      if (is.null(design_formula_str)) {
        design_formula_str <- paste0("~ ", factor_name)
      }
      
      # Store valid contrast with full structure matching manual creation
      imported_contrasts[[contrast_name]] <- list(
        name = contrast_name,
        group1_name = group1_name,
        group2_name = group2_name,
        group1_samples = group1_samples,
        group2_samples = group2_samples,
        design_analysis = list(
          design_strategy = list(
            approach = if (length(covariates_str) > 0) "user_defined_additive" else "user_defined",
            design_formula = design_formula_str,
            covariates = covariates_str,
            factor_name = factor_name,
            contrast_specification = c(factor_name, group1_name, group2_name),
            total_samples = length(c(group1_samples, group2_samples)),
            unused_samples = nrow(values$meta_data) - length(c(group1_samples, group2_samples))
          ),
          enhanced_metadata = enhanced_metadata
        ),
        enhanced_metadata = enhanced_metadata,
        creation_timestamp = Sys.time()
      )
      
      message(paste("Imported contrast:", contrast_name, 
                    "| Group1:", group1_name, paste0("(", length(group1_samples), " samples)"),
                    "| Group2:", group2_name, paste0("(", length(group2_samples), " samples)")))
    }
    
    # Update values
    if (length(imported_contrasts) > 0) {
      # Merge with existing contrasts (new ones override)
      if (is.null(values$created_contrasts)) {
        values$created_contrasts <- list()
      }
      
      for (name in names(imported_contrasts)) {
        values$created_contrasts[[name]] <- imported_contrasts[[name]]
      }
      
      # Set values$metadata to enable proceeding to analysis
      # (This is required by Tab 3 analysis configuration)
      if (is.null(values$metadata)) {
        values$metadata <- values$meta_data
        message("Set values$metadata from values$meta_data for analysis configuration")
      }
      
      # Show success notification
      success_msg <- paste0("Successfully imported ", length(imported_contrasts), " contrast(s)")
      if (length(invalid_contrasts) > 0) {
        success_msg <- paste0(success_msg, ". ", length(invalid_contrasts), " contrast(s) skipped due to errors.")
      }
      
      showNotification(
        success_msg,
        type = "message",
        duration = 5
      )
      
      # Show details about invalid contrasts if any
      if (length(invalid_contrasts) > 0) {
        showNotification(
          HTML(paste0("Skipped contrasts:<br>", paste(invalid_contrasts, collapse = "<br>"))),
          type = "warning",
          duration = 10
        )
      }
      
    } else {
      showNotification(
        "No valid contrasts found in the file. Check sample names match your data.",
        type = "error",
        duration = 10
      )
    }
    
  }, error = function(e) {
    message(paste("Error importing contrasts:", e$message))
    showNotification(
      paste("Error importing contrasts:", e$message),
      type = "error",
      duration = 10
    )
  })
})

