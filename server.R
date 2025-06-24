# Server for RNA-seq DEG Analysis App
# Author: Eren Ada, PhD

# All packages and utilities are loaded in global.R

server <- function(input, output, session) {
  
  # Reactive values to store data
  values <- reactiveValues(
    count_data = NULL,
    meta_data = NULL,
    validation_status = NULL,
    design_factors = NULL,
    design_formula = NULL,
    reference_levels = list(),
    contrasts = list()
  )
  
  # File upload handling for count matrix
  observeEvent(input$count_file, {
    req(input$count_file)
    
    tryCatch({
      ext <- tools::file_ext(input$count_file$datapath)
      
      if (ext == "csv") {
        values$count_data <- read.csv(
          input$count_file$datapath,
          sep = input$count_sep,
          header = input$count_header,
          row.names = if(input$count_rownames) 1 else NULL,
          stringsAsFactors = FALSE
        )
      } else if (ext %in% c("tsv", "txt")) {
        values$count_data <- read.delim(
          input$count_file$datapath,
          sep = input$count_sep,
          header = input$count_header,
          row.names = if(input$count_rownames) 1 else NULL,
          stringsAsFactors = FALSE
        )
      }
      
      # Validate count data
      if (!is.null(values$count_data)) {
        validate_data()
      }
      
    }, error = function(e) {
      showNotification(
        paste("Error reading count file:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # File upload handling for metadata
  observeEvent(input$meta_file, {
    req(input$meta_file)
    
    tryCatch({
      ext <- tools::file_ext(input$meta_file$datapath)
      
      if (ext == "csv") {
        values$meta_data <- read.csv(
          input$meta_file$datapath,
          sep = input$meta_sep,
          header = input$meta_header,
          row.names = if(input$meta_rownames) 1 else NULL,
          stringsAsFactors = FALSE
        )
      } else if (ext %in% c("tsv", "txt")) {
        values$meta_data <- read.delim(
          input$meta_file$datapath,
          sep = input$meta_sep,
          header = input$meta_header,
          row.names = if(input$meta_rownames) 1 else NULL,
          stringsAsFactors = FALSE
        )
      }
      
      # Validate data
      if (!is.null(values$meta_data)) {
        validate_data()
      }
      
    }, error = function(e) {
      showNotification(
        paste("Error reading metadata file:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # Data validation function
  validate_data <- function() {
    if (!is.null(values$count_data) && !is.null(values$meta_data)) {
      values$validation_status <- validate_input_data(values$count_data, values$meta_data)
    }
  }
  
  # Count matrix preview
  output$count_preview <- DT::renderDataTable({
    req(values$count_data)
    
    DT::datatable(
      values$count_data,
      options = list(
        scrollX = TRUE,
        scrollY = "400px",
        pageLength = 10,
        dom = 'frtip'
      ),
      rownames = TRUE
    )
  })
  
  # Metadata preview
  output$meta_preview <- DT::renderDataTable({
    req(values$meta_data)
    
    DT::datatable(
      values$meta_data,
      options = list(
        scrollX = TRUE,
        scrollY = "400px",
        pageLength = 10,
        dom = 'frtip'
      ),
      rownames = TRUE
    )
  })
  
  # Data summary
  output$data_summary <- renderText({
    if (is.null(values$count_data) && is.null(values$meta_data)) {
      return("No data uploaded yet.")
    }
    
    summary_text <- ""
    
    if (!is.null(values$count_data)) {
      summary_text <- paste0(summary_text,
        "Count Matrix:\n",
        "- Dimensions: ", nrow(values$count_data), " genes × ", ncol(values$count_data), " samples\n",
        "- Sample names: ", paste(head(colnames(values$count_data), 3), collapse = ", "), 
        if(ncol(values$count_data) > 3) "..." else "", "\n\n"
      )
    }
    
    if (!is.null(values$meta_data)) {
      summary_text <- paste0(summary_text,
        "Metadata:\n",
        "- Dimensions: ", nrow(values$meta_data), " samples × ", ncol(values$meta_data), " variables\n",
        "- Variables: ", paste(head(colnames(values$meta_data), 3), collapse = ", "),
        if(ncol(values$meta_data) > 3) "..." else "", "\n\n"
      )
    }
    
    return(summary_text)
  })
  
  # Validation status output
  output$validation_status <- renderUI({
    if (is.null(values$validation_status)) {
      return(div(
        class = "alert alert-info",
        icon("info-circle"),
        " Upload both count matrix and metadata files to validate data compatibility."
      ))
    }
    
    if (values$validation_status$valid) {
      div(
        class = "alert alert-success",
        icon("check-circle"),
        " Data validation passed! You can proceed to the next step.",
        br(),
        "Sample matching: ", values$validation_status$n_matched, " samples found in both files."
      )
    } else {
      div(
        class = "alert alert-danger",
        icon("exclamation-triangle"),
        " Data validation failed:",
        br(),
        tags$ul(
          lapply(values$validation_status$errors, function(error) {
            tags$li(error)
          })
        )
      )
    }
  })
  
  # Next button output (conditional on validation)
  output$next_button <- renderUI({
    if (!is.null(values$validation_status) && values$validation_status$valid) {
      div(
        style = "text-align: center; margin: 20px 0;",
        actionButton(
          "go_to_design",
          "Next: Experimental Design",
          icon = icon("arrow-right"),
          class = "btn-primary btn-lg",
          style = "font-size: 16px; padding: 12px 24px;"
        )
      )
    }
  })
  
  # Navigation button click handler
  observeEvent(input$go_to_design, {
    updateTabsetPanel(session, "main_tabs", selected = "tab_design")
  })
  
  # ===== EXPERIMENTAL DESIGN TAB LOGIC =====
  
  # Initialize sample grouping
  values$group1_samples <- character(0)
  values$group2_samples <- character(0)
  values$available_samples <- character(0)
  values$created_contrasts <- list()
  
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
  
  # Initialize filtering
  values$filtered_samples <- character(0)
  values$selected_filters <- list()
  values$filter_trigger <- 0
  
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
  
  # Contrast preview
  output$contrast_preview <- renderUI({
    group1_name <- if(is.null(input$group1_name) || input$group1_name == "") "Group1" else input$group1_name
    group2_name <- if(is.null(input$group2_name) || input$group2_name == "") "Group2" else input$group2_name
    
    if (length(values$group1_samples) == 0 && length(values$group2_samples) == 0) {
      return(p("No samples assigned to groups yet.", style = "color: #7F8C8D;"))
    }
    
    # Show design preview if both groups have samples
    if (length(values$group1_samples) > 0 && length(values$group2_samples) > 0) {
      # Get the factor name for preview
      preview_factor_name <- if(is.null(input$contrast_factor_name) || input$contrast_factor_name == "") {
        "contrast_group"
      } else {
        gsub("[^A-Za-z0-9_]", "_", input$contrast_factor_name)
      }
      
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
          span("User-defined groups"),
          br(),
          span("Formula: ", style = "font-weight: bold;"),
          code(paste0("~ ", preview_factor_name), style = "background: #fff; padding: 2px;"),
          br(),
          span("Contrast: ", style = "font-weight: bold;"),
          span(paste0(group1_name, " vs ", group2_name))
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
    can_create <- length(values$group1_samples) > 0 && length(values$group2_samples) > 0 &&
                 !is.null(input$group1_name) && input$group1_name != "" &&
                 !is.null(input$group2_name) && input$group2_name != ""
    
    if (can_create) {
      shinyjs::enable("create_contrast")
    } else {
      shinyjs::disable("create_contrast")
    }
  })
  
  # Simple user-defined grouping approach
  create_contrast_design <- function(group1_samples, group2_samples, group1_name, group2_name, factor_name = "contrast_group") {
    req(values$meta_data)
    
    # Validate and clean factor name
    factor_name <- gsub("[^A-Za-z0-9_]", "_", factor_name)  # Replace invalid characters
    if (factor_name == "" || is.na(factor_name)) factor_name <- "contrast_group"
    if (!grepl("^[A-Za-z]", factor_name)) factor_name <- paste0("f_", factor_name)  # Ensure starts with letter
    
    # Create enhanced metadata with contrast grouping
    enhanced_metadata <- values$meta_data
    
    # Add custom factor based on user selections
    contrast_factor <- character(nrow(enhanced_metadata))
    contrast_factor[rownames(enhanced_metadata) %in% group1_samples] <- group1_name
    contrast_factor[rownames(enhanced_metadata) %in% group2_samples] <- group2_name
    contrast_factor[!rownames(enhanced_metadata) %in% c(group1_samples, group2_samples)] <- "Unused"
    
    enhanced_metadata[[factor_name]] <- factor(contrast_factor, levels = c(group2_name, group1_name, "Unused"))
    
    # Simple, reliable design strategy
    design_info <- list(
      approach = "user_defined",
      design_formula = paste0("~ ", factor_name),
      contrast_specification = c(factor_name, group1_name, group2_name),
      primary_factor = factor_name,
      factor_name = factor_name,
      group1_name = group1_name,
      group2_name = group2_name,
      total_samples = length(c(group1_samples, group2_samples)),
      unused_samples = sum(contrast_factor == "Unused")
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
    
    contrast_name <- paste0(input$group1_name, "_vs_", input$group2_name)
    
    # Check for duplicate names
    if (contrast_name %in% names(values$created_contrasts)) {
      showNotification("Contrast name already exists. Please use different group names.", type = "warning")
      return()
    }
    
    # Create simple user-defined contrast design
    factor_name <- if(is.null(input$contrast_factor_name) || input$contrast_factor_name == "") {
      "contrast_group"
    } else {
      input$contrast_factor_name
    }
    
    contrast_design <- create_contrast_design(
      values$group1_samples, 
      values$group2_samples,
      input$group1_name,
      input$group2_name,
      factor_name
    )
    
    # Create contrast object
    values$created_contrasts[[contrast_name]] <- list(
      name = contrast_name,
      group1_name = input$group1_name,
      group2_name = input$group2_name,
      group1_samples = values$group1_samples,
      group2_samples = values$group2_samples,
      design_analysis = contrast_design,
      enhanced_metadata = contrast_design$enhanced_metadata,
      creation_timestamp = Sys.time()
    )
    
    showNotification("Contrast created successfully with experimental design analysis!", type = "message")
    
    # Clear groups
    values$available_samples <- c(values$available_samples, values$group1_samples, values$group2_samples)
    values$group1_samples <- character(0)
    values$group2_samples <- character(0)
    
    # Reset group names
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
              strong("DESeq2 Design:", style = "color: #1976d2;"),
              br(), br(),
              
              # Design approach
              div(
                strong("Approach: ", style = "color: #d32f2f;"),
                span("User-defined contrast groups", style = "color: #388e3c;")
              ),
              br(),
              
              # Design formula
              div(
                strong("Design Formula:", style = "color: #d32f2f;"),
                br(),
                code(paste0("design = ", contrast$design_analysis$design_strategy$design_formula))
              ),
              br(),
              
              # Results command with detailed explanation
              div(
                strong("Results Command:", style = "color: #d32f2f;"),
                br(),
                code(paste0("results(dds, contrast = c('", 
                           paste(contrast$design_analysis$design_strategy$contrast_specification, collapse = "', '"), 
                           "'))")),
                br(),
                div(
                  style = "background-color: #fff3cd; padding: 6px; border-radius: 3px; margin-top: 5px; font-size: 11px;",
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
          ),
          column(3,
            div(
              style = "text-align: right;",
              actionButton(
                paste0("remove_contrast_", name),
                "Remove",
                icon = icon("trash"),
                class = "btn-outline-danger btn-sm"
              )
            )
          )
        )
      )
    })
    
    do.call(div, contrast_items)
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
} 