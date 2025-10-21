# Server logic for Tab 1: Data Input
# Author: Eren Ada, PhD
# 
# This file contains all server-side logic for data upload and validation

# =============================================================================
# TAB 1: DATA INPUT & VALIDATION
# =============================================================================

# =============================================================================
# PROJECT SETTINGS: Organism & ID Type
# =============================================================================

# Store organism selection in reactive values
observeEvent(input$organism, {
  req(input$organism)
  
  # Store organism selection
  values$organism <- input$organism
  
  # Load organism database conditionally
  tryCatch({
    # Show loading notification
    notification_id <- showNotification(
      paste("Loading", input$organism, "genome annotation database..."),
      duration = NULL,
      closeButton = FALSE,
      type = "message"
    )
    
    # Load the appropriate organism database
    orgdb <- get_orgdb(input$organism)
    
    # Store the loaded database
    values$orgdb <- orgdb
    
    # Remove loading notification
    removeNotification(notification_id)
    
    # Show success notification
    showNotification(
      paste(input$organism, "genome annotation database loaded successfully"),
      type = "message",
      duration = 3
    )
    
    message(paste("Organism database loaded:", input$organism))
    
  }, error = function(e) {
    showNotification(
      paste("Error loading organism database:", e$message),
      type = "error",
      duration = 10
    )
    message(paste("Error loading organism database:", e$message))
  })
  
  # Invalidate enrichment cache when organism changes
  if (exists("enrichment_results", where = values)) {
    values$enrichment_results <- list()
    message("Enrichment cache invalidated due to organism change")
  }
})

# Store ID type selection in reactive values
observeEvent(input$id_type, {
  req(input$id_type)
  
  # Store ID type selection
  values$id_type <- input$id_type
  
  message(paste("ID type selected:", input$id_type))
  
  # Invalidate enrichment cache when ID type changes
  if (exists("enrichment_results", where = values)) {
    values$enrichment_results <- list()
    message("Enrichment cache invalidated due to ID type change")
  }
})

# =============================================================================
# FILE UPLOAD HANDLING
# =============================================================================

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

