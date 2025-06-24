# Test App Startup Script
# Author: Eren Ada, PhD
# Quick test to verify the app can start without errors

cat("Testing RNA-seq DEG Analysis App startup...\n\n")

# Test 1: Package availability
cat("1. Testing package availability...\n")
tryCatch({
  source("check_packages.R")
  cat("✓ Package check completed\n\n")
}, error = function(e) {
  cat("✗ Package check failed:", e$message, "\n\n")
  stop("Package check failed")
})

# Test 2: App loading
cat("2. Testing app component loading...\n")
tryCatch({
  source("app.R")
  cat("✓ App loaded successfully\n\n")
}, error = function(e) {
  cat("✗ App loading failed:", e$message, "\n\n")
  stop("App loading failed")
})

# Test 3: Validation functions
cat("3. Testing validation functions...\n")
tryCatch({
  # Test with sample data
  test_counts <- data.frame(
    Sample1 = c(100, 50, 200),
    Sample2 = c(150, 75, 180),
    Sample3 = c(80, 45, 220)
  )
  rownames(test_counts) <- c("Gene1", "Gene2", "Gene3")
  
  test_meta <- data.frame(
    Condition = c("Control", "Treatment", "Control")
  )
  rownames(test_meta) <- c("Sample1", "Sample2", "Sample3")
  
  result <- validate_input_data(test_counts, test_meta)
  if (result$valid) {
    cat("✓ Validation functions working correctly\n\n")
  } else {
    cat("✗ Validation failed unexpectedly\n\n")
  }
}, error = function(e) {
  cat("✗ Validation function test failed:", e$message, "\n\n")
})

# Summary
cat(paste(rep("=", 50), collapse = ""), "\n")
cat("🎉 APP STARTUP TEST COMPLETE\n")
cat(paste(rep("=", 50), collapse = ""), "\n")
cat("\nThe app is ready to run! Use one of these methods:\n")
cat("• shiny::runApp()\n")
cat("• shiny::runApp(port = 3838)\n")
cat("• shiny::runApp(launch.browser = TRUE)\n\n")

cat("To access the app:\n")
cat("• Local: http://localhost:3838\n")
cat("• Or open in RStudio by clicking 'Run App'\n\n") 