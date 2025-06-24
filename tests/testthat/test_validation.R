# Test file for validation utilities
# Author: Eren Ada, PhD

library(testthat)

# Source the validation functions
source("../../R/utils_validation.R")

test_that("validate_count_matrix works correctly", {
  # Test valid count matrix
  valid_counts <- data.frame(
    Sample1 = c(100, 50, 200),
    Sample2 = c(150, 75, 180),
    Sample3 = c(80, 45, 220)
  )
  rownames(valid_counts) <- c("Gene1", "Gene2", "Gene3")
  
  result <- validate_count_matrix(valid_counts)
  expect_true(result$valid)
  expect_equal(length(result$errors), 0)
  
  # Test invalid count matrix (negative values)
  invalid_counts <- valid_counts
  invalid_counts[1, 1] <- -10
  
  result <- validate_count_matrix(invalid_counts)
  expect_false(result$valid)
  expect_true(any(grepl("negative values", result$errors)))
})

test_that("validate_metadata works correctly", {
  # Test valid metadata
  valid_meta <- data.frame(
    Condition = c("Control", "Treatment", "Control"),
    Batch = c(1, 1, 2)
  )
  rownames(valid_meta) <- c("Sample1", "Sample2", "Sample3")
  
  result <- validate_metadata(valid_meta)
  expect_true(result$valid)
  expect_equal(length(result$errors), 0)
})

test_that("validate_sample_matching works correctly", {
  # Test matching samples
  counts <- data.frame(
    Sample1 = c(100, 50),
    Sample2 = c(150, 75),
    Sample3 = c(80, 45)
  )
  
  meta <- data.frame(
    Condition = c("Control", "Treatment", "Control")
  )
  rownames(meta) <- c("Sample1", "Sample2", "Sample3")
  
  result <- validate_sample_matching(counts, meta)
  expect_true(result$valid)
  expect_equal(result$n_matched, 3)
}) 