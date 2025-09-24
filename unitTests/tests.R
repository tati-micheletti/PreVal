# Tests for Experimental Design Function

Require::Require("testthat")
Require::Require("data.table")
source("R/experimentPlan.R") # Assumes the function is in this file

# --- Test Suite for generateExperimentPlan() ---

# Generate the default plan once to use in multiple tests for efficiency
defaultPlan <- generateExperimentPlan(outputPath = tempfile()) # Use tempfile to avoid clutter

# Test 1: Basic Structure and Integrity
test_that("Function returns a valid data.table with correct structure", {
  
  expect_s3_class(defaultPlan, "data.table")
  
  expectedCols <- c(
    "typeValidation", "testType", "developmentId", "numberOfCovariates",
    "trainStartYear", "trainEndYear", "testStartYear", "testEndYear",
    "totalTimeSpan", "forecastHorizon"
  )
  expect_named(defaultPlan, expectedCols, ignore.order = TRUE)
  
  # None of the core planning columns should ever contain NAs
  expect_true(all(!is.na(defaultPlan)))
  
})

# Test 2: Argument Handling and Customization
test_that("Function arguments correctly modify the output plan", {
  
  customPlan <- generateExperimentPlan(
    startYear = 2015,
    endYear = 2018,
    numberOfCovariatesList = c(5, 10),
    outputPath = tempfile()
  )
  
  expect_equal(min(customPlan$trainStartYear), 2015)
  expect_equal(max(customPlan$testEndYear), 2018)
  expect_equal(sort(unique(customPlan$numberOfCovariates)), c(5, 10))
  
})

# Test 3: Core Logic for "PreVal" Rows
test_that("PreVal and crossVal rows adhere to all logical rules", {
  
  prevalRows <- defaultPlan[typeValidation == "PreVal"]
  allFuture <- defaultPlan[testType == "Future"]
  allInternal <- defaultPlan[testType == "Internal"]
  
  # Rule: All PreVal rows must be of testType "Future".
  expect_true(all(prevalRows$testType == "Future"))
  
  # Rule: The test year must always be strictly after the training period.
  expect_true(all(prevalRows$testStartYear > prevalRows$trainEndYear))
  
  # Rule: For all Future tests, the test period must be a single year.
  expect_true(all(allFuture$testStartYear == allFuture$testEndYear))
  
  # Rule: For all Internal tests, the test period must match the test period.
  expect_true(all(allInternal$trainStartYear == allInternal$testStartYear,
                  allInternal$trainEndYear == allInternal$testEndYear,
                  allInternal$forecastHorizon == 0))
  # Rule: all 
  expect_true(all(allInternal$typeValidation == "CrossVal"))
})

# Test 4: Core Logic for "CrossVal" Rows (Internal Tests)
test_that("CrossVal 'Internal' rows adhere to all logical rules", {
  
  cvInternalRows <- defaultPlan[typeValidation == "CrossVal" & testType == "Internal"]
  
  # Rule: For internal tests, the test period must exactly match the training period.
  expect_true(all(cvInternalRows$trainStartYear == cvInternalRows$testStartYear))
  expect_true(all(cvInternalRows$trainEndYear == cvInternalRows$testEndYear))
  
})

# Test 5: Core Logic for "CrossVal" Rows (Future Tests)
test_that("CrossVal 'Future' rows adhere to all logical rules", {
  
  cvFutureRows <- defaultPlan[typeValidation == "CrossVal" & testType == "Future"]
  
  # Rule: The test year must always be strictly after the training period.
  expect_true(all(cvFutureRows$testStartYear > cvFutureRows$trainEndYear))
  
  # Rule: For all Future tests, the test period must be a single year.
  expect_true(all(cvFutureRows$testStartYear == cvFutureRows$testEndYear))
  
})

# Test 6: Helper Column Calculation Accuracy
test_that("Helper columns 'totalTimeSpan' and 'forecastHorizon' are calculated correctly", {
  
  # Rule: totalTimeSpan should always equal (testEndYear - trainStartYear) + 1.
  expect_equal(defaultPlan$totalTimeSpan, (defaultPlan$testEndYear - defaultPlan$trainStartYear) + 1)
  
  # Rule: forecastHorizon must be 0 for all Internal tests.
  internalRows <- defaultPlan[testType == "Internal"]
  expect_true(all(internalRows$forecastHorizon == 0))
  
  # Rule: forecastHorizon must be (testStartYear - trainEndYear) for all Future tests.
  futureRows <- defaultPlan[testType == "Future"]
  expect_equal(futureRows$forecastHorizon, (futureRows$testStartYear - futureRows$trainEndYear))
  expect_true(all(futureRows$forecastHorizon >= 1))
  
})

# Test 7: File Output
test_that("Function correctly writes a file to the specified path", {
  
  tempPath <- tempfile(fileext = ".csv")
  
  # Suppress console output from the function for a clean test run
  suppressMessages({
    generateExperimentPlan(outputPath = tempPath)
  })
  
  expect_true(file.exists(tempPath))
  
  # Clean up the temporary file
  unlink(tempPath)
  
})
