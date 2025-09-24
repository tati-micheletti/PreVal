#' @title Generate a Definitive Experimental Design Plan for Forecasting
#'
#' @description
#' This function creates a comprehensive, machine-readable data.table that outlines
#' all combinations for a forecasting experiment. It generates scenarios for both
#' predictive validation (PreVal) and traditional cross-validation (CrossVal),
#' including tests of CV-developed models on future data.
#'
#' @param startYear An integer specifying the first year of available data.
#'   Defaults to `2004`.
#' @param endYear An integer specifying the last year of available data.
#'   Defaults to `2019`.
#' @param numberOfCovariatesList A numeric vector listing the different numbers
#'   of covariates to be tested in the models. Defaults to `c(2, 4, 8, 12, 20)`.
#' @param outputPath A character string specifying the file path to save the
#'   resulting CSV file. Defaults to `"experimentalDesign.csv"`.
#'
#' @return
#' A data.table containing the full experimental plan. Each row represents a
#' unique model run to be performed. The function also saves this table as a CSV
#' file to the specified `outputPath`.
#'
#' @examples
#' # Run with default settings and save to "experimentalDesign.csv"
#' # myPlan <- generateExperimentPlan()
#'
#' # Run with a shorter time period and fewer covariate options
#' # customPlan <- generateExperimentPlan(startYear = 2010, endYear = 2018,
#' #                                      numberOfCovariatesList = c(5, 10, 15))
#'
#' # Save the output to a different file
#' # generateExperimentPlan(outputPath = "my_custom_plan.csv")
#'
generateExperimentPlan <- function(startYear = 2004,
                                   endYear = 2025,
                                   numberOfCovariatesList = c(2, 4, 8, 12, 20),
                                   outputPath = "data/experimentalDesign.csv") {
  
  Require::Require("data.table")
  
  # --- 1. Initial Setup ---
  allYears <- startYear:endYear
  resultsList <- list()
  
  # --- 2. Generate All Combinations Programmatically ---
  
  # -- Part A: Generate "PreVal" Rows --
  for (numCov in numberOfCovariatesList) {
    for (trainWindowSize in 1:(length(allYears) - 1)) {
      for (startYearTrain in allYears) {
        endYearTrain <- startYearTrain + trainWindowSize - 1
        if (endYearTrain >= endYear) break
        
        devId <- paste("PreVal_Dev", startYearTrain, endYearTrain, sep = "_")
        
        for (predictionYear in (endYearTrain + 1):endYear) {
          resultsList[[length(resultsList) + 1]] <- data.table(
            typeValidation = "PreVal",
            testType = "Future",
            developmentId = devId,
            numberOfCovariates = numCov,
            trainStartYear = startYearTrain,
            trainEndYear = endYearTrain,
            testStartYear = predictionYear,
            testEndYear = predictionYear
          )
        }
      }
    }
  }
  
  # -- Part B: Generate "CrossVal" Rows --
  for (numCov in numberOfCovariatesList) {
    for (blockSize in 1:length(allYears)) {
      for (startYearPool in allYears) {
        endYearPool <- startYearPool + blockSize - 1
        if (endYearPool > endYear) break
        
        devId <- paste("CV_Dev", startYearPool, endYearPool, sep = "_")
        
        # B.1: The single row for the INTERNAL k-fold CV process.
        resultsList[[length(resultsList) + 1]] <- data.table(
          typeValidation = "CrossVal",
          testType = "Internal",
          developmentId = devId,
          numberOfCovariates = numCov,
          trainStartYear = startYearPool,
          trainEndYear = endYearPool,
          testStartYear = startYearPool,
          testEndYear = endYearPool
        )
        
        # B.2: The rows for testing the FINAL CV model against the FUTURE.
        if (endYearPool < endYear) {
          for (predictionYear in (endYearPool + 1):endYear) {
            resultsList[[length(resultsList) + 1]] <- data.table(
              typeValidation = "CrossVal",
              testType = "Future",
              developmentId = devId,
              numberOfCovariates = numCov,
              trainStartYear = startYearPool,
              trainEndYear = endYearPool,
              testStartYear = predictionYear,
              testEndYear = predictionYear
            )
          }
        }
      }
    }
  }
  
  # Combine everything into one master table
  experimentPlan <- rbindlist(resultsList)
  
  # --- 3. Add Helper Columns for Analysis ---
  experimentPlan[, totalTimeSpan := (testEndYear - trainStartYear) + 1]
  experimentPlan[, forecastHorizon := testStartYear - trainEndYear]
  experimentPlan[testType == "Internal", forecastHorizon := 0]
  
  # --- 4. Finalize, Save, and Return ---
  
  # Print summary statistics to the console
  cat(sprintf("Successfully generated the experiment plan with %d total rows.\n", nrow(experimentPlan)))
  cat(sprintf("- PreVal rows: %d\n", experimentPlan[typeValidation == "PreVal", .N]))
  cat(sprintf("- CrossVal rows: %d\n\n", experimentPlan[typeValidation == "CrossVal", .N]))
  
  # Save the plan to the specified file path
  fwrite(experimentPlan, outputPath)
  cat(sprintf("Experiment plan saved to '%s'.\n", outputPath))
  
  # Return the data.table object for use in the R session
  return(experimentPlan)
}
