# EXAMPLE CODE FOR COMPARISON...

Require::Require("data.table")
Require::Require("ggplot2")
Require::Require("gridExtra")
Require::Require("ggridges")
Require::Require("future")
Require::Require("future.apply")

fls213 <- list.files("G:/My Drive/Postdoc DE-TUD/DFG PreVal Project/Results/213_09MAR26", full.names = TRUE)
fls68 <- list.files("G:/My Drive/Postdoc DE-TUD/DFG PreVal Project/Results/68_09MAR26", full.names = TRUE)
fls200 <- list.files("G:/My Drive/Postdoc DE-TUD/DFG PreVal Project/Results/GPU_09MAR26", full.names = TRUE)

allFls <- c(fls213, fls68, fls200)
n <- length(allFls)
last_printed <- -1

rawLosses <- rbindlist(lapply(seq_along(allFls), function(indx){
  pct <- floor(indx / n * 100)
  if (pct %% 5 == 0 && pct != last_printed) {
    cat(sprintf("%d%% ready (%d/%d files)\n", pct, indx, n))
    last_printed <<- pct
  }
  vals <- readRDS(allFls[indx])
  groupId <- sub("_[^_]+_rawLosses\\.rds$", "", basename(allFls[indx]))
  toDivide <- sub("_rawLosses.rds$", "", basename(allFls[indx]))
  parts <- tstrsplit(
    toDivide,
    "_"
  )
  if (parts[[2]] == "Inf")
    parts[[2]] <- 30
  numCov <- as.integer(parts[[2]])
  startTrain <- as.integer(parts[[3]])
  endTrainWindow <- as.integer(parts[[4]])
  predictionYear <- as.integer(parts[[5]])
  validationType <- parts[[6]]
  
  dt <- data.table(
    groupId = groupId,
    numberOfCovariates = numCov,
    trainStartYear = startTrain,
    trainEndWindow = endTrainWindow,
    predictionYear = predictionYear,
    typeValidation = validationType,
    loss = vals
  )
  return(dt)
  
}), use.names = TRUE)

experimentPlan <- fread("G:/My Drive/Postdoc DE-TUD/DFG PreVal Project/Results/experimentalDesignFinal.csv")
# In experimentPlan: trainStartYear 2007 ==> trainStartYear 2008
# In experimentPlan: numberOfCovariates Inf ==> trainStartYear 30

experimentPlan[numberOfCovariates == Inf, numberOfCovariates := 30]
experimentPlan[trainStartYear == 2007, trainStartYear := 2008]
experimentPlan[trainEndYear == 2007, trainEndYear := 2008]
experimentPlan[trainEndYear == 2007, trainEndYear := 2008]
experimentPlan[valStartYear == 2007, valStartYear := 2008]
experimentPlan[valEndYear == 2007, valEndYear := 2008]
experimentPlan[testStartYear == 2007, testStartYear := 2008]
experimentPlan[testEndYear == 2007, testEndYear := 2008]

pall_Data <- merge(rawLosses, experimentPlan, by = c("groupId", "typeValidation", "numberOfCovariates", "trainStartYear"), all = TRUE)
# Combine the rawLosses with the experimentPlan for the number of sample points! This may also tell us a lot about the results
names(pall_Data)[names(pall_Data) == "typeValidation"] <- "Scenario"

pall_Data[, Scenario := factor(Scenario, 
                               levels = c("Internal", "FutureTainted", "FutureUnseen"))]
pall_Data[, Complexity := paste0("Covariates: ", numberOfCovariates)]
pall_Data[, Complexity := factor(Complexity, 
                                 levels = c("Covariates: 2", "Covariates: 5", 
                                            "Covariates: 10", "Covariates: 30"))]
# 1. Create Summary Table
# We calculate the Mean and SD of the Loss for every triplet
summaryDt <- pall_Data[, .(
  meanLoss = mean(loss),
  sdLoss = sd(loss),
  # We keep these columns as they are constant within a Group/Scenario
  ForecastHorizon = first(forecastHorizon),
  Complexity = first(Complexity)
), by = .(groupId, Scenario)]


# PLOT 5: The Ultimate PLOT == Density of loss for each caribou decision for all scenarios
# 1. Prepare Data and Define Horizon Bins
plotDataAll <- copy(pall_Data)

# Create the specific Bins you requested
plotDataAll[forecastHorizon %in% 2:3, HorizonBin := "Horizon: 2-3 Years"]
plotDataAll[forecastHorizon %in% 4:6, HorizonBin := "Horizon: 4-6 Years"]
plotDataAll[forecastHorizon %in% 7:8, HorizonBin := "Horizon: 7-8 Years"]
plotDataAll[forecastHorizon %in% 9:10, HorizonBin := "Horizon: 9-10 Years"]

# Filter to keep only the requested range
dataFuture <- plotDataAll[Scenario != "Internal" & !is.na(HorizonBin)]
dataInternal <- plotDataAll[Scenario == "Internal"]

# 2. Project Internal Baseline into the Bins
# We link the internal baseline to the bins via the Group ID
internalTemplate <- unique(dataFuture[, .(groupId, HorizonBin, Complexity)])

dataInternalProjected <- merge(internalTemplate, 
                               dataInternal[, .(groupId, loss, Scenario)], 
                               by = "groupId", 
                               allow.cartesian = TRUE)

# Combine for plotting
plotDataBinned <- rbind(dataFuture[, .(groupId, Scenario, Complexity, HorizonBin, loss)],
                        dataInternalProjected)

# Factor Ordering
plotDataBinned[, Scenario := factor(Scenario, levels = c("Internal", "FutureTainted", "FutureUnseen"))]
plotDataBinned[, Complexity := factor(Complexity, levels = c("Covariates: 2", "Covariates: 5", "Covariates: 10", "Covariates: 30"))]
plotDataBinned[, HorizonBin := factor(HorizonBin, levels = c("Horizon: 2-3 Years", "Horizon: 4-6 Years", "Horizon: 7-8 Years", "Horizon: 9-10 Years"))]

# 3. Calculate Means and Staggered Annotation Positions
meansDt <- plotDataBinned[, .(mu = mean(loss, na.rm = TRUE)), by = .(Scenario, Complexity, HorizonBin)]
meansDt[Scenario == "Internal", vPos := 1.5]
meansDt[Scenario == "FutureTainted", vPos := 3.5]
meansDt[Scenario == "FutureUnseen", vPos := 5.5]

# plotDataBinned[Scenario == FutureUnseen, Scenario := "Forecast (unseen) with Predictive Validation"]
# plotDataBinned[Scenario == Internal, Scenario := "Forecast (seen) with Cross-Validation"]
# plotDataBinned[Scenario == FutureTainted, Scenario := "Forecast (unseen) with Cross-Validation"]

# 4. Create the Grid Plot
pFinalGridBinned <- ggplot(plotDataBinned, aes(x = loss)) +
  # Filled Density Forms
  geom_density(aes(fill = Scenario, color = Scenario), alpha = 0.3, linewidth = 0.5) +
  # Scenario Mean Lines
  geom_vline(data = meansDt, aes(xintercept = mu, color = Scenario), 
             linetype = "dashed", linewidth = 0.7) +
  # # Staggered Annotations (to prevent overlap)
  # geom_text(data = meansDt, 
  #           aes(x = mu, y = Inf, label = round(mu, 3), color = Scenario, vjust = vPos),
  #           size = 3, fontface = "bold") +
  # Random Baseline Reference (2.40)
  geom_vline(xintercept = 2.3979, linetype = "dotted", color = "black", linewidth = 0.8) +
  
  # THE GRID
  facet_grid(Complexity ~ HorizonBin, scales = "free_y") +
  
  # Aesthetics
  scale_fill_manual(values = c("Internal" = "#4daf4a", "FutureTainted" = "#377eb8", "FutureUnseen" = "#e41a1c"),
                    labels = c(
                      "Cross-validation",
                      "Forecast with cross-validated model",
                      "Forecast with predictive validated model"
                    )) +
  scale_color_manual(values = c("Internal" = "#4daf4a", "FutureTainted" = "#377eb8", "FutureUnseen" = "#e41a1c"),
                     labels = c(
                       "Cross-validation",
                       "Forecast with cross-validated model",
                       "Forecast with predictive validated model"
                     )) +
  
  theme_minimal() +
  coord_cartesian(xlim = c(1.9, 3.1)) + 
  
  labs(
    title = "Predictive vs. Cross validation for different forecast horizons and model complexity",
    x = "Prediction Loss (Lower is better)",
    y = "Density (Strata)"
  ) +
  
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 11),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# Display result
pFinalGridBinned

# Plot 7. Look at it also from a sample size perspective!


