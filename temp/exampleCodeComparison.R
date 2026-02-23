# EXAMPLE CODE FOR COMPARISON...

Require::Require("data.table")
Require::Require("ggplot2")
Require::Require("gridExtra")
Require::Require("ggridges")

pall_Data <- fread("C:/Users/Tati/GitHub/PreVal/outputs/interimData_20FEB26.csv")
pall_Data[, Scenario := factor(Scenario, levels = c("Internal", "FutureTainted", "FutureUnseen"))]
pall_Data[, Complexity := factor(Complexity, 
                                 levels = c("Covariates: 2", "Covariates: 5", 
                                            "Covariates: 10", "Covariates: Inf"))]
# 1. Create Summary Table
# We calculate the Mean and SD of the Loss for every triplet
summaryDt <- pall_Data[, .(
  meanLoss = mean(Loss),
  sdLoss = sd(Loss),
  # We keep these columns as they are constant within a Group/Scenario
  ForecastHorizon = first(ForecastHorizon),
  forcastHorizon = first(forcastHorizon),
  Complexity = first(Complexity)
), by = .(Group, Scenario)]

# Plot 1: The Optimism Bias (Forecast Distance)
# This addresses H2. It shows how the model's performance decays as we predict 
# further into the future. The "Honesty Gap" is the distance between the 
# Internal line (0 horizon) and the Future lines.

p1 <- ggplot(summaryDt, aes(x = ForecastHorizon, y = meanLoss, color = Scenario)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE) +
  facet_wrap(~Complexity) +
  scale_color_manual(values = c("Internal" = "#4daf4a", "FutureTainted" = "#377eb8", "FutureUnseen" = "#e41a1c")) +
  theme_minimal() +
  labs(
    title = "Figure 1: Performance Decay over Time (Optimism Bias)",
    x = "Forecast Horizon (Years)",
    y = "Mean Log-Loss (Lower is better)",
    color = "Scenario"
  )

# 1. Aggregate raw data to get the means for every scenario/group
groupMeans <- pall_Data[, .(
  meanLoss = mean(Loss),
  ForecastHorizon = first(ForecastHorizon),
  Complexity = first(Complexity)
), by = .(Group, Scenario)]

# 2. Split the data into two tables
# Table A: Just the baselines (Internal scenarios)
baselines <- groupMeans[Scenario == "Internal", .(Group, internalBaseline = meanLoss)]

# Table B: Just the future forecasts
futureResults <- groupMeans[Scenario != "Internal"]

# 3. Join the baseline to the future results based on the Group name
# This puts the 'internalBaseline' value onto every future row in that triplet
combinedDt <- merge(futureResults, baselines, by = "Group")

# 4. Calculate the Honesty Gap
# Gap = How much worse is the forecast compared to the cheat baseline?
combinedDt[, honestyGap := meanLoss - internalBaseline]

# 6. Plot
p1b <- ggplot(combinedDt, aes(x = ForecastHorizon, y = honestyGap, color = Scenario)) +
  geom_point(alpha = 0.3, size = 1.5) +
  # We use 'lm' (linear) because it is the most robust to sparse data
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) + 
  facet_wrap(~Complexity) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  scale_color_manual(values = c("FutureTainted" = "#377eb8", "FutureUnseen" = "#e41a1c"),
                     labels = c("FutureTainted" = "Standard Predictive Gap", 
                                "FutureUnseen" = "Honest Forecasting Gap")) +
  labs(
    title = "Figure 1b: The Growth of the Honesty Gap",
    subtitle = "Zero represents the Internal (cheating) baseline",
    x = "Forecast Horizon (Years)",
    y = "Optimism Bias (Future Loss - Internal Loss)"
  )

# Plot 2: The Complexity Trap (Parsimony)
# This addresses H1. It shows if adding more variables actually helps or hurts 
# the forecast. If "Inf" has lower loss than "2" in Internal but higher loss 
# in FutureUnseen, we have proven the trap.
# 1. Aggregate to the Group level
groupMeans <- pall_Data[, .(
  meanLoss = mean(Loss),
  ForecastHorizon = first(ForecastHorizon),
  Complexity = first(Complexity)
), by = .(Group, Scenario)]

# 3. THE ALIGNMENT STRATEGY
# Identify all groups that performed a 2-year forecast
targetGroups <- groupMeans[ForecastHorizon == 2, unique(Group)]

# Subset the data to include ONLY those groups
# This naturally pulls the Internal (0), Tainted (2), and Unseen (2) for each triplet
plot2_dt <- groupMeans[Group %in% targetGroups]

# 4. Plot
p2 <- ggplot(plot2_dt, aes(x = Complexity, y = meanLoss, color = Scenario, group = Scenario)) +
  # Using points for the raw group results
  geom_point(alpha = 0.2, position = position_dodge(width = 0.5)) +
  # Adding a line through the means of all groups
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  theme_minimal() +
  scale_color_manual(values = c("Internal" = "#4daf4a", "FutureTainted" = "#377eb8", "FutureUnseen" = "#e41a1c")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    title = "Figure 2: The Complexity Trap (Parsimony Analysis)",
    subtitle = "Comparing model scenarios for a 2-year Forecast Horizon",
    x = "Model Complexity (Number of Covariates)",
    y = "Mean Log-Loss (Lower is better)",
    color = "Validation Level"
  )

#Plot 3: The Value of History (History × Horizon)
# The Research Question: Does "Big Data" (more years of history) actually result in better forecasts?
# Control: We fix model complexity at 10 covariates.
# Facet: We look at a few specific horizons (e.g., 1, 3, and 5 years into the future).

# Create a copy for plotting
plotDt <- copy(pall_Data)

# Extract Training Window Size (History) from the Group string
# Grp_[Complexity]_[StartYear]_[EndYear]_[TestYear]
plotDt[, trainStart := as.numeric(tstrsplit(Group, "_")[[3]])]
plotDt[, trainEnd   := as.numeric(tstrsplit(Group, "_")[[4]])]
plotDt[, windowSize := (trainEnd - trainStart) + 1]

# Create aggregated summary for Plot 3
summaryDt <- plotDt[, .(
  meanLoss = mean(Loss),
  ForecastHorizon = first(ForecastHorizon),
  Complexity = first(Complexity),
  windowSize = first(windowSize)
), by = .(Group, Scenario)]

target_horizons <- seq(2, 9, 1)
# Filter for a specific complexity level
plot3_dt2 <- summaryDt[Complexity == "Covariates: 2" & ForecastHorizon %in% target_horizons,]
plot3_dt5 <- summaryDt[Complexity == "Covariates: 5" & ForecastHorizon %in% target_horizons,]
plot3_dt10 <- summaryDt[Complexity == "Covariates: 10" & ForecastHorizon %in% target_horizons,]
plot3_dt20 <- summaryDt[Complexity == "Covariates: Inf" & ForecastHorizon %in% target_horizons,]

for (pp in c(2,5,10,20)){
  assign(paste0("p3.", pp), ggplot(get(paste0("plot3_dt", pp)), aes(x = windowSize, y = meanLoss, color = Scenario)) +
           geom_point(alpha = 0.5) +
           geom_smooth(method = "lm", se = TRUE) +
           facet_wrap(~ForecastHorizon, labeller = label_both, ncol = 2) +
           scale_color_manual(values = c("Internal" = "#4daf4a", "FutureTainted" = "#377eb8", "FutureUnseen" = "#e41a1c")) +
           labs(
             title = "Figure 3: Does more History improve Forecasts?",
             subtitle = "Controlled for Complexity = 10 Covariates",
             x = "Training Window Size (Number of Historical Years)",
             y = "Mean Log-Loss (Lower is better)",
             color = "Scenario"
           ) +
           theme_minimal() +
           theme(legend.position = "bottom"))
}

# Plot 4. 
# 2. Create the Spaghetti Density Plot
p4_all <- ggplot(pall_Data, aes(x = Loss, color = Scenario)) +
  # Draw a thin line for every single group/scenario combination
  # We use a very low alpha (transparency) so they only look dark where they overlap
  stat_density(aes(group = interaction(Group, Scenario)), 
               geom = "path", position = "identity", alpha = 0.05, linewidth = 0.2) +
  
  # Draw a thick line for the "Global Average" of each scenario
  stat_density(geom = "line", linewidth = 2) +
  
  # Add the baseline reference (Random guessing)
  geom_vline(xintercept = 2.3979, linetype = "dotted", color = "black", size = 1) +
  
  scale_color_manual(values = c("Internal" = "#4daf4a", "FutureTainted" = "#377eb8", "FutureUnseen" = "#e41a1c")) +
  theme_minimal() +
  coord_cartesian(xlim = c(1.5, 3.5)) + # Focus on the meaningful range
  labs(
    title = "Figure 4: Universal Bias: The Systematic Nature of the Honesty Gap",
    subtitle = "Faint lines represent individual groups; thick lines represent the global population average.",
    x = "Log-Loss (Surprise Value)",
    y = "Density"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
p4_all

# 2. Pre-calculate the Mean Densities (The "Honest Curve" Logic)
# We calculate the density for every group/scenario combination on a fixed grid
calc_mean_density <- function(df) {
  # Define a common grid for the X axis
  x_grid <- seq(1.5, 3.5, length.out = 200)
  
  # Calculate density for every subgroup
  densities <- df[, {
    d <- density(Loss, from = 1.5, to = 3.5, n = 200)
    list(x = d$x, y = d$y)
  }, by = .(Group, Scenario, Complexity)]
  
  # Average the 'y' values across groups for each x-point
  mean_curves <- densities[, .(mean_y = mean(y)), by = .(Scenario, Complexity, x)]
  return(list(thin = densities, thick = mean_curves))
}

curves <- calc_mean_density(pall_Data)

# Common grid for averaging shapes
x_grid <- seq(1.5, 3.5, length.out = 200)

# Calculate individual curves
thin_densities <- pall_Data[, {
  d <- density(Loss, from = 1.5, to = 3.5, n = 200)
  list(x = d$x, y = d$y)
}, by = .(Group, Scenario, Complexity)]

# Average the y-values per x-point across groups
thick_curves <- thin_densities[, .(mean_y = mean(y)), by = .(Scenario, Complexity, x)]

# 3. CALCULATE GLOBAL MEANS FOR THE DOTTED LINES
# This ensures the vertical lines exactly match the mathematical averages
mean_vlines <- pall_Data[, .(mu = mean(Loss)), by = .(Scenario, Complexity)]

# 4. Create the Plot
p4_facet <- ggplot() +
  # Layer 1: Thin lines (The raw group densities)
  geom_line(data = thin_densities, 
            aes(x = x, y = y, color = Scenario, group = interaction(Group, Scenario)), 
            alpha = 0.05, linewidth = 0.2) +
  
  # Layer 2: Thick lines (The standardized curve average)
  geom_line(data = thick_curves, 
            aes(x = x, y = mean_y, color = Scenario), 
            linewidth = 1.5) +
  
  # LAYER 3: ADD THE VERTICAL DOTTED LINES (THE MEANS)
  geom_vline(data = mean_vlines, 
             aes(xintercept = mu, color = Scenario), 
             linetype = "dotted", linewidth = 1) +
  
  # Layer 4: The Random Baseline (Reference)
  geom_vline(xintercept = 2.3979, linetype = "dashed", color = "grey20", linewidth = 0.8) +
  
  facet_wrap(~Complexity, scales = "free_y") +
  scale_color_manual(values = c("Internal" = "#4daf4a", "FutureTainted" = "#377eb8", "FutureUnseen" = "#e41a1c")) +
  theme_minimal() +
  coord_cartesian(xlim = c(1.8, 3.0)) +
  labs(
    title = "Figure 4: Generalization Stability: The Robustness of Honest Validation",
    subtitle = "Faint lines = individual groups. Thick lines = curve average. Dotted lines = scenario means.",
    x = "Prediction Loss (Lower is better)",
    y = "Mean Density"
  ) +
  theme(
    legend.position = "bottom", 
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )
p4_facet

## PLOTS

# Update p1
p1 <- p1 + labs(
  title = "Figure 1: Forecasting Decay: The Inevitable Rise of Prediction Error",
  subtitle = "Assessing absolute model performance as the temporal distance between training and testing increases."
)

# Update p1b
p1b <- p1b + labs(
  title = "Figure 1b: The Honesty Gap: Quantifying the Illusion of Model Accuracy",
  subtitle = "Measuring the 'Optimism Bias' (Future Error - Internal Baseline) across forecast horizons."
)

# Update p2
p2 <- p2 + labs(
  title = "Figure 2: The Complexity Trap: Why 'Bigger' Models Fail to Forecast Better",
  subtitle = "Identifying the parsimony peak by evaluating the impact of covariate count on generalization."
)

# Update p3 Loop
for (pp in c(2,5,10,20)){
  # Determine label for Inf
  label_pp <- if(pp == 20) "Full Model" else paste0(pp, " Covariates")
  
  p_temp <- get(paste0("p3.", pp)) + labs(
    title = "Figure 3: The Myth of 'Big History': More Data Does Not Guarantee Better Forecasts",
    subtitle = paste0("Assessing the impact of training window size on forecast skill (", label_pp, ").")
  )
  assign(paste0("p3.", pp), p_temp)
}

# Update p4
p4 <- p4 + labs(
  title = "Figure 4: The Risk Audit: Unmasking the Catastrophic Failures of Blind Forecasting",
  subtitle = "Visualizing the distribution of decision errors to reveal risks hidden by mean averages."
)

p1
p1b
p2
p3.2
p3.5
p3.10
p3.20
p4

# PLOT 5: The Ultimate PLOT
# 1. Prepare Data and Define Horizon Bins
plotDataAll <- copy(pall_Data)

# Create the specific Bins you requested
plotDataAll[ForecastHorizon %in% 2:3, HorizonBin := "Horizon: 2-3 Years"]
plotDataAll[ForecastHorizon %in% 4:6, HorizonBin := "Horizon: 4-6 Years"]
plotDataAll[ForecastHorizon %in% 7:8, HorizonBin := "Horizon: 7-8 Years"]
plotDataAll[ForecastHorizon %in% 9:10, HorizonBin := "Horizon: 9-10 Years"]

# Filter to keep only the requested range
dataFuture <- plotDataAll[Scenario != "Internal" & !is.na(HorizonBin)]
dataInternal <- plotDataAll[Scenario == "Internal"]

# 2. Project Internal Baseline into the Bins
# We link the internal baseline to the bins via the Group ID
internalTemplate <- unique(dataFuture[, .(Group, HorizonBin, Complexity)])

dataInternalProjected <- merge(internalTemplate, 
                               dataInternal[, .(Group, Loss, Scenario)], 
                               by = "Group", 
                               allow.cartesian = TRUE)

# Combine for plotting
plotDataBinned <- rbind(dataFuture[, .(Group, Scenario, Complexity, HorizonBin, Loss)],
                        dataInternalProjected)

# Factor Ordering
plotDataBinned[, Scenario := factor(Scenario, levels = c("Internal", "FutureTainted", "FutureUnseen"))]
plotDataBinned[, Complexity := factor(Complexity, levels = c("Covariates: 2", "Covariates: 5", "Covariates: 10", "Covariates: Inf"))]
plotDataBinned[, HorizonBin := factor(HorizonBin, levels = c("Horizon: 2-3 Years", "Horizon: 4-6 Years", "Horizon: 7-8 Years", "Horizon: 9-10 Years"))]

# 3. Calculate Means and Staggered Annotation Positions
meansDt <- plotDataBinned[, .(mu = mean(Loss)), by = .(Scenario, Complexity, HorizonBin)]
meansDt[Scenario == "Internal", vPos := 1.5]
meansDt[Scenario == "FutureTainted", vPos := 3.5]
meansDt[Scenario == "FutureUnseen", vPos := 5.5]

# plotDataBinned[Scenario == FutureUnseen, Scenario := "Forecast (unseen) with Predictive Validation"]
# plotDataBinned[Scenario == Internal, Scenario := "Forecast (seen) with Cross-Validation"]
# plotDataBinned[Scenario == FutureTainted, Scenario := "Forecast (unseen) with Cross-Validation"]

# 4. Create the Grid Plot
pFinalGridBinned <- ggplot(plotDataBinned, aes(x = Loss)) +
  
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

# PLOT 6
# 1. Prepare Data
# 1. Aggregate to group means
groupMeans <- pall_Data[, .(
  meanLoss = mean(Loss),
  ForecastHorizon = first(ForecastHorizon)
), by = .(Group, Scenario, Complexity)]

# 2. Apply NEW Horizon Binning (2-3, 4-6, 7-8, 9-10)
groupMeans[ForecastHorizon %in% 2:3, HorizonBin := "Horizon: 2-3 Years"]
groupMeans[ForecastHorizon %in% 4:6, HorizonBin := "Horizon: 4-6 Years"]
groupMeans[ForecastHorizon %in% 7:8, HorizonBin := "Horizon: 7-8 Years"]
groupMeans[ForecastHorizon %in% 9:10, HorizonBin := "Horizon: 9-10 Years"]

# 3. Join with Internal Baselines
baselines <- groupMeans[Scenario == "Internal", .(Group, internalBaseline = meanLoss)]
futureResults <- groupMeans[Scenario != "Internal" & !is.na(HorizonBin)]

gapDt <- merge(futureResults, baselines, by = "Group")
gapDt[, honestyGap := meanLoss - internalBaseline]

# 4. Factor Ordering
gapDt[, Complexity := factor(Complexity, levels = c("Covariates: 2", "Covariates: 5", "Covariates: 10", "Covariates: Inf"))]
gapDt[, Scenario := factor(Scenario, levels = c("FutureTainted", "FutureUnseen"), labels = c("Standard Forecast Gap", "Honest Forecast Gap"))]
gapDt[, HorizonBin := factor(HorizonBin, levels = c("Horizon: 2-3 Years", "Horizon: 4-6 Years", "Horizon: 7-8 Years", "Horizon: 9-10 Years"))]

# 5. Plot
p6 <- ggplot(gapDt, aes(x = HorizonBin, y = honestyGap, color = Scenario, group = Scenario)) +
  geom_point(alpha = 0.2) + #, position = position_dodge(width = 0.4)
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) + #, position = position_dodge(width = 0.4)
  stat_summary(fun = mean, geom = "point", size = 4) + #, position = position_dodge(width = 0.4)
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_grid(Complexity ~ .) +
  theme_minimal() +
  scale_color_manual(values = c("Standard Forecast Gap" = "#377eb8", "Honest Forecast Gap" = "#e41a1c")) +
  labs(
    # title = "Figure 2: The Complexity Penalty: Quantifying Overfitting Risk",
    x = "Model Complexity (Number of Covariates)",
    y = "Optimism Bias (Future Loss - Internal Baseline)",
    color = "Validation Method"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 14)
  )
p6

