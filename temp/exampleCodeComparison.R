# EXAMPLE CODE FOR COMPARISON...TEST FIRST 2 TRIPLETS

library(data.table)
library(ggplot2)
library(gridExtra)

# 1. Identify the first two groups
results <- as.data.table(sim$fittedModelsPaths)
targetGroupIds <- unique(results$groupId)[1:2]
comparisonData <- results[groupId %in% targetGroupIds]

# 2. Print Summary Comparison Table
# This lets you see the variation in Mean Loss and Accuracy across complexities
message("--- Summary Comparison for First Two Triplets ---")
summary_tab <- comparisonData[, .(
  Complexity = first(numberOfCovariates), # From your experimentPlan merge
  testLoss = round(testLossMean, 4),
  testAcc  = round(testAccuracy * 100, 2)
), by = .(groupId, typeValidation)]
print(summary_tab)

# 3. Calculate the "Honesty Gap" for both
gaps <- summary_tab[, .(
  HonestyGap = testLoss[typeValidation == "FutureUnseen"] - testLoss[typeValidation == "Internal"],
  AccuracyLoss = testAcc[typeValidation == "Internal"] - testAcc[typeValidation == "FutureUnseen"]
), by = groupId]

message("\n--- The Honesty Gap (Optimism Bias) ---")
print(gaps)

# 4. Visualize Distributions with Faceting
message("\nLoading raw loss distributions for comparison...")

distData <- rbindlist(lapply(1:nrow(comparisonData), function(i) {
  rawLosses <- readRDS(comparisonData[i, rawLossPath])
  data.table(
    Loss = rawLosses,
    Scenario = comparisonData[i, typeValidation],
    Group = comparisonData[i, groupId],
    Complexity = paste0("Covariates: ", summary_tab[groupId == comparisonData[i, groupId], Complexity][1])
  )
}))

# Fix factor levels for logical ordering in plot
distData[, Scenario := factor(Scenario, levels = c("Internal", "FutureTainted", "FutureUnseen"))]

# Create the Plot
ggplot(distData, aes(x = Loss, fill = Scenario)) +
  geom_density(alpha = 0.5) +
  # Add vertical lines for the means
  geom_vline(data = distData[, .(mu = mean(Loss)), by = .(Group, Scenario)], 
             aes(xintercept = mu, color = Scenario), linetype = "dashed") +
  facet_wrap(~Group + Complexity, scales = "free_y", ncol = 1) +
  theme_minimal() +
  labs(
    title = "Comparison of Validation Scenarios across Triplets",
    subtitle = "Higher complexity should theoretically lead to a wider 'Honesty Gap'",
    x = "Log-Loss (Lower is better)",
    y = "Density"
  ) +
  scale_fill_manual(values = c("Internal" = "#4daf4a", "FutureTainted" = "#377eb8", "FutureUnseen" = "#e41a1c")) +
  scale_color_manual(values = c("Internal" = "#4daf4a", "FutureTainted" = "#377eb8", "FutureUnseen" = "#e41a1c"))