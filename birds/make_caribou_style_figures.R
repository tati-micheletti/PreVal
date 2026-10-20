# The caribou summary figures, redone for the birds (same questions, same style). Reads allModels.csv of one species' analysis folder.
#   Rscript birds/make_caribou_style_figures.R <analysis folder> <output folder> <species label>
.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))
suppressMessages({library(data.table); library(ggplot2)})
a <- commandArgs(trailingOnly = TRUE)
anaDir <- if (length(a) >= 1) a[1] else "outputs/birds/results/analysis"
outDir <- if (length(a) >= 2) a[2] else "outputs/birds/results/figures_caribou_style"
label <- if (length(a) >= 3) a[3] else "greenfinch"
dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
M <- fread(file.path(anaDir, "allModels.csv"))
M <- M[typeValidation != "FutureTaintedGrouped"]
M[, `:=`(realized = testLossMean, reported = valLossBest)]
reg <- c(FutureUnseen = "PreVal", FutureTainted = "Status quo (random split)", FutureTaintedSpatial = "Spatial-block CV", Internal = "Random CV reference (leaky)")
cols <- c("PreVal" = "#2171B5", "Status quo (random split)" = "#E08A00", "Spatial-block CV" = "#2E8B57", "Random CV reference (leaky)" = "#B03A48")
M[, regime := factor(reg[typeValidation], levels = reg)]
lv <- sort(unique(M$complexity)); lvl <- paste(lv, "covariates")
M[, cov := factor(paste(complexity, "covariates"), levels = lvl)]
th <- theme_bw(base_size = 12) + theme(legend.position = "bottom", strip.background = element_rect(fill = "grey85"))
sv <- function(p, f, w = 11, h = 6) ggsave(file.path(outDir, f), p, width = w, height = h, dpi = 170, bg = "white")
tci <- function(x) { n <- length(x); m <- mean(x); s <- sd(x) / sqrt(n) * qt(.975, n - 1); c(m = m, lo = m - s, hi = m + s) }
W <- dcast(M, splitId + complexity + cov + testYear + horizon + windowLength ~ typeValidation, value.var = c("realized", "reported"))

# ---- Fig 1 (H1): is cross-validation a misleading measure of forecast quality? ----------------------------------------------------
pa <- W[, .(panel = "A. Forecasting with the status quo (y) vs the reference CV (x), same test cases", x = realized_Internal, y = realized_FutureTainted, cov)]
pb <- W[, .(panel = "B. The status quo's own reported error (x) vs its forecast error (y)", x = reported_FutureTainted, y = realized_FutureTainted, cov)]
P1 <- rbind(pa, pb)
p1 <- ggplot(P1, aes(x, y, colour = cov)) + geom_point(size = .7, alpha = .35) + geom_abline(slope = 1, intercept = 0, linetype = 2) + facet_wrap(~panel, scales = "free") +
  labs(x = "Loss (lower is better)", y = "Loss when forecasting", colour = "Covariates",
       title = paste0(label, ": is cross-validation a misleading measure of forecast quality?"),
       subtitle = "Dashed line: no difference. Dots above the line: forecasting is worse than what CV suggests. One dot = one split and covariate level") + th +
  guides(colour = guide_legend(override.aes = list(size = 2, alpha = 1), nrow = 1))
sv(p1, "fig1_H1.png", w = 13, h = 6)

# ---- Fig 1b: H1 per forecast year ---------------------------------------------------------------------------------------------------
Y1 <- W[, .(d = mean(realized_FutureTainted - realized_Internal)), by = .(cov, testYear)]
S1 <- Y1[, as.list(tci(d)), by = cov]; S1[, testYear := min(Y1$testYear) - 1]
p1b <- ggplot(Y1, aes(d, testYear)) + geom_vline(xintercept = 0, linetype = 2) + geom_point(size = 1.4, colour = "grey25") +
  geom_errorbarh(data = S1, aes(xmin = lo, xmax = hi, y = testYear), height = .6, colour = "#B03A48", inherit.aes = FALSE) +
  geom_point(data = S1, aes(m, testYear), shape = 18, size = 3.2, colour = "#B03A48", inherit.aes = FALSE) + facet_wrap(~cov, ncol = 4) +
  scale_y_continuous(breaks = seq(2008, 2024, 4)) +
  labs(x = "Forecast loss minus reference-CV loss (positive = CV is misleading)", y = "Forecast year", title = paste0(label, ": H1 per forecast year"),
       subtitle = "Dots: forecast years. Red diamond and bar: mean and t interval across years") + th
sv(p1b, "fig1b_H1_per_year.png", w = 14, h = 7)

# ---- Fig 2 (H2): PreVal vs the status quo, one dot per forecast year ------------------------------------------------------------------
Y2 <- W[, .(d = mean(realized_FutureUnseen - realized_FutureTainted)), by = .(cov, testYear)]
S2 <- Y2[, as.list(tci(d)), by = cov]; S2[, testYear := min(Y2$testYear) - 1]
p2 <- ggplot(Y2, aes(d, testYear)) + geom_vline(xintercept = 0, linetype = 2) + geom_point(size = 1.4, colour = "grey25") +
  geom_errorbarh(data = S2, aes(xmin = lo, xmax = hi, y = testYear), height = .6, colour = "#2171B5", inherit.aes = FALSE) +
  geom_point(data = S2, aes(m, testYear), shape = 18, size = 3.2, colour = "#2171B5", inherit.aes = FALSE) + facet_wrap(~cov, ncol = 4) +
  scale_y_continuous(breaks = seq(2008, 2024, 4)) +
  labs(x = "Difference in realized test loss (negative = PreVal better)", y = "Forecast year", title = paste0(label, ": PreVal vs the status quo (both forecast the same year), one dot per forecast year"),
       subtitle = "Blue diamond and bar: mean and t interval across forecast years (paired on identical test cases)") + th
sv(p2, "fig2_H2_forest.png", w = 14, h = 7)

# ---- Fig 3: does adding covariates hurt out of sample? (relative to the smallest model, as in the caribou figure) -----------------------
L <- M[, .(loss = mean(realized)), by = .(testYear, complexity, regime)]
L[, pen := loss - loss[complexity == min(complexity)], by = .(testYear, regime)]
LM <- L[, .(pen = mean(pen)), by = .(complexity, regime)]
p3 <- ggplot(L, aes(factor(complexity), pen)) + geom_hline(yintercept = 0, linetype = 2) + geom_line(aes(group = testYear), colour = "grey70", linewidth = .3) +
  geom_line(data = LM, aes(group = regime, colour = regime), linewidth = 1.3) + geom_point(data = LM, aes(colour = regime), size = 2.3) +
  scale_colour_manual(values = cols) + facet_wrap(~regime, nrow = 1) +
  labs(x = "Number of covariates", y = "Realized loss minus the 2-covariate model's (per forecast year)", title = paste0(label, ": does adding covariates hurt out-of-sample?"),
       subtitle = "Grey: one line per forecast year. Colour: mean. Above 0 = worse than the 2-covariate model") + th + theme(legend.position = "none")
sv(p3, "fig3_complexity_by_year.png", w = 15, h = 5.5)

# ---- Fig 3b: absolute loss by number of covariates (median and middle 50% of splits), no reference level ----------------------------------------
A <- M[, .(med = median(realized), q1 = quantile(realized, .25), q3 = quantile(realized, .75)), by = .(complexity, regime)]
p3b <- ggplot(A, aes(factor(complexity), med, colour = regime, fill = regime, group = regime)) + geom_ribbon(aes(ymin = q1, ymax = q3), alpha = .12, colour = NA) +
  geom_line(linewidth = 1.1) + geom_point(size = 2.2) + scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
  labs(x = "Number of covariates (ranked by importance)", y = "Median loss on the forecast year (lower = better)", colour = NULL, fill = NULL,
       title = paste0(label, ": loss by number of covariates, all forecast distances"), subtitle = "Line: median over splits. Band: middle 50% of splits. No reference level") + th
sv(p3b, "fig3b_complexity_curves_median_IQR.png", w = 9.5, h = 5.8)

# ---- Fig 4: does the advantage depend on how far ahead we forecast? --------------------------------------------------------------------------
H <- rbind(W[, .(horizon, d = realized_FutureUnseen - realized_FutureTainted, what = "PreVal - Status quo")],
           W[, .(horizon, d = realized_FutureTaintedSpatial - realized_FutureTainted, what = "Spatial-block - Status quo")])
H <- H[horizon <= 15]
p4 <- ggplot(H, aes(horizon, d)) + geom_hline(yintercept = 0, linetype = 2) + geom_point(position = position_jitter(width = .12, height = 0), size = .35, alpha = .12) +
  geom_smooth(method = "loess", formula = y ~ x, colour = "#2171B5", fill = "grey70", linewidth = 1) + facet_wrap(~what, scales = "free_y") + scale_x_continuous(breaks = 1:15) +
  labs(x = "Forecast horizon (forecast year minus last history year)", y = "Difference in realized test loss", title = paste0(label, ": does the advantage depend on how far ahead we forecast?"),
       subtitle = "One dot = one split and covariate level; line = smooth fit with its interval") + th
sv(p4, "fig4_horizon.png", w = 13, h = 5.5)

# ---- Fig 6: how did training end? -------------------------------------------------------------------------------------------------------------
E <- M[, .(`stopped at cap` = mean(epochsRun >= epochCap), `stopped by patience` = mean(epochsRun < epochCap)), by = regime]
E <- melt(E, id.vars = "regime", variable.name = "end", value.name = "share")
p6 <- ggplot(E, aes(regime, share, fill = end)) + geom_col(width = .8) + scale_fill_manual(values = c("stopped at cap" = "grey60", "stopped by patience" = "grey85")) +
  scale_x_discrete(labels = function(x) sub(" \\(", "\n(", x)) +
  labs(x = NULL, y = "Share of models", fill = NULL, title = paste0(label, ": how did training end?"), subtitle = "Dark: training hit the epoch cap while validation loss was still improving") + th
sv(p6, "fig6_how_training_ended.png", w = 9, h = 5)

# ---- Fig 9: paired penalty by forecast distance (median and middle 50% of splits) -----------------------------------------------------------------
M[, hb := cut(horizon, c(0, 1, 2, 4, 9, Inf), labels = c("Horizon 1", "Horizon 2", "Horizon 3-4", "Horizon 5-9", "Horizon 10+"))]
M[, pen := realized - realized[complexity == min(complexity)], by = .(splitId, typeValidation)]
G <- rbind(M[, .(hb = factor("All horizons", levels = c("All horizons", levels(M$hb))), complexity, regime, pen)], M[, .(hb, complexity, regime, pen)])
GS <- G[, .(med = median(pen), q1 = quantile(pen, .25), q3 = quantile(pen, .75)), by = .(hb, complexity, regime)]
p9 <- ggplot(GS, aes(factor(complexity), med, colour = regime, fill = regime, group = regime)) + geom_hline(yintercept = 0, linetype = 2) +
  geom_ribbon(aes(ymin = q1, ymax = q3), alpha = .1, colour = NA) + geom_line(linewidth = 1) + geom_point(size = 1.6) + facet_wrap(~hb, nrow = 1) +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
  labs(x = "Number of covariates", y = "Loss minus the same split's loss with the fewest covariates", colour = NULL, fill = NULL,
       title = paste0(label, ": does adding covariates hurt? Paired within each split"),
       subtitle = "Line: median across splits. Band: middle 50% of splits. Above 0 = worse than the simplest model") + th
sv(p9, "fig9_paired_penalty.png", w = 18, h = 5.5)
cat("figures written to", outDir, "\n")
