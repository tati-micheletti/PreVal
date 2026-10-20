# Figures for the bird results (same style as the caribou summary figures). Reads allModels.csv of one species' analysis folder.
#   Rscript birds/make_result_figures.R <analysis folder> <output folder> <species label>
.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "4.6"), .libPaths()))
suppressMessages({library(data.table); library(ggplot2)})
a <- commandArgs(trailingOnly = TRUE)
anaDir <- if (length(a) >= 1) a[1] else "outputs/birds/results/analysis"
outDir <- if (length(a) >= 2) a[2] else "outputs/birds/results/figures"
label <- if (length(a) >= 3) a[3] else "greenfinch"
dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
M <- fread(file.path(anaDir, "allModels.csv")); M[, `:=`(realized = testLossMean, reported = valLossBest)]
reg <- c(FutureUnseen = "PreVal", FutureTainted = "Status quo (random split)", FutureTaintedGrouped = "Route-grouped CV", FutureTaintedSpatial = "Spatial-block CV", Internal = "Random CV reference (leaky)")
cols <- c("PreVal" = "#0072B2", "Status quo (random split)" = "#D55E00", "Route-grouped CV" = "#CC79A7", "Spatial-block CV" = "#009E73", "Random CV reference (leaky)" = "grey45")
M <- M[typeValidation != "FutureTaintedGrouped"]      # route-grouped CV is no longer shown (decision of 2026-10-21)
M[, regime := factor(reg[typeValidation], levels = reg)]
th <- theme_minimal(base_size = 12) + theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank())
ci <- function(x) { x <- x[is.finite(x)]; n <- length(x); m <- mean(x); s <- if (n > 1) sd(x) / sqrt(n) * qt(.975, n - 1) else NA_real_; list(m = m, lo = m - s, hi = m + s, n = n) }
sv <- function(p, f, w = 9, h = 5.5) ggsave(file.path(outDir, f), p, width = w, height = h, dpi = 170, bg = "white")

# per (forecast year, horizon, regime): mean over splits and covariate levels
Y <- M[, .(loss = mean(realized)), by = .(testYear, horizon, regime)]
# ---- Fig 1: realized loss by forecast distance, all regimes ------------------------------------------------------------
S <- Y[, { r <- ci(loss); .(m = r$m, lo = r$lo, hi = r$hi, n = r$n) }, by = .(horizon, regime)][n >= 4]
p1 <- ggplot(S, aes(horizon, m, colour = regime, fill = regime)) + geom_ribbon(aes(ymin = lo, ymax = hi), alpha = .12, colour = NA) + geom_line(linewidth = .9) + geom_point(size = 1.6) +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols) + scale_x_continuous(breaks = 1:15) +
  labs(x = "Forecast distance (years ahead of the last training/validation year)", y = "Loss on the forecast year (lower = better)",
       title = paste0(label, ": which validation scheme gives the best forecasts depends on the distance"),
       subtitle = "Mean over forecast years (ribbon = 95% interval across years; only distances with at least 4 forecast years)") + th
sv(p1, "fig1_loss_by_horizon.png")

# ---- Fig 2: difference to the status quo by forecast distance ----------------------------------------------------------------
Wd <- dcast(Y, testYear + horizon ~ regime, value.var = "loss")
setnames(Wd, names(reg), names(reg), skip_absent = TRUE)
dd <- rbindlist(list(Wd[, .(testYear, horizon, d = `PreVal` - `Status quo (random split)`, what = "PreVal minus status quo")],
                     Wd[, .(testYear, horizon, d = `Spatial-block CV` - `Status quo (random split)`, what = "Spatial-block minus status quo")]))
dd[, what := factor(what, c("PreVal minus status quo", "Spatial-block minus status quo"))]
D <- dd[, { r <- ci(d); .(m = r$m, lo = r$lo, hi = r$hi, n = r$n) }, by = .(horizon, what)][n >= 4]
p2 <- ggplot() + geom_hline(yintercept = 0, colour = "grey30") + geom_point(data = dd[horizon <= 12], aes(horizon, d), colour = "grey70", size = .9, alpha = .7) +
  geom_errorbar(data = D, aes(horizon, ymin = lo, ymax = hi), width = .25, colour = "#222222") + geom_point(data = D, aes(horizon, m), size = 2.2, colour = "#222222") +
  facet_wrap(~what, ncol = 1) + scale_x_continuous(breaks = 1:12) + coord_cartesian(xlim = c(.5, 12.5)) +
  labs(x = "Forecast distance (years)", y = "Loss difference (below 0 = better than the status quo)",
       title = paste0(label, ": difference to the status quo, forecast distance by forecast distance"),
       subtitle = "Grey dots = single forecast years; black = mean and 95% interval across years") + th
sv(p2, "fig2_difference_to_status_quo_by_horizon.png", h = 6)

# ---- Fig 3: covariate ladder (penalty relative to 2 covariates) --------------------------------------------------------------
L <- M[, .(loss = mean(realized)), by = .(testYear, complexity, regime)]
L[, pen := loss - loss[complexity == min(complexity)], by = .(testYear, regime)]
LS <- L[, { r <- ci(pen); .(m = r$m, lo = r$lo, hi = r$hi) }, by = .(complexity, regime)]
p3 <- ggplot(LS, aes(factor(complexity), m, colour = regime, group = regime, fill = regime)) + geom_hline(yintercept = 0, colour = "grey60") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = .1, colour = NA) + geom_line(linewidth = .9) + geom_point(size = 1.8) +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
  labs(x = "Number of covariates (ranked by importance)", y = "Loss relative to the 2-covariate model (below 0 = better)",
       title = paste0(label, ": does adding covariates hurt, and does it depend on the validation scheme?"),
       subtitle = "Mean over forecast years (ribbon = 95% interval across years)") + th
sv(p3, "fig3_covariate_ladder.png")
L2 <- L[regime %in% c("PreVal", "Status quo (random split)")]
p3b <- ggplot(L2, aes(factor(complexity), pen, colour = regime)) + geom_hline(yintercept = 0, colour = "grey60") +
  geom_line(aes(group = interaction(testYear, regime)), alpha = .25) + stat_summary(aes(group = regime), fun = mean, geom = "line", linewidth = 1.2) +
  scale_colour_manual(values = cols) + facet_wrap(~regime, ncol = 2) +
  labs(x = "Number of covariates", y = "Loss relative to the 2-covariate model", title = paste0(label, ": every forecast year (thin) and the mean (thick)"),
       subtitle = "Both validation schemes show the same U shape: best at about 5 to 15 covariates, worse with all 95") + th + theme(legend.position = "none")
sv(p3b, "fig3b_covariate_ladder_per_year.png", h = 4.8)

# ---- Fig 4: H1 per forecast year -----------------------------------------------------------------------------------------------
H <- dcast(M[, .(loss = mean(realized)), by = .(testYear, regime)], testYear ~ regime, value.var = "loss")
H[, d := `Status quo (random split)` - `Random CV reference (leaky)`]
p4 <- ggplot(H, aes(testYear, d)) + geom_hline(yintercept = 0, colour = "grey30") + geom_col(fill = "#D55E00", width = .7) + geom_hline(yintercept = mean(H$d), linetype = 2) +
  labs(x = "Forecast year", y = "Status quo minus random-CV reference (loss)", title = paste0(label, ": a model checked with a random split forecasts worse than random CV suggests"),
       subtitle = sprintf("Positive in %d of %d forecast years; mean %+.3f (dashed)", sum(H$d > 0), nrow(H), mean(H$d))) + th
sv(p4, "fig4_H1_per_year.png", h = 4.5)

# ---- Fig 5: what each scheme reports versus what happens -----------------------------------------------------------------------
R <- M[, .(reported = mean(reported), realized = mean(realized)), by = .(testYear, regime)]
RL <- melt(R, id.vars = c("testYear", "regime"), variable.name = "what", value.name = "loss")
RS <- RL[, { r <- ci(loss); .(m = r$m, lo = r$lo, hi = r$hi) }, by = .(regime, what)]
RS[, what := factor(what, c("reported", "realized"), c("Reported by its own validation", "Realized on the forecast year"))]
p5 <- ggplot(RS, aes(regime, m, fill = what)) + geom_col(position = position_dodge(.75), width = .7) + geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(.75), width = .25) +
  scale_fill_manual(values = c("grey70", "#333333")) + coord_cartesian(ylim = c(1.9, max(RS$hi) + .02)) + scale_x_discrete(labels = function(x) sub(" \\(", "\n(", x)) +
  labs(x = NULL, y = "Loss (lower = better)", title = paste0(label, ": reported versus realized loss"),
       subtitle = "A gap between the two bars = the scheme's validation was too optimistic (or, for spatial and route-grouped schemes, too pessimistic)") + th
sv(p5, "fig5_reported_vs_realized.png", w = 10)

# ---- Fig 6 : how many covariates would each scheme pick? -----------------------------------------------------------
sel <- M[, { k <- which.min(reported); .(chosen = complexity[k], realizedChosen = realized[k]) }, by = .(splitId, regime)]
SC <- sel[, .N, by = .(regime, chosen)][, share := N / sum(N), by = regime]
p6 <- ggplot(SC, aes(factor(chosen), share, fill = regime)) + geom_col(position = position_dodge(.8), width = .75) + scale_fill_manual(values = cols) + scale_y_continuous(labels = scales::percent) +
  labs(x = "Number of covariates picked by the scheme's own lowest validation loss", y = "Share of splits", title = paste0(label, ": which model size would each scheme choose? "),
       subtitle = "Chosen per split by each scheme's own lowest validation loss") + th
sv(p6, "fig6_covariate_choice.png", w = 10)
cat("figures written to", outDir, "\n")
