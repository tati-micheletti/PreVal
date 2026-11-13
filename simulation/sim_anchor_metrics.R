# Realism-anchor metrics of a route-count table (real or simulated), and the real greenfinch targets with the tolerances accepted by Tati on 2026-11-05.
# Same definitions as the ones used to measure the real targets (section 9 of ANALYSIS_PLAN_SIMULATION.md):
#   mean, variance over mean, share of zeros of the count; route-years per year; years per route; ICC = share of the variance of log1p(count) between routes (ANOVA);
#   spatial correlation of the route means of log1p(count) by distance band = mean cross-product of centred route means over their variance (2,139 routes -> a fixed random sample of 1,500).
suppressMessages(library(data.table))

anchorBands <- c(0, 25, 50, 100, 150, 300, 600, 1500)           # km
anchorMetrics <- function(tab, nSample = 1500L) {
  tab <- data.table::as.data.table(tab); y <- tab$Reviere; ly <- log1p(y)
  a <- tab[, .(m = mean(log1p(Reviere)), n = .N, v = stats::var(log1p(Reviere))), by = SITE_ID][n > 1]
  within <- stats::weighted.mean(a$v, a$n - 1); between <- stats::var(a$m) - within / mean(a$n)
  rr <- tab[, .(m = mean(log1p(Reviere)), x = mean(x), y = mean(y)), by = SITE_ID]
  i <- withr_seed(1L, sample(nrow(rr), min(nSample, nrow(rr))))
  dd <- as.matrix(stats::dist(rr[i, .(x, y)])) / 1000; mm <- outer(rr$m[i] - mean(rr$m[i]), rr$m[i] - mean(rr$m[i])); up <- upper.tri(dd)
  sp <- tapply(mm[up], cut(dd[up], anchorBands), mean) / stats::var(rr$m[i])
  c(mean = mean(y), varOverMean = stats::var(y) / mean(y), zeros = mean(y == 0), routeYears = nrow(tab), perYear = nrow(tab) / data.table::uniqueN(tab$year), yearsPerRoute = nrow(tab) / data.table::uniqueN(tab$SITE_ID),
    ICC = between / (between + within), sp0_25 = sp[[1]], sp25_50 = sp[[2]], sp50_100 = sp[[3]], sp100_150 = sp[[4]], sp150_300 = sp[[5]])
}
withr_seed <- function(seed, expr) { had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE); if (had) old <- get(".Random.seed", envir = globalenv()); on.exit(if (had) assign(".Random.seed", old, envir = globalenv())); set.seed(seed); force(expr) }

# real greenfinch targets (data_birds/final_Chloris_chloris.rds, measured 2026-11-04, ANALYSIS_PLAN_SIMULATION.md section 9) and tolerances (accepted 2026-11-05)
anchorTargets <- c(mean = 3.87, varOverMean = 8.0, zeros = 0.326, routeYears = 20800, perYear = 1040, yearsPerRoute = 9.72, ICC = 0.787,
                   sp0_25 = 0.104, sp25_50 = 0.024, sp50_100 = 0.005, sp100_150 = 0.000, sp150_300 = -0.006)
# check(): TRUE/FALSE per metric. Rules: mean +-5% (relative); zeros +-0.03; ICC +-0.10; spatial +-0.04 in the 0-25 and 25-50 km bands and |x| < 0.03 beyond 100 km (the 50-100 km band
# has no stated tolerance: +-0.04 is used); route-years per year +-10% (relative); variance over mean +-30% (relative); years per route is reported, no tolerance.
anchorCheck <- function(m) {
  t <- anchorTargets
  c(mean = abs(m[["mean"]] / t[["mean"]] - 1) <= 0.05, zeros = abs(m[["zeros"]] - t[["zeros"]]) <= 0.03, ICC = abs(m[["ICC"]] - t[["ICC"]]) <= 0.10,
    sp0_25 = abs(m[["sp0_25"]] - t[["sp0_25"]]) <= 0.04, sp25_50 = abs(m[["sp25_50"]] - t[["sp25_50"]]) <= 0.04, sp50_100 = abs(m[["sp50_100"]] - t[["sp50_100"]]) <= 0.04,
    sp100_150 = abs(m[["sp100_150"]]) < 0.03, sp150_300 = abs(m[["sp150_300"]]) < 0.03,
    perYear = abs(m[["perYear"]] / t[["perYear"]] - 1) <= 0.10, varOverMean = abs(m[["varOverMean"]] / t[["varOverMean"]] - 1) <= 0.30)
}
