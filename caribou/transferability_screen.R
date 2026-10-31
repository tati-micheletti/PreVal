# Covariate transferability screen for CARIBOU (section 4 of caribou/ANALYSIS_PLAN_CARIBOU_NN.md). Descriptive only, run BEFORE any engine run.
# Question: which of the 30 candidate covariates keep their effect on step selection from year to year, and where do the effects change sign?
#
# Input  : the prepared model table (output of caribouNN_Global::prepareNNdata, i.e. `preparedDataFinal`: 11 rows per stratum, observed step first, years >= 2013,
#          disturbance clocks already recoded to the cap of 40 years, engineered covariates + interactions, UNSCALED). Default results_firstpass/preparedModelInputs.csv.
#          The recode rules are therefore NOT re-implemented here; the script checks that the recode was applied (cap of the clocks) and stops if not.
#          The only thing copied from prepareNNdata is the z-scaling rule of its `scaledCol` (global mean/sd over all rows, clipped to +-zClip = 10), marked below.
# Order  : the global importance ranking of the first caribou pass (results_firstpass/featureTable.csv, columns Feature, Importance).
# Model  : per year and per covariate, a conditional logit (used step versus its 10 available steps; strata = stratum id) on the standardised covariate, with
#          animal-clustered robust standard errors (survival::coxph with strata + cluster; with one event per stratum this IS the conditional logit),
#          and ONE joint model per year with all identifiable covariates. Covariates that are constant within a stratum (the *_start main effects) cancel out of a
#          conditional logit; they are reported as "not identifiable" and enter only through their interactions with logSl.
# Variants: all   = all animals;  ge3yrs = only animals with strata in >= 3 different years (separates a change in time from a change of animals);
#           modalfix = only strata with the commonest fix interval (default 8 h +-0.5 h; separates ecology from a change of the collar schedule).
# Output : <out>/covariate_transferability_caribou.csv (one row per covariate x model x variant) and companions (by year, top-k flip shares, fix rate, composition, groups).
# Settings (environment): CARIBOU_PREPARED, CARIBOU_FEATURETABLE, CARIBOU_OUT (default results), CARIBOU_CORES (default min(8, cores - 1)), CARIBOU_SCRATCH (temp files).
# Definitions (fixed here, before any engine result exists):
#   flip (primary)  = at least 3 of the years have the sign opposite to the majority sign;   flip (strict) = at least one year significantly positive AND one significantly negative (|z| > 1.96);
#   unstable        = I2 > 0.75 (Cochran Q on the yearly estimates, inverse-variance weights from the robust SE);
#   independent group = a covariate family with at least one flipping covariate; all interactions with step length are counted with the movement family (they are tied to logSl by construction).
suppressMessages({library(data.table); library(survival); library(parallel)})
envChr <- function(name, default) { v <- Sys.getenv(name, ""); if (nzchar(v)) v else default }
here <- tryCatch(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]))), error = function(e) ".")
root <- normalizePath(file.path(here, ".."), mustWork = FALSE); if (!file.exists(file.path(root, "results_firstpass")) && file.exists("results_firstpass")) root <- normalizePath(".")
prepared <- envChr("CARIBOU_PREPARED", file.path(root, "results_firstpass", "preparedModelInputs.csv"))
ftPath <- envChr("CARIBOU_FEATURETABLE", file.path(root, "results_firstpass", "featureTable.csv"))
outDir <- envChr("CARIBOU_OUT", file.path(root, "results")); dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
nCores <- as.integer(envChr("CARIBOU_CORES", as.character(max(1L, min(8L, parallel::detectCores() - 1L))))); zClip <- 10; steps <- 11L; minYears <- 3L
logf <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")
stopifnot(file.exists(prepared), file.exists(ftPath))

# ---- 1. read: candidate list in importance order, then only the columns that are needed ----
ft <- fread(ftPath); setorder(ft, -Importance); cand <- ft$Feature; imp <- setNames(ft$Importance, ft$Feature)
hdr <- names(fread(prepared, nrows = 0)); miss <- setdiff(cand, hdr); if (length(miss)) stop("candidates missing from the prepared table: ", paste(miss, collapse = ", "))
keep <- unique(c("indiv_step_id", "case_", "id", "year", "jurisdiction", "subpop", "dt_", "sl_", "ta_", cand))
logf("reading", basename(prepared), "(", length(keep), "columns )"); d <- fread(prepared, select = intersect(keep, hdr))
stopifnot(nrow(d) %% steps == 0, all(d$case_[seq.int(1L, nrow(d), by = steps)]), sum(d$case_) == nrow(d) / steps)           # contiguous blocks of 11, observed step first
d[, stratum := rep(seq_len(nrow(d) / steps), each = steps)]
# the disturbance clocks must already be recoded (calendar-year counters would be ~ year - 1900 = 113..122 -> log about 4.7): cap of 40 years -> log(41) = 3.714
clk <- intersect(c("timeSinceFire_endLog", "timeSinceHarvest_endLog", "timeSinceFire_startLog", "timeSinceHarvest_startLog"), names(d))
if (length(clk) && any(vapply(clk, function(v) max(d[[v]]) > log(41) + 1e-6, logical(1)))) stop("the disturbance clocks are not capped at 40 years: this table was not made by prepareNNdata with the recode")
logf("rows", nrow(d), "| strata", d[, uniqueN(stratum)], "| years", paste(range(d$year), collapse = "-"), "| animals", d[, uniqueN(id)], "| clocks capped (recode applied)")

# ---- 2. identifiable covariates and the z-scaling (rule copied from prepareNNdata::scaledCol: global mean/sd, clipped to +-zClip) ----
first <- seq.int(1L, nrow(d), by = steps)
within <- vapply(cand, function(v) { x <- d[[v]]; any(x != rep(x[first], each = steps)) }, logical(1))
ident <- cand[within]; logf("identifiable (vary within a stratum):", length(ident), "of", length(cand), "| not identifiable:", paste(cand[!within], collapse = ", "))
for (v in cand) { x <- d[[v]]; s <- sd(x); set(d, j = v, value = if (!is.finite(s) || s == 0) rep(0, length(x)) else pmax(pmin((x - mean(x)) / s, zClip), -zClip)) }
family <- function(v) ifelse(grepl("^inter_|^logSl$|^cosTa$|^sinTa$", v), "movement (step length, turn angle, interactions)",
                      ifelse(grepl("^prop_", v), "land cover", ifelse(grepl("^timeSince", v), "disturbance clocks", "distance to infrastructure")))
fam <- setNames(vapply(cand, family, ""), cand)

# ---- 3. observed-step summaries per year (data checks a and b) ----
obs <- d[case_ == TRUE]
animalYears <- obs[, .(nY = uniqueN(year)), by = id]; ge3 <- animalYears[nY >= minYears, id]
modalDt <- obs[, .N, by = .(dtr = round(dt_))][order(-N)][1, dtr]; logf("modal fix interval:", modalDt, "h | animals present in >=", minYears, "years:", length(ge3), "of", nrow(animalYears))
fix <- obs[, .(nStrata = .N, nAnimals = uniqueN(id), meanFixHours = mean(dt_), share4h = mean(abs(dt_ - 4) <= 0.5), share8h = mean(abs(dt_ - 8) <= 0.5), shareOther = mean(abs(dt_ - 4) > 0.5 & abs(dt_ - 8) > 0.5),
            medianStepM = median(sl_), meanLogSl = mean(log(sl_ + 1)), meanAbsTurn = mean(abs(ta_))), by = year][order(year)]
firstYear <- obs[, .(first = min(year)), by = id]; setkey(firstYear, id)
comp <- obs[, .(nStrata = .N, nAnimals = uniqueN(id), newAnimals = sum(!unique(id) %in% obs[year < .BY$year, id]),
                topJurisdiction = names(sort(table(jurisdiction), decreasing = TRUE))[1], shareTopJurisdiction = max(table(jurisdiction)) / .N,
                nSubpop = uniqueN(subpop)), by = year][order(year)]
comp[, `:=`(shareNew = newAnimals / nAnimals, note = ifelse(year == min(year), "first year: every animal is new", ""))]
fwrite(fix, file.path(outDir, "covariate_transferability_caribou_check_fixrate.csv")); fwrite(comp, file.path(outDir, "covariate_transferability_caribou_check_composition.csv"))

# ---- 4. yearly conditional logits ----
fitOne <- function(dd, vars) {                                   # one conditional logit; returns coefficients and robust SE
  n <- nrow(dd); df <- data.frame(tm = rep(1, n), ev = as.integer(dd$case_), st = dd$stratum, an = dd$id)
  for (v in vars) df[[v]] <- dd[[v]]
  f <- as.formula(paste("Surv(tm, ev) ~", paste0("`", vars, "`", collapse = " + "), "+ strata(st) + cluster(an)"))
  fit <- try(coxph(f, data = df, ties = "efron", control = coxph.control(iter.max = 40)), silent = TRUE)
  if (inherits(fit, "try-error") || any(!is.finite(coef(fit)))) return(data.table(covariate = vars, beta = NA_real_, se = NA_real_))
  data.table(covariate = vars, beta = unname(coef(fit)), se = unname(sqrt(diag(fit$var))))
}
runTask <- function(task) {
  dd <- task$data; out <- list()
  for (v in task$single) out[[length(out) + 1L]] <- cbind(fitOne(dd, v), model = "single")
  out[[length(out) + 1L]] <- cbind(fitOne(dd, task$single), model = "joint")
  r <- rbindlist(out); r[, `:=`(year = task$year, variant = task$variant, nStrata = nrow(dd) / steps, nAnimals = uniqueN(dd$id))]; r
}
years <- sort(unique(d$year)); needed <- c("case_", "stratum", "id", "dt_", ident)
tasks <- list()
for (yy in years) {
  dy <- d[year == yy, needed, with = FALSE]
  tasks[[length(tasks) + 1L]] <- list(year = yy, variant = "all", data = dy, single = ident)
  tasks[[length(tasks) + 1L]] <- list(year = yy, variant = "ge3yrs", data = dy[id %in% ge3], single = ident)
  tasks[[length(tasks) + 1L]] <- list(year = yy, variant = "modalfix", data = dy[abs(dt_ - modalDt) <= 0.5], single = ident)
}
tasks <- tasks[order(-vapply(tasks, function(t) nrow(t$data), numeric(1)))]; rm(d); invisible(gc())
logf("fitting", length(tasks), "year x variant tasks on", nCores, "cores (", length(ident), "single models + 1 joint model each )")
cl <- makeCluster(nCores); on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
clusterEvalQ(cl, suppressMessages({library(data.table); library(survival)}))
clusterExport(cl, c("fitOne", "steps"), envir = environment())
res <- rbindlist(parLapplyLB(cl, tasks, runTask)); stopCluster(cl)
res[, z := beta / se]; fwrite(res, file.path(outDir, "covariate_transferability_caribou_byYear.csv")); logf("fits done:", nrow(res), "year-level estimates;", sum(is.na(res$beta)), "failed")

# ---- 5. heterogeneity across years ----
het <- function(b, s, yr) {
  ok <- is.finite(b) & is.finite(s) & s > 0; b <- b[ok]; s <- s[ok]; n <- length(b); if (n < 3) return(NULL)
  w <- 1 / s^2; m <- sum(w * b) / sum(w); Q <- sum(w * (b - m)^2); df <- n - 1; z <- b / s
  nPos <- sum(b > 0); nNeg <- sum(b < 0); opp <- min(nPos, nNeg)
  data.table(nYears = n, meanBeta = m, minBeta = min(b), maxBeta = max(b), Q = Q, df = df, pHeterogeneity = pchisq(Q, df, lower.tail = FALSE), I2 = max(0, (Q - df) / Q),
             nPositive = nPos, nNegative = nNeg, oppositeSignYears = opp, sigPositiveYears = sum(z > 1.96), sigNegativeYears = sum(z < -1.96))
}
H <- res[, het(beta, se, year), by = .(covariate, model, variant)]
H[, `:=`(rankByImportance = match(covariate, cand), importance = imp[covariate], family = fam[covariate],
         flipPrimary = oppositeSignYears >= 3L, flipStrict = sigPositiveYears >= 1L & sigNegativeYears >= 1L, unstable = I2 > 0.75)]
setorder(H, variant, model, rankByImportance)
nonId <- data.table(covariate = cand[!within], rankByImportance = match(cand[!within], cand), importance = imp[cand[!within]], family = fam[cand[!within]], model = "single", variant = "all",
                    note = "not identifiable in a conditional logit (constant within a stratum); acts only through its interaction with logSl")
H <- rbindlist(list(H, nonId), fill = TRUE); fwrite(H, file.path(outDir, "covariate_transferability_caribou.csv"))

# ---- 6. flip share in the top k of the importance order (identifiable covariates only) ----
topk <- rbindlist(lapply(c(2, 5, 10, 20, 30), function(k) {
  top <- cand[seq_len(min(k, length(cand)))]; top <- top[top %in% ident]
  H[model == "single" & covariate %in% top, .(k = k, nIdentifiable = .N, nFlipPrimary = sum(flipPrimary), shareFlipPrimary = mean(flipPrimary), nFlipStrict = sum(flipStrict), shareFlipStrict = mean(flipStrict),
                                              nUnstable = sum(unstable), shareUnstable = mean(unstable), meanI2 = mean(I2)), by = variant]
}))
setorder(topk, variant, k); fwrite(topk, file.path(outDir, "covariate_transferability_caribou_topk.csv"))

# ---- 7. independent flipping groups, and the checks ----
grp <- H[model == "single" & !is.na(flipPrimary), .(nCovariates = .N, nFlipPrimary = sum(flipPrimary), nFlipStrict = sum(flipStrict), nUnstable = sum(unstable), flippers = paste(covariate[flipPrimary], collapse = "; ")), by = .(variant, family)]
grpTot <- grp[, .(independentGroupsFlipping = sum(nFlipPrimary > 0), independentGroupsFlippingStrict = sum(nFlipStrict > 0), rawFlippingCovariates = sum(nFlipPrimary)), by = variant]
fwrite(grp, file.path(outDir, "covariate_transferability_caribou_check_groups.csv")); fwrite(grpTot, file.path(outDir, "covariate_transferability_caribou_check_groups_total.csv"))
# (a) does the yearly step-length effect follow the collar schedule? Spearman over years (all strata) and the same effect inside the commonest fix interval
lg <- merge(res[covariate == "logSl" & model == "single" & variant == "all", .(year, betaLogSl = beta)], fix[, .(year, meanFixHours, share8h, meanLogSl)], by = "year")
fixCor <- data.table(check = c("yearly logSl effect vs mean fix interval (Spearman)", "yearly logSl effect vs share of 8-h fixes (Spearman)", "yearly logSl effect vs mean observed log step length (Spearman)"),
                     rho = c(cor(lg$betaLogSl, lg$meanFixHours, method = "spearman"), cor(lg$betaLogSl, lg$share8h, method = "spearman"), cor(lg$betaLogSl, lg$meanLogSl, method = "spearman")), nYears = nrow(lg))
fwrite(fixCor, file.path(outDir, "covariate_transferability_caribou_check_fixrate_cor.csv"))
cmp <- dcast(H[model == "single" & covariate %in% c("logSl", "cosTa", "sinTa") , .(covariate, variant, I2, oppositeSignYears, meanBeta)], covariate ~ variant, value.var = c("I2", "oppositeSignYears", "meanBeta"))
fwrite(cmp, file.path(outDir, "covariate_transferability_caribou_check_movement_variants.csv"))
logf("top-k flip share (primary definition, variant all):"); print(topk[variant == "all", .(k, nIdentifiable, nFlipPrimary, shareFlipPrimary = round(shareFlipPrimary, 2), nFlipStrict, nUnstable)])
logf("independent flipping groups:"); print(grpTot); logf("fix-rate checks:"); print(fixCor); logf("finished; outputs in", outDir)
