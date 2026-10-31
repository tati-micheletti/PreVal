# Unit tests of the five-regime split builder and its gates, on a synthetic panel (no real data)
suppressMessages(library(data.table))
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
for (f in c("seeds.R", "caseIndex.R", "regimeSplits.R", "verifyRegimes.R")) source(file.path(here, "..", "R", f))
nFail <- 0L
check <- function(cond, msg) { ok <- isTRUE(cond); cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) nFail <<- nFail + 1L }

set.seed(42)
nUnit <- 400; years <- 2005:2020
units <- data.table(unit = sprintf("r%03d", 1:nUnit), x = runif(nUnit, 4e6, 4.8e6), y = runif(nUnit, 2.7e6, 3.5e6))
tab <- rbindlist(lapply(years, function(yy) { u <- units[runif(nUnit) < 0.45 + 0.02 * (yy - 2005)]; u[, year := yy]; u }))
setnames(tab, "unit", "SITE_ID")
idx <- buildCaseIndex(tab)
sp <- assignBlocks(idx, blockKm = 100, bufferKm = 20, nFolds = 4L, seed = 1L)

b <- buildRegimeSplits(idx, s = 2008, e = 2014, testYear = 2015, seed = 123L, spatial = sp, minTrainFraction = 0.5)
check(isTRUE(b$ok), paste("split builds:", paste(b$reason, collapse = "")))
check(setequal(names(b$splits), REGIMES), "all six regimes are present (including PreVal + spatial blocks)")
v <- verifyRegimeSplits(idx, b, 2008, 2014, 2015, "t1", sp)
check(all(v$ok), sprintf("all %d gate checks pass on a normal split", nrow(v)))
sz <- sapply(b$splits, function(m) c(sum(m$set == "train"), sum(m$set == "val"), sum(m$set == "test")))
check(all(apply(sz[, !colnames(sz) %in% c("FutureUnseenSpatial", "FutureUnseenGrouped")], 1, function(r) length(unique(r)) == 1)) && length(unique(sz[1, ])) == 1 && length(unique(sz[3, ])) == 1,
      "equal training and test size in all seven regimes, equal validation size in all but PreVal + spatial and PreVal on unseen routes")
gm <- b$splits$FutureUnseenGrouped; gv <- gm$row[gm$set == "val"]; gt <- gm$row[gm$set == "train"]
check(length(intersect(idx$unit[gt], idx$unit[gv])) == 0 && all(idx$year[gv] == 2014) && max(idx$year[gt]) <= 2013 && sz[2, "FutureUnseenGrouped"] == sz[2, "FutureUnseenSpatial"],
      "PreVal on unseen routes: validation = last-year cases of whole routes that are absent from training in EVERY year; same validation size as PreVal + spatial")
check(sum(b$splits$FutureTainted$set == "train") == sum(gm$set == "train") && sz[1, "FutureUnseenGrouped"] == b$sizes[["nTrain"]], "PreVal on unseen routes trains on the common training size")
cmb <- b$splits$FutureUnseenSpatial; vv <- cmb$row[cmb$set == "val"]
check(sz[2, "FutureUnseenSpatial"] < sz[2, "FutureUnseen"] && all(idx$year[vv] == 2014) && all(sp$key[vv] %in% b$spatialValBlocks) && b$valSizes[["FutureUnseenSpatial"]] == sz[2, "FutureUnseenSpatial"],
      "PreVal + spatial: smaller validation set = last-year cases inside the spatial regime's validation blocks")
check(!any(blockZone(idx, b$spatialValBlocks, sp)[cmb$row[cmb$set == "train"]]) && max(idx$year[cmb$row[cmb$set == "train"]]) <= 2013, "PreVal + spatial: training = earlier years outside those blocks and their buffer")
check(b$sizes[["nTrain"]] == min(b$poolSizes) && b$poolSizes[["FutureTaintedSpatial"]] <= b$poolSizes[["FutureTainted"]],
      "equal training size = the smallest pool (spatial blocking loses its buffer)")
b2 <- buildRegimeSplits(idx, s = 2008, e = 2014, testYear = 2015, seed = 123L, spatial = sp, minTrainFraction = 0.5)
check(identical(b$splits, b2$splits), "same seed gives identical splits")
b3 <- buildRegimeSplits(idx, s = 2008, e = 2014, testYear = 2015, seed = 124L, spatial = sp, minTrainFraction = 0.5)
check(!identical(b$splits$FutureTainted, b3$splits$FutureTainted), "a different seed gives different splits")

# full-pool design (caribou-style): only PreVal, status quo and Internal are built, nothing is trimmed, Internal draws only from units of the pool
bf <- buildRegimeSplits(idx, s = 2008, e = 2014, testYear = 2015, seed = 123L, spatial = sp, regimes = c("FutureUnseen", "FutureTainted", "Internal"), matchUnits = TRUE)
check(isTRUE(bf$ok) && setequal(names(bf$splits), c("FutureUnseen", "FutureTainted", "Internal")), "full-pool design builds PreVal, status quo and Internal only")
szf <- sapply(bf$splits, function(m) c(sum(m$set == "train"), sum(m$set == "val"), sum(m$set == "test")))
bu <- buildRegimeSplits(idx, s = 2008, e = 2014, testYear = 2015, seed = 123L, spatial = sp, minTrainFraction = 0.5)
check(bf$sizes[["nTrain"]] == bf$poolSizes[["FutureTainted"]] && bf$sizes[["nTrain"]] > bu$sizes[["nTrain"]] && all(apply(szf, 1, function(r) length(unique(r)) == 1)),
      "full-pool design: training = the whole PreVal / status-quo pool (not trimmed to the spatial pool), equal sizes in all three regimes")
check(all(verifyRegimeSplits(idx, bf, 2008, 2014, 2015, "full", sp)$ok), "gates pass on the full-pool split, including the matched-Internal check")
itu <- idx$unit[bf$splits$Internal$row[bf$splits$Internal$set != "test"]]; Upl <- idx$unit[which(idx$year >= 2008 & idx$year <= 2014)]
check(all(itu %in% Upl), "matchUnits: Internal uses only units that are in the pool of years s..e")
# validation share of PreVal on unseen units (unseenValShare, 2026-10-31): default 0.15 = the old behaviour, 0.30 doubles the validation set
rg3 <- c("FutureUnseen", "FutureTainted", "FutureUnseenGrouped", "Internal")
bd <- buildRegimeSplits(idx, s = 2008, e = 2014, testYear = 2015, seed = 123L, spatial = sp, regimes = rg3, matchUnits = TRUE)
bd15 <- buildRegimeSplits(idx, s = 2008, e = 2014, testYear = 2015, seed = 123L, spatial = sp, regimes = rg3, matchUnits = TRUE, unseenValShare = 0.15)
bd30 <- buildRegimeSplits(idx, s = 2008, e = 2014, testYear = 2015, seed = 123L, spatial = sp, regimes = rg3, matchUnits = TRUE, unseenValShare = 0.30)
nV <- bd$sizes[["nVal"]]
check(identical(bd$splits, bd15$splits) && bd$valSizes[["FutureUnseenGrouped"]] == max(10L, floor(0.15 * nV)), "unseenValShare: the default (0.15) gives the same splits as before the parameter existed")
check(isTRUE(bd30$ok) && bd30$valSizes[["FutureUnseenGrouped"]] == max(10L, floor(0.30 * nV)) && bd30$valSizes[["FutureUnseenGrouped"]] > bd15$valSizes[["FutureUnseenGrouped"]], "unseenValShare 0.30: validation of PreVal on unseen units = 30% of the validation-year cases")
check(all(verifyRegimeSplits(idx, bd30, 2008, 2014, 2015, "v30", sp)$ok) && bd30$sizes[["nTrain"]] <= bd15$sizes[["nTrain"]], "gates pass with a larger share; the common training size can only shrink")
yrOf <- function(m, st) idx$year[m$row[m$set == st]]
check(max(yrOf(b$splits$FutureUnseen, "train")) < min(yrOf(b$splits$FutureUnseen, "val")), "PreVal: validation strictly later than training")
check(max(yrOf(b$splits$FutureTainted, "train")) >= min(yrOf(b$splits$FutureTainted, "val")), "status quo: validation years overlap training years")
g <- b$splits$FutureTaintedGrouped
check(length(intersect(idx$unit[g$row[g$set == "train"]], idx$unit[g$row[g$set == "val"]])) == 0, "grouped: no route in train and validation")
tn <- b$splits$FutureTainted
check(length(intersect(idx$unit[tn$row[tn$set == "train"]], idx$unit[tn$row[tn$set == "val"]])) > 0,
      "status quo: the same routes sit in train and validation (the leak)")

# the gates detect tampering
tamper <- function(regime, mutate) { bad <- b; bad$splits[[regime]] <- copy(bad$splits[[regime]]); mutate(bad$splits[[regime]]); bad }
caught <- function(bad, id) inherits(try(verifyRegimeSplits(idx, bad, 2008, 2014, 2015, id, sp), silent = TRUE), "try-error")
bad <- tamper("FutureTainted", function(m) m[which(set == "train")[1], row := m$row[which(set == "val")[1]]])
check(caught(bad, "t2"), "gate stops on a case in two sets")
vrow <- g[set == "val"]$row[1]
sameUnit <- setdiff(which(idx$unit == idx$unit[vrow] & idx$year >= 2008 & idx$year <= 2014), vrow)[1]
if (!is.na(sameUnit)) {
  bad <- tamper("FutureTaintedGrouped", function(m) m[which(set == "train")[1], row := sameUnit])
  check(caught(bad, "t3"), "gate stops on a route in train and validation (grouped)")
}
bad <- tamper("Internal", function(m) m[which(set == "train")[1], row := m$row[which(set == "test")[1]]])
check(caught(bad, "t4"), "gate stops on a test case inside the Internal training set")
bad <- tamper("FutureTaintedSpatial", function(m) { vb <- unique(sp$key[m$row[m$set == "val"]])
  z <- blockZone(idx, vb, sp); cand <- setdiff(which(z & idx$year >= 2008 & idx$year <= 2014), m$row)[1]
  m[which(set == "train")[1], row := cand] })
check(caught(bad, "t5"), "gate stops on a training case inside a validation block or its buffer (spatial)")

check(!buildRegimeSplits(idx, 2008, 2014, 2015, seed = 1L, spatial = sp, minTrainFraction = 0.999)$ok, "split is skipped when blocking removes too much training data")
check(!buildRegimeSplits(idx, 2008, 2014, 2015, seed = 1L, spatial = sp, minTest = 10000L)$ok, "split is skipped when the test set is too small")

shares <- sapply(1:200, function(k) {
  bb <- buildRegimeSplits(idx, 2008, 2014, 2015, seed = k, spatial = sp, regimes = c("FutureUnseen", "FutureTainted"))
  mean(idx$year[bb$splits$FutureTainted[set == "val"]$row] == 2014) })
poolShare <- mean(idx$year[idx$year >= 2008 & idx$year <= 2014] == 2014)
check(abs(mean(shares) - poolShare) < 0.01, sprintf("status-quo validation draw is unbiased by year (%.3f vs pool %.3f)", mean(shares), poolShare))
# regression: the uniformity gate must not raise false alarms when blocking leaves the draw close to the whole pool
bad <- 0L
for (sd in 1:60) for (e in c(2010, 2014, 2018)) {
  bb <- buildRegimeSplits(idx, s = 2006, e = e, testYear = e + 1, seed = sd, spatial = sp, minTrainFraction = 0.5)
  if (isTRUE(bb$ok) && inherits(try(verifyRegimeSplits(idx, bb, 2006, e, e + 1, "reg", sp), silent = TRUE), "try-error")) bad <- bad + 1L }
check(bad == 0L, sprintf("no false alarms of the uniformity gate in 180 random splits (%d)", bad))
cat(sprintf("\n%d failure(s)\n", nFail)); if (nFail) quit(status = 1)
