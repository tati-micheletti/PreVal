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

b <- buildRegimeSplits(idx, s = 2008, e = 2014, testYear = 2015, seed = 123L, spatial = sp)
check(isTRUE(b$ok), paste("split builds:", paste(b$reason, collapse = "")))
check(setequal(names(b$splits), REGIMES), "all five regimes are present")
v <- verifyRegimeSplits(idx, b, 2008, 2014, 2015, "t1", sp)
check(all(v$ok), sprintf("all %d gate checks pass on a normal split", nrow(v)))
sz <- sapply(b$splits, function(m) c(sum(m$set == "train"), sum(m$set == "val"), sum(m$set == "test")))
check(all(apply(sz, 1, function(r) length(unique(r)) == 1)), "equal train, validation and test sizes in all five regimes")
check(b$sizes[["nTrain"]] == min(b$poolSizes) && b$poolSizes[["FutureTaintedSpatial"]] <= b$poolSizes[["FutureTainted"]],
      "equal training size = the smallest pool (spatial blocking loses its buffer)")
b2 <- buildRegimeSplits(idx, s = 2008, e = 2014, testYear = 2015, seed = 123L, spatial = sp)
check(identical(b$splits, b2$splits), "same seed gives identical splits")
b3 <- buildRegimeSplits(idx, s = 2008, e = 2014, testYear = 2015, seed = 124L, spatial = sp)
check(!identical(b$splits$FutureTainted, b3$splits$FutureTainted), "a different seed gives different splits")

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
  bb <- buildRegimeSplits(idx, s = 2006, e = e, testYear = e + 1, seed = sd, spatial = sp)
  if (isTRUE(bb$ok) && inherits(try(verifyRegimeSplits(idx, bb, 2006, e, e + 1, "reg", sp), silent = TRUE), "try-error")) bad <- bad + 1L }
check(bad == 0L, sprintf("no false alarms of the uniformity gate in 180 random splits (%d)", bad))
cat(sprintf("\n%d failure(s)\n", nFail)); if (nFail) quit(status = 1)
