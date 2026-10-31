# New-animals arm: test strata of held-out animals that no regime ever trains or validates on (synthetic panel, no real data)
suppressMessages(library(data.table))
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
for (f in c("seeds.R", "caseIndex.R", "regimeSplits.R", "verifyRegimes.R", "spatialTestSplits.R", "newAnimalsSplits.R", "generatePlan.R", "rankCovariates.R")) source(file.path(here, "..", "R", f))
nFail <- 0L
check <- function(cond, msg) { ok <- isTRUE(cond); cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) nFail <<- nFail + 1L }

set.seed(21); nUnit <- 120; years <- 2005:2016
units <- data.table(SITE_ID = sprintf("a%03d", 1:nUnit), x = runif(nUnit, 4e6, 4.5e6), y = runif(nUnit, 2.7e6, 3.2e6))
tab <- rbindlist(lapply(years, function(yy) { u <- units[runif(nUnit) < 0.45 + 0.02 * (yy - 2005)]; u <- u[rep(seq_len(.N), sample(5:15, .N, TRUE))]; u[, year := yy]; u }))
tab[, `:=`(a = rnorm(.N), count = rpois(.N, 3))]
# caribou-like case index: several strata per animal and year (buildCaseIndex allows one case per unit and year, as for the birds)
idx <- data.table(row = seq_len(nrow(tab)), unit = tab$SITE_ID, year = as.integer(tab$year), x = tab$x, y = tab$y)

# held-out animals: a function of the window only, about one fold of four
h1 <- newAnimalsHeldOut(idx, 2007, 2012); h2 <- newAnimalsHeldOut(idx, 2007, 2012); h3 <- newAnimalsHeldOut(idx, 2008, 2012)
nAn <- uniqueN(idx$unit[idx$year >= 2007])
check(identical(h1, h2) && !identical(h1, h3) && abs(length(h1) / nAn - 0.25) < 0.05, sprintf("held-out animals: deterministic, a function of the window (s, e) only, about 25%% of the animals (%.2f)", length(h1) / nAn))

b1 <- buildNewAnimalsSplits(idx, 2007, 2012, 2013, seed = 11L, minTrain = 100L, minVal = 20L, minTest = 30L)
b2 <- buildNewAnimalsSplits(idx, 2007, 2012, 2015, seed = 12L, minTrain = 100L, minVal = 20L, minTest = 30L)
check(isTRUE(b1$ok) && isTRUE(b2$ok) && setequal(names(b1$splits), NEWANIMALS_REGIMES), paste("two splits of one window build the three regimes:", paste(b1$reason, b2$reason, collapse = "")))
check(identical(b1$heldAnimals, b2$heldAnimals), "the held-out animals are the same for every forecast year of a window")
v1 <- verifyNewAnimalsSplits(idx, b1, 2007, 2012, 2013, "t1"); v2 <- verifyNewAnimalsSplits(idx, b2, 2007, 2012, 2015, "t2")
check(all(v1$ok) && all(v2$ok), sprintf("all %d gate checks pass on both splits", nrow(v1) + nrow(v2)))

# independent of the gate: no animal of the test set is in ANY pool of ANY regime, in any year
un <- function(b, rg, st) as.character(idx$unit[b$splits[[rg]][set == st]$row])
tu <- unique(un(b1, "FutureTainted", "test"))
leak <- vapply(NEWANIMALS_REGIMES, function(rg) any(c(un(b1, rg, "train"), un(b1, rg, "val")) %in% tu), logical(1))
check(!any(leak) && all(tu %in% b1$heldAnimals) && all(idx$year[b1$splits$FutureTainted[set == "test"]$row] == 2013), "no test animal appears in the training or validation set of any regime; test strata are in the forecast year")
# the animals are new in every YEAR, also before the window: the pools hold no stratum of a held-out animal at all
check(!any(vapply(NEWANIMALS_REGIMES, function(rg) any(idx$unit[c(b1$splits[[rg]][set != "test"]$row)] %in% b1$heldAnimals), logical(1))), "no stratum of a held-out animal (any year) is in any pool")
sz <- vapply(b1$splits, function(m) c(sum(m$set == "train"), sum(m$set == "val"), sum(m$set == "test")), numeric(3))
check(all(apply(sz, 1, function(r) length(unique(r)) == 1)) && sz[2, 1] == b1$sizes[["nVal"]], sprintf("equal training, validation and test size in all three regimes (%s)", paste(sz[, 1], collapse = "/")))
pv <- b1$splits$FutureUnseenGrouped; gc <- b1$splits$FutureTaintedGrouped
check(!length(intersect(un(b1, "FutureUnseenGrouped", "train"), un(b1, "FutureUnseenGrouped", "val"))) && all(idx$year[pv[set == "val"]$row] == 2012) && max(idx$year[pv[set == "train"]$row]) <= 2011,
      "PreVal: validation = last-year strata of animals absent from training in every year; training = earlier years")
check(!length(intersect(un(b1, "FutureTaintedGrouped", "train"), un(b1, "FutureTaintedGrouped", "val"))) && min(idx$year[gc[set == "val"]$row]) < 2012, "animal-grouped CV: whole animals in validation (all years), none in training")
check(length(intersect(un(b1, "FutureTainted", "train"), un(b1, "FutureTainted", "val"))) > 0, "status quo: the same animals sit in training and validation (the leak)")

# the gates detect tampering
bad <- b1; bad$splits$FutureTainted <- copy(bad$splits$FutureTainted)
hRow <- which(as.character(idx$unit) %in% b1$heldAnimals & idx$year == 2010)[1]; bad$splits$FutureTainted[which(set == "train")[1], row := hRow]
check(inherits(try(verifyNewAnimalsSplits(idx, bad, 2007, 2012, 2013, "t3"), silent = TRUE), "try-error"), "gate stops on a training stratum of a held-out animal")
bad <- b1; bad$splits$FutureUnseenGrouped <- copy(bad$splits$FutureUnseenGrouped)
vu <- un(b1, "FutureUnseenGrouped", "val")[1]; stranger <- which(as.character(idx$unit) == vu & idx$year == 2009 & !as.character(idx$unit) %in% b1$heldAnimals)[1]
if (!is.na(stranger)) { bad$splits$FutureUnseenGrouped[which(set == "train")[1], row := stranger]; check(inherits(try(verifyNewAnimalsSplits(idx, bad, 2007, 2012, 2013, "t4"), silent = TRUE), "try-error"), "gate stops on a validation animal that is also in PreVal's training") }

# ranking: the held-out animals never enter the pool of the ranking
pool0 <- .rankPool(idx, 2007:2011); pool1 <- .rankPool(idx, 2007:2011, excludeUnits = b1$heldAnimals)
check(identical(pool0, .rankPool(idx, 2007:2011, NULL)) && length(pool1) < length(pool0) && !any(as.character(idx$unit[pool1]) %in% b1$heldAnimals) && all(idx$year[pool1] %in% 2007:2011) && length(pool0) == sum(idx$year %in% 2007:2011),
      "ranking pool: without excludeUnits it is all cases of the years (as before); with it the held-out animals are gone")

# the whole chain: plan, manifests, post-hoc audit
pl <- generatePlan(idx, startYear = 2005, endYear = 2016, minTrainYears = 3, horizons = 1:3, covariateLevels = c(2, 4), arm = "newAnimals", unseenValShare = 0.30,
                   minTrain = 100, minVal = 20, minTest = 30, minTrainFraction = 0.3)
check(nrow(pl$plan) > 0 && setequal(unique(pl$plan$typeValidation), NEWANIMALS_REGIMES) && all(pl$plan$arm == "newAnimals") && all(pl$checks$ok), sprintf("generatePlan(arm = newAnimals): %d models, %d splits, all gates pass", nrow(pl$plan), uniqueN(pl$plan$splitId)))
dir <- file.path(tempdir(), "na"); dir.create(dir); writeManifests(pl$bundles, dir)
au <- auditManifests(idx, pl$plan, dir, pl$spatial)
check(all(au$ok) && any(grepl("^N3", au$check)), "the post-hoc audit of the manifests uses the new-animals gates and passes")
check(all(vapply(pl$bundles, function(b) all(b$valSizes == b$valSizes[1]), logical(1))), "plan: validation size is the same in the three regimes of every split")
cat(sprintf("\n%d failure(s)\n", nFail)); if (nFail) quit(status = 1)
